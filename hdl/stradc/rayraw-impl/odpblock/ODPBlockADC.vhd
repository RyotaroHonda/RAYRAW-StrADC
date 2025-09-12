library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_MISC.ALL;
use IEEE.numeric_std.all;

Library xpm;
use xpm.vcomponents.all;

library mylib;
use mylib.defDataBusAbst.all;
use mylib.defDelimiter.all;
use mylib.defADC.all;
use mylib.defLaccp.all;

use mylib.defYaenamiAdc.all;
use mylib.defRayrawStrAdcROV1.all;

entity ODPBlockADC is
  generic(
    kNumInput     : integer:= 32;
    enDEBUG       : boolean := false
  );
  port(
    -- System --
    rst             : in  std_logic;
    rstAdc          : in  std_logic;
    rstAdcCycle     : in  std_logic;
    sysClk          : in  std_logic;
    indepClk        : in  std_logic;
    --userReg         : in  std_logic_vector(kPosHbdUserReg'length-1 downto 0);
    LaccpFineOffset : in signed(kWidthLaccpFineOffset-1 downto 0);

    -- Control registers --
    adcMask         : in  std_logic_vector(kNumInput-1 downto 0);

    enBypassDelay   : in  std_logic;
    enBLSuppressor  : in  std_logic;
    polBLSuppressor : in  std_logic;
    thBLSuppressor  : in  std_logic_vector(kNumAdcBit-1 downto 0);

    -- Data flow control --
    daqOn           : in  std_logic;
    hbfThrottlingOn : in  std_logic;
    triggerGate     : in  std_logic;

    -- heartbeat count for TDC
    hbCount         : in  std_logic_vector(kWidthStrHbc-1 downto 0);

    -- delimiter --
    validDelimiter  : in  std_logic;
    dInDelimiter    : in  std_logic_vector(kWidthData-1 downto 0);

    -- ADC In --
    adcDClkP      : in std_logic_vector(kNumAsicBlock-1 downto 0);  -- ADC DCLK (forwarded fast clock)
    adcDClkN      : in std_logic_vector(kNumAsicBlock-1 downto 0);
    adcDataP      : in std_logic_vector(kNumAsicBlock*kNumAdcCh-1 downto 0); -- ADC DATA
    adcDataN      : in std_logic_vector(kNumAsicBlock*kNumAdcCh-1 downto 0);
    adcFrameP     : in std_logic_vector(kNumAsicBlock-1 downto 0);  -- ADC FRAME
    adcFrameN     : in std_logic_vector(kNumAsicBlock-1 downto 0);

    -- Data Out --
    validOut        : out std_logic_vector(kNumInput-1 downto 0);
    dOut            : out DataArrayType(kNumInput-1 downto 0)
    --dOut            : out DataArrayType(kNumInput-1 downto 0)(kWidthData-1 downto 0)

  );
end ODPBlockADC;

architecture RTL of ODPBlockADC is
  attribute mark_debug : boolean;

  -- System --
  signal sync_reset    : std_logic;
  signal daq_off_reset : std_logic;

  -- Signal decralation ---------------------------------------------
  -- ADC unit --
  signal is_ready_asic      : std_logic_vector(kNumAsicBlock-1 downto 0);
  signal bitslip_err_asic   : std_logic_vector(kNumAsicBlock-1 downto 0);

  attribute mark_debug of is_ready_asic      : signal is enDEBUG;
  attribute mark_debug of bitslip_err_asic   : signal is enDEBUG;

  signal valid_adc_data : std_logic_vector(kNumAsicBlock-1 downto 0);
  signal adc_data       : PackedAdcBlockArray;
  type DebugDataType is array (integer range 0 to 31) of std_logic_vector(9 downto 0);
  signal debug_adc_data : DebugDataType;

  signal valid_data     : std_logic;
  signal valid_data_mask : std_logic_vector(kNumInput -1 downto 0);
  signal adc_data_in    : AdcArrayType(kNumInput-1 downto 0)(kWidthPackedAdc-1 downto 0);

  attribute mark_debug of valid_adc_data      : signal is enDEBUG;
  --attribute mark_debug of debug_adc_data      : signal is enDEBUG;
  --attribute mark_debug of adc_data      : signal is enDEBUG;

  -- ADC delay buffer --
  signal valid_data_delay    : std_logic_vector(kNumInput -1 downto 0);
  signal adc_data_delay      : AdcArrayType(kNumInput-1 downto 0)(kWidthPackedAdc-1 downto 0);

  -- BL suppressor --
  signal valid_data_bls       : std_logic_vector(kNumInput -1 downto 0);
  signal adc_data_bls         : AdcArrayType(kNumInput-1 downto 0)(kWidthPackedAdc-1 downto 0);
  type SfCType is array (integer range 0 to kNumInput-1) of std_logic_vector(kWidthAdcSFType-1 downto 0);
  signal sf_type_out          : SfCType;

  -- Data formatter --
  signal dout_formatter       : DataArrayType(kNumInput-1 downto 0);
  signal valid_data_formatter : std_logic_vector(kNumInput -1 downto 0);
  signal sf_timestamp         : signed(kPosSfhTime'length -1 downto 0);
  signal long_hbcount         : signed(kPosSfhTime'length -1 downto 0);
  signal extended_ofs         : signed(kPosSfhTime'length -1 downto 0);

  -- Trigger emulation --
  signal valid_data_trigger     : std_logic_vector(kNumInput -1 downto 0);

  -- Delimiter inserter --
  signal valid_inserter         : std_logic_vector(kNumInput -1 downto 0);
  signal dout_inserter          : DataArrayType(kNumInput-1 downto 0);
  --signal dout_inserter          : DataArrayType(kNumInput-1 downto 0)(kWidthData-1 downto 0);

  function GetDebugFlag(index : integer) return boolean is
  begin
    if(index = 16) then
      return true;
    else
      return false;
    end if;
  end function;

begin
  -- =========================== body ===============================
  daq_off_reset <= not daqOn;

  validOut  <= valid_inserter;
  dOut      <= dout_inserter;

  -- ADCs ---------------------------------------------------------
  u_ADC : entity mylib.RayrawStrAdcRO
    generic map
    (
      kFreqMultiply => 3,
      enDEBUG       => false
    )
    port map
    (
      -- SYSTEM port --
      rst           => rst or rstAdc,
      srstAdcCycle  => rstAdcCycle,
      clkSys        => sysClk,
      clkIdelayRef  => indepClk,
      frameRefPatt  => "1100000000",

      -- Status --
      isReady       => is_ready_asic,
      bitslipErr    => bitslip_err_asic,

      -- Data Out --
      validOut      => valid_adc_data,
      adcDataOut    => adc_data,

      -- ADC In --
      adcDClkP      => adcDClkP ,
      adcDClkN      => adcDClkN ,
      adcDataP      => adcDataP ,
      adcDataN      => adcDataN ,
      adcFrameP     => adcFrameP,
      adcFrameN     => adcFrameN
    );

  valid_data  <= and_reduce(valid_adc_data);

  gen_convdata : for i in 0 to kNumInput-1 generate
  begin
    valid_data_mask(i)  <= valid_data and (not adcMask(i));
    adc_data_in(i)      <= adc_data(i);
    debug_adc_data(i)   <= adc_data(i)(9 downto 0);


  end generate;

  -- ADC delay buffer --------------------------------------------------------
  u_ADCDelayBuffer : entity mylib.ADCDelayBuffer
    generic map(
      kNumInput       => kNumInput,
      kWidthInputData => kWidthPackedAdc,
      enDEBUG         => false
      )
    port map
    (
      -- system --
      clk             => sysClk,
      enBypass        => enBypassDelay,

      -- Data In --
      validIn         => valid_data_mask,
      dIn             => adc_data_in,

      -- Data Out --
      vaildOut        => valid_data_delay,
      dOut            => adc_data_delay
      );

  long_hbcount(sf_timestamp'high downto sf_timestamp'high - hbCount'length +1)  <= signed(hbCount);
  long_hbcount(sf_timestamp'high - hbCount'length downto 0)                     <= (others => '0');
  extended_ofs(LaccpFineOffset'high downto 0)                     <= LaccpFineOffset;
  extended_ofs(extended_ofs'high downto LaccpFineOffset'high+1)   <= (others => LaccpFineOffset(LaccpFineOffset'high));
  sf_timestamp  <= long_hbcount + extended_ofs;

  --sf_timestamp(sf_timestamp'high downto sf_timestamp'high - hbCount'length +1) <= hbCount;
  --sf_timestamp(sf_timestamp'high - hbCount'length downto 0) <= (others => '0');

  -- Heartbeat frame definition ------------------------------------------------
  gen_adcdata : for i in 0 to kNumInput-1 generate
  begin
    -- BL suppression ----------------------------------------------------------
    u_SBLS : entity mylib.SimpleBLSuppressor
    generic map(
      kNumSample        => 4,
      kWidthAdcData     => kNumAdcBit,
      kNumFsModeWords   => 256,
      --enDEBUG           => GetDebugFlag(i)
      enDEBUG           => false
    )
    port map(
      -- system --
      srst              => sync_reset,
      clk               => sysClk,
      enUnit            => enBLSuppressor,
      polarityIn        => polBLSuppressor,
      thresholdIn       => thBLSuppressor,

      -- Data In --
      validIn           => valid_data_delay(i),
      dIn               => adc_data_delay(i),

      -- Data Out --
      vaildOut          => valid_data_bls(i),
      dOut              => adc_data_bls(i),
      typeOut           => sf_type_out(i)
    );

    -- SubFrameFormatter --
    u_formatter : entity mylib.SubFrameFormatter
      generic map(
        kNumSample        => 4,
        kValidNumSample   => 3,
        kWidthAdcData     => kNumAdcBit,
        kChannel          => i,
        kNumFsModeWords   => 256
      )
      port map(
        -- system --
        suppMode          => enBLSuppressor,
        timeStamp         => std_logic_vector(sf_timestamp),

        -- Data In --
        validIn           => valid_data_bls(i),
        typeIn            => sf_type_out(i),
        dIn               => adc_data_bls(i),

        -- Data Out --
        vaildOut          => valid_data_formatter(i),
        dOut              => dout_formatter(i)
      );

    -- Delimiter inserter --
    valid_data_trigger(i)   <= triggerGate and valid_data_formatter(i);

    u_dlmist : entity mylib.DelimiterInserterAdc
      generic map
        (
          --enDEBUG      => GetDebugFlag(i)
          enDEBUG      => false
        )
        port map
        (
          -- system --
          clk             => sysClk,
          syncReset       => sync_reset,
          --userRegIn       => userReg,
          signBit         => LaccpFineOffset(LaccpFineOffset'high),

          -- TDC in --
          validIn         => valid_data_trigger(i),
          dataIn          => dout_formatter(i),

          -- Delimiter word input --
          validDelimiter  => validDelimiter,
          dInDelimiter    => dInDelimiter,
          daqOn           => daqOn,
          hbfThrottlingOn => hbfThrottlingOn,

          -- output --
          validOut        => valid_inserter(i),
          dOut            => dout_inserter(i)
        );

  end generate;

  -- Reset sequence --
  u_reset_gen_sys   : entity mylib.ResetGen
    port map(rst, sysClk, sync_reset);

end RTL;
