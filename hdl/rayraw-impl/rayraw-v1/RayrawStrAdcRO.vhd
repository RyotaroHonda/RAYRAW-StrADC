library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_MISC.ALL;
use ieee.numeric_std.all;

Library UNISIM;
use UNISIM.vcomponents.all;

library mylib;
use mylib.defYaenamiAdc.all;
use mylib.defRayrawStrAdcROV1.all;

entity RayrawStrAdcRO is
  generic
  (
    kFreqMultiply     : integer:= 4;
    enDEBUG           : boolean:= false
  );
  port
  (
    -- SYSTEM port --
    rst           : in std_logic; -- Asynchronous reset (active high)
    srstAdcCycle  : in std_logic; -- Synchronous to clkSys, reset ADC cycle number
    clkSys        : in std_logic; -- System clock (global clock)
    clkIdelayRef  : in std_logic; -- 200 MHz ref. clock.
    frameRefPatt  : in AdcDataType; -- ADC FRAME reference bit pattern

    -- Status --
    isReady       : out std_logic_vector(kNumAsicBlock-1 downto 0); -- If high, data outputs are valid
    bitslipErr    : out std_logic_vector(kNumAsicBlock-1 downto 0); -- Indicate bitslip failure

    -- Data Out --
    validOut      : out std_logic_vector(kNumAsicBlock-1 downto 0); -- FIFO output is valid
    adcDataOut    : out PackedAdcBlockArray; -- Packed de-serialized ADC data

    -- ADC In --
    adcDClkP      : in std_logic_vector(kNumAsicBlock-1 downto 0);  -- ADC DCLK (forwarded fast clock)
    adcDClkN      : in std_logic_vector(kNumAsicBlock-1 downto 0);
    adcDataP      : in std_logic_vector(kNumAsicBlock*kNumAdcCh-1 downto 0); -- ADC DATA
    adcDataN      : in std_logic_vector(kNumAsicBlock*kNumAdcCh-1 downto 0);
    adcFrameP     : in std_logic_vector(kNumAsicBlock-1 downto 0);  -- ADC FRAME
    adcFrameN     : in std_logic_vector(kNumAsicBlock-1 downto 0)

  );
end RayrawStrAdcRO;

architecture RTL of RayrawStrAdcRO is
  -- Internal signal definition ---------------------------------------------------------
  -- System --
  signal sync_reset     : std_logic;

  -- ADC clock domain --
  -- YANEAMI ADC --
  type AdcDataAsicArray is array (integer range kNumAsicBlock-1 downto 0) of AdcDataArray;
  signal adc_data_out   : AdcDataAsicArray;

  signal is_ready       : std_logic_vector(isReady'range);
  signal bitslip_error  : std_logic_vector(isReady'range);
  signal empty_fifo     : std_logic_vector(isReady'range);
  signal clk_adc        : std_logic_vector(isReady'range);

  -- AdcPacker --
  signal sync_rst_adccycle  : std_logic_vector(kNumAsicBlock-1 downto 0);
  signal valid_packer_out   : std_logic_vector(kNumAsicBlock-1 downto 0);

  -- FIFO --
  type AdcFifoType  is array (integer range kNumAsicBlock-1 downto 0) of PackedArrayType;
  signal dout_packer_out    : AdcFifoType;

  signal wr_rst_busy        : std_logic_vector(kNumAsicBlock-1 downto 0);


  COMPONENT stradc_cdc_fifo
    PORT (
      rst : IN STD_LOGIC;
      wr_clk : IN STD_LOGIC;
      rd_clk : IN STD_LOGIC;
      din : IN STD_LOGIC_VECTOR(319 DOWNTO 0);
      wr_en : IN STD_LOGIC;
      rd_en : IN STD_LOGIC;
      dout : OUT STD_LOGIC_VECTOR(319 DOWNTO 0);
      full : OUT STD_LOGIC;
      empty : OUT STD_LOGIC;
      valid : OUT STD_LOGIC;
      rd_data_count : OUT STD_LOGIC_VECTOR(3 DOWNTO 0);
      wr_rst_busy : OUT STD_LOGIC;
      rd_rst_busy : OUT STD_LOGIC
    );
  END COMPONENT;

  -- SYSTEM CLK domain --
  signal adc_data_block_out   : PackedAdcBlockArray;

  type SyncType is array (integer range kNumAsicBlock-1 downto 0) of std_logic_vector(kSyncLength-1 downto 0);
  signal sync_is_ready            : SyncType;
  signal sync_bitslip_error       : SyncType;
  signal rden_fifo                : std_logic;
  signal read_valid               : std_logic_vector(isReady'range);
  signal dout_fifo                : AdcFifoType;

  constant kReadCycle             : integer:= 5; -- Read FIFO once every 5 cycles

  constant kReadOnTh   : unsigned(3 downto 0):= "1010";
  constant kReadOffTh  : unsigned(3 downto 0):= "0110";
  type FifoRdCountType is array(integer range kNumAsicBlock-1 downto 0) of std_logic_vector(kReadOnTh'range);
  signal fifo_rd_count            : FifoRdCountType;
  signal read_ready               : std_logic_vector(kNumAsicBlock-1 downto 0);


  attribute mark_debug        : boolean;
  attribute mark_debug of rden_fifo      : signal is enDEBUG;
  attribute mark_debug of read_ready     : signal is enDEBUG;
  attribute mark_debug of fifo_rd_count  : signal is enDEBUG;

-- debug ---------------------------------------------------------------

begin
  -- ======================================================================
  --                                 body
  -- ======================================================================

  isReady     <= sync_is_ready(kSyncLength-1);
  bitslipErr  <= sync_bitslip_error(kSyncLength-1);

  validOut    <= read_valid;
  adcDataOut  <= adc_data_block_out;


  -- Clock domain crossing ----------------------------------------------------------
  u_adc_to_sys : process(clkSys)
  begin
    if(clkSys'event and clkSys = '1') then
      sync_is_ready       <= sync_is_ready(kSyncLength-2 downto 0) & is_ready;
      sync_bitslip_error  <= sync_bitslip_error(kSyncLength-2 downto 0) & bitslip_error;

--      rden_fifo <= not or_reduce(empty_fifo);
    end if;
  end process;

  -- ADC CLK domain ----------------------------------------------------
  gen_adc : for i in 0 to kNumAsicBlock-1 generate
  begin

    -- Clock Domain Crossing --
    u_sync_rst : entity mylib.synchronizer
      port map(clk_adc(i), srstAdcCycle, sync_rst_adccycle(i));
    -- Clock Domain Crossing --

    gen_ch : for j in 0 to kNumAdcCh-1 generate
      adc_data_block_out(kNumAdcCh*i +j)   <= dout_fifo(i)(kPackingFactor*kNumAdcBit*(j+1)-1 downto kPackingFactor*kNumAdcBit*j);
    end generate;

    u_adc : entity mylib.YaenamiAdc
      generic map
      (
        genIDELAYCTRL      => GetGenFlagIdelayCtrl(i),
        kDiffTerm          => TRUE,
        kIoStandard        => "LVDS",
        kIoDelayGroup      => GetIdelayGroup(i),
        kFreqRefClk        => 200.0,
        enDEBUG            => FALSE
      )
      port map
      (
        -- SYSTEM port --
        rst           => rst,
        invPolarity   => GetInvPolarity(i),
        clkIdelayRef  => clkIdelayRef,
        tapValueIn    => GetTapValues(i),
        tapValueOut   => open,
        enBitslip     => '1',
        frameRefPatt  => frameRefPatt,

        -- Status --
        isReady       => is_ready(i),
        bitslipErr    => bitslip_error(i),

        -- Data Out --
        adcClk        => clk_adc(i),
        adcDataOut    => adc_data_out(i),
        adcFrameOut   => open,

        -- ADC In --
        adcDClkP      => adcDClkP(i),
        adcDClkN      => adcDClkN(i),
        adcDataP      => adcDataP((kNumAdcCh)*(i+1)-1 downto (kNumAdcCh)*i),
        adcDataN      => adcDataN((kNumAdcCh)*(i+1)-1 downto (kNumAdcCh)*i),
        adcFrameP     => adcFrameP(i),
        adcFrameN     => adcFrameN(i)

      );

    u_packer : entity mylib.PackerArray
      generic map(
        kFreqMultiply => kFreqMultiply
      )
      port map(
        srst      => sync_rst_adccycle(i),
        clk       => clk_adc(i),
        validIn   => is_ready(i),
        dIn       => adc_data_out(i),
        validOut  => valid_packer_out(i),
        dOut      => dout_packer_out(i)
      );

    u_fifo : stradc_cdc_fifo
      PORT MAP (
        rst    => srstAdcCycle,
        wr_clk => clk_adc(i),
        rd_clk => clkSys,
        din => dout_packer_out(i),
        wr_en => valid_packer_out(i) and (not wr_rst_busy(i)),
        rd_en => rden_fifo,
        dout => dout_fifo(i),
        full => open,
        empty => empty_fifo(i),
        valid => read_valid(i),
        rd_data_count => fifo_rd_count(i),
        wr_rst_busy => wr_rst_busy(i),
        rd_rst_busy => open
      );

  end generate;

  process(clkSys)
    variable count  : integer range 0 to kReadCycle+1 := 0;
  begin
    if(clkSys'event and clkSys = '1') then
      if(sync_reset = '1' or srstAdcCycle = '1') then
        read_ready  <= (others => '0');
        rden_fifo   <= '0';
        count       := 0;
      else
        for i in 0 to kNumAsicBlock-1 loop
          if(unsigned(fifo_rd_count(i)) >= kReadOnTh) then
            read_ready(i)   <= '1';
          elsif(unsigned(fifo_rd_count(i)) <= kReadOffTh) then
            read_ready(i)   <= '0';
          else
            null;
--            read_ready(i)   <= '0';
          end if;
        end loop;

        if(and_reduce(read_ready) = '1') then
          if(count = kReadCycle-1) then
            rden_fifo <= '1';
            count     := 0;
          else
            rden_fifo <= '0';
            count     := count +1;
          end if;
        end if;
      end if;
    end if;
  end process;


  -- Clock domain crossing ---------------------------------------------------------

  -- Reset sequence --
  u_reset_gen_sys   : entity mylib.ResetGen
    port map(rst, clkSys, sync_reset);

end RTL;
