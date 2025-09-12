library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

library mylib;
use mylib.defDataBusAbst.all;
--use mylib.defADC.all;
use mylib.defADCDelayBuffer.all;

entity ADCDelayBuffer is
  generic (
    kNumInput         : integer:= 32;
    kWidthInputData   : integer;
    enDEBUG           : boolean:= false
  );
  port (
    -- system --
    clk               : in  std_logic;  -- base clock
    enBypass          : in  std_logic;  -- Enable bypass route

    -- Data In --
    validIn           : in  std_logic_vector(kNumInput -1 downto 0);
    dIn               : in  AdcArrayType(kNumInput-1 downto 0)(kWidthInputData-1 downto 0);

    -- Data Out --
    vaildOut          : out std_logic_vector(kNumInput -1 downto 0);
    dOut              : out AdcArrayType(kNumInput-1 downto 0)(kWidthInputData-1 downto 0)
  );
end ADCDelayBuffer;

architecture RTL of ADCDelayBuffer is

  -- signal decralation ----------------------------------------------
  -- output --
  signal reg_valid            : std_logic_vector(kNumInput -1 downto 0);
  --signal reg_is_confilicted   : std_logic_vector(kNumInput -1 downto 0);
  signal reg_dout             : AdcArrayType(kNumInput-1 downto 0)(kWidthInputData-1 downto 0);

  -- BRAM
  constant kWidthDataUnit     : integer:= 1+ kWidthInputData;
  constant kWidthDataBuffer   : integer:= kWidthDataUnit*1*kNumInput;
  signal waddr  : std_logic_vector(kWidthAddrBuffer-1 downto 0) :=(others=>'0');
  signal wdata  : std_logic_vector(kWidthDataBuffer-1 downto 0);
  signal raddr  : std_logic_vector(kWidthAddrBuffer-1 downto 0);
  signal rdata  : std_logic_vector(kWidthDataBuffer-1 downto 0);

  component adc_delay_bram
  port (
    clka  : IN  STD_LOGIC;
    wea   : IN  STD_LOGIC_VECTOR(0 DOWNTO 0);
    addra : IN  STD_LOGIC_VECTOR(kWidthAddrBuffer-1 DOWNTO 0);
    dina  : IN  STD_LOGIC_VECTOR(kWidthDataBuffer-1 DOWNTO 0);
    clkb  : IN  STD_LOGIC;
    enb   : IN STD_LOGIC;
    addrb : IN  STD_LOGIC_VECTOR(kWidthAddrBuffer-1 DOWNTO 0);
    doutb : OUT STD_LOGIC_VECTOR(kWidthDataBuffer-1 DOWNTO 0)
  );
  end component;

  attribute mark_debug : boolean;
  attribute mark_debug of waddr   : signal is enDEBUG;
  attribute mark_debug of wdata   : signal is enDEBUG;
  attribute mark_debug of raddr   : signal is enDEBUG;
  attribute mark_debug of rdata   : signal is enDEBUG;

begin
  -- ============================= body ==============================

  -- output --
  vaildOut        <= reg_valid          when(enBypass = '0') else validIn;
  dOut            <= reg_dout           when(enBypass = '0') else dIn;

  -- BRAM w/r data --
  gen_ch : for i in 0 to kNumInput-1 generate

    -- wdata --
    wdata((i+1)*kWidthDataUnit -1)                            <= validIn(i);
    wdata((i+1)*kWidthDataUnit -2 downto i*kWidthDataUnit)    <= dIn(i);

    -- rdata --
    reg_valid(i)          <=  rdata((i+1)*kWidthDataUnit -1);
    reg_dout(i)           <=  rdata((i+1)*kWidthDataUnit -2 downto i*kWidthDataUnit);

  end generate;

  -- BRAM w/r addr --
  addr_process : process(clk)
  begin
    if(clk'event and clk = '1') then
      waddr <= std_logic_vector(unsigned(waddr)+1);
      raddr <= std_logic_vector(unsigned(waddr)+kOffsetRAddr);
    end if;
  end process;

  -- BRAM --
  u_BRAM : adc_delay_bram
  port map(
    clka  => clk,
    wea   => "1",
    addra => waddr,
    dina  => wdata,
    clkb  => clk,
    enb   => '1',
    addrb => raddr,
    doutb => rdata
  );

end RTL;
