library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.numeric_std.all;

library mylib;
use mylib.defDataBusAbst.all;
use mylib.defADC.all;
use mylib.defDelimiter.all;

entity DelimiterInserterAdc is
  generic(
    enDEBUG     : boolean:= false
  );
  port(
    clk         : in std_logic;   -- base clock
    syncReset   : in std_logic;   -- Synchronous reset
    signBit         : in std_logic;

    -- adc in --
    validIn         : in  std_logic;
    dataIn          : in  std_logic_vector(kWidthData-1 downto 0);

    -- delimiter in --
    validDelimiter  : in  std_logic;
    dInDelimiter    : in  std_logic_vector(kWidthData-1 downto 0);
    daqOn           : in  std_logic;
    hbfThrottlingOn : in  std_logic;

    -- Data out --
    validOut        : out std_logic;
    dOut            : out std_logic_vector(kWidthData-1 downto 0)
  );
end DelimiterInserterAdc;

architecture RTL of DelimiterInserterAdc is
  attribute mark_debug  : boolean;

  -- data input
  signal delimiter_valid      : std_logic;
  signal delimiter_data       : std_logic_vector(dInDelimiter'range);

  signal adc_valid            : std_logic;
  signal adc_data             : std_logic_vector(dInDelimiter'range);

  -- data merge
  constant kBuffLength        : integer:= 3;
  signal buff_delimiter_valid : std_logic_vector(kBuffLength-1 downto 0)  := (others=>'0');
  signal buff_delimiter_data  : IntDataArrayType(kBuffLength-1 downto 0)(kWidthData-1 downto 0);

  signal delayed_daq_on       : std_logic;
  signal sr_daq_on            : std_logic_vector(kBuffLength-1 downto -1);

  constant kWidthPtr          : integer:= 3;
  signal write_ptr            : unsigned(kWidthPtr-1 downto 0);
  signal read_ptr             : unsigned(kWidthPtr-1 downto 0);
  signal ram_in               : std_logic_vector(kWidthData downto 0);
  signal ram_out              : std_logic_vector(kWidthData downto 0);

  signal valid_adc_buf        : std_logic;
  signal dout_adc_buf         : std_logic_vector(dInDelimiter'range);

  signal merge_valid          : std_logic;
  signal merge_data           : std_logic_vector(dInDelimiter'range);

  -- data output
  signal is_2nd_delimiter     : std_logic;
  signal num_word             : unsigned(kPosHbdGenSize'length-4 downto 0);

  signal data_valid_out       : std_logic;
  signal data_out             : std_logic_vector(dOut'range);

  -- debug ----------------------------------------------------------
  attribute mark_debug of delimiter_valid   : signal is enDEBUG;
  attribute mark_debug of buff_delimiter_valid : signal is enDEBUG;
  attribute mark_debug of is_2nd_delimiter  : signal is enDEBUG;
  attribute mark_debug of valid_adc_buf     : signal is enDEBUG;

  --attribute mark_debug of num_word          : signal is enDEBUG;
  attribute mark_debug of data_out          : signal is enDEBUG;
  attribute mark_debug of data_valid_out    : signal is enDEBUG;

begin
  -- =========================== body ===============================

  -- data input
  delimiter_valid <= validDelimiter;
  delimiter_data  <= dInDelimiter;

  u_adc : process(clk)
  begin
    if(clk'event and clk = '1') then
      if(syncReset = '1') then
        adc_valid  <= '0';
      else
        if(validIn ='1' and hbfThrottlingOn = '0') then
          adc_data   <= dataIn;
          adc_valid  <= '1';
        else
          adc_valid  <= '0';
        end if;
      end if;
    end if;
  end process;

  -- data merge
  delayed_daq_on  <= sr_daq_on(kBuffLength-1);
  buff_delimiter : process(clk)
  begin
    if(clk'event and clk = '1') then
      if(signBit = '0') then
        sr_daq_on <= sr_daq_on(kBuffLength-2 downto 0) & daqOn & '0';
      else
        sr_daq_on <= sr_daq_on(kBuffLength-2 downto -1) & daqOn;
      end if;

      buff_delimiter_valid(kBuffLength-1) <= delimiter_valid;
      buff_delimiter_data(kBuffLength-1)  <= delimiter_data;
      for i in 0 to kBuffLength-2 loop
        buff_delimiter_valid(i) <= buff_delimiter_valid(i+1);
        buff_delimiter_data(i)  <= buff_delimiter_data(i+1);
      end loop;
    end if;
  end process;

  ram_in  <= adc_valid & adc_data;
  u_adcBuf : entity mylib.MyDPRamARRT
    generic map(
      kWidthAddr  => kWidthPtr,
      kWidthData  => kWidthData+1
      )
    port map(
      clk   => clk,
      we    => '1',
      addra => std_logic_vector(write_ptr),
      addrb => std_logic_vector(read_ptr),
      di    => ram_in,
      doa   => open,
      dob   => ram_out
      );


  dout_adc_buf  <= ram_out(kWidthData-1 downto 0);
  valid_adc_buf <= ram_out(kWidthData);

  u_pointer : process(clk)
  begin
    if(syncReset = '1') then
      write_ptr <= (others => '0');
      read_ptr  <= (others => '0');
    elsif(clk'event and clk = '1') then
      if(adc_valid = '1') then
        write_ptr     <= write_ptr + 1;
      end if;

      if(buff_delimiter_valid(0) = '0') then
        if(write_ptr /= read_ptr) then
          read_ptr      <= read_ptr +1;
        end if;
      end if;
    end if;
  end process;




  -- data output
  merger : process(clk)
  begin
    if(clk'event and clk = '1') then
      -- reset or daq_off
      if(syncReset = '1' or delayed_daq_on = '0') then
        data_valid_out    <= '0';
        is_2nd_delimiter  <= '0';
        num_word          <= (others=>'0');

      -- delimiter
      elsif(buff_delimiter_valid(0)='1')then
        data_valid_out    <= '1';
        -- insert the gen adc size into the 2nd delimiter word.
        if(is_2nd_delimiter='1')then
          data_out(kPosHbdDataType'range)  <= kDatatypeHeartbeatT2;
          --data_out(kPosIHbdGenSize'range) <= std_logic_vector(num_word) & "000";
          data_out(kPosHbdGenSize'range)   <= std_logic_vector(num_word) & "000";
          --data_out(kPosHbdUserReg'range) <= userRegIn;
          is_2nd_delimiter  <= '0';
          num_word          <= (others=>'0');
        else
          data_out          <= buff_delimiter_data(0);
          is_2nd_delimiter  <= '1';
        end if;

      -- adc data
      else
        data_valid_out  <= valid_adc_buf;

        if(valid_adc_buf = '1') then
          data_out        <= dout_adc_buf;
          -- count the user data
--          if(dout_adc_buf(kPosHbdDataType'range) = kDatatypeAdcData) then
          num_word      <= num_word + 1;
--          end if;

        end if;

      end if;
    end if;
  end process;

  validOut  <= data_valid_out;
  dOut      <= data_out;

end RTL;
