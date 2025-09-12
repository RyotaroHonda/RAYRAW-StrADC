library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_MISC.ALL;
use IEEE.NUMERIC_STD.ALL;
use ieee.math_real.all;

library mylib;
use mylib.defDataBusAbst.all;
use mylib.defADC.all;

entity SimpleBLSuppressor is
  generic (
    kNumSample        : integer;
    kWidthAdcData     : integer;
    kNumFsModeWords   : integer;
    enDEBUG           : boolean:= false
  );
  port (
    -- system --
    srst              : in  std_logic;
    clk               : in  std_logic;
    enUnit            : in  std_logic;  -- Enable BL suppression
    polarityIn        : in  std_logic;  -- 0: Negative swing pulse, 1: positive swing pulse
    thresholdIn       : in  std_logic_vector(kWidthAdcData-1 downto 0);

    -- Data In --
    validIn           : in  std_logic;
    dIn               : in  std_logic_vector(kNumSample*kWidthAdcData -1 downto 0);

    -- Data Out --
    vaildOut          : out std_logic;
    dOut              : out std_logic_vector(kNumSample*kWidthAdcData -1 downto 0);
    typeOut           : out std_logic_vector(kWidthAdcSFType-1 downto 0)
  );
end SimpleBLSuppressor;

architecture RTL of SimpleBLSuppressor is
  constant kNegPulse    : std_logic:= '0';
  constant kPosPulse    : std_logic:= '1';

  constant kZero        : std_logic_vector(kWidthAdcData-1 downto 0):= (others => '0');

  signal reg_valid      : std_logic;
  signal reg_valid_out  : std_logic;
  signal frame_boundary : std_logic_vector(1 downto 0);
  signal reg_valid_sr   : std_logic_vector(1 downto 0);
  type IntDataArray is array (integer range 0 to kNumSample-1) of std_logic_vector(kWidthAdcData-1 downto 0);
  signal adc_data       : IntDataArray;

  signal reg_dout       : std_logic_vector(dOut'range);
  signal reg_dout2      : std_logic_vector(dOut'range);
  signal dmux_out       : std_logic_vector(dOut'range);

  constant kWidthCounter  : integer:= integer(ceil(log2(real(kNumFsModeWords))));
  signal counter        : std_logic_vector(kWidthCounter-1 downto 0);

  signal dmux_type_out  : std_logic_vector(typeOut'range);

  -- debug --
  attribute mark_debug  : boolean;
  attribute mark_debug of reg_valid       : signal is enDEBUG;
  attribute mark_debug of reg_valid_out   : signal is enDEBUG;
  attribute mark_debug of frame_boundary  : signal is enDEBUG;
  attribute mark_debug of reg_valid_sr    : signal is enDEBUG;
  attribute mark_debug of dmux_out        : signal is enDEBUG;
  attribute mark_debug of dmux_type_out   : signal is enDEBUG;

begin
  -- =========================== body ===============================

  vaildOut  <= reg_valid_out;
  dOut      <= dmux_out;
  typeOut   <= dmux_type_out;

  breakdown : for i in 0 to kNumSample-1 generate
  begin
    adc_data(i)   <= dIn((i+1)*kWidthAdcData-1 downto i*kWidthAdcData);
  end generate;

  u_check : process(clk)
    variable  judge_flag   : std_logic_vector(kNumSample-1 downto 0):= (others => '0');
  begin
    if(clk'event and clk = '1') then
      if(srst = '1') then
        reg_valid_sr    <= (others => '0');
        frame_boundary  <= (others => '0');
        counter         <= (others => '0');
        reg_valid       <= '0';
        judge_flag      := (others => '0');
      else
        reg_valid     <= validIn;
        reg_dout      <= dIn;
        reg_dout2     <= reg_dout;

        if(enUnit = '1') then
          -- Base line suppression mode --
          if(polarityIn = kNegPulse) then
            for i in 0 to kNumSample-1 loop
              if(unsigned(adc_data(i)) <= unsigned(thresholdIn) and adc_data(i) /= kZero) then
                judge_flag(i)   := '1';
              else
                judge_flag(i)   := '0';
              end if;
            end loop;
          elsif(polarityIn = kPosPulse) then
            for i in 0 to kNumSample-1 loop
              if(unsigned(adc_data(i)) >= unsigned(thresholdIn) and adc_data(i) /= kZero) then
                judge_flag(i)   := '1';
              else
                judge_flag(i)   := '0';
              end if;
            end loop;
          end if;

          -- Packed ADC cycle --
          if(or_reduce(judge_flag) = '1' and validIn = '1') then
            frame_boundary  <= frame_boundary(0) & '1';
          elsif(or_reduce(judge_flag) = '0' and validIn = '1') then
            frame_boundary  <= frame_boundary(0) & '0';
          end if;

          -- Sys clock cycle --
          if(or_reduce(judge_flag) = '1' and validIn = '1') then
            reg_valid_sr  <= reg_valid_sr(0) & '1';
          else
            reg_valid_sr  <= reg_valid_sr(0) & '0';
          end if;
        else
          -- Full streaming mode --
          judge_flag    := (others => '0');
          reg_valid_sr  <= reg_valid_sr(0) & validIn;

          -- Packed ADC cycle --
          if(validIn = '1') then
            counter       <= std_logic_vector(unsigned(counter) +1);

            if(unsigned(counter) = 0) then
              frame_boundary  <= frame_boundary(0) & '1';
            else
              frame_boundary  <= frame_boundary(0) & '0';
            end if;
          end if;
        end if;
      end if;
    end if;
  end process;

  u_mux : process(clk)
    variable size_count  : std_logic_vector(kPosSftDataSize'length-3 downto 0):= (others => '0');
  begin
    if(clk'event and clk = '1') then
      if(srst = '1') then
        reg_valid_out <= '0';
        size_count    := (others => '0');
      else
        if(frame_boundary = "01" and reg_valid_sr(0) = '1') then
          reg_valid_out <= '1';
          dmux_out      <= (others => '1');
          dmux_type_out <= kCTypeSfHead;
          size_count    := std_logic_vector(unsigned(size_count) +1);
        elsif(frame_boundary = "10" and reg_valid = '1' and enUnit = '1') then
          reg_valid_out <= '1';
          size_count    := std_logic_vector(unsigned(size_count) +1);
          dmux_out      <= (size_count'range => size_count, others => '0');
          size_count    := (others => '0');
          dmux_type_out <= kCTypeSfTrail;

        elsif(reg_valid_sr(1) = '1') then
          reg_valid_out <= '1';
          dmux_out      <= reg_dout2;
          dmux_type_out <= kCTypeSfPayload;
          size_count    := std_logic_vector(unsigned(size_count) +1);
        else
          reg_valid_out <= '0';
        end if;
      end if;
    end if;
  end process;



end RTL;
