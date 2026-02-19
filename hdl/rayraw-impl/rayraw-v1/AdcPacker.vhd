library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use ieee.numeric_std.all;

library mylib;
use mylib.defYaenamiAdc.all;
use mylib.defRayrawStrAdcROV1.all;

entity AdcPacker is
  generic(
    kFreqMultiply : integer:= 4
  );
  port(
    srst      : in std_logic;
    clk       : in std_logic;
    validIn   : in std_logic;
    dIn       : in AdcDataType;
    validOut  : out std_logic;
    dOut      : out PackedAdcType
  );
end AdcPacker;

architecture RTL of AdcPacker is
  -- internal signal declaration --------------------------------------
  signal reg_packed_adc   : PackedAdcType;
  signal index            : integer range -1 to 5;
  signal valid_out        : std_logic;

  -- =============================== body ===============================
begin

  validOut  <= valid_out;
  dOut      <= reg_packed_adc;

  process(clk)
  begin
    if(clk'event and clk = '1') then
      if(srst = '1') then
        index         <= kFreqMultiply-1;
        reg_packed_adc  <= (others => '0');
        valid_out       <= '0';
      else
        if(validIn = '1') then
          reg_packed_adc((index+1)*kNumAdcBit-1 downto index*kNumAdcBit)  <= dIn;

          if(index = 0) then
            index       <= kFreqMultiply-1;
            valid_out   <= '1';
          else
            index       <= index-1;
            valid_out   <= '0';
          end if;

        else
          index         <= kFreqMultiply-1;
          reg_packed_adc  <= (others => '0');
          valid_out <= '0';
        end if;
      end if;
    end if;
  end process;

end RTL;
