library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use ieee.numeric_std.all;

library mylib;
use mylib.defYaenamiAdc.all;
use mylib.defRayrawStrAdcROV1.all;

entity PackerArray is
  generic(
    kFreqMultiply : integer:= 4
  );
  port(
    srst      : in std_logic;
    clk       : in std_logic;
    validIn   : in std_logic;
    dIn       : in AdcDataArray;
    validOut  : out std_logic;
    dOut      : out PackedArrayType
  );
end PackerArray;

architecture RTL of PackerArray is
  -- internal signal declaration --------------------------------------
  constant kOne           : std_logic_vector(kNumAdcCh-1 downto 0):= (others => '1');
  signal valid_out        : std_logic_vector(kNumAdcCh-1 downto 0);

  -- =============================== body ===============================
begin

  validOut  <= '1' when(valid_out = kOne) else '0';

  gen_packer : for i in 0 to kNumAdcCh-1 generate
  begin
    u_packer : entity mylib.AdcPacker
      generic map(
        kFreqMultiply => kFreqMultiply
      )
      port map(
        srst      => srst,
        clk       => clk,
        validIn   => validIn,
        dIn       => dIn(i),
        validOut  => valid_out(i),
        dOut      => dOut((i+1)*kPackingFactor*kNumAdcBit-1 downto i*kPackingFactor*kNumAdcBit)
      );
  end generate;


end RTL;
