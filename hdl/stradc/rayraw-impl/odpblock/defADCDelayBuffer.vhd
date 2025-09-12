library ieee;
use ieee.std_logic_1164.all;

library mylib;

package defADCDelayBuffer is

  -- Data Unit --
  -- Valid + PackedAdcDat
--  constant kWidthDataUnit     : integer  := 1+

  -- BRAM information
  constant kDepthBuffer       : integer  := 256;
  constant kWidthAddrBuffer   : integer  := 8;   -- log2(kDepthBuffer)


  -- setting delay clock
  constant kDelayClock        : integer  := 257; -- 3~kDepthBuffer+1
  constant kOffsetRAddr       : integer  := kDepthBuffer-kDelayClock+3;

end package defADCDelayBuffer;
