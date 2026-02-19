library ieee, mylib;
use ieee.std_logic_1164.all;
use mylib.defBCT.all;

package defIOManager is
  -- Local Address  -------------------------------------------------------
  constant kSelExtIn0              : LocalAddressType := x"000"; -- W/R, [1:0]
  constant kSelExtIn1              : LocalAddressType := x"010"; -- W/R, [1:0]
  constant kSelExtIn2              : LocalAddressType := x"020"; -- W/R, [1:0]
  constant kSelExtIn3              : LocalAddressType := x"030"; -- W/R, [1:0]

  constant kSelIntIn0              : LocalAddressType := x"100"; -- W/R, [2:0]
  constant kSelIntIn1              : LocalAddressType := x"110"; -- W/R, [2:0]
  constant kSelIntIn2              : LocalAddressType := x"120"; -- W/R, [2:0]
  constant kSelIntIn3              : LocalAddressType := x"130"; -- W/R, [2:0]

  constant kAddrAmon               : LocalAddressType := x"200"; -- W/R, [1:0]

end package defIOManager;

