library ieee, mylib;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use mylib.defBCT.all;

package defStrDaqRayraw is

  -- Local Address --------------------------------------------------------
  constant kTdcMask               : LocalAddressType := x"000"; -- W/R, [31:0], tdc data mask
  constant kAdcMask               : LocalAddressType := x"010"; -- W/R, [31:0], adc data mask

  constant kRegBls                : LocalAddressType := x"020"; -- W/R, [1:0], Register setting for BLS
  constant kWidthBls              : integer:= 8;
  constant kIndexEnBls            : integer:= 0;
  constant kIndexBlsPol           : integer:= 1;

  constant kThBls                 : LocalAddressType := x"030"; -- W/R, [9:0], Threshold setting for BLS

  constant kEnBypass              : LocalAddressType := x"040"; -- W/R, [1:0], Enable bypass route in ODPBlock
  constant kWidthBypass           : integer:= 8;
  constant kIndexDelay            : integer:= 0;
  constant kIndexParing           : integer:= 1;
  constant kIndexOfsCorr          : integer:= 2;

  constant kTotFilterControl      : LocalAddressType := x"050"; -- W/R, [1:0], TOT Filter control reg
  constant kWidthTotFilterCreg    : integer:= 8;
  constant kIndexTotFilter        : integer:= 0;
  constant kIndexTotZeroThrough   : integer:= 1;
  constant kTotMinTh              : LocalAddressType := x"060"; -- W/R, [15:0], TOT Filter min. th.
  constant kTotMaxTh              : LocalAddressType := x"070"; -- W/R, [15:0], TOT Filter max. th.

  constant kTriggerEmuControl     : LocalAddressType := x"080"; -- W/R, [1:0], Trigger emulation mode
  constant kTrgGateDelay          : LocalAddressType := x"090"; -- W/R, [7:0], Trigger gate delay
  constant kTrgGateWidth          : LocalAddressType := x"0A0"; -- W/R, [31:0], Trigger gate width

  constant kHbfThrottControl      : LocalAddressType := x"0B0"; -- W/R, [3:0], Hbf throttling control

  constant kHbdUserReg            : LocalAddressType := x"0C0"; -- W,   [15:0], Hb 2nd delimiter user data

  constant kSelfRecoveryMode      : LocalAddressType := x"0D0"; -- W/R, [0:0], Activate self recovery mode

  end package defStrDaqRayraw;
