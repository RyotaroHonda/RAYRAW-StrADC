library ieee;
use ieee.std_logic_1164.all;

library mylib;
use mylib.defDataBusAbst.all;
use mylib.defDelimiter.all;

package defADC is
  -- ADC sub-frame ---------------------------------------------------------------------
  constant kWidthAdcSFType  : integer:= 2;
  constant kCTypeSfCAdc     : std_logic_vector(kWidthAdcSFType-1 downto 0):= "00";
  constant kCTypeSfHead     : std_logic_vector(kWidthAdcSFType-1 downto 0):= "01";
  constant kCTypeSfTrail    : std_logic_vector(kWidthAdcSFType-1 downto 0):= "10";
  constant kCTypeSfPayload  : std_logic_vector(kWidthAdcSFType-1 downto 0):= "11";

  -- sub-frame header --
  constant kPosSfhChannel   : std_logic_vector(kPosHbdDataType'low-1 downto kPosHbdDataType'low -5)  := (others => '0');
  constant kPosSfhNsample   : std_logic_vector(kPosSfhChannel'low-1  downto kPosSfhChannel'low  -2)  := (others => '0');
  constant kPosSfhMode      : std_logic_vector(kPosSfhNsample'low-1  downto kPosSfhNsample'low  -2)  := (others => '0');
  constant kPosSfhFsSize    : std_logic_vector(kPosSfhMode'low-1     downto kPosSfhMode'low     -11) := (others => '0');
  constant kPosSfhTime      : std_logic_vector(kPosSfhFsSize'low-1   downto kPosSfhFsSize'low   -29) := (others => '0');

  -- sub-frame trailer --
  constant kPosSftDataSize  : std_logic_vector(kPosHbdDataType'low-1 downto kPosHbdDataType'low -14) := (others => '0');

  -- sub-frame payload --
  constant kPosSfpAdc       : std_logic_vector(kPosSfhChannel'low-1  downto kPosSfhChannel'low  -40) := (others => '0');

  -- CADC data structure --
--  constant kPosChannel  : std_logic_vector(kPosHbdDataType'low-1 downto kPosHbdDataType'low -5)  := (others => '0');
--  constant kPosADC      : std_logic_vector(kPosChannel'low-1     downto kPosChannel'low     -37) := (others => '0');
--  constant kPosAdcTs   : std_logic_vector(kPosADC'low-1          downto kPosADC'low         -16) := (others => '0');
--
  -- Channel ---------------------------------------------------------------------------
  --constant kWidthChannel    : integer  := kPosChannel'length;

  -- Timestamp -------------------------------------------------------------------------
  --constant kWidthAdcTs      : integer  := kPosAdcTs'length;

end package defADC;
