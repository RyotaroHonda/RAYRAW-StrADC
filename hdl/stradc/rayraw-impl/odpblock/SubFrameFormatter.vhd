library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_MISC.ALL;
use IEEE.NUMERIC_STD.ALL;

library mylib;
use mylib.defDataBusAbst.all;
use mylib.defDelimiter.all;
use mylib.defADC.all;

entity SubFrameFormatter is
  generic (
    kNumSample        : integer;
    kValidNumSample   : integer;
    kWidthAdcData     : integer;
    kChannel          : integer;
    kNumFsModeWords   : integer
  );
  port (
    -- system --
    suppMode          : in  std_logic;  -- 0: Full-streaming mode, 1: suppression mode
    timeStamp         : in  std_logic_vector(kPosSfhTime'length-1 downto 0);

    -- Data In --
    validIn           : in  std_logic;
    typeIn            : in  std_logic_vector(kWidthAdcSFType-1 downto 0);
    dIn               : in  std_logic_vector(kNumSample*kWidthAdcData -1 downto 0);

    -- Data Out --
    vaildOut          : out std_logic;
    dOut              : out std_logic_vector(kWidthData-1 downto 0)
  );
end SubFrameFormatter;

architecture RTL of SubFrameFormatter is
  signal header_data, trailer_data, payload_data  : std_logic_vector(dOut'range);
  signal trailer_data_size  : std_logic_vector(kPosSftDataSize'length-1 downto 0);

begin
  -- =========================== body ===============================

  -- Header --
  header_data(kPosHbdDataType'range)  <= kDatatypeAdcHead;
  header_data(kPosSfhChannel'range)   <= std_logic_vector(to_unsigned(kChannel, kPosSfhChannel'length));
  header_data(kPosSfhNsample'range)   <= std_logic_vector(to_unsigned(kValidNumSample, kPosSfhNsample'length));
  header_data(kPosSfhMode'range)      <= '0' & suppMode;

  header_data(kPosSfhFsSize'range)    <= (others => '0') when(suppMode = '1') else (std_logic_vector(to_unsigned(kNumFsModeWords+1, kPosSfhFsSize'length-3)) & "000");
  header_data(kPosSfhTime'high downto 0) <= (kPosSfhTime'range => timeStamp, others => '0');

  -- Trailer --
  trailer_data_size                   <= dIn(kPosSftDataSize'length-1-3 downto 0) & "000";
  trailer_data(kPosHbdDataType'range) <= kDatatypeAdcTrail;
  trailer_data(kPosSftDataSize'high downto 0) <= (kPosSftDataSize'range => trailer_data_size, others => '0');

  -- Packed ADC --
  payload_data(kPosHbdDataType'range) <= kDatatypeAdcData;
  payload_data(kPosSfhChannel'range)  <= std_logic_vector(to_unsigned(kChannel, kPosSfhChannel'length));
  payload_data(kPosSfpAdc'high downto 0) <= (kPosSfpAdc'range => dIn, others => '0');

  -- MUX --
  vaildOut  <= validIn;
  dOut      <= header_data  when(typeIn = kCTypeSfHead) else
               trailer_data when(typeIn = kCTypeSfTrail) else
               payload_data when(typeIn = kCTypeSfPayload) else
               payload_data when(typeIn = kCTypeSfCAdc) else (others => '0');


end RTL;
