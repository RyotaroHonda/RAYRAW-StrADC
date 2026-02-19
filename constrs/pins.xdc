#  ---------------------------------------------------------------------
# Pin assign --
#  ---------------------------------------------------------------------

# System ---------------------------------------------------------------
set_property PACKAGE_PIN A24 [get_ports PROGB_ON]
set_property PACKAGE_PIN D23 [get_ports BASE_CLKP]
set_property PACKAGE_PIN D24 [get_ports BASE_CLKN]
set_property PACKAGE_PIN A20 [get_ports USR_RSTB]
set_property PACKAGE_PIN A8 [get_ports LED[1]]
set_property PACKAGE_PIN A9 [get_ports LED[2]]
set_property PACKAGE_PIN C9 [get_ports LED[3]]
set_property PACKAGE_PIN B9 [get_ports LED[4]]
set_property PACKAGE_PIN U26 [get_ports DIP[1]]
set_property PACKAGE_PIN V26 [get_ports DIP[2]]
set_property PACKAGE_PIN W26 [get_ports DIP[3]]
set_property PACKAGE_PIN Y26 [get_ports DIP[4]]
set_property PACKAGE_PIN AB26 [get_ports DIP[5]]
set_property PACKAGE_PIN AC26 [get_ports DIP[6]]
set_property PACKAGE_PIN AD26 [get_ports DIP[7]]
set_property PACKAGE_PIN AE26 [get_ports DIP[8]]
set_property PACKAGE_PIN N12 [get_ports VP]
set_property PACKAGE_PIN P11 [get_ports VN]

# GTX ------------------------------------------------------------------
set_property PACKAGE_PIN D6 [get_ports GTX_REFCLK_P]
set_property PACKAGE_PIN D5 [get_ports GTX_REFCLK_N]
set_property PACKAGE_PIN F2 [get_ports GTX_TX_P[1]]
set_property PACKAGE_PIN G4 [get_ports GTX_RX_P[1]]
set_property PACKAGE_PIN F1 [get_ports GTX_TX_N[1]]
set_property PACKAGE_PIN G3 [get_ports GTX_RX_N[1]]

# SPI flash ------------------------------------------------------------
set_property PACKAGE_PIN B24 [get_ports SPI_MOSI]
set_property PACKAGE_PIN A25 [get_ports SPI_DIN]
set_property PACKAGE_PIN C23 [get_ports FCSB]

# EEPROM ---------------------------------------------------------------
set_property PACKAGE_PIN B10 [get_ports EEP_CS[1]]
set_property PACKAGE_PIN A10 [get_ports EEP_SK[1]]
set_property PACKAGE_PIN B15 [get_ports EEP_DI[1]]
set_property PACKAGE_PIN A15 [get_ports EEP_DO[1]]

# NIM-IO ---------------------------------------------------------------
set_property PACKAGE_PIN B17 [get_ports NIM_IN[1]]
set_property PACKAGE_PIN A17 [get_ports NIM_IN[2]]
set_property PACKAGE_PIN B16 [get_ports NIM_OUT[1]]
set_property PACKAGE_PIN E15 [get_ports NIM_OUT[2]]

# JItter cleaner -------------------------------------------------------
set_property PACKAGE_PIN C12 [get_ports CDCE_PDB]

# MIKUMARI -------------------------------------------------------------
set_property PACKAGE_PIN AE17 [get_ports MIKUMARI_RXP]
set_property PACKAGE_PIN AF17 [get_ports MIKUMARI_RXN]
set_property PACKAGE_PIN AF14 [get_ports MIKUMARI_TXP]
set_property PACKAGE_PIN AF15 [get_ports MIKUMARI_TXN]

# ASIC -----------------------------------------------------------------
set_property PACKAGE_PIN AF24 [get_ports ASIC_REFC[0]]
set_property PACKAGE_PIN Y21 [get_ports ASIC_REFC[1]]
set_property PACKAGE_PIN R26 [get_ports ASIC_REFC[2]]
set_property PACKAGE_PIN K25 [get_ports ASIC_REFC[3]]
set_property PACKAGE_PIN AE21 [get_ports ASIC_SSB[0]]
set_property PACKAGE_PIN AF25 [get_ports ASIC_SSB[1]]
set_property PACKAGE_PIN Y25 [get_ports ASIC_SSB[2]]
set_property PACKAGE_PIN R23 [get_ports ASIC_SSB[3]]
set_property PACKAGE_PIN L25 [get_ports ASIC_SCK]
set_property PACKAGE_PIN K26 [get_ports ASIC_MOSI]


set_property PACKAGE_PIN AC21 [get_ports ASIC_DISCRI[0]]
set_property PACKAGE_PIN AD21 [get_ports ASIC_DISCRI[1]]
set_property PACKAGE_PIN AF22 [get_ports ASIC_DISCRI[2]]
set_property PACKAGE_PIN AE22 [get_ports ASIC_DISCRI[3]]
set_property PACKAGE_PIN AC22 [get_ports ASIC_DISCRI[4]]
set_property PACKAGE_PIN AF23 [get_ports ASIC_DISCRI[5]]
set_property PACKAGE_PIN AE23 [get_ports ASIC_DISCRI[6]]
set_property PACKAGE_PIN AD23 [get_ports ASIC_DISCRI[7]]
set_property PACKAGE_PIN AD24 [get_ports ASIC_DISCRI[8]]
set_property PACKAGE_PIN AC24 [get_ports ASIC_DISCRI[9]]
set_property PACKAGE_PIN AE25 [get_ports ASIC_DISCRI[10]]
set_property PACKAGE_PIN AD25 [get_ports ASIC_DISCRI[11]]
set_property PACKAGE_PIN AB24 [get_ports ASIC_DISCRI[12]]
set_property PACKAGE_PIN AB25 [get_ports ASIC_DISCRI[13]]
set_property PACKAGE_PIN AA23 [get_ports ASIC_DISCRI[14]]
set_property PACKAGE_PIN AA25 [get_ports ASIC_DISCRI[15]]
set_property PACKAGE_PIN W25 [get_ports ASIC_DISCRI[16]]
set_property PACKAGE_PIN W24 [get_ports ASIC_DISCRI[17]]
set_property PACKAGE_PIN V24 [get_ports ASIC_DISCRI[18]]
set_property PACKAGE_PIN U24 [get_ports ASIC_DISCRI[19]]
set_property PACKAGE_PIN U25 [get_ports ASIC_DISCRI[20]]
set_property PACKAGE_PIN U22 [get_ports ASIC_DISCRI[21]]
set_property PACKAGE_PIN T25 [get_ports ASIC_DISCRI[22]]
set_property PACKAGE_PIN R25 [get_ports ASIC_DISCRI[23]]
set_property PACKAGE_PIN P25 [get_ports ASIC_DISCRI[24]]
set_property PACKAGE_PIN P26 [get_ports ASIC_DISCRI[25]]
set_property PACKAGE_PIN P24 [get_ports ASIC_DISCRI[26]]
set_property PACKAGE_PIN P23 [get_ports ASIC_DISCRI[27]]
set_property PACKAGE_PIN N26 [get_ports ASIC_DISCRI[28]]
set_property PACKAGE_PIN M26 [get_ports ASIC_DISCRI[29]]
set_property PACKAGE_PIN M25 [get_ports ASIC_DISCRI[30]]
set_property PACKAGE_PIN M24 [get_ports ASIC_DISCRI[31]]

# TRIGGER_OUT ----------------------------------------------------------

# APD_BIAS -------------------------------------------------------------
set_property PACKAGE_PIN K22 [get_ports CP_CS_B]
set_property PACKAGE_PIN K23 [get_ports CP_SCLK]
set_property PACKAGE_PIN L22 [get_ports CP_DIN]
set_property PACKAGE_PIN L23 [get_ports CP_CL_B]

# ASIC_ADC -------------------------------------------------------------
set_property PACKAGE_PIN U2 [get_ports ADC_DATA_P[0]]
set_property PACKAGE_PIN U6 [get_ports ADC_DATA_P[1]]
set_property PACKAGE_PIN V2 [get_ports ADC_DATA_P[2]]
set_property PACKAGE_PIN V3 [get_ports ADC_DATA_P[3]]
set_property PACKAGE_PIN V4 [get_ports ADC_DATA_P[4]]
set_property PACKAGE_PIN W6 [get_ports ADC_DATA_P[5]]
set_property PACKAGE_PIN Y3 [get_ports ADC_DATA_P[6]]
set_property PACKAGE_PIN Y6 [get_ports ADC_DATA_P[7]]
set_property PACKAGE_PIN AD1 [get_ports ADC_DATA_P[8]]
set_property PACKAGE_PIN AE3 [get_ports ADC_DATA_P[9]]
set_property PACKAGE_PIN AD4 [get_ports ADC_DATA_P[10]]
set_property PACKAGE_PIN AF5 [get_ports ADC_DATA_P[11]]
set_property PACKAGE_PIN AA5 [get_ports ADC_DATA_P[12]]
set_property PACKAGE_PIN AE6 [get_ports ADC_DATA_P[13]]
set_property PACKAGE_PIN AD6 [get_ports ADC_DATA_P[14]]
set_property PACKAGE_PIN AB6 [get_ports ADC_DATA_P[15]]
set_property PACKAGE_PIN AA8 [get_ports ADC_DATA_P[16]]
set_property PACKAGE_PIN Y8 [get_ports ADC_DATA_P[17]]
set_property PACKAGE_PIN AE8 [get_ports ADC_DATA_P[18]]
set_property PACKAGE_PIN AC8 [get_ports ADC_DATA_P[19]]
set_property PACKAGE_PIN V8 [get_ports ADC_DATA_P[20]]
set_property PACKAGE_PIN V9 [get_ports ADC_DATA_P[21]]
set_property PACKAGE_PIN W10 [get_ports ADC_DATA_P[22]]
set_property PACKAGE_PIN AD10 [get_ports ADC_DATA_P[23]]
set_property PACKAGE_PIN V11 [get_ports ADC_DATA_P[24]]
set_property PACKAGE_PIN AD11 [get_ports ADC_DATA_P[25]]
set_property PACKAGE_PIN AE12 [get_ports ADC_DATA_P[26]]
set_property PACKAGE_PIN AB12 [get_ports ADC_DATA_P[27]]
set_property PACKAGE_PIN AA13 [get_ports ADC_DATA_P[28]]
set_property PACKAGE_PIN AE13 [get_ports ADC_DATA_P[29]]
set_property PACKAGE_PIN AC13 [get_ports ADC_DATA_P[30]]
set_property PACKAGE_PIN Y13 [get_ports ADC_DATA_P[31]]


set_property PACKAGE_PIN U1 [get_ports ADC_DATA_N[0]]
set_property PACKAGE_PIN U5 [get_ports ADC_DATA_N[1]]
set_property PACKAGE_PIN V1 [get_ports ADC_DATA_N[2]]
set_property PACKAGE_PIN W3 [get_ports ADC_DATA_N[3]]
set_property PACKAGE_PIN W4 [get_ports ADC_DATA_N[4]]
set_property PACKAGE_PIN W5 [get_ports ADC_DATA_N[5]]
set_property PACKAGE_PIN Y2 [get_ports ADC_DATA_N[6]]
set_property PACKAGE_PIN Y5 [get_ports ADC_DATA_N[7]]
set_property PACKAGE_PIN AE1 [get_ports ADC_DATA_N[8]]
set_property PACKAGE_PIN AE2 [get_ports ADC_DATA_N[9]]
set_property PACKAGE_PIN AD3 [get_ports ADC_DATA_N[10]]
set_property PACKAGE_PIN AF4 [get_ports ADC_DATA_N[11]]
set_property PACKAGE_PIN AB5 [get_ports ADC_DATA_N[12]]
set_property PACKAGE_PIN AE5 [get_ports ADC_DATA_N[13]]
set_property PACKAGE_PIN AD5 [get_ports ADC_DATA_N[14]]
set_property PACKAGE_PIN AC6 [get_ports ADC_DATA_N[15]]
set_property PACKAGE_PIN AA7 [get_ports ADC_DATA_N[16]]
set_property PACKAGE_PIN Y7 [get_ports ADC_DATA_N[17]]
set_property PACKAGE_PIN AF8 [get_ports ADC_DATA_N[18]]
set_property PACKAGE_PIN AD8 [get_ports ADC_DATA_N[19]]
set_property PACKAGE_PIN V7 [get_ports ADC_DATA_N[20]]
set_property PACKAGE_PIN W8 [get_ports ADC_DATA_N[21]]
set_property PACKAGE_PIN W9 [get_ports ADC_DATA_N[22]]
set_property PACKAGE_PIN AE10 [get_ports ADC_DATA_N[23]]
set_property PACKAGE_PIN W11 [get_ports ADC_DATA_N[24]]
set_property PACKAGE_PIN AE11 [get_ports ADC_DATA_N[25]]
set_property PACKAGE_PIN AF12 [get_ports ADC_DATA_N[26]]
set_property PACKAGE_PIN AC12 [get_ports ADC_DATA_N[27]]
set_property PACKAGE_PIN AA12 [get_ports ADC_DATA_N[28]]
set_property PACKAGE_PIN AF13 [get_ports ADC_DATA_N[29]]
set_property PACKAGE_PIN AD13 [get_ports ADC_DATA_N[30]]
set_property PACKAGE_PIN Y12 [get_ports ADC_DATA_N[31]]


set_property PACKAGE_PIN U7 [get_ports ADC_DFRAME_P[0]]
set_property PACKAGE_PIN AB1 [get_ports ADC_DFRAME_P[1]]
set_property PACKAGE_PIN AE7 [get_ports ADC_DFRAME_P[2]]
set_property PACKAGE_PIN Y11 [get_ports ADC_DFRAME_P[3]]
set_property PACKAGE_PIN V6 [get_ports ADC_DFRAME_N[0]]
set_property PACKAGE_PIN AC1 [get_ports ADC_DFRAME_N[1]]
set_property PACKAGE_PIN AF7 [get_ports ADC_DFRAME_N[2]]
set_property PACKAGE_PIN Y10 [get_ports ADC_DFRAME_N[3]]
set_property PACKAGE_PIN AA3 [get_ports ADC_DCLK_P[0]]
set_property PACKAGE_PIN AA4 [get_ports ADC_DCLK_P[1]]
set_property PACKAGE_PIN AC9 [get_ports ADC_DCLK_P[2]]
set_property PACKAGE_PIN AB11 [get_ports ADC_DCLK_P[3]]
set_property PACKAGE_PIN AA2 [get_ports ADC_DCLK_N[0]]
set_property PACKAGE_PIN AB4 [get_ports ADC_DCLK_N[1]]
set_property PACKAGE_PIN AD9 [get_ports ADC_DCLK_N[2]]
set_property PACKAGE_PIN AC11 [get_ports ADC_DCLK_N[3]]

# MEZZANINE ------------------------------------------------------------
#  ---------------------------------------------------------------------
# Port attribute --
#  ---------------------------------------------------------------------

# System ---------------------------------------------------------------
set_property IOSTANDARD LVCMOS33 [get_ports PROGB_ON]
set_property IOSTANDARD LVDS_25 [get_ports BASE_CLKP]
set_property IOSTANDARD LVCMOS33 [get_ports USR_RSTB]
set_property IOSTANDARD LVCMOS33 [get_ports LED[*]]
set_property IOSTANDARD LVCMOS12 [get_ports DIP[*]]
set_property PULLUP TRUE [get_ports DIP[*]]

# GTX ------------------------------------------------------------------

# SPI flash ------------------------------------------------------------
set_property IOSTANDARD LVCMOS33 [get_ports SPI_MOSI]
set_property IOB TRUE [get_ports SPI_MOSI]
set_property IOSTANDARD LVCMOS33 [get_ports SPI_DIN]
set_property IOB TRUE [get_ports SPI_DIN]
set_property IOSTANDARD LVCMOS33 [get_ports FCSB]
set_property IOB TRUE [get_ports FCSB]

# EEPROM ---------------------------------------------------------------
set_property IOSTANDARD LVCMOS33 [get_ports EEP_CS[*]]
set_property IOSTANDARD LVCMOS33 [get_ports EEP_SK[*]]
set_property IOSTANDARD LVCMOS33 [get_ports EEP_DI[*]]
set_property IOSTANDARD LVCMOS33 [get_ports EEP_DO[*]]

# NIM-IO ---------------------------------------------------------------
set_property IOSTANDARD LVCMOS33 [get_ports NIM_IN[*]]
set_property IOSTANDARD LVCMOS33 [get_ports NIM_OUT[*]]

# JItter cleaner -------------------------------------------------------
set_property IOSTANDARD LVCMOS33 [get_ports CDCE_PDB]

# MIKUMARI -------------------------------------------------------------
set_property IOSTANDARD LVDS [get_ports MIKUMARI_RXP]
set_property DIFF_TERM TRUE [get_ports MIKUMARI_RXP]
set_property IOSTANDARD LVDS [get_ports MIKUMARI_TXP]

# ASIC -----------------------------------------------------------------
set_property IOSTANDARD LVCMOS12 [get_ports ASIC_REFC[*]]
set_property IOSTANDARD LVCMOS12 [get_ports ASIC_SSB[*]]
set_property IOSTANDARD LVCMOS12 [get_ports ASIC_SCK]
set_property IOSTANDARD LVCMOS12 [get_ports ASIC_MOSI]


set_property IOSTANDARD LVCMOS12 [get_ports ASIC_DISCRI[*]]

# TRIGGER_OUT ----------------------------------------------------------

# APD_BIAS -------------------------------------------------------------
set_property IOSTANDARD LVCMOS33 [get_ports CP_CS_B]
set_property IOSTANDARD LVCMOS33 [get_ports CP_SCLK]
set_property IOSTANDARD LVCMOS33 [get_ports CP_DIN]
set_property IOSTANDARD LVCMOS33 [get_ports CP_CL_B]

# ASIC_ADC -------------------------------------------------------------
set_property IOSTANDARD LVDS [get_ports ADC_DATA_P[*]]
set_property DIFF_TERM TRUE [get_ports ADC_DATA_P[*]]




set_property IOSTANDARD LVDS [get_ports ADC_DFRAME_P[*]]
set_property DIFF_TERM TRUE [get_ports ADC_DFRAME_P[*]]
set_property IOSTANDARD LVDS [get_ports ADC_DCLK_P[*]]
set_property DIFF_TERM TRUE [get_ports ADC_DCLK_P[*]]

# MEZZANINE ------------------------------------------------------------
