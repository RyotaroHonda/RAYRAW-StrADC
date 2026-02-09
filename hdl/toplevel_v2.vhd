library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_MISC.ALL;
use ieee.numeric_std.all;

library UNISIM;
use UNISIM.VComponents.all;

library mylib;
use mylib.defBCT.all;
use mylib.defBusAddressMap.all;
use mylib.defCDCM.all;
use mylib.defMikumari.all;
use mylib.defMikumariUtil.all;
use mylib.defLaccp.all;
use mylib.defHeartBeatUnit.all;
use mylib.defFreeRunScaler.all;
use mylib.defSiTCP.all;
use mylib.defRBCP.all;

use mylib.defYaenamiAdc.all;
use mylib.defRayrawStrAdcROV1.all;
use mylib.defYAENAMIController.all; -- Slow Control
use mylib.defMAX.all; -- APD_BIAS

use mylib.defDataBusAbst.all;
use mylib.defDelimiter.all;
use mylib.defTDC.all;
--use mylib.defStrLRTDC.all;
use mylib.defIOManager.all;

entity toplevel is
  Port (
-- System ---------------------------------------------------------------
PROGB_ON            : out std_logic;
BASE_CLKP           : in std_logic;
BASE_CLKN           : in std_logic;
USR_RSTB            : in std_logic;
LED                 : out std_logic_vector(4 downto 1);
DIP                 : in std_logic_vector(8 downto 1);
VP                  : in std_logic;
VN                  : in std_logic;

-- GTX ------------------------------------------------------------------
GTX_REFCLK_P        : in std_logic;
GTX_REFCLK_N        : in std_logic;
GTX_TX_P            : out std_logic_vector(1 downto 1);
GTX_RX_P            : in std_logic_vector(1 downto 1);
GTX_TX_N            : out std_logic_vector(1 downto 1);
GTX_RX_N            : in std_logic_vector(1 downto 1);

-- SPI flash ------------------------------------------------------------
SPI_MOSI            : out std_logic;
SPI_DIN             : in std_logic;
FCSB                : out std_logic;

-- EEPROM ---------------------------------------------------------------
EEP_CS              : out std_logic_vector(1 downto 1);
EEP_SK              : out std_logic_vector(1 downto 1);
EEP_DI              : out std_logic_vector(1 downto 1);
EEP_DO              : in std_logic_vector(1 downto 1);

--EEP2_SCL            : out std_logic;
--EEP2_SDA            : inout std_logic;

-- NIM-IO ---------------------------------------------------------------
NIM_IN              : in std_logic_vector(2 downto 1);
NIM_OUT             : out std_logic_vector(4 downto 1);

-- JItter cleaner -------------------------------------------------------
CDCE_PDB            : out std_logic;

-- MIKUMARI -------------------------------------------------------------
MIKUMARI_RXP        : in std_logic;
MIKUMARI_RXN        : in std_logic;
MIKUMARI_TXP        : out std_logic;
MIKUMARI_TXN        : out std_logic;

-- ASIC -----------------------------------------------------------------
ASIC_REFC           : out std_logic_vector(3 downto 0);
ASIC_SSB            : out std_logic_vector(3 downto 0);
ASIC_SCK            : out std_logic;
ASIC_MOSI           : out std_logic;

ASIC_DISCRI         : in std_logic_vector(31 downto 0);

-- MISC -----------------------------------------------------------------
EN3V3A              : out std_logic;
AMUX_ADDR           : out std_logic_vector(1 downto 0);
--TEST_POINT          : out std_logic_vector(3 downto 0);

-- APD_BIAS -------------------------------------------------------------
CP_CS_B             : out std_logic;
CP_SCLK             : out std_logic;
CP_DIN              : out std_logic;
CP_CL_B             : in std_logic;

-- ASIC_ADC -------------------------------------------------------------
ADC_DATA_P          : in std_logic_vector(31 downto 0);
ADC_DATA_N          : in std_logic_vector(31 downto 0);


ADC_DFRAME_P        : in std_logic_vector(3 downto 0);
ADC_DFRAME_N        : in std_logic_vector(3 downto 0);
ADC_DCLK_P          : in std_logic_vector(3 downto 0);
ADC_DCLK_N          : in std_logic_vector(3 downto 0)

-- MEZZANINE ------------------------------------------------------------

  );
end toplevel;

architecture Behavioral of toplevel is
  attribute mark_debug : string;
  constant kEnDebugTop  : string:= "false";

  -- System --------------------------------------------------------------------------------
  -- AMANEQ specification
  constant kNumLED      : integer:= 4;
  constant kNumBitDIP   : integer:= 4;
  constant kNumNIM      : integer:= 2;
  constant kNumGtx      : integer:= 1;

  signal sitcp_reset  : std_logic;
  signal raw_pwr_on_reset : std_logic;
  signal pwr_on_reset : std_logic;
  signal system_reset : std_logic;
  signal user_reset   : std_logic;
  signal hbu_reset    : std_logic;

  signal mii_reset    : std_logic;
  signal emergency_reset  : std_logic_vector(kNumGtx-1 downto 0);

  signal bct_reset    : std_logic;
  signal rst_from_bus : std_logic;

  signal delayed_usr_rstb : std_logic;

  signal module_ready     : std_logic;

  signal sync_nim_in      : std_logic_vector(NIM_IN'range);
  signal tmp_nim_out      : std_logic_vector(NIM_OUT'range);

  signal local_frame_flag : std_logic_vector(kWidthFrameFlag-1 downto 0);
  signal frame_flag_out   : std_logic_vector(kWidthFrameFlag-1 downto 0);

  signal local_trigger_in : std_logic;

  -- Hit Input definition --
  constant kNumInput    : integer:= 32;

  -- DIP -----------------------------------------------------------------------------------
  signal dip_sw       : std_logic_vector(DIP'range);
  subtype DipID is integer range 0 to 8;
  type regLeaf is record
    Index : DipID;
  end record;
  constant kSiTCP       : regLeaf := (Index => 1);
  constant kTriggerOut  : regLeaf := (Index => 2);
  constant kStandAlone  : regLeaf := (Index => 3);
  constant kNC4         : regLeaf := (Index => 4);
  constant kNC5         : regLeaf := (Index => 4);
  constant kNC6         : regLeaf := (Index => 4);
  constant kNC7         : regLeaf := (Index => 4);
  constant kNC8         : regLeaf := (Index => 4);
  constant kDummy       : regLeaf := (Index => 0);


  -- MIKUMARI -----------------------------------------------------------------------------
  constant  kPcbVersion : string:= "v1";
--  constant  kPcbVersion : string:= "GN-2006-1";

  function GetMikuIoStd(version: string) return string is
  begin
    case version is
      when  "v1" => return "LVDS";
      when others       => return "LVDS_25";
    end case;
  end function;

  constant kNumMikumari       : integer:= 1;
  constant kIdMikuSec         : integer:= 0;

  signal miku_txp, miku_txn, miku_rxp, miku_rxn   : std_logic_vector(kNumMikumari-1 downto 0);

  subtype MikuScalarPort is std_logic_vector(kNumMikumari-1 downto 0);

  -- CDCM --
  signal power_on_init        : std_logic;

  signal cbt_lane_up          : MikuScalarPort;
  signal pattern_error        : MikuScalarPort;
  signal watchdog_error       : MikuScalarPort;

  signal mod_clk, gmod_clk    : std_logic;

  --type TapValueArray  is array(kNumMikumari-1 downto 0) of std_logic_vector(kWidthTap-1 downto 0);
  --type SerdesOffsetArray is array(kNumMikumari-1 downto 0) of signed(kWidthSerdesOffset-1 downto 0);
  signal tap_value_out        : TapArrayType(kNumMikumari-1 downto 0);
  signal bitslip_num_out      : BitslipArrayType(kNumMikumari-1 downto 0);
  signal serdes_offset        : SerdesOfsArrayType(kNumMikumari-1 downto 0);

  -- Mikumari --
  type MikuDataArray is array(kNumMikumari-1 downto 0) of std_logic_vector(7 downto 0);
  type MikuPulseTypeArray is array(kNumMikumari-1 downto 0) of MikumariPulseType;

  signal miku_tx_ack        : MikuScalarPort;
  signal miku_data_tx       : MikuDataArray;
  signal miku_valid_tx      : MikuScalarPort;
  signal miku_last_tx       : MikuScalarPort;
  signal busy_pulse_tx      : MikuScalarPort;

  signal mikumari_link_up   : MikuScalarPort;
  signal miku_data_rx       : MikuDataArray;
  signal miku_valid_rx      : MikuScalarPort;
  signal miku_last_rx       : MikuScalarPort;
  signal checksum_err       : MikuScalarPort;
  signal frame_broken       : MikuScalarPort;
  signal recv_terminated    : MikuScalarPort;

  signal pulse_tx, pulse_rx : MikuScalarPort;
  signal pulse_type_tx, pulse_type_rx  : MikuPulseTypeArray;

 -- LACCP --
  signal laccp_reset        : MikuScalarPort;
  signal laccp_pulse_out    : std_logic_vector(kNumLaccpPulse-1 downto 0);

  signal is_ready_for_daq   : MikuScalarPort;
  signal sync_pulse_out     : std_logic;

  signal is_ready_laccp_intra   : std_logic_vector(kNumExtIntraPort-1 downto 0);
  signal valid_laccp_intra_in   : std_logic_vector(kNumExtIntraPort-1 downto 0);
  signal valid_laccp_intra_out  : std_logic_vector(kNumExtIntraPort-1 downto 0);
  signal data_laccp_intra_in    : ExtIntraType;
  signal data_laccp_intra_out   : ExtIntraType;

  -- RLIGP --
  --type LinkAddrArray is array(kNumMikumari-1 downto 0) of std_logic_vector(kPosRegister'range);

  signal link_addr_partter  : IpAddrArrayType(kNumMikumari-1 downto 0);
  signal valid_link_addr    : MikuScalarPort;

  -- RCAP --
--  signal idelay_tap_in      : unsigned(tap_value_out'range);

  signal valid_hbc_offset   : std_logic;
  signal hbc_offset         : std_logic_vector(kWidthHbCount-1 downto 0);
  signal laccp_fine_offset  : signed(kWidthLaccpFineOffset-1 downto 0);
  signal local_fine_offset  : signed(kWidthLaccpFineOffset-1 downto 0);

  -- Heartbeat --
  signal hbu_is_synchronized  : std_logic;
  signal heartbeat_signal   : std_logic;
  signal heartbeat_count    : std_logic_vector(kWidthHbCount-1 downto 0);
  signal hbf_number         : std_logic_vector(kWidthHbfNum-1 downto 0);
  signal hbf_state          : HbfStateType;
  signal frame_ctrl_gate    : std_logic;
  signal hbf_num_mismatch   : std_logic;

  attribute mark_debug of is_ready_for_daq   : signal is kEnDebugTop;
  attribute mark_debug of sync_pulse_out     : signal is kEnDebugTop;
  attribute mark_debug of serdes_offset      : signal is kEnDebugTop;
  attribute mark_debug of laccp_fine_offset  : signal is kEnDebugTop;
  attribute mark_debug of local_fine_offset  : signal is kEnDebugTop;

  -- Mikumari Util ------------------------------------------------------------
  signal cbt_init_from_mutil  : MikuScalarPort;
  signal rst_from_miku        : std_logic;

  -- Scaler -------------------------------------------------------------------
  constant kMsbScr      : integer:= kNumSysInput+kNumInput-1;
  signal scr_en_in      : std_logic_vector(kMsbScr downto 0):= (others => '0');
  signal scr_gate       : std_logic_vector(kNumScrGate-1 downto 0);

  -- YSC ----------------------------------------------------------------------
  constant kNumAsic     : integer:= 4;

  -- MAX1932Controller --------------------------------------------------------

  -- Streaming ADC ------------------------------------------------------------
  COMPONENT vio_0
  PORT (
    clk : IN STD_LOGIC;
    probe_out0 : OUT STD_LOGIC_VECTOR(0 DOWNTO 0);
    probe_out1 : OUT STD_LOGIC_VECTOR(0 DOWNTO 0)
  );
  END COMPONENT;

  signal reset_vio         : std_logic_vector(0 downto 0);
  signal reset_adc_cycle   : std_logic_vector(0 downto 0);


  -- Streaming TDC ------------------------------------------------------------
  -- scaler --
  constant kNumScrThr   : integer:= 5;
  signal hit_out        : std_logic_vector(kNumInput-1 downto 0):= (others => '0');
  signal scr_thr_on     : std_logic_vector(kNumScrThr-1 downto 0);
  signal daq_is_runnig  : std_logic;

  signal signal_in_merge    : std_logic_vector(kNumInput-1 downto 0);
  signal strtdc_trigger_in  : std_logic;

  -- VitalBlock output --
  signal vital_rden         : std_logic;
  signal vital_dout         : std_logic_vector(kWidthData-1 downto 0);
  signal vital_empty        : std_logic;
  signal vital_almost_empty : std_logic;
  signal vital_valid        : std_logic;

  -- Link buffer --
  signal din_link_buf       : std_logic_vector(kWidthData-1 downto 0);
  signal pfull_link_buf     : std_logic;
  signal full_link_buf      : std_logic;

  --attribute mark_debug of vital_valid : signal is "true";
  --attribute mark_debug of pfull_link_buf : signal is "true";
  --attribute mark_debug of full_link_buf : signal is "true";


  COMPONENT LinkBuffer
  PORT (
    rst : IN STD_LOGIC;
    wr_clk : IN STD_LOGIC;
    rd_clk : IN STD_LOGIC;
    din : IN STD_LOGIC_VECTOR(63 DOWNTO 0);
    wr_en : IN STD_LOGIC;
    rd_en : IN STD_LOGIC;
    dout : OUT STD_LOGIC_VECTOR(7 DOWNTO 0);
    full : OUT STD_LOGIC;
    empty : OUT STD_LOGIC;
    valid : OUT STD_LOGIC;
    prog_full : OUT STD_LOGIC;
    wr_rst_busy : OUT STD_LOGIC;
    rd_rst_busy : OUT STD_LOGIC
  );
  END COMPONENT;

  -- IOM ----------------------------------------------------------------------------------
  signal intsig_from_iom        : std_logic_vector(3 downto 0);
  signal intsig_to_iom          : std_logic_vector(7 downto 0);

  -- C6C ----------------------------------------------------------------------------------
  signal c6c_reset              : std_logic;
  signal c6c_fast, c6c_slow     : std_logic;

  -- MIG ----------------------------------------------------------------------------------

  -- SDS ---------------------------------------------------------------------
  signal shutdown_over_temp     : std_logic;
  signal uncorrectable_flag     : std_logic;

  -- FMP ---------------------------------------------------------------------

  -- BCT -----------------------------------------------------------------------------------
  signal addr_LocalBus          : LocalAddressType;
  signal data_LocalBusIn        : LocalBusInType;
  signal data_LocalBusOut       : DataArray;
  signal re_LocalBus            : ControlRegArray;
  signal we_LocalBus            : ControlRegArray;
  signal ready_LocalBus         : ControlRegArray;

  -- TSD -----------------------------------------------------------------------------------
  type typeTcpData is array(kNumGtx-1 downto 0) of std_logic_vector(kWidthDataTCP-1 downto 0);
  signal wd_to_tsd                              : typeTcpData;
  signal we_to_tsd, empty_to_tsd, re_from_tsd   : std_logic_vector(kNumGtx-1 downto 0);

  -- SiTCP ---------------------------------------------------------------------------------
  type typeUdpAddr is array(kNumGtx-1 downto 0) of std_logic_vector(kWidthAddrRBCP-1 downto 0);
  type typeUdpData is array(kNumGtx-1 downto 0) of std_logic_vector(kWidthDataRBCP-1 downto 0);
  type typeIpAddr  is array(kNumGtx-1 downto 0) of std_logic_vector(31 downto 0);

  signal sitcp_ip_addr  : typeIpAddr;

  signal tcp_isActive, close_req, close_act    : std_logic_vector(kNumGtx-1 downto 0);

  signal tcp_tx_clk   : std_logic_vector(kNumGtx-1 downto 0);
  signal tcp_rx_wr    : std_logic_vector(kNumGtx-1 downto 0);
  signal tcp_rx_data  : typeTcpData;
  signal tcp_tx_full  : std_logic_vector(kNumGtx-1 downto 0);
  signal tcp_tx_wr    : std_logic_vector(kNumGtx-1 downto 0);
  signal tcp_tx_data  : typeTcpData;

  signal rbcp_addr    : typeUdpAddr;
  signal rbcp_wd      : typeUdpData;
  signal rbcp_we      : std_logic_vector(kNumGtx-1 downto 0); --: Write enable
  signal rbcp_re      : std_logic_vector(kNumGtx-1 downto 0); --: Read enable
  signal rbcp_ack     : std_logic_vector(kNumGtx-1 downto 0); -- : Access acknowledge
  signal rbcp_rd      : typeUdpData;

  signal rbcp_gmii_addr    : typeUdpAddr;
  signal rbcp_gmii_wd      : typeUdpData;
  signal rbcp_gmii_we      : std_logic_vector(kNumGtx-1 downto 0); --: Write enable
  signal rbcp_gmii_re      : std_logic_vector(kNumGtx-1 downto 0); --: Read enable
  signal rbcp_gmii_ack     : std_logic_vector(kNumGtx-1 downto 0); -- : Access acknowledge
  signal rbcp_gmii_rd      : typeUdpData;

  attribute mark_debug  of sitcp_ip_addr  : signal is kEnDebugTop;

  component WRAP_SiTCP_GMII_XC7K_32K
    port
      (
        CLK                   : in std_logic; --: System Clock >129MHz
        RST                   : in std_logic; --: System reset
        -- Configuration parameters
        FORCE_DEFAULTn        : in std_logic; --: Load default parameters
        EXT_IP_ADDR           : in std_logic_vector(31 downto 0); --: IP address[31:0]
        EXT_TCP_PORT          : in std_logic_vector(15 downto 0); --: TCP port #[15:0]
        EXT_RBCP_PORT         : in std_logic_vector(15 downto 0); --: RBCP port #[15:0]
        PHY_ADDR              : in std_logic_vector(4 downto 0);  --: PHY-device MIF address[4:0]

        -- EEPROM
        EEPROM_CS             : out std_logic; --: Chip select
        EEPROM_SK             : out std_logic; --: Serial data clock
        EEPROM_DI             : out    std_logic; --: Serial write data
        EEPROM_DO             : in std_logic; --: Serial read data
        --    user data, intialial values are stored in the EEPROM, 0xFFFF_FC3C-3F
        USR_REG_X3C           : out    std_logic_vector(7 downto 0); --: Stored at 0xFFFF_FF3C
        USR_REG_X3D           : out    std_logic_vector(7 downto 0); --: Stored at 0xFFFF_FF3D
        USR_REG_X3E           : out    std_logic_vector(7 downto 0); --: Stored at 0xFFFF_FF3E
        USR_REG_X3F           : out    std_logic_vector(7 downto 0); --: Stored at 0xFFFF_FF3F
        -- MII interface
        GMII_RSTn             : out    std_logic; --: PHY reset
        GMII_1000M            : in std_logic;  --: GMII mode (0:MII, 1:GMII)
        -- TX
        GMII_TX_CLK           : in std_logic; -- : Tx clock
        GMII_TX_EN            : out    std_logic; --: Tx enable
        GMII_TXD              : out    std_logic_vector(7 downto 0); --: Tx data[7:0]
        GMII_TX_ER            : out    std_logic; --: TX error
        -- RX
        GMII_RX_CLK           : in std_logic; -- : Rx clock
        GMII_RX_DV            : in std_logic; -- : Rx data valid
        GMII_RXD              : in std_logic_vector(7 downto 0); -- : Rx data[7:0]
        GMII_RX_ER            : in std_logic; --: Rx error
        GMII_CRS              : in std_logic; --: Carrier sense
        GMII_COL              : in std_logic; --: Collision detected
        -- Management IF
        GMII_MDC              : out std_logic; --: Clock for MDIO
        GMII_MDIO_IN          : in std_logic; -- : Data
        GMII_MDIO_OUT         : out    std_logic; --: Data
        GMII_MDIO_OE          : out    std_logic; --: MDIO output enable
        -- User I/F
        SiTCP_RST             : out    std_logic; --: Reset for SiTCP and related circuits
        IP_ADDR               : out    std_logic_vector(31 downto 0);
        -- TCP connection control
        TCP_OPEN_REQ          : in std_logic; -- : Reserved input, shoud be 0
        TCP_OPEN_ACK          : out    std_logic; --: Acknowledge for open (=Socket busy)
        TCP_ERROR             : out    std_logic; --: TCP error, its active period is equal to MSL
        TCP_CLOSE_REQ         : out    std_logic; --: Connection close request
        TCP_CLOSE_ACK         : in std_logic ;-- : Acknowledge for closing
        -- FIFO I/F
        TCP_RX_WC             : in std_logic_vector(15 downto 0); --: Rx FIFO write count[15:0] (Unused bits should be set 1)
        TCP_RX_WR             : out    std_logic; --: Write enable
        TCP_RX_DATA           : out    std_logic_vector(7 downto 0); --: Write data[7:0]
        TCP_TX_FULL           : out    std_logic; --: Almost full flag
        TCP_TX_WR             : in std_logic; -- : Write enable
        TCP_TX_DATA           : in std_logic_vector(7 downto 0); -- : Write data[7:0]
        -- RBCP
        RBCP_ACT              : out std_logic; -- RBCP active
        RBCP_ADDR             : out    std_logic_vector(31 downto 0); --: Address[31:0]
        RBCP_WD               : out    std_logic_vector(7 downto 0); --: Data[7:0]
        RBCP_WE               : out    std_logic; --: Write enable
        RBCP_RE               : out    std_logic; --: Read enable
        RBCP_ACK              : in std_logic; -- : Access acknowledge
        RBCP_RD               : in std_logic_vector(7 downto 0 ) -- : Read data[7:0]
        );
  end component;

  -- SFP transceiver -----------------------------------------------------------------------
  constant kPcsPmaLinkStatus  : integer:= 0;
  signal pcs_pma_status       : std_logic_vector(15 downto 0);

  constant kWidthPhyAddr  : integer:= 5;
  constant kMiiPhyad      : std_logic_vector(kWidthPhyAddr-1 downto 0):= "00000";
  signal mii_init_mdc, mii_init_mdio : std_logic;

  component mii_initializer is
    port(
      -- System
      CLK         : in std_logic;
      --RST         => system_reset,
      RST         : in std_logic;
      -- PHY
      PHYAD       : in std_logic_vector(kWidthPhyAddr-1 downto 0);
      -- MII
      MDC         : out std_logic;
      MDIO_OUT    : out std_logic;
      -- status
      COMPLETE    : out std_logic
      );
  end component;

  signal mmcm_reset_all   : std_logic;
  signal mmcm_reset       : std_logic_vector(kNumGtx-1 downto 0);
  signal mmcm_locked      : std_logic;

  signal gt0_qplloutclk, gt0_qplloutrefclk  : std_logic;
  signal gtrefclk_i, gtrefclk_bufg  : std_logic;
  signal txout_clk, rxout_clk       : std_logic_vector(kNumGtx-1 downto 0);
  signal user_clk, user_clk2, rxuser_clk, rxuser_clk2   : std_logic;

  signal eth_tx_clk       : std_logic_vector(kNumGtx-1 downto 0);
  signal eth_tx_en        : std_logic_vector(kNumGtx-1 downto 0);
  signal eth_tx_er        : std_logic_vector(kNumGtx-1 downto 0);
  signal eth_tx_d         : typeTcpData;

  signal eth_rx_clk       : std_logic_vector(kNumGtx-1 downto 0);
  signal eth_rx_dv        : std_logic_vector(kNumGtx-1 downto 0);
  signal eth_rx_er        : std_logic_vector(kNumGtx-1 downto 0);
  signal eth_rx_d         : typeTcpData;


  -- Clock ---------------------------------------------------------------------------
  signal clk_gbe, clk_sys   : std_logic;
  signal clk_locked         : std_logic;
  signal clk_sys_locked     : std_logic;
  signal clk_miku_locked    : std_logic;
  signal clk_spi            : std_logic;


  component clk_wiz_sys
    port
      (-- Clock in ports
        -- Clock out ports
        clk_sys          : out    std_logic;
        clk_indep_gtx    : out    std_logic;
        clk_spi          : out    std_logic;
--        clk_buf          : out    std_logic;
        -- Status and control signals
        reset            : in     std_logic;
        locked           : out    std_logic;
        clk_in1_p        : in     std_logic;
        clk_in1_n        : in     std_logic
        );
  end component;

  component mmcm_cdcm
    port
     (-- Clock in ports
      clk_in2           : in     std_logic;
      clk_in_sel           : in     std_logic;
      -- Clock out ports
      clk_slow          : out    std_logic;
      clk_fast          : out    std_logic;
      clk_tdc0          : out    std_logic;
      clk_tdc1          : out    std_logic;
      clk_tdc2          : out    std_logic;
      clk_tdc3          : out    std_logic;
      -- Status and control signals
      reset             : in     std_logic;
      locked            : out    std_logic;
      clk_in1           : in     std_logic
     );
    end component;

  signal clk_fast, clk_slow   : std_logic;
  signal clk_tdc              : std_logic_vector(kNumTdcClock-1 downto 0);
  signal clk_sys_div16        : std_logic;
  signal mmcm_cdcm_locked     : std_logic;
  signal mmcm_cdcm_reset      : std_logic;
  --signal pll_is_locked        : std_logic;

  component clk_wiz_adc
    port
     (-- Clock in ports
      -- Clock out ports
      clk_adc          : out    std_logic;
      -- Status and control signals
      reset             : in     std_logic;
      locked            : out    std_logic;
      clk_in1           : in     std_logic
     );
    end component;

  signal clk_adcref     : std_logic;
  signal clk_adc_locked : std_logic;


 begin
  -- ===================================================================================
  -- body
  -- ===================================================================================

  -- Global ----------------------------------------------------------------------------
  u_DelayUsrRstb : entity mylib.DelayGen
    generic map(kNumDelay => 128)
    port map(clk_sys, USR_RSTB, delayed_usr_rstb);

  --clk_miku_locked <= CDCE_LOCK;
  clk_miku_locked <= mmcm_cdcm_locked;
  clk_locked      <= clk_sys_locked and clk_miku_locked and clk_adc_locked;

  --c6c_reset       <= (not clk_sys_locked) or (not delayed_usr_rstb);
  c6c_reset       <= '1';
  mmcm_cdcm_reset <= (not delayed_usr_rstb);

  system_reset      <= (not clk_miku_locked) or (not USR_RSTB);
  raw_pwr_on_reset  <= (not clk_sys_locked);-- or (not USR_RSTB);
  u_KeepPwrOnRst : entity mylib.RstDelayTimer
    port map(raw_pwr_on_reset, X"0FFFFFFF", clk_sys, module_ready, pwr_on_reset);

  u_RstFromMiku : entity mylib.SigStretcher
    generic map(kLength => 8)
    port map(clk_slow, laccp_pulse_out(kDownPulseSysRst), rst_from_miku);

  user_reset      <= system_reset or rst_from_bus or rst_from_miku;
  bct_reset       <= system_reset or rst_from_miku;

  -- NIM input --
  u_sync_nimin1 : entity mylib.synchronizer port map(clk_slow, NIM_IN(1), sync_nim_in(1));
  u_sync_nimin2 : entity mylib.synchronizer port map(clk_slow, NIM_IN(2), sync_nim_in(2));

  -- NIM output --
  u_nimo_buf : process(clk_slow)
  begin
    if(clk_slow'event and clk_slow = '1') then
      NIM_OUT <= tmp_nim_out;
    end if;
  end process;

--  tmp_nim_out(1)  <= laccp_pulse_out(kDownPulseTrigger) when(dip_sw(kTriggerOut.Index) = '1') else heartbeat_signal;
--  tmp_nim_out(2)  <= tcp_isActive(0);

  dip_sw   <= DIP;

  LED         <= (clk_miku_locked and module_ready) & mikumari_link_up(kIdMikuSec) & is_ready_for_daq & daq_is_runnig;

  EN3V3A      <= '1';

  -- Mezzanine connection --------------------------------------------------------------
  MIKUMARI_TXP  <= miku_txp(kIdMikuSec);
  MIKUMARI_TXN  <= miku_txn(kIdMikuSec);
  miku_rxp(kIdMikuSec)  <= MIKUMARI_RXP;
  miku_rxn(kIdMikuSec)  <= MIKUMARI_RXN;

  -- MIKUMARI --------------------------------------------------------------------------
  u_KeepInit : entity mylib.RstDelayTimer
    port map(system_reset, X"0FFFFFFF", clk_slow, open, power_on_init );

  gen_mikumari : for i in 0 to kNumMikumari-1 generate
    laccp_reset(i) <= system_reset or (not mikumari_link_up(i));

    u_Miku_Inst : entity mylib.MikumariBlock
      generic map(
        -- CBT generic -------------------------------------------------------------
        -- CDCM-Mod-Pattern --
        kCdcmModWidth    => 8,
        -- CDCM-TX --
        kIoStandardTx    => GetMikuIoStd(kPcbVersion),
        kTxPolarity      => TRUE,
        -- CDCM-RX --
        genIDELAYCTRL    => TRUE,
        kDiffTerm        => TRUE,
        kIoStandardRx    => GetMikuIoStd(kPcbVersion),
        kRxPolarity      => TRUE,
        kIoDelayGroup    => "idelay_miku",
        kFixIdelayTap    => FALSE,
        kFreqFastClk     => 500.0,
        kFreqRefClk      => 200.0,
        -- Encoder/Decoder
        kNumEncodeBits   => 1,
        -- Master/Slave
        kCbtMode         => "Slave",
        -- DEBUG --
        enDebugCBT       => FALSE,

        -- MIKUMARI generic --------------------------------------------------------
        enScrambler      => TRUE,
        kHighPrecision   => FALSE,
        -- DEBUG --
        enDebugMikumari  => FALSE
      )
      port map(
        -- System ports -----------------------------------------------------------
        rst           => system_reset,
        pwrOnRst      => pwr_on_reset,
        clkSer        => clk_fast,
        clkPar        => clk_slow,
        clkIndep      => clk_gbe,
        clkIdctrl     => clk_gbe,
        initIn        => power_on_init or cbt_init_from_mutil(i),

        TXP           => MIKUMARI_TXP,
        TXN           => MIKUMARI_TXN,
        RXP           => MIKUMARI_RXP,
        RXN           => MIKUMARI_RXN,
        modClk        => mod_clk,
        tapValueIn    => (others => '0'),
        txBeat        => open,

        -- CBT ports ------------------------------------------------------------
        laneUp        => cbt_lane_up(i),
        idelayErr     => open,
        bitslipErr    => open,
        pattErr       => pattern_error(i),
        watchDogErr   => watchdog_error(i),

        tapValueOut   => tap_value_out(i),
        bitslipNum    => bitslip_num_out(i),
        serdesOffset  => serdes_offset(i),
        firstBitPatt  => open,

        -- Mikumari ports -------------------------------------------------------
        linkUp        => mikumari_link_up(i),

        -- TX port --
        -- Data I/F --
        dataInTx      => miku_data_tx(i),
        validInTx     => miku_valid_tx(i),
        frameLastInTx => miku_last_tx(i),
        txAck         => miku_tx_ack(i),

        pulseIn       => pulse_tx(i),
        pulseTypeTx   => pulse_type_tx(i),
        pulseRegTx    => "0000",
        busyPulseTx   => busy_pulse_tx(i),

        -- RX port --
        -- Data I/F --
        dataOutRx   => miku_data_rx(i),
        validOutRx  => miku_valid_rx(i),
        frameLastRx => miku_last_rx(i),
        checksumErr => checksum_err(i),
        frameBroken => frame_broken(i),
        recvTermnd  => recv_terminated(i),

        pulseOut    => pulse_rx(i),
        pulseTypeRx => pulse_type_rx(i),
        pulseRegRx  => open

      );

    --
    --idelay_tap_in   <= unsigned(tap_value_out);

    u_LACCP : entity mylib.LaccpMainBlock
      generic map
        (
          kPrimaryMode      => false,
          kNumInterconnect  => 1,
          enDebug           => false
        )
      port map
        (
          -- System --------------------------------------------------------
          rst               => laccp_reset(i),
          clk               => clk_slow,

          -- User Interface ------------------------------------------------
          isReadyForDaq     => is_ready_for_daq(i),
          laccpPulsesIn     => (others => '0'),
          laccpPulsesOut    => laccp_pulse_out,
          pulseInRejected   => open,

          -- RLIGP --
          addrMyLink        => sitcp_ip_addr(0),
          validMyLink       => not emergency_reset(0),
          addrPartnerLink   => link_addr_partter(i),
          validPartnerLink  => valid_link_addr(i),

          -- RCAP --
          idelayTapIn       => unsigned(tap_value_out(i)),
          serdesLantencyIn  => serdes_offset(i),
          idelayTapOut      => open,
          serdesLantencyOut => open,

          hbuIsSyncedIn     => hbu_is_synchronized,
          syncPulseIn       => '0',
          syncPulseOut      => sync_pulse_out,

          upstreamOffset    => (others => '0'),
          validOffset       => valid_hbc_offset,
          hbcOffset         => hbc_offset,
          fineOffset        => laccp_fine_offset,
          fineOffsetLocal   => local_fine_offset,

          -- LACCP Bus Port ------------------------------------------------
          -- Intra-port--
          isReadyIntraIn    => is_ready_laccp_intra,
          dataIntraIn       => data_laccp_intra_in,
          validIntraIn      => valid_laccp_intra_in,
          dataIntraOut      => data_laccp_intra_out,
          validIntraOut     => valid_laccp_intra_out,

          -- Interconnect --
          isReadyInterIn    => (others => '0'),
          existInterOut     => open,
          dataInterIn       => (others => (others => '0')),
          validInterIn      => (others => '0'),
          dataInterOut      => open,
          validInterOut     => open,

          -- MIKUMARI-Link -------------------------------------------------
          mikuLinkUpIn      => mikumari_link_up(i),

          -- TX port --
          dataTx            => miku_data_tx(i),
          validTx           => miku_valid_tx(i),
          frameLastTx       => miku_last_tx(i),
          txAck             => miku_tx_ack(i),

          pulseTx           => pulse_tx(i),
          pulseTypeTx       => pulse_type_tx(i),
          busyPulseTx       => busy_pulse_tx(i),

          -- RX port --
          dataRx            => miku_data_rx(i),
          validRx           => miku_valid_rx(i),
          frameLastRx       => miku_last_rx(i),
          checkSumErrRx     => checksum_err(i),
          frameBrokenRx     => frame_broken(i),
          recvTermndRx      => recv_terminated(i),

          pulseRx           => pulse_rx(i),
          pulseTypeRx       => pulse_type_rx(i)

        );
  end generate;

  --
  --u_sync_nim1 : entity mylib.synchronizer port map(clk_slow, NIM_IN(1), frame_ctrl_gate);
  frame_ctrl_gate <= '0';
  hbu_reset       <= system_reset when(dip_sw(kStandAlone.Index) = '1') else laccp_reset(0);

  u_HBU : entity mylib.HeartBeatUnit
    generic map
      (
        enDebug           => false
      )
    port map
      (
        -- System --
        rst               => hbu_reset,
        clk               => clk_slow,
        enStandAlone      => DIP(kStandAlone.Index),
        keepLocalHbfNum   => '1',

        -- Sync I/F --
        syncPulseIn       => sync_pulse_out,
        hbcOffsetIn       => hbc_offset,
        validOffsetIn     => valid_hbc_offset,
        isSynchronized    => hbu_is_synchronized,

        -- HeartBeat I/F --
        heartbeatOut      => heartbeat_signal,
        heartbeatCount    => heartbeat_count,
        hbfNumber         => hbf_number,
        hbfNumMismatch    => hbf_num_mismatch,
        clkDiv16          => clk_sys_div16,

        -- DAQ I/F --
        hbfCtrlGateIn     => frame_ctrl_gate,
        forceOn           => '1',
        frameState        => hbf_state,

        hbfFlagsIn        => local_frame_flag,
        frameFlags        => frame_flag_out,

        -- LACCP Bus --
        dataBusIn         => data_laccp_intra_out(GetExtIntraIndex(kPortHBU)),
        validBusIn        => valid_laccp_intra_out(GetExtIntraIndex(kPortHBU)),
        dataBusOut        => data_laccp_intra_in(GetExtIntraIndex(kPortHBU)),
        validBusOut       => valid_laccp_intra_in(GetExtIntraIndex(kPortHBU)),
        isReadyOut        => is_ready_laccp_intra(GetExtIntraIndex(kPortHBU))

      );

  -- MIKUMARI utility ---------------------------------------------------------------------
  u_MUTIL : entity mylib.MikumariUtil
    generic map(
      kNumMikumari => kNumMikumari,
      kSecondaryId => kIdMikuSec
    )
    port map(
      -- System ----------------------------------------------------
      rst               => user_reset,
      clk               => clk_slow,

      clockRootMode     => DIP(kStandAlone.Index),

      -- CBT status ports --
      cbtLaneUp           => cbt_lane_up,
      tapValueIn          => tap_value_out,
      bitslipNumIn        => bitslip_num_out,
      cbtInitOut          => cbt_init_from_mutil,
      tapValueOut         => open,
      rstOverMikuOut      => open,

      -- MIKUMARI Link ports --
      mikuLinkUp          => mikumari_link_up,

      -- LACCP ports --
      laccpUp             => is_ready_for_daq,
      partnerIpAddr       => link_addr_partter,
      hbcOffset           => hbc_offset,
      localFineOffset     => std_logic_vector(local_fine_offset),
      laccpFineOffset     => std_logic_vector(laccp_fine_offset),
      hbfState            => open,

      -- Local bus --
      addrLocalBus        => addr_LocalBus,
      dataLocalBusIn      => data_LocalBusIn,
      dataLocalBusOut     => data_LocalBusOut(kMUTIL.ID),
      reLocalBus          => re_LocalBus(kMUTIL.ID),
      weLocalBus          => we_LocalBus(kMUTIL.ID),
      readyLocalBus       => ready_LocalBus(kMUTIL.ID)
    );

  -- Scaler -------------------------------------------------------------------------------
  scr_en_in(kMsbScr - kIndexRealTime)       <= heartbeat_signal;
  scr_en_in(kMsbScr - kIndexDaqRunTime)     <= heartbeat_signal when(daq_is_runnig = '1') else '0';
  scr_en_in(kMsbScr - kIndexTotalThrotTime) <= scr_thr_on(0);
  scr_en_in(kMsbScr - kIndexInThrot1Time)   <= scr_thr_on(1);
  scr_en_in(kMsbScr - kIndexInThrot2Time)   <= scr_thr_on(2);
  scr_en_in(kMsbScr - kIndexOutThrotTime)   <= scr_thr_on(3);
  scr_en_in(kMsbScr - kIndexHbfThrotTime)   <= scr_thr_on(4);
  scr_en_in(kMsbScr - kIndexMikuError)      <= (pattern_error(kIdMikuSec) or checksum_err(kIdMikuSec) or frame_broken(kIdMikuSec) or recv_terminated(kIdMikuSec)) and is_ready_for_daq(kIdMikuSec);

  scr_en_in(kMsbScr - kIndexTrgReq)         <= '0';
  scr_en_in(kMsbScr - kIndexTrgRejected)    <= '0';

  scr_en_in(kMsbScr - kIndexGate1Time)      <= heartbeat_signal and scr_gate(1);
  scr_en_in(kMsbScr - kIndexGate2Time)      <= heartbeat_signal and scr_gate(2);

  scr_en_in(kNumInput-1 downto 0)           <= swap_vect(hit_out);

  scr_gate(0)   <= '1';
  process(clk_slow)
  begin
    if(clk_slow'event and clk_slow = '1') then
      if(user_reset = '1') then
        scr_gate(kNumScrGate-1 downto 1)  <= (others => '0');
      elsif(heartbeat_signal = '1') then
        scr_gate(1)   <= frame_flag_out(0);
        scr_gate(2)   <= frame_flag_out(1);
      end if;
    end if;
  end process;

  u_SCR: entity mylib.FreeRunScaler
    generic map(
      kNumHitInput        => kNumInput
    )
    port map(
      rst	                => system_reset,
      cntRst              => laccp_pulse_out(kDownPulseCntRst),
      clk	                => clk_slow,

      -- Module Input --
      hbCount             => (heartbeat_count'range => heartbeat_count, others => '0'),
      hbfNum              => (hbf_number'range => hbf_number, others => '0'),
      scrEnIn             => scr_en_in,
      scrRstOut           => open,

      scrgates            => scr_gate,

      -- Local bus --
      addrLocalBus        => addr_LocalBus,
      dataLocalBusIn      => data_LocalBusIn,
      dataLocalBusOut     => data_LocalBusOut(kSCR.ID),
      reLocalBus          => re_LocalBus(kSCR.ID),
      weLocalBus          => we_LocalBus(kSCR.ID),
      readyLocalBus       => ready_LocalBus(kSCR.ID)
      );

  --
  -- YSC (YAENAMI Slow Control) ---------------------------------------------------------
  u_YSC_Inst : entity mylib.YAENAMIController
    generic map  -- use generic parameters in SctDriver.vhd
    (
      kFreqSysClk   => 125_000_000,
      kNumIO        => 1, -- # of MOSI lines:
      kNumASIC      => kNumAsic, -- # of ASICs;
      enDebug       => false
    )
    port map
    (
      -- System --
      rst         => user_reset,   -- port name(defined in SctDriver) => signal name
      clk         => clk_slow,

      -- Rx Chip port --
      SSB         => ASIC_SSB,  -- vector
      MOSI        => ASIC_MOSI,
      SCK         => ASIC_SCK,

      -- Local bus --
      addrLocalBus      => addr_LocalBus,
      dataLocalBusIn    => data_LocalBusIn,
      dataLocalBusOut   => data_LocalBusOut(kYSC.ID),
      reLocalBus        => re_LocalBus(kYSC.ID),
      weLocalBus        => we_LocalBus(kYSC.ID),
      readyLocalBus     => ready_LocalBus(kYSC.ID)
  );

  -- APD_BIAS -------------------------------------------------------------------------
  u_APD_Inst : entity mylib.MAX1932Controller
    generic map(
      kSysClkFreq         => 125_000_000
    )
    port map(
      rst	          => user_reset,
      clk	          => clk_slow,

      -- Module output --
      CSB_SPI           => CP_CS_B,
      SCLK_SPI          => CP_SCLK,
      MOSI_SPI          => CP_DIN,

      -- Local bus --
      addrLocalBus	    => addr_LocalBus,
      dataLocalBusIn	  => data_LocalBusIn,
      dataLocalBusOut	  => data_LocalBusOut(kAPD.ID),
      reLocalBus	      => re_LocalBus(kAPD.ID),
      weLocalBus	      => we_LocalBus(kAPD.ID),
      readyLocalBus	    => ready_LocalBus(kAPD.ID)
    );


  -- ADC ----------------------------------------------------------------------------------
--  process(clk_slow, pwr_on_reset)
--  begin
--    if(pwr_on_reset = '1') then
--      asic_ref_clk  <= '0';
--    elsif(clk_slow'event and clk_slow = '1') then
--      asic_ref_clk  <= not asic_ref_clk;
--    end if;
--  end process;
--  ASIC_REFC(0)   <= asic_ref_clk;
--  ASIC_REFC(1)   <= asic_ref_clk;
--  ASIC_REFC(2)   <= asic_ref_clk;
--  ASIC_REFC(3)   <= asic_ref_clk;

  gen_oddr : for i in 0 to kNumAsic-1 generate
  begin

   u_ODDR_inst : ODDR
   generic map(
      DDR_CLK_EDGE => "OPPOSITE_EDGE", -- "OPPOSITE_EDGE" or "SAME_EDGE"
      INIT => '0',   -- Initial value for Q port ('1' or '0')
      SRTYPE => "SYNC") -- Reset Type ("ASYNC" or "SYNC")
   port map (
      Q => ASIC_REFC(i),   -- 1-bit DDR output
      C => clk_adcref,    -- 1-bit clock input
      CE => '1',  -- 1-bit clock enable input
      D1 => '1',  -- 1-bit data input (positive edge)
      D2 => '0',  -- 1-bit data input (negative edge)
      R => '0',    -- 1-bit reset input
      S => '0'     -- 1-bit set input
   );
   end generate;

  u_VIO : vio_0
  PORT MAP (
    clk => clk_slow,
    probe_out0 => reset_vio,
    probe_out1 => reset_adc_cycle
  );


  -- Streaming LR-TDC ---------------------------------------------------------------------
  --signal_in_merge   <= dcr_d & dcr_u & MAIN_IN_D & MAIN_IN_U;
  signal_in_merge   <= ASIC_DISCRI;
  strtdc_trigger_in <= laccp_pulse_out(kDownPulseTrigger) or local_trigger_in;

  u_SLT_Inst: entity mylib.StrDaqRayraw
    generic map(
      kNumInput          => kNumInput,
      kDivisionRatioTdc  => 1,
      kDivisionRatioAdc  => 4,
      enDEBUG            => false
    )
    port map(
      rst           => user_reset,
      rstAdc        => reset_vio(0),
      rstAdcCycle   => reset_adc_cycle(0),
      clk           => clk_slow,
      tdcClk        => clk_tdc,
      indepClk      => clk_gbe,

      radiationURE  => uncorrectable_flag,
      daqOn         => daq_is_runnig,
      scrThrEn      => scr_thr_on,
      hitOut        => hit_out,

      -- Data Link -----------------------------------------
      linkActive         => tcp_isActive(0),

      -- LACCP ------------------------------------------------------
      heartbeatIn       => heartbeat_signal,
      hbCount           => heartbeat_count,
      hbfNumber         => hbf_number,
      ghbfNumMismatchIn => hbf_num_mismatch,
      hbfState          => hbf_state,

      LaccpFineOffset   => laccp_fine_offset,

      frameFlagsIn      => frame_flag_out,

      -- Streaming TDC interface ------------------------------------
      sigIn             => signal_in_merge,
      triggerIn         => strtdc_trigger_in,

      dataRdEn          => vital_rden,
      dataOut           => vital_dout,
      dataRdValid       => vital_valid,

      -- ADC interface ----------------------------------------------
      adcDClkP          => ADC_DCLK_P,
      adcDClkN          => ADC_DCLK_N,
      adcDataP          => ADC_DATA_P,
      adcDataN          => ADC_DATA_N,
      adcFrameP         => ADC_DFRAME_P,
      adcFrameN         => ADC_DFRAME_N,

      -- LinkBuffer interface ---------------------------------------
      pfullLinkBufIn      => pfull_link_buf,
      emptyLinkInBufIn    => empty_to_tsd(0),

      addrLocalBus        => addr_LocalBus,
      dataLocalBusIn      => data_LocalBusIn,
      dataLocalBusOut     => data_LocalBusOut(kTDC.ID),
      reLocalBus          => re_LocalBus(kTDC.ID),
      weLocalBus          => we_LocalBus(kTDC.ID),
      readyLocalBus       => ready_LocalBus(kTDC.ID)
    );

  vital_rden  <= not pfull_link_buf;

  din_link_buf  <= vital_dout(7 downto 0) & vital_dout(15 downto 8) & vital_dout(23 downto 16) & vital_dout(31 downto 24) & vital_dout(39 downto 32) & vital_dout(47 downto 40) & vital_dout(55 downto 48) & vital_dout(63 downto 56);

  u_link_buf : LinkBuffer
    PORT map(
      rst     => user_reset,
      wr_clk  => clk_slow,
      rd_clk  => clk_sys,
      din     => din_link_buf,
      wr_en   => vital_valid,
      rd_en   => re_from_tsd(0),
      dout    => wd_to_tsd(0),
      full    => full_link_buf,
      empty   => empty_to_tsd(0),
      valid   => we_to_tsd(0),
      prog_full => pfull_link_buf,
      wr_rst_busy => open,
      rd_rst_busy => open
    );


  -- IOM ------------------------------------------------------------------------
  local_frame_flag(0)   <= dip_sw(kStandAlone.Index) and intsig_from_iom(0);
  local_frame_flag(1)   <= dip_sw(kStandAlone.Index) and intsig_from_iom(1);
  local_trigger_in      <= dip_sw(kStandAlone.Index) and intsig_from_iom(2);

  intsig_to_iom(0)      <= heartbeat_signal;
  intsig_to_iom(1)      <= tcp_isActive(0);
  intsig_to_iom(2)      <= laccp_pulse_out(kDownPulseTrigger);
  intsig_to_iom(3)      <= frame_flag_out(0);
  intsig_to_iom(4)      <= frame_flag_out(1);
  intsig_to_iom(5)      <= clk_sys_div16;
  intsig_to_iom(6)      <= '1';
  intsig_to_iom(7)      <= '1';


  u_IOM : entity mylib.IOManager
    port map(
      rst	                => user_reset,
      clk	                => clk_slow,

      -- Analog monitor --
      addrAmon            => AMON_ADDR,

      -- Ext Input
      extInput            => sync_nim_in,
      intOutput           => intsig_from_iom,

      -- Ext Output
      intInput            => intsig_to_iom,
      extOutput           => tmp_nim_out,

      -- Local bus --
      addrLocalBus        => addr_LocalBus,
      dataLocalBusIn      => data_LocalBusIn,
      dataLocalBusOut	    => data_LocalBusOut(kIOM.ID),
      reLocalBus          => re_LocalBus(kIOM.ID),
      weLocalBus          => we_LocalBus(kIOM.ID),
      readyLocalBus	      => ready_LocalBus(kIOM.ID)
      );

  -- C6C -------------------------------------------------------------------------------
  CDCE_PDB  <= '0';
--  u_C6C_Inst : entity mylib.CDCE62002Controller
--    generic map(
--      kSysClkFreq         => 125_000_000
--      )
--    port map(
--      rst                 => system_reset,
--      clk                 => clk_slow,
--      refClkIn            => gmod_clk,
--
--      chipReset           => c6c_reset,
--      clkIndep            => clk_sys,
--      chipLock            => CDCE_LOCK,
--
--      -- Module output --
--      PDB                 => CDCE_PDB,
--      REF_CLKP            => CDCE_REFP,
--      REF_CLKN            => CDCE_REFN,
--      CSB_SPI             => CDCE_LE,
--      SCLK_SPI            => CDCE_SCLK,
--      MOSI_SPI            => CDCE_SI,
--      MISO_SPI            => CDCE_SO,
--
--      -- Local bus --
--      addrLocalBus        => addr_LocalBus,
--      dataLocalBusIn      => data_LocalBusIn,
--      dataLocalBusOut     => data_LocalBusOut(kC6C.ID),
--      reLocalBus          => re_LocalBus(kC6C.ID),
--      weLocalBus          => we_LocalBus(kC6C.ID),
--      readyLocalBus       => ready_LocalBus(kC6C.ID)
--    );

  -- MIG -------------------------------------------------------------------------------

  -- TSD -------------------------------------------------------------------------------
  gen_tsd: for i in 0 to kNumGtx-1 generate
    u_TSD_Inst : entity mylib.TCP_sender
      port map(
        RST                     => pwr_on_reset,
        CLK                     => clk_sys,

        -- data from EVB --
        rdFromEVB               => wd_to_tsd(i),
        rvFromEVB               => we_to_tsd(i),
        emptyFromEVB            => empty_to_tsd(i),
        reToEVB                 => re_from_tsd(i),

        -- data to SiTCP
        isActive                => tcp_isActive(i),
        afullTx                 => tcp_tx_full(i),
        weTx                    => tcp_tx_wr(i),
        wdTx                    => tcp_tx_data(i)

        );
  end generate;

  -- SDS --------------------------------------------------------------------
  u_SDS_Inst : entity mylib.SelfDiagnosisSystem
    port map(
      rst               => user_reset,
      clk               => clk_slow,
      clkIcap           => clk_spi,

      -- Module input  --
      VP                => VP,
      VN                => VN,

      -- Module output --
      shutdownOverTemp  => shutdown_over_temp,
      uncorrectableAlarm => uncorrectable_flag,

      -- Local bus --
      addrLocalBus      => addr_LocalBus,
      dataLocalBusIn    => data_LocalBusIn,
      dataLocalBusOut   => data_LocalBusOut(kSDS.ID),
      reLocalBus        => re_LocalBus(kSDS.ID),
      weLocalBus        => we_LocalBus(kSDS.ID),
      readyLocalBus     => ready_LocalBus(kSDS.ID)
      );


  -- FMP --------------------------------------------------------------------
  u_FMP_Inst : entity mylib.FlashMemoryProgrammer
    port map(
      rst               => user_reset,
      clk               => clk_slow,
      clkSpi            => clk_spi,

      -- Module output --
      CS_SPI            => FCSB,
--      SCLK_SPI          => USR_CLK,
      MOSI_SPI          => SPI_MOSI,
      MISO_SPI          => SPI_DIN,

      -- Local bus --
      addrLocalBus      => addr_LocalBus,
      dataLocalBusIn    => data_LocalBusIn,
      dataLocalBusOut   => data_LocalBusOut(kFMP.ID),
      reLocalBus        => re_LocalBus(kFMP.ID),
      weLocalBus        => we_LocalBus(kFMP.ID),
      readyLocalBus     => ready_LocalBus(kFMP.ID)
      );


  -- BCT -------------------------------------------------------------------------------
  -- Actual local bus
  u_BCT_Inst : entity mylib.BusController
    port map(
      rstSys                    => bct_reset,
      rstFromBus                => rst_from_bus,
      reConfig                  => PROGB_ON,
      clk                       => clk_slow,
      -- Local Bus --
      addrLocalBus              => addr_LocalBus,
      dataFromUserModules       => data_LocalBusOut,
      dataToUserModules         => data_LocalBusIn,
      reLocalBus                => re_LocalBus,
      weLocalBus                => we_LocalBus,
      readyLocalBus             => ready_LocalBus,
      -- RBCP --
      addrRBCP                  => rbcp_addr(0),
      wdRBCP                    => rbcp_wd(0),
      weRBCP                    => rbcp_we(0),
      reRBCP                    => rbcp_re(0),
      ackRBCP                   => rbcp_ack(0),
      rdRBCP                    => rbcp_rd(0)
      );

  -- SiTCP Inst ------------------------------------------------------------------------
  u_SiTCPRst : entity mylib.ResetGen
    port map(pwr_on_reset or (not pcs_pma_status(kPcsPmaLinkStatus)) or rst_from_miku, clk_sys, sitcp_reset);

  gen_SiTCP : for i in 0 to kNumGtx-1 generate

    eth_tx_clk(i)      <= eth_rx_clk(0);

    u_SiTCP_Inst : WRAP_SiTCP_GMII_XC7K_32K
      port map
      (
        CLK               => clk_sys, --: System Clock >129MHz
        RST               => (sitcp_reset), --: System reset
        -- Configuration parameters
        FORCE_DEFAULTn    => dip_sw(kSiTCP.Index), --: Load default parameters
        EXT_IP_ADDR       => X"00000000", --: IP address[31:0]
        EXT_TCP_PORT      => X"0000", --: TCP port #[15:0]
        EXT_RBCP_PORT     => X"0000", --: RBCP port #[15:0]
        PHY_ADDR          => "00000", --: PHY-device MIF address[4:0]
        -- EEPROM
        EEPROM_CS         => EEP_CS(i+1), --: Chip select
        EEPROM_SK         => EEP_SK(i+1), --: Serial data clock
        EEPROM_DI         => EEP_DI(i+1), --: Serial write data
        EEPROM_DO         => EEP_DO(i+1), --: Serial read data
        --    user data, intialial values are stored in the EEPROM, 0xFFFF_FC3C-3F
        USR_REG_X3C       => open, --: Stored at 0xFFFF_FF3C
        USR_REG_X3D       => open, --: Stored at 0xFFFF_FF3D
        USR_REG_X3E       => open, --: Stored at 0xFFFF_FF3E
        USR_REG_X3F       => open, --: Stored at 0xFFFF_FF3F
        -- MII interface
        GMII_RSTn         => open, --: PHY reset
        GMII_1000M        => '1',  --: GMII mode (0:MII, 1:GMII)
        -- TX
        GMII_TX_CLK       => eth_tx_clk(i), --: Tx clock
        GMII_TX_EN        => eth_tx_en(i),  --: Tx enable
        GMII_TXD          => eth_tx_d(i),   --: Tx data[7:0]
        GMII_TX_ER        => eth_tx_er(i),  --: TX error
        -- RX
        GMII_RX_CLK       => eth_rx_clk(0), --: Rx clock
        GMII_RX_DV        => eth_rx_dv(i),  --: Rx data valid
        GMII_RXD          => eth_rx_d(i),   --: Rx data[7:0]
        GMII_RX_ER        => eth_rx_er(i),  --: Rx error
        GMII_CRS          => '0', --: Carrier sense
        GMII_COL          => '0', --: Collision detected
        -- Management IF
        GMII_MDC          => open, --: Clock for MDIO
        GMII_MDIO_IN      => '1', -- : Data
        GMII_MDIO_OUT     => open, --: Data
        GMII_MDIO_OE      => open, --: MDIO output enable
        -- User I/F
        SiTCP_RST         => emergency_reset(i), --: Reset for SiTCP and related circuits
        IP_ADDR           => sitcp_ip_addr(i),
        -- TCP connection control
        TCP_OPEN_REQ      => '0', -- : Reserved input, shoud be 0
        TCP_OPEN_ACK      => tcp_isActive(i), --: Acknowledge for open (=Socket busy)
        --    TCP_ERROR           : out    std_logic; --: TCP error, its active period is equal to MSL
        TCP_CLOSE_REQ     => close_req(i), --: Connection close request
        TCP_CLOSE_ACK     => close_act(i), -- : Acknowledge for closing
        -- FIFO I/F
        TCP_RX_WC         => X"0000",    --: Rx FIFO write count[15:0] (Unused bits should be set 1)
        TCP_RX_WR         => open, --: Read enable
        TCP_RX_DATA       => open, --: Read data[7:0]
        TCP_TX_FULL       => tcp_tx_full(i), --: Almost full flag
        TCP_TX_WR         => tcp_tx_wr(i),   -- : Write enable
        TCP_TX_DATA       => tcp_tx_data(i), -- : Write data[7:0]
        -- RBCP
        RBCP_ACT          => open, --: RBCP active
        RBCP_ADDR         => rbcp_gmii_addr(i), --: Address[31:0]
        RBCP_WD           => rbcp_gmii_wd(i),   --: Data[7:0]
        RBCP_WE           => rbcp_gmii_we(i),   --: Write enable
        RBCP_RE           => rbcp_gmii_re(i),   --: Read enable
        RBCP_ACK          => rbcp_gmii_ack(i),  --: Access acknowledge
        RBCP_RD           => rbcp_gmii_rd(i)    --: Read data[7:0]
        );

  u_RbcpCdc : entity mylib.RbcpCdc
  port map(
    -- Mikumari clock domain --
    rstSys      => system_reset,
    clkSys      => clk_slow,
    rbcpAddr    => rbcp_addr(i),
    rbcpWd      => rbcp_wd(i),
    rbcpWe      => rbcp_we(i),
    rbcpRe      => rbcp_re(i),
    rbcpAck     => rbcp_ack(i),
    rbcpRd      => rbcp_rd(i),

    -- GMII clock domain --
    rstXgmii    => pwr_on_reset,
    clkXgmii    => clk_sys,
    rbcpXgAddr  => rbcp_gmii_addr(i),
    rbcpXgWd    => rbcp_gmii_wd(i),
    rbcpXgWe    => rbcp_gmii_we(i),
    rbcpXgRe    => rbcp_gmii_re(i),
    rbcpXgAck   => rbcp_gmii_ack(i),
    rbcpXgRd    => rbcp_gmii_rd(i)
    );

    u_gTCP_inst : entity mylib.global_sitcp_manager
      port map(
        RST           => pwr_on_reset,
        CLK           => clk_sys,
        ACTIVE        => tcp_isActive(i),
        REQ           => close_req(i),
        ACT           => close_act(i),
        rstFromTCP    => open
        );
  end generate;

  -- SFP transceiver -------------------------------------------------------------------
  u_MiiRstTimer_Inst : entity mylib.MiiRstTimer
    port map(
      rst         => emergency_reset(0),
      clk         => clk_sys,
      rstMiiOut   => mii_reset
    );

  u_MiiInit_Inst : mii_initializer
    port map(
      -- System
      CLK         => clk_sys,
      --RST         => system_reset,
      RST         => mii_reset,
      -- PHY
      PHYAD       => kMiiPhyad,
      -- MII
      MDC         => mii_init_mdc,
      MDIO_OUT    => mii_init_mdio,
      -- status
      COMPLETE    => open
      );

  mmcm_reset_all  <= or_reduce(mmcm_reset);

  u_GtClockDist_Inst : entity mylib.GtClockDistributer2
    port map(
      -- GTX refclk --
      GT_REFCLK_P   => GTX_REFCLK_P,
      GT_REFCLK_N   => GTX_REFCLK_N,

      gtRefClk      => gtrefclk_i,
      gtRefClkBufg  => gtrefclk_bufg,

      -- USERCLK2 --
      mmcmReset     => mmcm_reset_all,
      mmcmLocked    => mmcm_locked,
      txOutClk      => txout_clk(0),
      rxOutClk      => rxout_clk(0),

      userClk       => user_clk,
      userClk2      => user_clk2,
      rxuserClk     => rxuser_clk,
      rxuserClk2    => rxuser_clk2,

      -- GTXE_COMMON --
      reset         => pwr_on_reset,
      clkIndep      => clk_gbe,
      clkQPLL       => gt0_qplloutclk,
      refclkQPLL    => gt0_qplloutrefclk
      );

  gen_pcspma : for i in 0 to kNumGtx-1 generate
    u_pcspma_Inst : entity mylib.GbEPcsPma
      port map(

        --An independent clock source used as the reference clock for an
        --IDELAYCTRL (if present) and for the main GT transceiver reset logic.
        --This example design assumes that this is of frequency 200MHz.
        independent_clock    => clk_gbe,

        -- Tranceiver Interface
        -----------------------
        gtrefclk             => gtrefclk_i,
        gtrefclk_bufg        => gtrefclk_bufg,

        gt0_qplloutclk       => gt0_qplloutclk,
        gt0_qplloutrefclk    => gt0_qplloutrefclk,

        userclk              => user_clk,
        userclk2             => user_clk2,
        rxuserclk            => rxuser_clk,
        rxuserclk2           => rxuser_clk2,

        mmcm_locked          => mmcm_locked,
        mmcm_reset           => mmcm_reset(i),

        -- clockout --
        txoutclk             => txout_clk(i),
        rxoutclk             => rxout_clk(i),

        -- Tranceiver Interface
        -----------------------
        txp                  => GTX_TX_P(i+1),
        txn                  => GTX_TX_N(i+1),
        rxp                  => GTX_RX_P(i+1),
        rxn                  => GTX_RX_N(i+1),

        -- GMII Interface (client MAC <=> PCS)
        --------------------------------------
        gmii_tx_clk          => eth_tx_clk(i),
        gmii_rx_clk          => eth_rx_clk(i),
        gmii_txd             => eth_tx_d(i),
        gmii_tx_en           => eth_tx_en(i),
        gmii_tx_er           => eth_tx_er(i),
        gmii_rxd             => eth_rx_d(i),
        gmii_rx_dv           => eth_rx_dv(i),
        gmii_rx_er           => eth_rx_er(i),
        -- Management: MDIO Interface
        -----------------------------

        mdc                  => mii_init_mdc,
        mdio_i               => mii_init_mdio,
        mdio_o               => open,
        mdio_t               => open,
        phyaddr              => "00000",
        configuration_vector => "00000",
        configuration_valid  => '0',

        -- General IO's
        ---------------
        status_vector        => pcs_pma_status,
        reset                => pwr_on_reset
        );
  end generate;

  -- Clock inst ------------------------------------------------------------------------
  --clk_slow  <= clk_sys;
  u_ClkMan_Inst   : clk_wiz_sys
    port map (
      -- Clock out ports
      clk_sys         => clk_sys,
      clk_indep_gtx   => clk_gbe,
      clk_spi         => clk_spi,
      -- Status and control signals
      reset           => '0',
      locked          => clk_sys_locked,
      -- Clock in ports
      clk_in1_p       => BASE_CLKP,
      clk_in1_n       => BASE_CLKN
      );

  u_BUFG :  BUFG
    port map (
      O => gmod_clk, -- 1-bit output: Clock output
      I => mod_clk  -- 1-bit input: Clock input
    );
--
--  --
--  u_BUFG_Fast_inst :  BUFG
--    port map (
--      O => clk_fast, -- 1-bit output: Clock output
--      I => c6c_fast  -- 1-bit input: Clock input
--    );
--
--  u_BUFG_Slow_inst :  BUFG
--    port map (
--      O => clk_slow, -- 1-bit output: Clock output
--      I => c6c_slow  -- 1-bit input: Clock input
--    );

  --
  u_CdcmMan_Inst : mmcm_cdcm
  port map(
    -- Clock in ports
--        clkfb_in          => clk_gfb,
      -- Clock out ports
      clk_slow        => clk_slow,
      clk_fast        => clk_fast,
      clk_tdc0        => clk_tdc(0),
      clk_tdc1        => clk_tdc(1),
      clk_tdc2        => clk_tdc(2),
      clk_tdc3        => clk_tdc(3),
--        clkfb_out         => clk_fb,
      -- Status and control signals
      reset           => mmcm_cdcm_reset,
      locked          => mmcm_cdcm_locked,
      clk_in1         => clk_sys,
      clk_in2         => gmod_clk,
      clk_in_sel      => dip_sw(kStandAlone.Index)
      );

  --
  u_ClkAdc_Inst : clk_wiz_adc
    port map (
    -- Clock out ports
     clk_adc    => clk_adcref,
    -- Status and control signals
     reset      => '0',
     locked     => clk_adc_locked,
     -- Clock in ports
     clk_in1    => clk_slow
    );

--  u_IBUFDS_SLOW_inst : IBUFDS
--    generic map (
--       DIFF_TERM => FALSE, -- Differential Termination
--       IBUF_LOW_PWR => FALSE, -- Low power (TRUE) vs. performance (FALSE) setting for referenced I/O standards
--       IOSTANDARD => "LVDS")
--    port map (
--       O => c6c_slow,  -- Buffer output
--       I => CLK_SLOWP,  -- Diff_p buffer input (connect directly to top-level port)
--       IB => CLK_SLOWN -- Diff_n buffer input (connect directly to top-level port)
--       );
--
--  u_IBUFDS_FAST_inst : IBUFDS
--    generic map (
--       DIFF_TERM => FALSE, -- Differential Termination
--       IBUF_LOW_PWR => FALSE, -- Low power (TRUE) vs. performance (FALSE) setting for referenced I/O standards
--       IOSTANDARD => "LVDS")
--    port map (
--       O => c6c_fast,  -- Buffer output
--       I => CLK_FASTP,  -- Diff_p buffer input (connect directly to top-level port)
--       IB => CLK_FASTN -- Diff_n buffer input (connect directly to top-level port)
--       );
--
end Behavioral;
