library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_MISC.ALL;
use ieee.numeric_std.all;

library UNISIM;
use UNISIM.VComponents.all;

library mylib;
--use mylib.defDCR.all;
use mylib.defBCT.all;
use mylib.defLaccp.all;
use mylib.defHeartBeatUnit.all;
use mylib.defDataBusAbst.all;
use mylib.defDelimiter.all;
use mylib.defTDC.all;
use mylib.defADC.all;
use mylib.defGateGen.all;
use mylib.defThrottling.all;
use mylib.defStrDaqRayraw.all;
use mylib.defYaenamiAdc.all;
use mylib.defRayrawStrAdcROV1.all;

entity StrDaqRayraw is
  generic(
    kNumInput           : integer:= 32;
    kDivisionRatioTdc   : integer:= 4;
    kDivisionRatioAdc   : integer:= 4;
    kNumScrThr          : integer:= 5;
    enDEBUG             : boolean:= false
  );
  port(
    -- System ----------------------------------------------------
    rst               : in std_logic;
    rstAdc            : in std_logic;
    rstAdcCycle       : in std_logic;
    clk               : in std_logic;
    tdcClk            : in std_logic_vector(kNumTdcClock-1 downto 0);
    indepClk          : in std_logic;

    radiationURE      : in std_logic;
    daqOn             : out std_logic;
    scrThrEn          : out std_logic_vector(kNumScrThr-1 downto 0);

    -- Data Link --------------------------------------------------
    linkActive        : in std_logic;

    -- LACCP ------------------------------------------------------
    heartbeatIn       : in std_logic;
    hbCount           : in std_logic_vector(kWidthStrHbc-1 downto 0);
    hbfNumber         : in std_logic_vector(kWidthStrHbf-1 downto 0);
    ghbfNumMismatchIn : in std_logic;
    hbfState          : in HbfStateType;

    LaccpFineOffset   : in signed(kWidthLaccpFineOffset-1 downto 0);

    frameFlagsIn      : in std_logic_vector(kWidthFrameFlag-1 downto 0);

    -- Streaming TDC interface ------------------------------------
    sigIn             : in std_logic_vector(kNumInput-1 downto 0);
    triggerIn         : in std_logic;
    hitOut            : out std_logic_vector(kNumInput-1 downto 0);

    dataRdEn          : in  std_logic;                                 --output fifo read enable
    dataOut           : out std_logic_vector(kWidthData-1 downto 0);   --output fifo data out
    dataRdValid       : out std_logic;                                 --output fifo valid flag

    -- ADC interface ----------------------------------------------
    adcDClkP      : in std_logic_vector(kNumAsicBlock-1 downto 0);  -- ADC DCLK (forwarded fast clock)
    adcDClkN      : in std_logic_vector(kNumAsicBlock-1 downto 0);
    adcDataP      : in std_logic_vector(kNumAsicBlock*kNumAdcCh-1 downto 0); -- ADC DATA
    adcDataN      : in std_logic_vector(kNumAsicBlock*kNumAdcCh-1 downto 0);
    adcFrameP     : in std_logic_vector(kNumAsicBlock-1 downto 0);  -- ADC FRAME
    adcFrameN     : in std_logic_vector(kNumAsicBlock-1 downto 0);

    -- LinkBuffer interface ---------------------------------------
    pfullLinkBufIn    : in std_logic;
    emptyLinkInBufIn  : in std_logic;

    -- Local bus --
    addrLocalBus        : in LocalAddressType;
    dataLocalBusIn      : in LocalBusInType;
    dataLocalBusOut     : out LocalBusOutType;
    reLocalBus          : in std_logic;
    weLocalBus          : in std_logic;
    readyLocalBus       : out std_logic
  );
end StrDaqRayraw;

architecture RTL of StrDaqRayraw is
  attribute mark_debug      : boolean;

  -- System --
  signal sync_reset       : std_logic;

  signal pre_vital_reset  : std_logic;
  signal request_vital_reset  : std_logic;
  constant kWidthVitalReset   : integer:= 8;
  signal sr_vital_reset       : std_logic_vector(kWidthVitalReset-1 downto 0);
  signal neg_edge_gate, daq_off_reset : std_logic;

  -- Internal signal declaration ---------------------------------
  -- Scaler --
  type ThrCountType   is array(kNumScrThr-1 downto 0) of unsigned(kWidthHbCount-1 downto 0);
  constant kMaxThrottCount  : unsigned(kWidthHbCount-1 downto 0):= (others => '1');
  signal thr_count      : ThrCountType;
  signal throttling_on  : std_logic_vector(kNumScrThr-1 downto 0);
  signal scr_thr_on     : std_logic_vector(kNumScrThr-1 downto 0);

  -- DAQ control --
  signal daq_is_running         : std_logic;
  constant kRecoveryWait        : unsigned(3 downto 0):= "0101";
  signal self_recovery          : std_logic;
  signal reg_self_recovery_mode : std_logic;

  signal reg_emumode_on         : std_logic_vector(kTriggerMode'range);
  signal reg_trigger_delay      : std_logic_vector(kWidthTrgDelay-1 downto 0);
  signal reg_trigger_width      : std_logic_vector(kWidthTrgWidth-1 downto 0);
  signal trigger_gate           : std_logic;

  signal local_hbf_num_mismatch : std_logic;
  signal local_hbf_num_mismatch_adc : std_logic;
  signal local_hbf_num_mismatch_tdc : std_logic;
  signal local_hbf_num_mismatch_at  : std_logic;

  attribute mark_debug of daq_is_running  : signal is enDEBUG;
  --attribute mark_debug of trigger_gate    : signal is enDEBUG;

  -- Throttling ---------------------------------------------------
  signal hbf_throttling_on          : std_logic;
  signal reg_hbf_throttling_ratio   : std_logic_vector(kNumHbfMode-1 downto 0);
  signal input_throttling_type2_on, input_throttling_type2_on_adc, input_throttling_type2_on_tdc  : std_logic;
  signal output_throttling_on       : std_logic;

  -- Delimiter ----------------------------------------------------
  signal delimiter_flags        : std_logic_vector(kWidthDelimiterFlag-1 downto 0);
  signal delimiter_data_valid   : std_logic;
  signal delimiter_dout         : std_logic_vector(kWidthData-1 downto 0);
  signal int_delimiter_data     : std_logic_vector(kWidthIntData-1 downto 0);
  signal reg_user_for_delimiter : std_logic_vector(kPosHbdUserReg'length-1 downto 0);

  attribute mark_debug of delimiter_data_valid  : signal is enDEBUG;
  --attribute mark_debug of delimiter_dout        : signal is enDEBUG;

  -- ODP ----------------------------------------------------------
  signal odp_wren   : std_logic_vector(2*kNumInput-1 downto 0);
  signal odp_dout   : DataArrayType(2*kNumInput-1 downto 0);

  signal reg_tdc_mask     : std_logic_vector(255 downto 0);
  signal reg_enbypass     : std_logic_vector(kWidthBypass-1 downto 0);
  signal reg_tot_filter_control : std_logic_vector(kWidthBypass-1 downto 0);
  signal reg_tot_minth, reg_tot_maxth : std_logic_vector(kWidthTOT-1 downto 0);

  signal reg_adc_mask     : std_logic_vector(255 downto 0);
  signal reg_bls          : std_logic_vector(kWidthBls-1 downto 0);
  signal reg_bls_th       : std_logic_vector(kNumAdcBit-1 downto 0);

  -- Vital --------------------------------------------------------
  signal incoming_buf_full, incoming_buf_full_tdc, incoming_buf_full_adc  : std_logic;

  -- TDC --
  signal rden_to_tdc      : std_logic;
  signal valid_vital_tdc  : std_logic;
  signal dout_vital_tdc   : std_logic_vector(kWidthData-1 downto 0);


  -- OfsCorr ------------------------------------------------------
  constant kBitHbLsb      : integer:= 13;
  signal reduced_ofs      : signed(kPosTiming'length downto 0);
  signal rden_from_ofscorr  : std_logic;

  function RoundingOff(ofs_in : in signed) return signed is
    variable pulse_1    : signed(ofs_in'length-1 downto 0):= (1 => '1', others => '0');
    variable round_ofs  : signed(ofs_in'length-1 downto 0);
  begin
    if(ofs_in(ofs_in'low) = '1') then
      round_ofs := ofs_in + pulse_1;
      return round_ofs(round_ofs'high downto 1);
    else
      return ofs_in(ofs_in'high downto ofs_in'low+1);
    end if;
  end function;

  -- Merger ADC and TDC -------------------------------------------
  signal pfull_tdcbuf     : std_logic;
  signal rden_from_mgrat  : std_logic_vector(1 downto 0);
  signal valid_in_mgrat   : std_logic_vector(1 downto 0);
  signal empty_in_mgrat   : std_logic_vector(1 downto 0);
  signal aempty_in_mgrat  : std_logic_vector(1 downto 0);

  --type MgrAtDataType is array(1 downto 0) of std_logic_vector(kWidthData-1 downto 0);
  signal din_mgrat        : DataArrayType(1 downto 0);

  signal valid_mgrat      : std_logic;
  signal dout_mgrat       : std_logic_vector(kWidthData-1 downto 0);

  -- Output throttling --------------------------------------------
  signal valid_out_throttoling      : std_logic;
  signal dout_out_throttoling       : std_logic_vector(kWidthData-1 downto 0);

  -- Replacer -----------------------------------------------------
  signal valid_ofscorr    : std_logic;
  signal dout_ofscorr     : std_logic_vector(kWidthData-1 downto 0);
  signal offset_in_hbd    : signed(LaccpFineOffset'range);

  -- bus process --
  signal state_lbus      : BusProcessType;

  -- Debug --
  --attribute mark_debug of heartbeatIn     : signal is enDEBUG;
  --attribute mark_debug of dout_vital     : signal is enDEBUG;
  --attribute mark_debug of valid_vital    : signal is enDEBUG;


begin
  -- ======================================================================
  --                                 body
  -- ======================================================================

  daqOn <= daq_is_running;
  scrThrEn          <= scr_thr_on;
  throttling_on(0)  <= input_throttling_type2_on or output_throttling_on or hbf_throttling_on;
  throttling_on(1)  <= '0';
  throttling_on(2)  <= input_throttling_type2_on;
  throttling_on(3)  <= output_throttling_on;
  throttling_on(4)  <= hbf_throttling_on;

  u_throttling_time : process(clk)
  begin
    if(clk'event and clk = '1') then
      if(sync_reset = '1') then
        thr_count   <= (others => (others => '0'));
      else
        for i in 0 to kNumScrThr-1 loop
          if(throttling_on(i) = '1') then
            thr_count(i)   <= thr_count(i)  +1;

            if(thr_count(i) = kMaxThrottCount) then
              scr_thr_on(i)  <= '1';
            else
              scr_thr_on(i)  <= '0';
            end if;
          else
            scr_thr_on(i)  <= '0';
          end if;
        end loop;
      end if;
    end if;
  end process;


  -- DAQ On/Off --
  local_hbf_num_mismatch <= local_hbf_num_mismatch_adc or local_hbf_num_mismatch_tdc or local_hbf_num_mismatch_at;
  u_daq_state : process(clk)
    variable  wait_count  : unsigned(3 downto 0):= (others => '0');
  begin
    if(clk'event and clk = '1') then
      if(sync_reset = '1') then
        daq_is_running      <= '0';
        self_recovery       <= '0';
        request_vital_reset <= '0';
      else
        if(local_hbf_num_mismatch = '1' and reg_self_recovery_mode = '1') then
          wait_count    := kRecoveryWait;
          self_recovery <= '1';
        elsif(emptyLinkInBufIn = '1' and wait_count = 0) then
          self_recovery <= '0';
        end if;

        if(heartbeatIn = '1') then
          if(self_recovery = '0' and hbfState = kActiveFrame and linkActive = '1') then
            daq_is_running  <= '1';
          else
            daq_is_running  <= '0';
          end if;

          if(self_recovery = '1' and wait_count /= 0) then
            wait_count  := wait_count -1;
          end if;

          if(self_recovery = '1' and wait_count = 1) then
            request_vital_reset   <= '1';
          end if;

        else
          request_vital_reset   <= '0';
        end if;
      end if;
    end if;
  end process;

  pre_vital_reset <= sr_vital_reset(kWidthVitalReset-1);
  u_vital_reset : process(clk)
  begin
    if(clk'event and clk = '1') then
      if(request_vital_reset = '1') then
        sr_vital_reset  <= (others => '1');
      else
        sr_vital_reset  <= sr_vital_reset(kWidthVitalReset-2 downto 0) & '0';
      end if;
    end if;
  end process;

  u_daqgate_edge    : entity mylib.EdgeDetector port map(clk, (not daq_is_running), neg_edge_gate);
  u_reset_gen_vital : entity mylib.ResetGen
    generic map(128)
    port map(neg_edge_gate, clk, daq_off_reset);

  -- Trigger emulation mode --
  u_gate : entity mylib.GateGen
    port map(
      syncReset       => sync_reset,
      clk             => clk,

      emuModeOn       => reg_emumode_on,
      delayReg        => reg_trigger_delay,
      widthReg        => reg_trigger_width,

      triggerIn       => triggerIn,
      gateOut         => trigger_gate

    );

  -- Heartbeat frame throttling --
  u_hbf_thro : entity mylib.hbfThrottling
    port map(
      clk                 => clk,

      -- Control registers --
      throttlingRatio     => reg_hbf_throttling_ratio,
      hbfNum              => hbfNumber,

      -- Status output --
      isWorking           => hbf_throttling_on
    );


  -- Delimiter generation --
  delimiter_flags(kIndexRadiationURE)     <= radiationURE;

  delimiter_flags(kIndexOverflow)         <= incoming_buf_full;
  delimiter_flags(kIndexGHbfNumMismatch)  <= ghbfNumMismatchIn;
  delimiter_flags(kIndexLHbfNumMismatch)  <= '0';

  delimiter_flags(kIndexInThrottlingT2)   <= input_throttling_type2_on;
  delimiter_flags(kIndexOutThrottling)    <= '0';
  delimiter_flags(kIndexHbfThrottling)    <= hbf_throttling_on;

  delimiter_flags(kIndexFrameFlag1)       <= frameFlagsIn(1);
  delimiter_flags(kIndexFrameFlag2)       <= frameFlagsIn(0);

  u_DelimiterGen: entity mylib.DelimiterGenerator
    generic map(
      enDEBUG       => false
    )
    port map(
      syncReset         => sync_reset,
      clk               => clk,

      -- Status input ----------------------------------
      flagsIn           => delimiter_flags,

      -- LACCP -----------------------------------------
      hbCount           => hbCount,
      hbfNumber         => hbfNumber,
      signBit           => LaccpFineOffset(LaccpFineOffset'high),
--      LaccpFineOffset   => LaccpFineOffset,

      -- Delimiter data output --
      validDelimiter    => delimiter_data_valid,
      dOutDelimiter     => delimiter_dout
    );

  int_delimiter_data(kPosIHbdDataType'range)  <= delimiter_dout(kPosHbdDataType'range);
  int_delimiter_data(kPosIHbdFlag'range)      <= delimiter_dout(kPosHbdFlag'range);
  int_delimiter_data(kPosIHbdHBFrame'range)   <= delimiter_dout(kPosHbdHBFrame'range);

  -- ODP block TDC --
  u_ODPBlock: entity mylib.ODPBlock
    generic map(
      kNumInput     => kNumInput,
      enDEBUG       => false
    )
    port map(
      -- System --
      rst             => rst,
      tdcClk          => tdcClk,
      baseClk         => clk,
      hitOut          => hitOut,
      --userReg         => reg_user_for_delimiter,
      LaccpFineOffset => LaccpFineOffset,

      -- Control registers --
      tdcMask         => reg_tdc_mask(kNumInput-1 downto 0),
      enBypassDelay   => reg_enbypass(kIndexDelay),
      enBypassParing  => reg_enbypass(kIndexParing),
      --enBypassOfsCorr => reg_enbypass(kIndexOfsCorr),

      enTotFilter     => reg_tot_filter_control(kIndexTotFilter),
      enTotZeroThrough => reg_tot_filter_control(kIndexTotZeroThrough),
      totMinTh        => reg_tot_minth,
      totMaxTh        => reg_tot_maxth,

      -- Data flow control --
      daqOn           => daq_is_running,
      hbfThrottlingOn => hbf_throttling_on,
      triggerGate     => trigger_gate,

      -- Heartbeat counter for TDC --
      hbCount         => hbCount,

      -- Delimiter word I/F --
      validDelimiter  => delimiter_data_valid,
      dInDelimiter    => int_delimiter_data,

      -- Signal input --
      sigIn           => sigIn,

      -- DAQ data output --
      validOut        => odp_wren(kNumInput-1 downto 0),
      dOut            => odp_dout(kNumInput-1 downto 0)
    );

  -- ODP block ADC --
  u_ODPBlockADC : entity mylib.ODPBlockADC
  generic map(
    kNumInput     => kNumInput,
    enDEBUG       => false
  )
  port map(
    -- System --
    rst             => rst,
    rstAdc          => rstAdc,
    rstAdcCycle     => rstAdcCycle,
    sysClk          => clk,
    indepClk        => indepClk,
    --userReg         : in  std_logic_vector(kPosHbdUserReg'length-1 downto 0);
    LaccpFineOffset => LaccpFineOffset,

    -- Control registers --
    adcMask         => reg_adc_mask(kNumInput-1 downto 0),
    enBypassDelay   => reg_enbypass(kIndexDelay),
    enBLSuppressor  => reg_bls(kIndexEnBls),
    polBLSuppressor => reg_bls(kIndexBlsPol),
    thBLSuppressor  => reg_bls_th,

    -- Data flow control --
    daqOn           => daq_is_running,
    hbfThrottlingOn => hbf_throttling_on,
    triggerGate     => trigger_gate,

    -- heartbeat count for TDC
    hbCount         => hbCount,

    -- delimiter --
    validDelimiter  => delimiter_data_valid,
    dInDelimiter    => delimiter_dout,

    -- ADC In --
    adcDClkP      => adcDClkP,
    adcDClkN      => adcDClkN,
    adcDataP      => adcDataP,
    adcDataN      => adcDataN,
    adcFrameP     => adcFrameP,
    adcFrameN     => adcFrameN,

    -- Data Out --
    validOut        => odp_wren(2*kNumInput-1 downto kNumInput),
    dOut            => odp_dout(2*kNumInput-1 downto kNumInput)

  );

  -- vital block --
  input_throttling_type2_on <= input_throttling_type2_on_adc or input_throttling_type2_on_tdc;

  rden_to_tdc <= rden_from_ofscorr and (not pfull_tdcbuf);
  u_VitalBlockTdc: entity mylib.VitalBlock
    generic map(
      kTdcType        => "RAYRAW-TDC",
      kNumInput       => kNumInput,
      kDivisionRatio  => kDivisionRatioTdc,
      enDEBUG         => false
    )
    port map(
      clk                 => clk,
      rst                 => rst or pre_vital_reset or daq_off_reset,
      daqGateIn           => daq_is_running,
      lhbfNumMismatch     => local_hbf_num_mismatch_tdc,

      -- ODPBlock input --
      odpWrenIn           => odp_wren(kNumInput-1 downto 0),
      odpDataIn           => odp_dout(kNumInput-1 downto 0),
      hbCount             => hbCount,

      -- Status output --
      bufferProgFull      => open,
      bufferFull          => incoming_buf_full_tdc,

      -- Throttling status --
      inThrottlingT2On    => input_throttling_type2_on_tdc,

      -- Output --
      rdenIn              => rden_to_tdc,
      dataOut             => dout_vital_tdc,
      emptyOut            => open,
      almostEmptyOut      => open,
      validOut            => valid_vital_tdc
    );

  --
  u_VitalBlockAdc: entity mylib.VitalBlock
    generic map(
      kTdcType        => "LRTDC", -- Use the LR-TDC merger block structure
      kNumInput       => kNumInput,
      kDivisionRatio  => kDivisionRatioAdc,
      enDEBUG         => false
    )
    port map(
      clk                 => clk,
      rst                 => rst or pre_vital_reset or daq_off_reset,
      daqGateIn           => daq_is_running,
      lhbfNumMismatch     => local_hbf_num_mismatch_adc,

      -- ODPBlock input --
      odpWrenIn           => odp_wren(2*kNumInput-1 downto kNumInput),
      odpDataIn           => odp_dout(2*kNumInput-1 downto kNumInput),
      hbCount             => hbCount,

      -- Status output --
      bufferProgFull      => open,
      bufferFull          => incoming_buf_full_adc,

      -- Throttling status --
      inThrottlingT2On    => input_throttling_type2_on_adc,

      -- Output --
      rdenIn              => rden_from_mgrat(1),
      dataOut             => din_mgrat(1),
      emptyOut            => empty_in_mgrat(1),
      almostEmptyOut      => aempty_in_mgrat(1),
      validOut            => valid_in_mgrat(1)
    );


  -- OfsCorrection V2 --
  reduced_ofs(kWidthFineCount downto 0) <= LaccpFineOffset(kBitHbLsb-1 downto kBitHbLsb-kWidthFineCount-1) when(reg_enbypass(kIndexOfsCorr) = '0') else (others => '0');
  reduced_ofs(reduced_ofs'high downto kWidthFineCount+1)  <= (others => LaccpFineOffset(LaccpFineOffset'high)) when(reg_enbypass(kIndexOfsCorr) = '0') else (others => '0');
  u_corv2 : entity mylib.OfsCorrectV2
    generic map(
      kWidthOfs           => kPosTiming'length,
      enDEBUG             => false
    )
    port map(
      syncReset           => sync_reset or pre_vital_reset or (not daq_is_running),
      clk                 => clk,
      enBypassOfsCorr     => reg_enbypass(kIndexOfsCorr),
      extendedOfs         => RoundingOff(signed(reduced_ofs)),

      -- Data In --
      rdEnOut             => rden_from_ofscorr,
      validIn             => valid_vital_tdc,
      dIn                 => dout_vital_tdc,

      -- Data Out --
      validOut            => valid_ofscorr,
      dOut                => dout_ofscorr
    );

  -- FIFO for TDC before merger unit  --
  u_tdc_buf : xpm_fifo_sync
    generic map (
      DOUT_RESET_VALUE    => "0",
      ECC_MODE            => "no_ecc",
      FIFO_MEMORY_TYPE    => "auto",
      FIFO_READ_LATENCY   => 1,
      FIFO_WRITE_DEPTH    => 256,
      FULL_RESET_VALUE    => 0,
      PROG_EMPTY_THRESH   => 5,
      PROG_FULL_THRESH    => 127,
      RD_DATA_COUNT_WIDTH => 9,
      READ_DATA_WIDTH     => 64,
      USE_ADV_FEATURES    => "1707",
      WAKEUP_TIME         => 0,
      WR_DATA_COUNT_WIDTH => 9,
      WRITE_DATA_WIDTH    => 64
    )
    port map (
      sleep            => '0',
      rst              => sync_reset or pre_vital_reset or (not daq_is_running),
      wr_clk           => clk,
      wr_en            => valid_ofscorr,
      din              => dout_ofscorr,
      prog_full        => pfull_tdcbuf,
      rd_en            => rden_from_mgrat(0),
      dout             => din_mgrat(0),
      data_valid       => valid_in_mgrat(0),
      empty            => empty_in_mgrat(0),
      almost_empty     => aempty_in_mgrat(0),
      injectsbiterr    => '0',
      injectdbiterr    => '0'
    );

  -- MergerUnit to combine TDC and ADC --
  u_MGR_AT : entity mylib.MergerUnit
    generic map(
      kType           => "Front", -- "Front" or "Back"
      kNumInput       => 2,
      enDEBUG         => false
    )
    port map(
      -- System --
      syncReset           => sync_reset or pre_vital_reset or (not daq_is_running),
      clk                 => clk,
      progFullFifo        => open,
      hbfNumMismatch      => local_hbf_num_mismatch_at,

      -- Merger input --
      rdenOut             => rden_from_mgrat,
      dataIn              => din_mgrat,
      emptyIn             => empty_in_mgrat,
      almostEmptyIn       => aempty_in_mgrat,
      validIn             => valid_in_mgrat,

      -- Merger output --
      rdenIn              => dataRdEn,
      dataOut             => dout_mgrat,
      emptyOut            => open,
      almostEmptyOut      => open,
      validOut            => valid_mgrat
    );

  --
  u_OutThrottle: entity mylib.OutputThrottling
    generic map(
      enDEBUG => false
    )
    port map(
      syncReset           => sync_reset,
      clk                 => clk,

      -- status input --
      intputThrottlingOn  => input_throttling_type2_on,
      pfullLinkIn         => pfullLinkBufIn,
      emptyLinkIn         => emptyLinkInBufIn,

      -- Status output --
      isWorking           => output_throttling_on,

      -- Data In --
      validIn             => valid_mgrat,
      dIn                 => dout_mgrat,

       -- Data Out --
      validOut            => valid_out_throttoling,
      dOut                => dout_out_throttoling

    );

  -- Replace 2nd delimiter with new delimiter --
  offset_in_hbd <= LaccpFineOffset when(reg_enbypass(kIndexOfsCorr) = '1') else (others => '0');
  u_replacer : entity mylib.DelimiterReplacer
    port map(
      syncReset           => sync_reset or pre_vital_reset or (not daq_is_running),
      clk                 => clk,
      userReg             => reg_user_for_delimiter,
      LaccpFineOffset     => offset_in_hbd,

      -- Data In --
      validIn             => valid_out_throttoling,
      dIn                 => dout_out_throttoling,

      -- Data Out --
      validOut            => dataRdValid,
      dOut                => dataOut
    );




  -- bus process -------------------------------------------------------------
  u_BusProcess : process(clk)
  begin
    if(clk'event and clk = '1') then
      if(sync_reset = '1') then
        reg_tdc_mask      <= (others => '0');
        reg_adc_mask      <= (others => '0');
        reg_bls           <= (others => '0');
        reg_bls_th        <= (others => '0');
        reg_enbypass      <= (others => '0');
        reg_tot_filter_control  <= (others => '0');
        reg_tot_minth           <= (others => '0');
        reg_tot_maxth           <= (others => '0');

        reg_emumode_on    <= (others => '0');
        reg_trigger_delay <= (others => '0');
        reg_trigger_width <= (others => '0');

        reg_hbf_throttling_ratio  <= (others => '0');

        reg_user_for_delimiter    <= (others => '0');

        reg_self_recovery_mode    <= '0';

        state_lbus        <= Init;
      else
        case state_lbus is
          when Init =>
            dataLocalBusOut     <= x"00";
            readyLocalBus       <= '0';
            state_lbus          <= Idle;

          when Idle =>
            readyLocalBus    <= '0';
            if(weLocalBus = '1' or reLocalBus = '1') then
              state_lbus    <= Connect;
            end if;

          when Connect =>
            if(weLocalBus = '1') then
              state_lbus    <= Write;
            else
              state_lbus    <= Read;
            end if;

          when Write =>
            if(addrLocalBus(kNonMultiByte'range) = kTdcMask(kNonMultiByte'range)) then
              case addrLocalBus(kMultiByte'range) is
                when k1stByte =>
                  reg_tdc_mask(7 downto 0)  <= dataLocalBusIn;
                when k2ndByte =>
                  reg_tdc_mask(15 downto 8)  <= dataLocalBusIn;
                when k3rdByte =>
                  reg_tdc_mask(23 downto 16)  <= dataLocalBusIn;
                when k4thByte =>
                  reg_tdc_mask(31 downto 24)  <= dataLocalBusIn;
                when others =>
                  null;
              end case;

            elsif(addrLocalBus(kNonMultiByte'range) = kAdcMask(kNonMultiByte'range)) then
              case addrLocalBus(kMultiByte'range) is
                when k1stByte =>
                  reg_adc_mask(7 downto 0)  <= dataLocalBusIn;
                when k2ndByte =>
                  reg_adc_mask(15 downto 8)  <= dataLocalBusIn;
                when k3rdByte =>
                  reg_adc_mask(23 downto 16)  <= dataLocalBusIn;
                when k4thByte =>
                  reg_adc_mask(31 downto 24)  <= dataLocalBusIn;
                when others =>
                  null;
              end case;

            elsif(addrLocalBus(kNonMultiByte'range) = kRegBls(kNonMultiByte'range)) then
              reg_bls  <= dataLocalBusIn;

            elsif(addrLocalBus(kNonMultiByte'range) = kThBls(kNonMultiByte'range)) then
              case addrLocalBus(kMultiByte'range) is
                when k1stByte =>
                  reg_bls_th(7 downto 0)  <= dataLocalBusIn;
                when k2ndByte =>
                  reg_bls_th(kNumAdcBit-1 downto 8)  <= dataLocalBusIn(kNumAdcBit-8-1 downto 0);
                when others =>
                  null;
              end case;

            elsif(addrLocalBus(kNonMultiByte'range) = kEnBypass(kNonMultiByte'range)) then
              reg_enbypass  <= dataLocalBusIn;

            elsif(addrLocalBus(kNonMultiByte'range) = kTotFilterControl(kNonMultiByte'range)) then
              reg_tot_filter_control  <= dataLocalBusIn;

            elsif(addrLocalBus(kNonMultiByte'range) = kTotMinTh(kNonMultiByte'range)) then
              case addrLocalBus(kMultiByte'range) is
                when k1stByte =>
                  reg_tot_minth(7 downto 0)   <= dataLocalBusIn;
                when k2ndByte =>
                  reg_tot_minth(kWidthTOT-1 downto 8)  <= dataLocalBusIn;
                when others =>
                  null;
              end case;

            elsif(addrLocalBus(kNonMultiByte'range) = kTotMaxTh(kNonMultiByte'range)) then
              case addrLocalBus(kMultiByte'range) is
                when k1stByte =>
                  reg_tot_maxth(7 downto 0)   <= dataLocalBusIn;
                when k2ndByte =>
                  reg_tot_maxth(kWidthTOT-1 downto 8)  <= dataLocalBusIn;
                when others =>
                  null;
              end case;

            elsif(addrLocalBus(kNonMultiByte'range) = kTriggerEmuControl(kNonMultiByte'range)) then
              reg_emumode_on  <= dataLocalBusIn(kTriggerMode'range);

            elsif(addrLocalBus(kNonMultiByte'range) = kTrgGateDelay(kNonMultiByte'range)) then
              reg_trigger_delay <= dataLocalBusIn;

            elsif(addrLocalBus(kNonMultiByte'range) = kTrgGateWidth(kNonMultiByte'range)) then
              case addrLocalBus(kMultiByte'range) is
                when k1stByte =>
                  reg_trigger_width(7 downto 0)   <= dataLocalBusIn;
                when k2ndByte =>
                  reg_trigger_width(15 downto 8)   <= dataLocalBusIn;
                when k3rdByte =>
                  reg_trigger_width(23 downto 16)   <= dataLocalBusIn;
                when k4thByte =>
                  reg_trigger_width(31 downto 24)   <= dataLocalBusIn;
                when others =>
                  null;
              end case;

            elsif(addrLocalBus(kNonMultiByte'range) = kHbfThrottControl(kNonMultiByte'range)) then
              reg_hbf_throttling_ratio <= dataLocalBusIn(kNumHbfMode-1 downto 0);

            elsif(addrLocalBus(kNonMultiByte'range) = kHbdUserReg(kNonMultiByte'range)) then
              case addrLocalBus(kMultiByte'range) is
                when k1stByte =>
                  reg_user_for_delimiter(7 downto 0)    <= dataLocalBusIn;
                when k2ndByte =>
                  reg_user_for_delimiter(15 downto 8)   <= dataLocalBusIn;
                when others =>
                  null;
              end case;

            elsif(addrLocalBus(kNonMultiByte'range) = kSelfRecoveryMode(kNonMultiByte'range)) then
              reg_self_recovery_mode <= dataLocalBusIn(0);

            else
              null;
            end if;
            state_lbus      <= Done;

          when Read =>
            if(addrLocalBus(kNonMultiByte'range) = kTdcMask(kNonMultiByte'range)) then
              case addrLocalBus(kMultiByte'range) is
                when k1stByte =>
                  dataLocalBusOut   <= reg_tdc_mask(7 downto 0);
                when k2ndByte =>
                  dataLocalBusOut   <= reg_tdc_mask(15 downto 8);
                when k3rdByte =>
                  dataLocalBusOut   <= reg_tdc_mask(23 downto 16);
                when k4thByte =>
                  dataLocalBusOut   <= reg_tdc_mask(31 downto 24);
                when others =>
                  null;
              end case;

            elsif(addrLocalBus(kNonMultiByte'range) = kAdcMask(kNonMultiByte'range)) then
              case addrLocalBus(kMultiByte'range) is
                when k1stByte =>
                  dataLocalBusOut   <= reg_adc_mask(7 downto 0);
                when k2ndByte =>
                  dataLocalBusOut   <= reg_adc_mask(15 downto 8);
                when k3rdByte =>
                  dataLocalBusOut   <= reg_adc_mask(23 downto 16);
                when k4thByte =>
                  dataLocalBusOut   <= reg_adc_mask(31 downto 24);
                when others =>
                  null;
              end case;

            elsif(addrLocalBus(kNonMultiByte'range) = kRegBls(kNonMultiByte'range)) then
              dataLocalBusOut   <= reg_bls;

            elsif(addrLocalBus(kNonMultiByte'range) = kThBls(kNonMultiByte'range)) then
              case addrLocalBus(kMultiByte'range) is
                when k1stByte =>
                  dataLocalBusOut   <= reg_bls_th(7 downto 0);
                when k2ndByte =>
                  dataLocalBusOut   <= "000000" & reg_bls_th(kNumAdcBit-1 downto 8);
                when others =>
                  null;
              end case;

            elsif(addrLocalBus(kNonMultiByte'range) = kEnBypass(kNonMultiByte'range)) then
              dataLocalBusOut   <= reg_enbypass;

            elsif(addrLocalBus(kNonMultiByte'range) = kTotFilterControl(kNonMultiByte'range)) then
              dataLocalBusOut   <= reg_tot_filter_control;

            elsif(addrLocalBus(kNonMultiByte'range) = kTotMinTh(kNonMultiByte'range)) then
              case addrLocalBus(kMultiByte'range) is
                when k1stByte =>
                  dataLocalBusOut   <= reg_tot_minth(7 downto 0);
                when k2ndByte =>
                  dataLocalBusOut   <= reg_tot_minth(15 downto 8);
                when others =>
                  null;
              end case;

            elsif(addrLocalBus(kNonMultiByte'range) = kTotMaxTh(kNonMultiByte'range)) then
              case addrLocalBus(kMultiByte'range) is
                when k1stByte =>
                  dataLocalBusOut   <= reg_tot_maxth(7 downto 0);
                when k2ndByte =>
                  dataLocalBusOut   <= reg_tot_maxth(15 downto 8);
                when others =>
                  null;
              end case;

            elsif(addrLocalBus(kNonMultiByte'range) = kTriggerEmuControl(kNonMultiByte'range)) then
              dataLocalBusOut <= (kTriggerMode'range => reg_emumode_on, others => '0');

            elsif(addrLocalBus(kNonMultiByte'range) = kTrgGateDelay(kNonMultiByte'range)) then
              dataLocalBusOut <= reg_trigger_delay;

            elsif(addrLocalBus(kNonMultiByte'range) = kTrgGateWidth(kNonMultiByte'range)) then
              case addrLocalBus(kMultiByte'range) is
                when k1stByte =>
                  dataLocalBusOut <= reg_trigger_width(7 downto 0);
                when k2ndByte =>
                  dataLocalBusOut <= reg_trigger_width(15 downto 8);
                when others =>
                  null;
              end case;

            elsif(addrLocalBus(kNonMultiByte'range) = kHbfThrottControl(kNonMultiByte'range)) then
              dataLocalBusOut <= (kNumHbfMode-1 downto 0 => reg_hbf_throttling_ratio, others => '0');

            elsif(addrLocalBus(kNonMultiByte'range) = kSelfRecoveryMode(kNonMultiByte'range)) then
              dataLocalBusOut <= (0 => reg_self_recovery_mode, others => '0');

            else
              null;
            end if;
            state_lbus    <= Done;

          when Done =>
            readyLocalBus <= '1';
            if(weLocalBus = '0' and reLocalBus = '0') then
              state_lbus  <= Idle;
            end if;

          -- probably this is error --
          when others =>
            state_lbus    <= Init;
        end case;
      end if;
    end if;
  end process u_BusProcess;


  -- Reset sequence --
  u_reset_gen_sys   : entity mylib.ResetGen
    port map(rst, clk, sync_reset);

end RTL;
