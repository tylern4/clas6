!-----------------------------------------------------------------------
!       BANKname BANKtype      ! Comments
 TABLE  TRGS    ! create write display delete! Scaler bank 
!
!   ATTributes:
!   -----------
!COL ATT-name FMT  Min     Max     ! Comments
!
  1  CLOCK_UG     I     0    9999999  ! Clock ungated
  2  FCUP_UG      I     0    9999999  ! FCUP ungated
  3  Microsec     I     0    9999999  ! Microsecond clock (will overflow during normal run)
  4  random_ug    I     0    9999999  ! Random pulser ungated
  5  MOR_ST       I     0    9999999  ! MOR.ST
  6  MOR_PC       I     0    9999999  ! MOR.PC
  7  MOR_PS       I     0    9999999  ! MOR.PS
  8  MOR_TAC      I     0    9999999  ! MOR.TAC
  9  MOR          I     0    9999999  ! Master OR
 10  PC           I     0    9999999  ! Pair Counter
 11  PS           I     0    9999999  ! Pair Spectrometer
 12  TAC          I     0    9999999  ! Total Absorption Counter
 13  ST           I     0    9999999  ! ST
 14  hel_sync     I     0    9999999  ! Helicity sync signal
 15  clock_ug_2   I     0    9999999  ! Duplicate of channel 1
 16  fcup_ug_2    I     0    9999999  ! Duplicate of channel 2
 17  CLOCK_G1     I     0    9999999  ! Clock with run gatei
 18  FCUP_G1      I     0    9999999  ! FCUP with Run Gate
 19  NotUsed19    I     0    9999999  ! Currently not used
 20  random_g1    I     0    9999999  ! Random pulser run gated
 21  MOR_ST_rg    I     0    9999999  ! MOR.ST  with run gate
 22  MOR_PC_rg    I     0    9999999  ! MOR.PC with run gate
 23  MOR_PS_rg    I     0    9999999  ! MOR.PS with run gate
 24  MOR_TAC_rg   I     0    9999999  ! MOR.TAC with run gate
 25  MOR_rg       I     0    9999999  ! MASTER_OR with run gate
 26  PC_rg        I     0    9999999  ! PC with run gate
 27  PS_rg        I     0    9999999  ! PS with run gate
 28  TAC_rg       I     0    9999999  ! TAC with run gate
 29  ST_rg        I     0    9999999  ! ST  with run gate
 30  random_g1_2  I     0    9999999  ! duplicate of channel 20
 31  clock_g1_2   I     0    9999999  ! duplicate of channel 17
 32  fcup_g1_2    I     0    9999999  ! duplicate of channel 18
 33  CLOCK_G2     I     0    9999999  ! CLOCK live gated
 34  FCUP_G2      I     0    9999999  ! FCUP live gated
 35  trig_or_g2   I     0    9999999  ! Trigger OR of 1-12 live gated
 36  random_g2    I     0    9999999  ! Random pulser live gated
 37  NotUsed37    I     0    9999999  ! Currently not used
 38  NotUsed38    I     0    9999999  ! Currently not used
 39  NotUsed39    I     0    9999999  ! Currently not used
 40  NotUsed40    I     0    9999999  ! Currently not used
 41  MOR_lg       I     0    9999999  ! MASTER_OR live gated
 42  NotUsed42    I     0    9999999  ! Currently not used
 43  NotUsed43    I     0    9999999  ! Currently not used
 44  NotUsed44    I     0    9999999  ! Currently not used
 45  NotUsed45    I     0    9999999  ! Currently not used
 46  random_g2_2  I     0    9999999  ! duplicate of channel 36
 47  clock_g2_2   I     0    9999999  ! duplicate of channel 33
 48  fcup_g2_2    I     0    9999999  ! duplicate of channel 34

 49  trig1_ug     I     0    9999999  ! Trigger 1 ungated, prescaled
 50  trig2_ug     I     0    9999999  ! Trigger 2 ungated, prescaled
 51  trig3_ug     I     0    9999999  ! Trigger 3 ungated, prescaled
 52  trig4_ug     I     0    9999999  ! Trigger 4 ungated, prescaled
 53  trig5_ug     I     0    9999999  ! Trigger 5 ungated, prescaled
 54  trig6_ug     I     0    9999999  ! Trigger 6 ungated, prescaled
 55  trig7_ug     I     0    9999999  ! Trigger 7 ungated, prescaled
 56  trig8_ug     I     0    9999999  ! Trigger 8 ungated, prescaled
 57  trig9_ug     I     0    9999999  ! Trigger 9 ungated, prescaled
 58  trig10_ug    I     0    9999999  ! Trigger 10 ungated, prescaled
 59  trig11_ug    I     0    9999999  ! Trigger 11 ungated, prescaled
 60  trig12_ug    I     0    9999999  ! Trigger 12 ungated, prescaled
 61  trig_or_ug   I     0    9999999  ! Trigger OR of 1-12 ungated,
 62  l1accept     I     0    9999999  ! Level 1 accept
 63  notused63    I     0    9999999  ! Currently not used
 64  notused64    I     0    9999999  ! Currently not used
 65  l2fail       I     0    9999999  ! Level2 fail
 66  l2pass       I     0    9999999  ! Level2 pass
 67  l2start      I     0    9999999  ! Level2 start
 68  l2clear      I     0    9999999  ! Level2 clear
 69  l2accept     I     0    9999999  ! Level2 accept
 70  l3accept     I     0    9999999  ! Level3 accept
 71  notused71    I     0    9999999  ! Currently not used
 72  notused72    I     0    9999999  ! Currently not used
 73  l2sec1_g  	  I     0    9999999  ! Level2 sec1 gated
 74  l2sec2_g  	  I     0    9999999  ! Level2 sec2 gated
 75  l2sec3_g  	  I     0    9999999  ! Level2 sec3 gated
 76  l2sec4_g  	  I     0    9999999  ! Level2 sec4 gated
 77  l2sec5_g  	  I     0    9999999  ! Level2 sec5 gated
 78  l2sec6_g  	  I     0    9999999  ! Level2 sec6 gated
 79  l2_or_g      I     0    9999999  ! OR level2 gated
 80  l2_ok_g      I     0    9999999  ! Level 2 OK gated
 81  trig1_lg     I     0    9999999  ! Trigger 1 live gated
 82  trig2_lg     I     0    9999999  ! Trigger 2 live gated
 83  trig3_lg  	  I     0    9999999  ! Trigger 3 live gated
 84  trig4_lg  	  I     0    9999999  ! Trigger 4 live gated
 85  trig5_lg  	  I     0    9999999  ! Trigger 5 live gated
 86  trig6_lg  	  I     0    9999999  ! Trigger 6 live gated
 87  trig7_lg  	  I     0    9999999  ! Trigger 7 live gated
 88  trig8_lg  	  I     0    9999999  ! Trigger 8 live gated
 89  trig9_lg     I     0    9999999  ! Trigger 9 live gated
 90  trig10_lg    I     0    9999999  ! Trigger 10 live gated
 91  trig11_lg    I     0    9999999  ! Trigger 11 live gated
 92  trig12_lg    I     0    9999999  ! Trigger 12 live gated
 93  notused93	  I     0    9999999  ! not used
 94  notused94    I     0    9999999  ! not used
 95  ignore95     I     0    9999999  ! ignore
 96  ignore96  	  I     0    9999999  ! ignore
!
 END TABLE
!




