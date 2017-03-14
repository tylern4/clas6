!-----------------------------------------------------------------------
!       BANKname BANKtype      !Comments
 TABLE   HEVT      ! create write display delete !Reconstructed event, Head bank
!
!COL ATT-name FMT Min    Max   !Comments
   1 ESTATUS  I   -1        1  ! Event Statuse after reconstruction
   2 NRUN     I    1      1000 ! Run Number (monotonically increasing)
   3 NEVENT   I    1      1000 ! Event Number in the run NRUN 
   4 TYPE     I    0      1000 ! Event Type (Data or MC)
   5 NPGP     I    0     1010  ! Number of final reconstructed particles*100 +
!                              + Number of geometrically reconstructed particles
   6 TRGPRS   I    0    100000 ! Trigger type*10000 + 
!                              + Prescale factor for that trigger (Event Class)
   7 FC       F    0.      999.! Faraday Cup (K)
   8 FCG      F    0.      999.! Faraday Cup Gated (K)
   9 TG       F    0.      999.! Clock Gated
  10 STT      F    0.      999.! Event Start Time 
  11 RF1      F    0.      999.! RF Time 
  12 RF2      F    0.      999.! RF Time
  13 CON1     F    0.   100000.! Control Rates 
  14 CON2     F    0.   100000.! Control Rates  
  15 CON3     F    0.   100000.! Control Rates 
  16 PTIME    I 100  100000000 ! Event Processing Time (UNIX time = seconds)
!
 END TABLE
