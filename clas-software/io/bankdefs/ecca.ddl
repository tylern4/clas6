!-----------------------------------------------------------------------
!       BANKname BANKtype      ! Comments
 TABLE  ECCA                 ! create write display delete ! Forward EC Calibration Bank
!
!   ATTributes:
!   -----------
!COL ATT-name FMT   Min    Max   ! Comments
  1  id       I       1     216  ! PMT ID
  2  aPED     F       0  1000.0  ! ADC pedestals (channels)
  3  aSIG     F       0  1000.0  ! ADC pedestal variance (channels)
  4  aMIP     F       0   100.0  ! ADC calibration from MIP (N.I.M.P/ch.)
  5  aMIPu    F       0   100.0  ! aMIP Error
  6  aSHR     F       0   100.0  ! ADC calibration from showers (GeV/ch.)
  7  aSHRu    F       0   100.0  ! aSHR Error
  8  stat     I       0  0xFFFF  ! 4 byte status word    
!
!    RELations:
!    ----------
!COL RELname  RELtype INTbank  ! Comments
!                     (COL)
!
 END TABLE
!
