!-----------------------------------------------------------------------
!       BANKname BANKtype      ! Comments
 TABLE  ECCL                 ! create write display delete ! Forward EC Calibration Bank
!
!   ATTributes:
!   -----------
!COL ATT-name FMT   Min    Max   ! Comments
  1  id       I       1     216  ! PMT ID
  2  lDB      F       0 99999.0  ! Stack atten length (database)(cm)
  3  lDBu     F       0 99999.0  ! lDB Error (cm)
  4  lMIP     F       0 99999.0  ! Stack atten length (MinIonPart)(cm) 
  5  lMIPu    F       0 99999.0  ! lMIP Error (cm)
  6  lSHR     F       0 99999.0  ! Stack atten length (showers)(cm)  
  7  lSHRu    F       0 99999.0  ! lSHR Error (cm) 
  8  stat     I       0  0xFFFF  ! 4 byte status word    
!
!    RELations:
!    ----------
!COL RELname  RELtype INTbank  ! Comments
!                     (COL)
!
 END TABLE
!
