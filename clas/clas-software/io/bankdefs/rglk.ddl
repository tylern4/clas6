!-----------------------------------------------------------------------
!       BANKname BANKtype      ! Comments
 TABLE  RGLK    ! create write display delete ! single region hits bank
!
! Positions and directions defined in CLAS coordinate system.
! Bank record is sector-wise.
!
!   ATTributes:
!   -----------
!COL ATT-name      FMT    Min    Max   ! Comments
!
  1  IREGION       I       1      3    ! region
  2  X             F    -999.   999.   ! X pos. of hit in CLAS (cm)
  3  Y             F    -999.   999.   ! Y pos. of hit in CLAS (cm)
  4  Z             F    -999.   999.   ! Z pos. of hit in CLAS (cm)
  5  THETA0        F    -999.   999.   ! polar angle of the link position (deg)
  6  PHI0          F    -999.   999.   ! azim. angle of the link position (deg)
  7  RTHETA        F    -999.   999.   ! polar angle of the link direction (deg)
  8  RPHI          F    -999.   999.   ! azim. angle of the link direction (deg)
  9  CHI2          F       0    999.   ! fit chi2        
 10  STATUS        I       0    100    ! MINUIT fit status (from 0=bad to 3=ok)
!
 END TABLE
!
