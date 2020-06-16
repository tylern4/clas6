!-----------------------------------------------------------------------
!       BANKname BANKtype      ! Comments
 TABLE  EVNT    ! create write display delete ! RECSIS reconstructed event bank
!
!   ATTributes:
!   -----------
!COL ATT-name FMT  Min     Max     ! Comments
!
  1  ID       I  -5000   5000    ! Particle Data Group ID (from SEB) 
  2  Pmom     F     0.   20.     ! momentum (from tracking)
  3  Mass     F     0.   10.     ! mass squared (from SEB =p**2(1.-betta**2)/beta**2) 
  4  Charge   I    -1     1      ! charge (from tracking)
  5  Betta    F     0.    1.     ! Particle velocity in the units of c (=R_trk/TOF/c)
  6  Cx       F    -1.    1.     ! x dir cosine at track origin 
  7  cy       F    -1.    1.     ! y dir cosine at track origin
  8  cz       F    -1.    1.     ! z dir cosine at track origin
  9  X        F  -100.  100.     ! X coordinate of vertex (cm)
 10  Y        F  -100.  100.     ! Y coordinate of vertex (cm)
 11  Z        F  -100.  100.     ! Z coordinate of vertex (cm)
 12  DCstat   I     0    50      ! Pointer to DCPB bank (=0 if DC is not involved) 
 13  CCstat   I     0    50      ! Pointer to CCPB bank (=0 if CC is not involved) 
 14  SCstat   I     0    50      ! Pointer to SCPB bank (=0 if SC is not involved)  
 15  ECstat   I     0    50      ! Pointer to ECPB bank (=0 if EC is not involved) 
 16  LCstat   I     0    50      ! Pointer to LCPB bank (=0 if LAC is not involved) 
 17  STstat   I     0    50      ! Pointer to STPB bank (=0 if ST is not involved) 
 18  Status   I     0   0xFFFF   ! Status word (=0 out of time particle)
!
!    RELations:
!    ----------
!COL RELname  RELtype INTbank  ! Comments
!                     (COL)
!
 END TABLE
!
