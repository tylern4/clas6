!-----------------------------------------------------------------------
!       BANKname BANKtype      ! Comments
 TABLE  MCTK    ! create write display delete ! GSIM Monte Carlo track bank
!
!   ATTributes:
!   -----------
!COL ATT-name FMT Min    Max   ! Comments
!
  1  cx       F    -1.   1.    ! x dir cosine at track origin
  2  cy       F    -1.   1.    ! y dir cosine
  3  cz       F    -1.   1.    ! z dir cosine
  4  pmom     F     0.   20.   ! momentum
  5  mass     F     0.   10.   ! mass
  6  charge   F    -1.    1.   ! charge
  7  id       I  -5000   5000  ! track Particle Data Group id
  8  flag     I     0  0xFFFF  ! track flag
  9  beg_vtx  I     0   65536  ! beginning vertex number 
 10  end_vtx  I     0   65536  ! ending vertex number
 11  parent   I     0   65536  ! parent track
!
!    RELations:
!    ----------
!COL RELname  RELtype INTbank  ! Comments
!                     (COL)
!
 END TABLE
!
