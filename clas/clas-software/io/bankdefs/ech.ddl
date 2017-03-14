!-----------------------------------------------------------------------
!       BANKname BANKtype      ! Comments
 TABLE  ECH            ! create write display delete ! GSIM Calorimeter track hit info
!
!   ATTributes:
!   -----------
!COL ATT-name FMT Min    Max   ! Comments
!
  1  x       F   -1000.   2000. ! x of hit
  2  y       F   -1000.   2000. ! y of hit
  3  z       F   -1000.   2000. ! z of hit
  4  cx      F    -1.     1.    ! track x dir cosine
  5  cy      F    -1.     1.    ! track y dir cosine
  6  cz      F    -1.     1.    ! track z dir cosine
  7  pmom    F     0.    20.    ! track momentum
  8  id      I  -5000   5000    ! track PDG id
  9  tof     F     0.   1000.   ! time of flight
 10  edepin  F     0.    20.    ! deposit energy (inner part)
 11  edepout F     0.    20.    ! deposit energy (outer part)
!
!    RELations:
!    ----------
!COL RELname  RELtype INTbank  ! Comments
!                     (COL)
!
 END TABLE
!
