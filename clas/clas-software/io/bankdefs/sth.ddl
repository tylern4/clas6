!-----------------------------------------------------------------------
!       BANKname BANKtype      ! Comments
 TABLE  STH            ! create write display delete ! GSIM ST hit info
!
!   ATTributes:
!   -----------
!COL ATT-name FMT Min    Max   ! Comments
  1  x     F   -1000.   2000. ! x of hit
  2  y     F   -1000.   2000. ! y of hit
  3  z     F   -1000.   2000. ! z of hit
  4  cx    F    -1.     1.    ! track x dir cosine
  5  cy    F    -1.     1.    ! track y dir cosine
  6  cz    F    -1.     1.    ! track z dir cosine
  7  pmom  F     0.    20.    ! track momentum
  8  track I     0    0xFFFF  ! track number
  9  id    I  -5000   5000    ! track PDG id
 10  tof   F     0.     1000. ! flight time
!
!    RELations:
!    ----------
!COL RELname  RELtype INTbank  ! Comments
!                     (COL)
!
 END TABLE
!
