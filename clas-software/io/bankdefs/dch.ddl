!-----------------------------------------------------------------------
!       BANKname BANKtype      ! Comments
 TABLE  DCH         ! create write display delete ! GSIM Drift chamber hits
!
!   ATTributes:
!   -----------
!COL ATT-name FMT Min    Max   ! Comments
  1  x     F   -1000.   2000.  ! x at layer center
  2  y     F   -1000.   2000.  ! y at layer center
  3  z     F   -1000.   2000.  ! z at layer center
  4  step  F     0.     20.    ! step size through layer
  5  dedx  F     0.     1.     ! energy deposit in layer
  6  pmom  F     0.    20.     ! track momentum at layer center 
  7  time  F     0.    100.    ! time of hit at layer center
  8  cx    F    -1.     1.     ! track x dir cosine at layer center
  9  cy    F    -1.     1.     ! track y dir cosine at layer center
 10  cz    F    -1.     1.     ! track z dir cosine at layer center
 11  track I     0    0xFFFF   ! track number
 12  id    I  -5000   5000     ! track PDG id
 13  layer I  -5000   5000     ! layer number
!
!    RELations:
!    ----------
!COL RELname  RELtype INTbank  ! Comments
!                     (COL)
!
 END TABLE
!
