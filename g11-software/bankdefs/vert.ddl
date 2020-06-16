!********************************************************************
!       BANKname BANKtype      ! Comments
 TABLE  VERT  ! create write display delete ! vertex Result bank
!
! THE COORDINATE SYSTEM USED IN THIS BANK IS THE DETECTOR COORDINATE SYSTEM.
!
!   ATTributes:
!   -----------
!COL ATT-name FMT    Min     Max    ! Comments
!
  1  vertex     I   -1000    1000   ! vertex id
  2  trk1       I       0      10   ! track #1
  3  trk2       I       0      10   ! track #2
  4  x          F  -1000.   1000.   ! x vector3_t vert{x,y,z}
  5  y          F  -1000.   1000.   ! y 
  6  z          F  -1000.   1000.   ! z
  7  sepd       F   -100.    100.   ! seperation distance
!
! 
!     
!
 END TABLE


