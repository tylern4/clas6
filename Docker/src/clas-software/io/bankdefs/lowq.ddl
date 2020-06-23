!********************************************************************
!       BANKname BANKtype      ! Comments
 TABLE  LOWQ  ! create write display delete ! FET reconstruction
!
! THE COORDINATE SYSTEM USED IN THIS BANK IS THE DETECTOR COORDINATE SYSTEM.
!
!   ATTributes:
!   -----------
!COL ATT-name FMT    Min     Max   ! Comments
!
  1  pid       I       0         10    ! particle id (GEANT)
  2  x          F     -100.     100.   ! vector3_t vert; Vertex position {x,y,z}
  3  y         F      -100.     100.   ! y
  4  z          F    -500.     500.    ! z
  5  E         F     0.     16.        ! vector4_t p; Energy
  6  px         F    -16.     16.      ! momentum {x,y,z}
  7  py        F     -16.      16.     !  py
  8  pz         F     -16.      16.   ! pz
  9  q          F     -16.      16.   ! charge
 10 qpid     F   -100.0 100.0 ! quality factor for the pid
 11 qtrk      F   -100.  100.0 ! quality factor for the trk
 12 flags     I    0         10000 ! set of flags defining track (ie, BEAM)
!
 END TABLE

