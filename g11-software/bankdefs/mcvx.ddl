!-----------------------------------------------------------------------
!       BANKname BANKtype      ! Comments
 TABLE  MCVX        ! create write display delete ! GSIM Monte Carlo vertex parameters
!
!   ATTributes:
!   -----------
!COL ATT-name FMT Min    Max   ! Comments
!
  1  x       F   -1000.  2000.    ! x of vertex
  2  y       F   -1000.  2000.    ! y
  3  z       F   -1000.  2000.    ! z
  4  tof     F     0.0   999999.   ! secs of flight
  5  flag    I       0   65536    ! vertex flag
!
 END TABLE
!
