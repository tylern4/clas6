!-------------------------------------------------------------------------------
!       BANKname BANKtype      ! Comments
 TABLE  TCSB  ! create   delete ! Toroidal Coordinate System Bank
!
!   ATTributes:
!   -----------
!COL ATT-name FMT Min    Max   ! Comments
!
  1  xpos       F  -100. 100.   ! x misalignment of TCS in HCS 		 
  2  ypos       F  -100. 100.   ! y misalignment of TCS in HCS	 
  3  zpos       F  -100. 100.   ! z misalignment of TCS in HCS	 
  4  sxpos      F    -1.   1.   ! sx sine of x-axis misorientation of TCS vs HCS		 
  5  sypos      F    -1.   1.   ! sy sine of y-axis misorientation of TCS vs HCS
  6  szpos      F    -1.   1.   ! sz sine of z-axis misorientation of TCS vs HCS
!
!    RELations:
!    ----------
!COL RELname  RELtype INTbank  ! Comments
!                     (COL)
 END TABLE
!
