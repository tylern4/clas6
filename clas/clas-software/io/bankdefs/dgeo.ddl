!-------------------------------------------------------------------------------
 TABLE  DGEO  ! create   delete ! Drift chamber GEOmetry bank
!
!   ATTributes:
!   -----------
!COL ATT-name FMT Min    Max   ! Comments
!
  1  ID_sec     I      1   18   ! ID_sector 
  2  ID_reg     I      1   18   ! ID_region 
  3  xpos       F  -100. 100.   ! x misalignment 		 
  4  ypos       F  -100. 100.   ! y misalignment 		 
  5  zpos       F  -100. 100.   ! z misalignment 		 
  6  sxpos      F    -1.   1.   ! sx sine of little x angle 		 
  7  sypos      F    -1.   1.   ! sy sine of little y angle 
  8  szpos      F    -1.   1.   ! sz sine of little z angle 
!
 END TABLE
!-------------------------------------------------------------------------------
