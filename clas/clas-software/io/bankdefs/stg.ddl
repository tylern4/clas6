!********************************************************************
!       BANKname BANKtype      ! Comments
 TABLE  STG  ! create write display delete ! Start Counter Geometry bank
!
!   ATTributes:
!   -----------
!COL ATT-name FMT    Min     Max   ! Comments
!
  1  id        I     1         6      ! sector id
  2  leg_x_max F     -100.     100.   ! distance from beam axis to inner face of leg 
  3  leg_x_min F     -100.     100.   !
  4  leg_y_max F     -100.     100.   ! 
  5  leg_y_min F     -100.     100.   ! 
  6  leg_z_max F     -100.     100.   !
  7  let_z_min F     -100.     100.   ! 
  8  nose_x_max  F   -100.     100.   !  
  9  nose_x_min  F   -100.     100.   ! 
 10  nose_y_max  F   -100.     100.   !
 11  nose_y_min  F   -100.     100.   !
 12  nose_z_max  F   -100.     100.   !
 13  nose_z_min  F   -100.     100.   !
 14  noseAngle F     0.0       360.0  !
 15  lleg      F     0.0       100.0  !
 16  lnose     F     0.0       100.0  !
!
 END TABLE


