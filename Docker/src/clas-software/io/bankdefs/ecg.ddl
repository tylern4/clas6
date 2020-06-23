!-----------------------------------------------------------------------
!       BANKname BANKtype      ! Comments
 TABLE  ECG        ! create write display delete ! Calorimeter geometry bank
!                                       
!   ATTributes:
!   -----------
!COL ATT-name FMT Min    Max   ! Comments
!
  1  L0        F     0      10  ! distance from the target 
  2  THE0      F     0   9999.  ! angle between the beam and perpendicular
  3  YLOW      F     0   9999.  ! 
  4  YHI       F  -999.     0.  ! 
  5  DYLOW     F     0     99.  ! 
  6  DYHI      F     0     99.  ! 
  6  LRTH      F     0     99.  ! thickness of the single layer
  7  TANGR     F     0    999.  ! 
  8  SECTOR    I     1      6   ! 
  9  PHISEC    F     0    999.  ! 
 10  X0OFF     F  -999.   999.  ! 
 11  Y0OFF     F  -999.   999.  ! 
 12  Z0OFF     F  -999.   999.  ! 
 13  ROTM11    F  -999.   999.  ! 
 14  ROTM12    F  -999.   999.  ! 
 15  ROTM13    F  -999.   999.  ! 
 16  ROTM21    F  -999.   999.  ! 
 17  ROTM22    F  -999.   999.  ! 
 18  ROTM23    F  -999.   999.  ! 
 19  ROTM31    F  -999.   999.  ! 
 20  ROTM32    F  -999.   999.  ! 
 21  ROTM33    F  -999.   999.  ! 
!
!    RELations:
!    ----------
!COL RELname  RELtype INTbank  ! Comments
!                     (COL)
!
 END TABLE
!
