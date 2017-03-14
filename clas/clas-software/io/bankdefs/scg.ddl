!-----------------------------------------------------------------------
!       BANKname BANKtype      ! Comments
 TABLE  SCG                 ! create write display delete ! GSIM & SC Scintillator geometry info
!
!   ATTributes:
!   -----------
!COL ATT-name FMT Min    Max   ! Comments
  1   id      I       1       0x00FF      !  paddle id#
  2   panel   I       1       0x00FF      ! panel number - to which plane (panel) the paddle belongs
  3  Xccw     F     -1000.     1000.      ! X center of CCW end of paddle in CLAS system
  4  Yccw     F     -1000.     1000.      ! Y center of CCW end 
  5  Zccw     F     -1000.     1000.      ! - Z center of CCW end
  6  Xcw      F    -1000.     1000.       ! X center of CW end 
  7  Ycw      F    -1000.     1000.       ! Y center of CW end
  8  Zcw      F    -1000.     1000.       !- Z center of CW end 
  9  WIDTH    F      0.         1         ! width (cm) (~|| CLAS theta) 
 10  THICK    F      0.         100.      ! thickness (cm) (~|| CLAS r) 
 11  DELTA    F    -100.      100.        ! the difference in length of the higher-theta slab minus the lower-theta slab divided by 4 (cm) 
!
!    RELations:
!    ----------
!COL RELname  RELtype INTbank  ! Comments
!                     (COL)
!
 END TABLE
!
