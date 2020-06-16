!-----------------------------------------------------------------------
!       BANKname BANKtype       ! Comments
 TABLE  LASR  ! create write display delete ! events from laser calibration of the EC
!
!   ATTributes:
!   -----------
!COL ATT-name FMT Min    Max   ! Comments
!
  1  ID       I         1000       10000    ! identifier  0-1099  TOF 1100-10000 EC laser  
  2  stat1    I   -100000000   100000000    ! status words different for each id
  3  stat2    I   -100000000   100000000    !  "
  4  stat3    I   -100000000   100000000    !  "
! 
 END TABLE