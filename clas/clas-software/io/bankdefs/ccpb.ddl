!-----------------------------------------------------------------------
!       BANKname BANKtype      ! Comments
 TABLE  CCPB    ! create write display delete ! CC hits involved in the event
!
!   ATTributes:
!   -----------
!COL ATT-name FMT  Min     Max     ! Comments
!
  1  ScSgHt   I     0   700      ! 100*sector + Cluster # in CCRC
  2  Nphe     F     0. 1000.     ! Number of photo-electrons*10
  3  Time     F    -5.  150.     ! Flight time relative to the evnt start time
  4  Path     F     0.  600.     ! Path lenght from target (from tracking)
  5  Chi2CC   F     0.   10.     ! Geometrical matching: angle between CC hit and
!                                  nearest SC hit (in rad)
  6  Status   I     0   0xFFFF   ! Status word - now 10*(CC segment number) 
!                                ! + 1000*( 1 + phy_index).
!                                ! PHY_INDEX = -1 : left  PMT (1,3,5...35)
!                                ! PHY_INDEX = +1 : right PMT (2,4,6...36)
!            ! PHY_INDEX =  0 : both left and right (electr. near midplane)
!
!    RELations:
!    ----------
!COL RELname  RELtype INTbank  ! Comments
!                     (COL)
!
 END TABLE
!
