!-----------------------------------------------------------------------
!       BANKname BANKtype      ! Comments
 TABLE  STRE         ! create write display delete ! extended Start counter results bank
!
!   ATTributes:
!   -----------
!COL ATT-name FMT Min    Max   ! Comments
!
  1  ID       I     0       6  ! sector
  2  Trk_no   I     0     1000 ! Pointer to track in HBTR
  3  ST_TIME  F -999999.9  999999.9   ! flight time from ST (ns)
  4  ST_L     F     0.  999999.9   ! flight path from ST (cm)
  5  st_pos   F     0.  999999.9   ! position within the start counter
  6  status   I     0       10 	! Status word
  7  st_edep  F     0.  999999.9   ! energy deposit in ST
  8  st_corr  F     0.       1.    ! energy correction factor for angle
!
 END TABLE 

