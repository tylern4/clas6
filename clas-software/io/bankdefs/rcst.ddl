!-----------------------------------------------------------------------
!       BANKname BANKtype      ! Comments
 TABLE  RCST    ! create write display delete ! Roc status bank
!
!   ATTributes:
!   -----------
!COL ATT-name 	   	    FMT  Min     Max     ! Comments
!     	      
  1 down_count                I    0    9999999  ! download count
  2 prestart_count            I    0    9999999  ! prestart count
  3 go_count                  I    0    9999999  ! go count
  4 trig_count                I    0    9999999  ! total trigger count
  5 event_count               I    0    9999999  ! phys event count
  6 sync_count                I    0    9999999  ! force-sync event count
  7 run_trig_count            I    0    9999999  ! trigger count current run
  8 run_event_count           I    0    9999999  ! phys event count current run
  9 run_sync_count            I    0    9999999  ! force-sync event count current run
 10 pause_count               I    0    9999999  ! coda pause count
 11 end_count                 I    0    9999999  ! end count
 12 illegal_count             I    0    9999999  ! illegal count
 13 run_illegal_count         I    0    9999999  ! illegal count current run
 14 phys_sync_count           I    0    9999999  ! physics-sync event count
 15 run_phys_sync_count       I    0    9999999  ! physics-sync event count current run
!
 END TABLE
!
