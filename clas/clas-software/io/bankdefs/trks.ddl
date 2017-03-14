!       BANKname BANKtype      ! Comments
 TABLE  TRKS  ! create write display delete ! TRacK Status Bank 
!
! THIS IS THE INTERPACKAGE MATCHING BANK
! modified: F.Klein 12/3/97: return id of CC,SC,EC that match to the track
!   ATTributes:
!   -----------
!COL ATT-name FMT    Min     Max   ! Comments
!
  1  trk_lev    I    100      420    ! Tracknr + analysis_level*100
  2  beta       F      0.       1.   ! Beta of the track in unit of c
  3  st_time    F  -1000.    1000.   ! Start time of the event (track: photon trigg)
  4  cc_time    F      0.    1000.   ! CC calibrated time for this track (ns)
  5  sc_time    F      0.    1000.   ! SC calibrated time for this track (ns)
  6  ec_time    F      0.    1000.   ! EC calibrated time for this track (ns)
  7  st_id      I      0       48    ! ST hit id (ptr. to STx: photon trigg)
                                     !   or SC paddle# (el. trigg)
  8  cc_id      I      0       10    ! CC hit id (ptr. to CCRC)
  9  sc_id      I      0       10    ! SC hit id (ptr. to SCRW)
 10  ec_id      I      0       10    ! EC Cluster-id (ptr. to ECHB)
!                                    ! hit_whole*10000+hit_inner*100+hit_outer
 END TABLE
!