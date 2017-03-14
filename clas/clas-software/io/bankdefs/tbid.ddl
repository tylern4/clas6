!       BANKname BANKtype      ! Comments
 TABLE  TBID  ! create write display delete ! Time Based particle ID
!
! Hit Based Tracking Particle ID
!
!   ATTributes:
!   -----------
!COL ATT-name FMT    Min     Max     ! Comments
!
  1  track      I       1      100   ! Track (index to TBTR) or zero if neutral
  2  sec        I       1        6   ! Sector track is in
  3  beta       F      0.       1.   ! Beta of the track in units of c
  4  vtime      F      0.   10000.   ! vertex time of track
  5  sc_stat    I  -10000   100000   ! status of hit matching to SC: see sc.h
  6  sc_id      I       1       48   ! Pointer to SCRC bank
  7  sc_time    F -10000.   10000.   ! SC calibrated time for this track (ns)
  8  sc_qual    F      0.   10000.   ! quality of match for SC
  9  sc_vtime   F -10000.   10000.   ! time at vertex for SC(ns)
 10  sc_beta    F      0.       1.   ! Beta calculated from TOF from SC
 11  cc_stat    I  -10000   100000   ! status of hit matching to CC: see sc.h
 12  cc_id      I       1      100   ! pointer to CC01 bank
 13  cc_time    F -10000.   10000.   ! CC calibrated time for this track (ns)
 14  cc_qual    F      0.   10000.   ! quality of match for CC
 15  cc_vtime   F -10000.   10000.   ! time at vertex for CC(ns)
 16  cc_beta    F      0.       1.   ! Beta as calculated by the EC
 17  ec_stat    I  -10000   100000   ! status of hit matching to ec: see sc.h
 18  ec_id      I       1      100   ! Pointer to ECHB bank
 19  ec_time    F -10000.   10000.   ! EC calibrated time for this track (ns)
 20  ec_qual    F      0.   10000.   ! EC quality factor
 21  ec_vtime   F -10000.   10000.   ! time at vertex for EC(ns)
 22  ec_beta    F      0.       1.   ! Beta as calculated by the EC
 23  st_stat    I  -10000   100000   ! status of hit matching to ST
 24  st_id      I       1      100   ! Pointer to STR bank
 25  st_time    F -10000.   10000.   ! ST calibrated time for this track (ns)
 26  st_qual    F      0.   10000.   ! ST quality factor
 27  st_vtime   F -10000.   10000.   ! time at vertex for ST(ns)
 28  st_beta    F      0.       1.   ! Beta as calculated by the ST
 29  lac_stat    I  -10000   100000  ! status of hit matching to LAC
 30  lac_id      I       1      100  ! Pointer to EC1R bank
 31  lac_time    F -10000.   10000.  ! LAC calibrated time for this track (ns)
 32  lac_qual    F      0.   10000.  ! LAC quality factor
 33  lac_vtime   F -10000.   10000.  ! time at vertex for LAC(ns)
 34  lac_beta    F      0.       1.  ! Beta as calculated by the LAC
!
! see clasBID_t in pid.h
!
 END TABLE
!
