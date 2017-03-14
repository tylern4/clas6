!
!-----------------------------------------------------------------------
!       BANKname BANKtype      ! Comments
 TABLE  TAGR  ! create write display delete ! Tagger result bank (AL-LYM-FR 9/29/1997 --- FYDW)
!
!   ATTributes:
!   -----------
!COL ATT-name FMT Min    Max   ! Comments
!
  1  ERG       F    0.  10.     ! Energy of the photon in GeV
  2  TTAG      F    -20.  200.  ! Time of the photon has reconstructed in the Tagger
  3  TPHO      F    -20.  200.  ! Time of the photon after RF correction
  4  STAT      I    0   4096    ! Status ( 7 or 15 are Good) other values have problems (see tag_process_TAGR.F) 
  5  T_id      I    1   121     ! T counter Id
  6  E_id      I    1   767     ! E counter Id
!
 END TABLE
