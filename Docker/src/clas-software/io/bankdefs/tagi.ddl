!-----------------------------------------------------------------------
!       BANKname BANKtype      ! Comments
 TABLE  TAGI  ! create write display delete ! Tagger Intermediary event bank
!		
!
!   ATTributes:
!   -----------
!COL ATT-name FMT Min    Max   ! Comments
!
  1  IDT      I    0     121    ! T id
  2  TIMEL    F    -999999.9 999999.9 ! time information (Left counters channels)
  3  TIMER    F    -999999.9 999999.9 ! time information (Right counters channels)
  4  IDE      I    1     767    ! E id
  5  TIMEE    F    -999999.9 999999.9 ! time information (E counters)
  6  TIMEMEAN F    -999999.9 999999.9 ! time information (left/right mean value | after alignement)
  7  TRF      F    -999999.9 999999.9 ! time information (mean val - RF | after alignement)
  8  NEXTTIME F    -999999.9 999999.9 ! time difference with the next T when in coincidence | after alignment)
!
 END TABLE
!
