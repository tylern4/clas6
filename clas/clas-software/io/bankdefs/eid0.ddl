!-----------------------------------------------------------------------
!       BANKname BANKtype      ! Comments
 TABLE  EID0           ! create write display delete ! Fast electron identification
!                                         output bank
!
!   ATTributes:
!   -----------
!COL ATT-name   FMT Min    Max          ! Comments
!    
  1  jeid0sec    I  1      6       !  Sector # for electron candidate
  2  jeid0cc     I  1      100     !  Pointer to the hit in CCRC bank
  3  jeid0ec     I  1      100     !  Pointer to the hit in ECHB bank
  4  jeid0sc     I  1      100     !  Pointer to the hit in SCRX bank
!
!    RELations:
!    ----------
!COL RELname  RELtype INTbank  ! Comments
!                     (COL)
!
 END TABLE
!
