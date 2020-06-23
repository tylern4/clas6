!-----------------------------------------------------------------------
!       BANKname BANKtype      ! Comments
 TABLE  TAGE       B16  ! create write display delete 
!
! Tagger E-counter data event bank
! record no.: 1    (up to 16 entries per ID (multihit tdc))
! address ID:  | id1 (bit 8-15) | id2 (bit 0-7) |
!   for id1=0 or id1=1:   ID=(id1*256)+id2 = E-counter no.: 1...384
!   for id1=2:            id2=T-counter no.    (for id=1...61)
!                         id2=ST trigger for Sec.1&2,3&4,5&6  (for id2=62..64)
!   for id1=4:            id2=PS-counter       (for id2=1...32)
!                         id2=mid-level logic  (for id2=101-116)
!                
!   ATTributes:
!   -----------
!COL ATT-name FMT Min    Max   ! Comments
!
  1  ID       I    1    1136   ! the address of the hit detector element
  2  TDC      I    0   32767   ! tdc information (channels) [multihit tdc]
!
 END TABLE
!
