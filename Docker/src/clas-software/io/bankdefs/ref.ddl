!-----------------------------------------------------------------------
!       BANKname BANKtype      ! Comments
 TABLE  REF      B16   ! create write display delete ! Reference signals for pipeline TDCs
!
!   ATTributes:
!   -----------
!COL ATT-name FMT Min    Max   ! Comments
!
  1  ID       I   257    1572  ! roc id (high byte) & slot number (low byte)
  2  TDC      I     0   65535  ! tdc information (channels)
!
 END TABLE
!
