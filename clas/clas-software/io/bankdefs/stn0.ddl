!-----------------------------------------------------------------------
!       BANKname BANKtype      ! Comments
 TABLE  STN0      B16   ! create write display delete ! NEW Start counter TDC bank
!
!   ATTributes:
!   -----------
!COL ATT-name FMT Min    Max   ! Comments
!
  1  ID       I     1      24  ! detector element
  2  TDC      I     0   65536  ! tdc information (channels)
!
!    RELations:
!    ----------
!COL RELname  RELtype INTbank  ! Comments
!                     (COL)
!
 END TABLE
!
