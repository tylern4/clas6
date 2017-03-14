!-----------------------------------------------------------------------
!       BANKname BANKtype      ! Comments
 TABLE  DC0       B16  ! create write display delete ! Drift chamber event bank
!
!   ATTributes:
!   -----------
!COL ATT-name FMT Min    Max   ! Comments
!
  1  ID       I   257    9408  ! the address of the hit detector element
  2  TDC      I     0   65536  ! tdc information (channels)
!
 END TABLE
!
