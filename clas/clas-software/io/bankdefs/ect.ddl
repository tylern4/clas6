!-----------------------------------------------------------------------
!       BANKname BANKtype      ! Comments
 TABLE  ECT      B16   ! create write display delete ! Forward calorimeter event bank (TDCs only)
!
!   ATTributes:
!   -----------
!COL ATT-name FMT Min    Max   ! Comments
!
  1  ID       I   257    1572  ! the address of the hit detector element
  2  TDC      I     0   65535  ! tdc information (channels)
!
 END TABLE
!
