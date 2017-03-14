!-----------------------------------------------------------------------
!       BANKname BANKtype      ! Comments
 TABLE  SHT      B16   ! create write display delete ! Scintillation Hodoscope event bank (TDCs only)
!
!   ATTributes:
!   -----------
!COL ATT-name FMT Min    Max   ! Comments
!
  1  ID       I   257    1282  ! the address of the hit detector element
  2  TDC      I     0   65535  ! tdc information (channels)
!
 END TABLE
!
