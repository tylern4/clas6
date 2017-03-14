!-----------------------------------------------------------------------
!       BANKname BANKtype      ! Comments
 TABLE  SCT      B16   ! create write display delete  ! Scintillation counter event bank (TDC)
!
!   ATTributes:
!   -----------
!COL ATT-name FMT Min    Max   ! Comments
!
  1  ID       I     1      96  ! the address of the hit detector element
  2  TDC      I     0   65535  ! tdc information (channels)
!
 END TABLE
!
