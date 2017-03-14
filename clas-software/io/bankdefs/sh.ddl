!-----------------------------------------------------------------------
!       BANKname BANKtype      ! Comments
 TABLE  SH       B16   ! create write display delete ! Scintillation Hodoscope event bank
!
!   ATTributes:
!   -----------
!COL ATT-name FMT Min    Max   ! Comments
!
  1  ID       I   257    1282  ! the address of the hit detector element
  2  TDC      I     0   65536  ! tdc information (channels)
  3  ADC      I     0   65536  ! adc information (channels)
!
!    RELations:
!    ----------
!COL RELname  RELtype INTbank  ! Comments
!                     (COL)
!
 END TABLE
!
