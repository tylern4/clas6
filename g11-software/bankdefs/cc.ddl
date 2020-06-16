!-----------------------------------------------------------------------
!       BANKname BANKtype      ! Comments
 TABLE  CC       B16 ! create write display delete ! Forward Cerenkov counters event bank
!
!   ATTributes:
!   -----------
!COL ATT-name FMT Min    Max   ! Comments
!
  1  ID       I     1      36  ! the address of the hit detector element
  2  TDC      I     0  100000  ! tdc information (channels)
  3  ADC      I     0  100000  ! adc information (channels)
!
!    RELations:
!    ----------
!COL RELname  RELtype INTbank  ! Comments
!                     (COL)
!
 END TABLE
!
