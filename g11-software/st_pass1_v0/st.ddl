!-----------------------------------------------------------------------
!       BANKname BANKtype      ! Comments
 TABLE  ST         B16   ! create write display delete ! Start counter event bank
!
!   ATTributes:
!   -----------
!COL ATT-name FMT Min    Max   ! Comments
!
  1  ID       I     1       6  ! the sector of the hit detector element
  2  TDC      I     0   65536  ! PMT tdc information
  3  ADC      I     0   65536  ! PMT adc information
!
!    RELations:
!    ----------
!COL RELname  RELtype INTbank  ! Comments
!                     (COL)
!
 END TABLE
!
