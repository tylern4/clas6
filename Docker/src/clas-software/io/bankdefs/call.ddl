!-----------------------------------------------------------------------
!       BANKname BANKtype      ! Comments
 TABLE  CALL      B16   ! create write display delete ! Catch-ALL event bank
!
!   ATTributes:
!   -----------
!COL ATT-name FMT Min    Max   ! Comments
!
  1  ID       I     1      10  ! catch-all element(RF,laser diode,etc)
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
