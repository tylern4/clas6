!-----------------------------------------------------------------------
!       BANKname BANKtype      ! Comments
 TABLE  DSPS       B16  ! create write display delete ! Down Stream Pair Spectrometer event bank
!
!   ATTributes:
!   -----------
!COL ATT-name FMT Min    Max   ! Comments
!
  1  ID       I    1    8     ! paddle ID (left = 1 to 4)( right = 5 to 8)
  2  TDC      I    0    4096  ! tdc information
  3  ADC      I    0   65536  ! adc information 
!
 END TABLE
!
