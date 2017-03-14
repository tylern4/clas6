!-----------------------------------------------------------------------
!       BANKname BANKtype      ! Comments
 TABLE  ECPC         ! create write display delete ! EC Particle Calibration bank
!
!   ATTributes:
!   -----------
!COL ATT-name FMT Min    Max   ! Comments
!
  1  ID       I   101     336  ! layer(1-3) * 100 + strip ID
  2  TDC      F  -100   65536  ! tdc (channels)
  3  ADC      F  -100   65536  ! adc - pedestal (channels)
!
!    RELations:
!    ----------
!COL RELname  RELtype INTbank  ! Comments
!                     (COL)
!
 END TABLE
!
