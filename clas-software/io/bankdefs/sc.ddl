!-----------------------------------------------------------------------
!       BANKname BANKtype      ! Comments
 TABLE  SC       B16   ! create write display delete ! Scintillation counter event bank
!
!   ATTributes:
!   -----------
!COL ATT-name FMT Min    Max   ! Comments
!
  1  ID       I     1      48  ! the address of the hit detector element
  2  TDCL     I     0   65536  ! tdc information (channels)
  3  ADCL     I     0   65536  ! adc information (channels)
  4  TDCR     I     0   65536  ! tdc information (channels)
  5  ADCR     I     0   65536  ! adc information (channels)
!
!    RELations:
!    ----------
!COL RELname  RELtype INTbank  ! Comments
!                     (COL)
!
 END TABLE
!
