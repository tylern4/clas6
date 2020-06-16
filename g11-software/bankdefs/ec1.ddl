!-----------------------------------------------------------------------
!       BANKname BANKtype      ! Comments
 TABLE  EC1      B16   ! create write display delete ! Large angle calorimeter event bank
!
!   ATTributes:
!   -----------
!COL ATT-name FMT Min    Max   ! Comments
!
  1  ID       I   257    1064  ! the address of the hit detector element
  2  TDCL     I     0  100000  ! tdc information (channels)
  3  ADCL     I     0  100000  ! adc information (channels)
  4  TDCR     I     0  100000  ! tdc information (channels)
  5  ADCR     I     0  100000  ! adc information (channels)
!
!    RELations:
!    ----------
!COL RELname  RELtype INTbank  ! Comments
!                     (COL)
!
 END TABLE
!
