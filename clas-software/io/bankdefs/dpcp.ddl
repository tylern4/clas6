!-----------------------------------------------------------------------
!       BANKname BANKtype      ! Comments
 TABLE  DPCP   ! create write display delete ! Translated pair counter 
! pedestal bank, measured during the pedestal run.
!
!   ATTributes:
!   -----------
!COL ATT-name FMT Min    Max   ! Comments
!
  1   ID        I     1  999999  ! the address of the hit detector element
  2   MN_mean   I     0  100000  ! adc pedestal mean value (channel)
  3   MN_sigma  F     0  100000  ! sigma of the pedestal distribution (channel)
  4   LT_mean   I     0  100000  ! adc pedestal mean value (channel)
  5   LT_sigma  F     0  100000  ! sigma of the pedestal distribution (channel)
  6   RB_mean   I     0  100000  ! adc pedestal mean value (channel)
  7   RB_sigma  F     0  100000  ! sigma of the pedestal distribution (channel)
  8   LB_mean   I     0  100000  ! adc pedestal mean value (channel)
  9   LB_sigma  F     0  100000  ! sigma of the pedestal distribution (channel)
  10  RT_mean   I     0  100000  ! adc pedestal mean value (channel)
  11  RT_sigma  F     0  100000  ! sigma of the pedestal distribution (channel)
  12  VT_mean   I     0  100000  ! adc pedestal mean value (channel)
  13  VT_sigma  F     0  100000  ! sigma of the pedestal distribution (channel)
!
!    RELations:
!    ----------
!COL RELname  RELtype INTbank  ! Comments
!                     (COL)
!
 END TABLE
!
