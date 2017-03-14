!-----------------------------------------------------------------------
!       BANKname BANKtype      ! Comments
 TABLE  DTCP      ! create write display delete ! Translated TAC  
! pedestal bank, measured during the pedestal run.
!
!   ATTributes:
!   -----------
!COL ATT-name FMT Min    Max   ! Comments
!
  1   ID        I     1  999999  ! the address of the hit detector element
  2   LT_mean   I     0  100000  ! adc pedestal mean value (channel)
  3   LT_sigma  F     0  100000  ! sigma of the pedestal distribution (channel)
  4   RT_mean   I     0  100000  ! adc pedestal mean value (channel)
  5   RT_sigma  F     0  100000  ! sigma of the pedestal distribution (channel)
  6   LB_mean   I     0  100000  ! adc pedestal mean value (channel)
  7   LB_sigma  F     0  100000  ! sigma of the pedestal distribution (channel)
  8   RB_mean   I     0  100000  ! adc pedestal mean value (channel)
  9   RB_sigma  F     0  100000  ! sigma of the pedestal distribution (channel)
  10  sum1_mean I     0  100000  ! adc pedestal mean value (channel)
  11  sum1_sigma F     0  100000  ! sigma of the pedestal distribution (channel)
  12  sum2_mean I     0  100000  ! adc pedestal mean value (channel)
  13  sum2_sigma F     0  100000  ! sigma of the pedestal distribution (channel)
  14  sum3_mean I     0  100000  ! adc pedestal mean value (channel)
  15  sum3_sigma F     0  100000  ! sigma of the pedestal distribution (channel)
!
!    RELations:
!    ----------
!COL RELname  RELtype INTbank  ! Comments
!                     (COL)
!
 END TABLE
!
