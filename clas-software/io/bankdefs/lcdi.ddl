!-----------------------------------------------------------------------
!       BANKname BANKtype      ! Comments
 TABLE  LCDI        ! create write display delete ! LAC channel discriminator thresholds
!
!   ATTributes:
!   -----------
!COL ATT-name FMT Min    Max   ! Comments
!
  1   crate       I     0  10      ! CAMAC crate number
  1   slot        I     0  30      ! slot
  1   mask        I     0  999999  ! mask
  1   threshold   I     0  100000  ! actual threshold value (mV)
!
!    RELations:
!    ----------
!COL RELname  RELtype INTbank  ! Comments
!                     (COL)
!
 END TABLE
!
