!-----------------------------------------------------------------------
!       BANKname BANKtype      ! Comments
 TABLE  SCDI        ! create write display delete ! SC channel discriminator thresholds
!
!   ATTributes:
!   -----------
!COL ATT-name FMT Min    Max   ! Comments
!
  1   crate       I     0  10      ! CAMAC crate number
  1   slot        I     0  30      ! slot
  1   mask        I     0  999999  ! mask
  1   threshold   I     0  100000  ! actual threshold value (mV)
  1   width       I     0  100000  ! actual width value
!
!    RELations:
!    ----------
!COL RELname  RELtype INTbank  ! Comments
!                     (COL)
!
 END TABLE
!
