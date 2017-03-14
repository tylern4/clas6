!-----------------------------------------------------------------------
!       BANKname BANKtype      ! Comments
 TABLE  CPED       ! create write display delete ! Untranslated pedestal bank, measured during the pedestal run.
!
!   ATTributes:
!   -----------
!COL ATT-name FMT Min    Max   ! Comments
!
  1   slot        I     0  30      ! ADC slot
  2   channel     I     0  70      ! ADC channel
  3   mean        I     0  100000  ! adc pedestal mean value (channel)
  4   sigma       F     0  100000  ! sigma of the pedestal distribution (channel)
  5   offset      I     0  100000  ! offset for sparsification threshold calculation
!
!    RELations:
!    ----------
!COL RELname  RELtype INTbank  ! Comments
!                     (COL)
!
 END TABLE
!
