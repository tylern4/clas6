!-----------------------------------------------------------------------
!       BANKname BANKtype      ! Comments
 TABLE  SPAR       ! create write display delete ! Untranslated pedestal bank, read out from the hardware.
!
!   ATTributes:
!   -----------
!COL ATT-name FMT Min    Max   ! Comments
!
  1   slot        I     0  30      ! ADC slot
  2   channel     I     0  70      ! ADC channel
  3   spar        I     0  100000  ! sparsification threshold (channel)
  4   pedmean     I     0  100000  ! pedestal mean value measured, using internal 500ns gate (channel)
!
!    RELations:
!    ----------
!COL RELname  RELtype INTbank  ! Comments
!                     (COL)
!
 END TABLE
!
