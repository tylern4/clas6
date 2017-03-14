!-----------------------------------------------------------------------
!       BANKname BANKtype      ! Comments
 TABLE  ECPE       ! create write display delete ! Translated EC pedestal bank, measured during the pedestal run.
!
!   ATTributes:
!   -----------
!COL ATT-name FMT Min    Max   ! Comments
!
  1  ID         I   257    1572  ! the address of the hit detector element
  2  mean       I     0   65536  ! adc pedestal mean value (channel)
  3  sigma      F     0   65536  ! sigma of the pedestal distribution (channel)
!
!    RELations:
!    ----------
!COL RELname  RELtype INTbank  ! Comments
!                     (COL)
!
 END TABLE
!
