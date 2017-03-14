!-----------------------------------------------------------------------
!       BANKname BANKtype      ! Comments
 TABLE  DPSP       ! create write display delete ! Translated pair specrometer 
! pedestal bank, measured during the pedestal run.
!
!   ATTributes:
!   -----------
!COL ATT-name FMT Min    Max   ! Comments
!
  1  ID        I     1  999999  ! the address of the hit detector element
  2  mean      I     0  100000  ! adc pedestal mean value (channel)
  3  sigma     F     0  100000  ! sigma of the pedestal distribution (channel)
!
!    RELations:
!    ----------
!COL RELname  RELtype INTbank  ! Comments
!                     (COL)
!
 END TABLE
!
