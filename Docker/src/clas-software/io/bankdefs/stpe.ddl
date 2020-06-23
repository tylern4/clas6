!-----------------------------------------------------------------------
!       BANKname BANKtype      ! Comments
 TABLE  STPE        ! create write display delete ! Translated start counter pedestal bank, measured during the pedestal run. 
!
!   ATTributes:
!   -----------
!COL ATT-name FMT Min    Max   ! Comments
!
  1  ID        I     1      6   ! the address of the hit detector element
  2  mean      I     0   65536  ! adc pedestal mean value (channel)
  3  sigma     F     0   65536  ! sigma of the pedestal distribution (channel)
!
!    RELations:
!    ----------
!COL RELname  RELtype INTbank  ! Comments
!                     (COL)
!
 END TABLE
!
