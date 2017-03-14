!-----------------------------------------------------------------------
!       BANKname BANKtype      ! Comments
 TABLE  SCPE       ! create write display delete !Translated SC pedestal bank, measured during the pedestal run. 
!
!   ATTributes:
!   -----------
!COL ATT-name FMT Min    Max   ! Comments
!
  1  ID              I     1      48  ! the address of the hit detector element
  2  mean_left       I     0   65536  ! left adc pedestal mean value (channel)
  3  sigma_left      F     0   65536  ! sigma of the pedestal distribution for left adc (channel)
  4  mean_right      I     0   65536  ! right adc pedestal mean value (channel)
  5  sigma_right     F     0   65536  ! sigma of the pedestal distribution for right adc (channel
!
!    RELations:
!    ----------
!COL RELname  RELtype INTbank  ! Comments
!                     (COL)
!
 END TABLE
!
