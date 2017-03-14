!-----------------------------------------------------------------------
!       BANKname BANKtype      ! Comments
 TABLE  ST1          ! create write display delete ! Start counter intermediate results bank
!
!   ATTributes:
!   -----------
!COL ATT-name FMT Min    Max   ! Comments
!
  1  ID      I     1       3  ! Pair id
  2  status  I     0     9999 ! status word
  3  TIME_1  F     -999999.9 999999.9 ! Time (ns) for side 1
  4  ADC_1   F     -999999.9 999999.9 ! Pedestal-subtracted adc for side 1
  5  TIME_2  F     -999999.9 999999.9 ! Time (ns) for side 2
  6  ADC_2   F     0.  999999.9   ! Pedestal-subtracted ADC  for side 2
!
 END TABLE
! 
