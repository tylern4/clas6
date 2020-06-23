!-----------------------------------------------------------------------
!       BANKname BANKtype      ! Comments
 TABLE  TACO  ! create write display delete ! down stream output bank
!		
!
!   ATTributes:
!   -----------
!COL ATT-name FMT Min    Max   ! Comments
!
  1  ID       I    1          2         ! ID  1= TAC, 2 = USLG
  2  TIME     F    -999999.9  999999.9  !  TDC time 
  3  ELT      F    -999999.9  999999.9  ! energy deposit TAC = left top
  4  ERT      F    -999999.9  999999.9  ! energy deposit TAC = right top
  5  ELB      F    -999999.9  999999.9  ! energy deposit TAC = left bottom
  6  ERB      F    -999999.9  999999.9  ! energy deposit TAC = right bottom
  7  ESUM     F    -999999.9  999999.9  ! energy deposit TAC = sum1
  8  ESUM2    F    -999999.9  999999.9  ! energy deposit TAC = sum2
  9  ESUM3    F    -999999.9  999999.9  ! energy deposit TAC = sum3
 10  TID      I    0     121            ! T id of the corresponding T
!
 END TABLE
!
