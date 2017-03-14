!-----------------------------------------------------------------------
!       BANKname BANKtype      ! Comments
 TABLE  PCO   ! create write display delete ! down stream output bank
!		
!
!   ATTributes:
!   -----------
!COL ATT-name FMT Min    Max   ! Comments
!
  1  TIME     F    -999999.9  999999.9 !  TDC time 
  2  ELT      F    -999999.9  999999.9  ! energy deposit PC = left top
  3  ERB      F    -999999.9  999999.9  ! energy deposit PC = right bottom
  4  ELB      F    -999999.9  999999.9  ! energy deposit PC = left bottom 
  5  ERT      F    -999999.9  999999.9  ! energy deposit PC = right top
  6  EMAIN    F    -999999.9  999999.9  ! energy deposit PC = MAIN
  7  EVETO    F    -999999.9  999999.9  ! energy deposit PC = veto
  8  TID      I    0     121            ! T id of the corresponding T
!
 END TABLE
!
