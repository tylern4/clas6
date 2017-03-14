!
!-----------------------------------------------------------------------
!       BANKname BANKtype      ! Comments
 TABLE  TAGM  ! create write display delete ! Tagger result bank based on T-counter Multi-hit TDCs
!
!   ATTributes:
!   -----------
!COL ATT-name FMT    Min     Max   ! Comments
!
  1  energy  F         0.     10.  ! Energy of the photon in GeV
  2  t       F    -32000.  32000.  ! T-counter time (ns)
  3  e_t     F    -32000.  32000.  ! E-counter time (ns)
  4  status  I         0    4096   ! Status (not yet used) 
  5  tid     I         1     121   ! T channel Id
  6  eid     I         1     767   ! E channel Id
!
 END TABLE
