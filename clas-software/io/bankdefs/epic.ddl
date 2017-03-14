!-----------------------------------------------------------------------
!       BANKname BANKtype      ! Comments
 TABLE  EPIC          ! create write display delete 
!
!   Online epics channel values
!   record no.=0 :  read out of lots of epics channels (every 20 sec)
!   record no.=1 :  read out of BPMs and Active Colemator (every 2 sec)
!
!   ATTributes:
!   -----------
!COL ATT-name FMT Min    Max   ! Comments
!
  1  value    F   -999999.0   999999.0 ! value of the epics channel
  2  char1    A    0          255      ! char[32] name;
  3  char2    A    0          255      ! 
  4  char3    A    0          255      ! 
  5  char4    A    0          255      ! 
  6  char5    A    0          255      ! 
  7  char6    A    0          255      ! 
  8  char7    A    0          255      ! 
  9  char8    A    0          255      ! 
!
 END TABLE