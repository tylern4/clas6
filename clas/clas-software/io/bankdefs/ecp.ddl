!-----------------------------------------------------------------------
!       BANKname BANKtype      ! Comments
 TABLE  ECP               ! create write display delete 
!                               EC plane for track matching geometry bank 
!
!   ATTributes:
!   -----------
!COL ATT-name FMT Min    Max   ! Comments
1    N1x        F   -1.0   +1.0      ! x component of outward normal to plane
2    N1y        F   -1.0   +1.0      ! y component of outward normal to plane
3    N1z        F   -1.0   +1.0      ! z component of outward normal to plane
4    R1n        F   0.0  9999.9      ! distanse in cm from origin to plane
5    PlW        F   0.0  9999.9      ! whole plane depth
6    PlI        F   0.0  9999.9      ! inner plane depth
7    PlO        F   0.0  9999.9      ! outer plane depth
!
!    RELations:
!    ----------
!COL RELname  RELtype INTbank  ! Comments
!                     (COL)
!
 END TABLE
!
