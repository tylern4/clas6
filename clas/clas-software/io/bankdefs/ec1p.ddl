!       BANKname BANKtype      ! Comments
 TABLE  EC1P               ! create write display delete 
!                               EC1 plane for track matching geometry bank 
!
!   ATTributes:
!   -----------
!COL ATT-name FMT Min    Max   ! Comments
1    N1x        F   -1.0   +1.0      ! x component of outward normal to plane
2    N1y        F   -1.0   +1.0      ! y component of outward normal to plane
3    N1z        F   -1.0   +1.0      ! z component of outward normal to plane
4    R1n        F   0.0  9999.9      ! distanse in cm from origin to plane
!
!    RELations:
!    ----------
!COL RELname  RELtype INTbank  ! Comments
!                     (COL)
!
 END TABLE
