!-----------------------------------------------------------------------
!       BANKname BANKtype      ! Comments
 TABLE  SCP               ! create write display delete !Sc Scintillator panel geometry bank 
!
!   ATTributes:
!   -----------
!COL ATT-name FMT Min    Max   ! Comments
1    N1x        F   -1.0   +1.0      ! x component of outward normal to panel 1
2    N1y        F   -1.0   +1.0      ! y component of outward normal to panel 1
3    N1z        F   -1.0   +1.0      ! z component of outward normal to panel 1
4    R1n        F   0.0  9999.9      ! min.dist.inner(cm)from origin to panel 1
5    N2x        F   -1.0   +1.0      ! x component of outward normal to panel 2
6    N2y        F   -1.0   +1.0      ! y component of outward normal to panel 2
7    N2z        F   -1.0   +1.0      ! z component of outward normal to panel 2
8    R2n        F   0.0  9999.9      ! min.dist.inner(cm)from origin to panel 2
9    N3x        F   -1.0   +1.0      ! x component of outward normal to panel 3
10   N3y        F   -1.0   +1.0      ! y component of outward normal to panel 3
11   N3z        F   -1.0   +1.0      ! z component of outward normal to panel 3
12   R3n        F   0.0  9999.9      ! min.dist.inner(cm)from origin to panel 3
13   N4x        F   -1.0   +1.0      ! x component of outward normal to panel 4
14   N4y        F   -1.0   +1.0      ! y component of outward normal to panel 4
15   N4z        F   -1.0   +1.0      ! z component of outward normal to panel 4
16   R4n        F   0.0  9999.9      ! min.dist.inner(cm)from origin to panel 4
!
!    RELations:
!    ----------
!COL RELname  RELtype INTbank  ! Comments
!                     (COL)
!
 END TABLE
!
