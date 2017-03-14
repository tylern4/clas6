!-----------------------------------------------------------------------
!       BANKname BANKtype      ! Comments
 TABLE  SCMD  ! create write display delete !Sc reconstruction bank for DC M.K.
!
!   ATTributes:
!   -----------
!COL ATT-name  FMT  Min      Max        ! Comments
1   ID          I    0       0xFFFF     ! paddle # 
2   STATUS      I    0       0xFFFF     ! general status information 
3   X_NORM      F  -999999.9 999999.9   ! X of unit vector normal to paddle
4   Y_NORM      F  -999999.9 999999.9   ! Y of unit vector normal to paddle
5   D_NORM      F    0.      999999.9   ! normal distance to paddle plane
6   X_PANEL     F  -999999.9 999999.9   ! X of unit vector along the panel
7   Y_PANEL     F  -999999.9 999999.9   ! Y of unit vector along the panel
8   B_PAD       F  -999999.9 999999.9   ! Begin of paddle along the panel
9   E_PAD       F  -999999.9 999999.9   ! End of paddle along the panel
10  TOF         F  -999999.9 999999.9   ! time of flight (nS)
11  EDP         F    0.      999999.9   ! energy deposited (GeV) 
12  P_TIME      F  -999999.9 999999.9   ! position i.r.t. mid.plane from time
13  P_ATLN      F  -999999.9 999999.9   ! position i.r.t. mid.plane from atten.
!
!    RELations:
!    ----------
!COL RELname  RELtype INTbank  ! Comments
!                     (COL)
!
 END TABLE
!
