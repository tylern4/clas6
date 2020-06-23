!-----------------------------------------------------------------------
!       BANKname BANKtype      ! Comments
 TABLE  SCGD  ! create write display delete !SC Geometry of paddles for DC (in SS) - M.K.
!
!   ATTributes:
!   -----------
!COL ATT-name  FMT  Min      Max        ! Comments
1   ID          I    0       0xFFFF     ! paddle # inside the plane (1-23(max))
2   NORM_Z      F  -999999.9 999999.9   ! Z(X') coordinate of unit normal to plane
3   NORM_X      F  -999999.9 999999.9   ! Z(X') coordinate of unit normal to plane
4   NORM_D      F  -999999.9 999999.9   ! distance to plane along unit vector
5   ALON_Z      F  -999999.9 999999.9   ! half width in X direction (along id's)
6   ALON_X      F  -999999.9 999999.9   ! half length of SC in Y direction
7   BEG_PD      F  -999999.9 999999.9   ! begin point of the paddle along id
8   END_PD      F  -999999.9 999999.9   ! end point of the paddle along id
9   PDL_SH      F  -999999.9 999999.9   ! shift of the paddle in respect to the Mid Plane
10  R_BEAM      F  -999999.9 999999.9   ! distance from the beam to the counte center in M.P.
!
!    RELations:
!    ----------
!COL RELname  RELtype INTbank  ! Comments
!                     (COL)
!
 END TABLE
!
