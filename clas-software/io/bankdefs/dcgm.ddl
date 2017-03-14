!-----------------------------------------------------------------------
!       BANKname BANKtype      ! Comments
 TABLE  DCGM       ! create display delete ! DC geometry info by layer - in sector coordinate system
!
!   ATTributes:
!   -----------
!COL ATT-name   FMT   Min    Max             ! Comments
  1  x_curve     F  -1000.0  1000.0          ! x center of curvature (cm)
  2  y_curve     F  -1000.0  1000.0          ! y center of curvature (cm)
  3  z_curve     F  -1000.0  1000.0          ! z center of curvature (cm)
  4  r_curve     F       0.0  1000.0         ! radius of curvature (cm)
  5  theta_start F  -1000.0  1000.0          ! angle of first logical wire WRT the center of curvature (rad)
  6  d_theta     F  -1000.0  1000.0          ! delta theta between wires WRT center of curvature
  7  x_nmid      F  -1000.0  1000.0          ! x normal vector to the midplane
  8  y_nmid      F  -1000.0  1000.0          ! y normal vector to the midplane
  9  z_nmid      F  -1000.0  1000.0          ! z normal vector to the midplane
 10  theta_min   F  -1000.0  1000.0          ! theta of first physical wire (rad)
 11  theta_max   F  -1000.0  1000.0          ! theta of last physical wire (rad)
 12  min_wire    I    -1000  1000            ! minimum physical wire number
 13  max_wire    I    -1000  1000            ! maximum physical wire number
 14  stereo      F  -1000.0  1000.0          ! approximate stereo angle
 15  cell_size   F  -1000.0  1000.0          ! cell size (cm)
 16  x_norm      F  -1000.0  1000.0          ! x normal vector to the plane(region 1)
 17  y_norm      F  -1000.0  1000.0          ! y normal vector to the plane(region 1)
 18  z_norm      F  -1000.0  1000.0          ! z normal vector to the plane(region 1)
 19  p_dist      F  -1000.0  1000.0          ! distance of plane to origin(cm) (region 1)
 20  p_sep       F  -1000.0  1000.0          ! planar separation(cm) (region 1)
 21  max_cylw    I    -1000  1000            ! maximum cylindrical wire
!
!    RELations:
!    ----------
!COL RELname  RELtype INTbank  ! Comments
!                     (COL)
!
 END TABLE
!