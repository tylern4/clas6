!-----------------------------------------------------------------------
!       BANKname BANKtype      ! Comments
 TABLE  ICHB     ! create write display delete ! Inner crystal calorimeter reconstruction bank
!
!   ATTributes:
!   -----------
!COL ATT-name FMT Min    Max   ! Comments
!
  1  Eclust     F   0.0     20.0  ! Cluster energy 
  2  Eclmax     F   0.0     20.0  ! Max energy in a crystal of the cluster 
  3  Tclust     F -1000.0 1000.0  ! Reconstructed cluster time
  4  T_next     F -1000.0 1000.0  ! Time to next closest hit
  5  xclust     F -1000.0 1000.0  ! lab coordinate X,
  6  yclust     F -1000.0 1000.0  ! lab coordinate Y,
  7  zclust     F -1000.0 1000.0  ! lab coordinate Z,
  8  xclmax     F -1000.0 1000.0  ! lab coordinate X,
  9  yclmax     F -1000.0 1000.0  ! lab coordinate Y,
 10  M2_x       F -1000.0 1000.0  ! second moment of X_hit pattern
 11  M2_y       F -1000.0 1000.0  ! second moment of Y_hit pattern
 12  M3_x       F -1000.0 1000.0  ! Third moment of X_hit pattern
 13  M3_y       F -1000.0 1000.0  ! Third moment of Y_hit pattern
 14  ncryst     I  0    1000      ! Number of crystal in the cluster
!
!
 END TABLE
!
