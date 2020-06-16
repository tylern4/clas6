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
  4  Tclmax     F -1000.0 1000.0  ! Time in a crystal with Max enrgy
  5  xclust     F -1000.0 1000.0  ! lab coordinate X,
  6  yclust     F -1000.0 1000.0  ! lab coordinate Y,
  7  zclust     F -1000.0 1000.0  ! lab coordinate Z,
  8  xclmax     F -1000.0 1000.0  ! lab coordinate X,
  9  yclmax     F -1000.0 1000.0  ! lab coordinate Y,
 10  zclmax     F -1000.0 1000.0  ! lab coordinate Z,
 11  res1       F -1000.0 1000.0  ! lab coordinate error,
 12  res2       F -1000.0 1000.0  ! lab coordinate error,
 13  res3       F -1000.0 1000.0  ! lab coordinate error,
 14  ncryst     I  0    1000      ! Number of crystal in the cluster
!
!
 END TABLE
!
