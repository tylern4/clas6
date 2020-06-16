!-----------------------------------------------------------------------
!       BANKname BANKtype      ! Comments
 TABLE  ECHB     ! create write display delete ! Forward calorimeter result bank
!
!   ATTributes:
!   -----------
!COL ATT-name FMT Min    Max   ! Comments
!
  1  Sect      I    0   0xFFFF  ! Sector number & Layer number
  2  E__hit    F   0.0     6.0  ! energy found 
  3  dE_hit    F   0.0     6.0  ! error on the energy found 
  4  t_hit     F   0.0  9999.0  ! time found 
  5  dt_hi     F   0.0  9999.0  ! error time found 
  6  i_hit     F   0.0  9999.0  ! sector rectangular coordinate 
  7  j_hit     F   0.0  9999.0  ! sector rectangular coordinate 
  8  di_hit    F   0.0  9999.0  ! sector rectangular coordinate error,
  9  dj_hit    F   0.0  9999.0  ! sector rectangular coordinate error,
 10  x_hit     F   0.0  9999.0  ! lab coordinate,
 11  y_hit     F   0.0  9999.0  ! lab coordinate,
 12  z_hit     F   0.0  9999.0  ! lab coordinate,
 13  dx_hit    F   0.0  9999.0  ! lab coordinate error,
 14  dy_hit    F   0.0  9999.0  ! lab coordinate error, 
 15  dz_hit    F   0.0  9999.0  ! lab coordinate error,
 16  u2_hit    F   0.0  9999.0  ! second moment of u  _hit pattern
 17  v2_hit    F   0.0  9999.0  ! second moment of v  _hit pattern
 18  w2_hit    F   0.0  9999.0  ! second moment of w  _hit pattern
 19  u3_hit    F -9999.0  9999.0  ! third moment of u  _hit pattern 
 20  v3_hit    F -9999.0  9999.0  ! third moment of v  _hit pattern 
 21  w3_hit    F -9999.0  9999.0  ! third moment of w  _hit pattern 
 22  u4_hit    F -9999.0  9999.0  ! forth moment of u  _hit pattern 
 23  v4_hit    F -9999.0  9999.0  ! forth moment of v  _hit pattern 
 24  w4_hit    F -9999.0  9999.0  ! forth moment of w  _hit pattern 
 25  centr_U   F     0.0  9999.0  ! peak position on U axis 
 26  centr_V   F     0.0  9999.0  ! peak position on V axis 
 27  centr_W   F     0.0  9999.0  ! peak position on W axis 
 28  path_U    F     0.0  9999.0  ! path length from hit position to U axis 
 29  path_V    F     0.0  9999.0  ! path length from hit position to V axis 
 30  path_W    F     0.0  9999.0  ! path length from hit position to W axis 
 31  Nstrp_U   I     0      36  ! Number of U strips in the hit
 32  Nstrp_V   I     0      36  ! Number of V strips in the hit
 33  Nstrp_W   I     0      36  ! Number of W strips in the hit
 34  MatchID1  I     0      30  ! Id of matched hit in the layer1
 35  CH21      F     0.    999. ! Quality measure of matching with layer1
 36  MatchID2  I     0      30  ! Id of matched hit in the layer2
 37  CH22      F     0.    999. ! Quality measure of matching with layer2
 38  istat     I    0    0xFFFF ! Number of hits & hit ID
!!
! For matching if current layer is WHOLE then layer1=INNER and layer2=OUTER
!              if current layer is INNER then layer1=WHOLE and layer2=OUTER
!              if current layer is OUTER then layer1=WHOLE and layer2=INNER
 END TABLE
!
