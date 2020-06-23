!-----------------------------------------------------------------------
!       BANKname BANKtype      ! Comments
 TABLE  ECRB     ! create write display delete ! Forward calorimeter result bank
!
!   ATTributes:
!   -----------
!COL ATT-name FMT Min    Max   ! Comments
!
  1  E_in     F   0.0     6.0  ! energy found for the inner layer
  2  E_out    F   0.0     6.0  ! energy found for the outer layer
  3  dE_in    F   0.0     6.0  ! error on the energy found for the inner layer
  4  dE_out   F   0.0     6.0  ! error on the energy found for the outer layer
  5  t_in     F   0.0  9999.0  ! time found for the inner layer
  6  t_out    F   0.0  9999.0  ! time found for the outer layer
  7  dt_in    F   0.0  9999.0  ! error on the time found for the inner layer
  8  dt_out   F   0.0  9999.0  ! error on the time found for the outer layer
  9  i_in     F   0.0  9999.0  ! sector rectangular coordinate for the inner layer
 10  j_in     F   0.0  9999.0  ! sector rectangular coordinate for the inner layer
 11  i_out    F   0.0  9999.0  ! sector rectangular coordinate for the outer layer
 12  j_out    F   0.0  9999.0  ! sector rectangular coordinate for the outer layer
 13  di_in    F   0.0  9999.0  ! sector rectangular coordinate error, inner layer
 14  dj_in    F   0.0  9999.0  ! sector rectangular coordinate error, inner layer
 15  di_out   F   0.0  9999.0  ! sector rectangular coordinate error, outer layer
 16  dj_out   F   0.0  9999.0  ! sector rectangular coordinate error, outer layer
 17  x_in     F   0.0  9999.0  ! lab coordinate, inner layer
 18  y_in     F   0.0  9999.0  ! lab coordinate, inner layer
 19  z_in     F   0.0  9999.0  ! lab coordinate, inner layer
 20  x_out    F   0.0  9999.0  ! lab coordinate, outer layer
 21  y_out    F   0.0  9999.0  ! lab coordinate, outer layer
 22  z_out    F   0.0  9999.0  ! lab coordinate, outer layer
 23  dx_in    F   0.0  9999.0  ! lab coordinate error, inner layer
 24  dy_in    F   0.0  9999.0  ! lab coordinate error, inner layer
 25  dz_in    F   0.0  9999.0  ! lab coordinate error, inner layer
 26  dx_out   F   0.0  9999.0  ! lab coordinate error, outer layer
 27  dy_out   F   0.0  9999.0  ! lab coordinate error, outer layer
 28  dz_out   F   0.0  9999.0  ! lab coordinate error, outer layer
 29  u2_in    F   0.0  9999.0  ! second moment of u inner hit pattern
 30  v2_in    F   0.0  9999.0  ! second moment of v inner hit pattern
 31  w2_in    F   0.0  9999.0  ! second moment of w inner hit pattern
 32  u2_out   F   0.0  9999.0  ! second moment of u outer hit pattern
 33  v2_out   F   0.0  9999.0  ! second moment of v outer hit pattern
 34  w2_out   F   0.0  9999.0  ! second moment of w outer hit pattern
 35  u3_in    F   0.0  9999.0  ! third moment of u inner hit pattern 
 36  v3_in    F   0.0  9999.0  ! third moment of v inner hit pattern 
 37  w3_in    F   0.0  9999.0  ! third moment of w inner hit pattern 
 38  u3_out   F   0.0  9999.0  ! third moment of u outer hit pattern 
 39  v3_out   F   0.0  9999.0  ! third moment of v outer hit pattern 
 40  w3_out   F   0.0  9999.0  ! third moment of w outer hit pattern 
 41  i2       F   0.0  9999.0  ! second moment of overall shower, sector coordinates
 42  j2       F   0.0  9999.0  ! second moment of overall shower, sector coordinates
 43  i3       F   0.0  9999.0  ! third moment of overall shower, sector coordinates
 44  j3       F   0.0  9999.0  ! third moment of overall shower, sector coordinates
 45  spare1   F  -9999. 99999. ! spare
 46  spare2   F  -9999. 99999. ! spare
 47  spare3   F  -9999. 99999. ! spare
 48  spare4   F  -9999. 99999. ! spare
 49  istat    I    0    0xFFFF ! status word
!!
 END TABLE
!
