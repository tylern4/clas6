!       BANKname BANKtype      ! Comments
 TABLE  EC1R     ! create write display delete ! LA calorimeter result bank
!
!   ATTributes:
!   -----------
!COL ATT-name FMT Min    Max   ! Comments
!
  1  E_tot    F   0.0     6.0  ! cluster energy (sum of inner+outer)
  2  dE_tot   F   0.0     6.0  !  error on the cluster energy
  3  t_tot    F   0.0  9999.0  ! time
  4  dt_tot   F   0.0  9999.0  ! error on the time
  5  x_m      F -999.0  9999.0 ! x-in CLAS frame
  6  y_m      F -999.0  9999.0 ! y-in CLAS frame
  7  z_m      F -999.0  9999.0 ! z-in CLAS frame
  8  dx_m     F   0.0  9999.0  ! error on x
  9  dy_m     F   0.0  9999.0  ! error on y
 10  dz_m     F   0.0  9999.0  ! error on z
 11  E_in     F   0.0     6.0  !  cluster energy in inner layer
 12  t_in     F -999.0 9999.0  ! time from inner layer
 13  x_in     F -999.0 9999.0  ! lab coordinate , inner layer
 14  y_in     F -999.0 9999.0  ! lab coordinate , inner layer
 15  x_out    F -999.0 9999.0  ! lab coordinate , outer layer
 16  y_out    F -999.0 9999.0  ! lab coordinate , outer layer
 17  x2_in_l  F  -0.5   0.5    ! second moment of x inner left
 18  x2_in_r  F  -0.5   0.5    ! second moment of x inner right 
 19  y2_in_l  F  -0.5   0.5    ! second moment of y inner left
 20  y2_in_r  F  -0.5   0.5    ! second moment of y inner right
 21  x2_out_l F  -0.5   0.5    ! second moment of x outer hit left
 22  x2_out_r F  -0.5   0.5    ! second moment of x outer hit right
 23  y2_out_l F  -0.5   0.5    ! second moment of y outer hit left
 24  y2_out_r F  -0.5   0.5    ! second moment of y outer hit right
 25  i_in     I   1    40      ! cluster center in X inner short layer
 26  j_in     I   1    24      ! cluster center in Y inner long layer
 27  i_out    I   1    40      ! cluster center in X outer layers
 28  j_out    I   1    24      ! cluster center in Y outer layer
 29  a_in_xl  F   0.0   6.0    ! energy sum in inner short left pmts
 30  a_in_xr  F   0.0   6.0    ! energy sum in inner short right pmts
 31  a_in_yl  F   0.0   6.0    ! energy sum in inner long  left pmts
 32  a_in_yr  F   0.0   6.0    ! energy sum in inner long  right pmts
 33  a_out_xl F   0.0   6.0    ! energy sum in outer short left pmts
 34  a_out_xr F   0.0   6.0    ! energy sum in outer short right pmts
 35  a_out_yl F   0.0   6.0    ! energy sum in outer long left pmts
 36  a_out_yr F   0.0   6.0    ! energy sum in outer long right pmts
 37  t_in_xs  F   0.0   9999.0 ! tdc sum in inner short righ+left  for cluster center
 38  t_in_xd  F -9999.0 9999.0 ! tdc dif in inner short right-left 
 39  t_in_ys  F   0.0   9999.0 ! tdc sum in inner long  righ+left
 40  t_in_yd  F -9999.0 9999.0 ! tdc dif in inner long  right-left
 41  t_out_xs F   0.0   9999.0 ! tdc sum in outer short righ+left 
 42  t_out_xd F -9999.0 9999.0 ! tdc dif in outer short right-left 
 43  t_out_ys F   0.0   9999.0 ! tdc sum in outer long righ+left
 44  t_out_yd F -9999.0 9999.0 ! tdc dif in outer long right-left 
 45  ibl      I    1    2      ! LAC  block number
 46  ncluster I    1    10000  ! 1000xNclust4+100xNclust3+10xNclust2+Nclust1 
 47  pmtfired I    0    512    ! Number of fired pmt (more than threshold) 
 48  z_in     F   0.0  99999.  ! Z in CLAS frame
 49  z_out    F   0.0  99999.  ! Z out in CLAS frame
 50  istat    I    0    0xFFFF ! status word
!!
 END TABLE
!
