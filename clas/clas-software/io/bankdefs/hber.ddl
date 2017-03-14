!********************************************************************
!       BANKname BANKtype      ! Comments
 TABLE  HBER  ! create write display delete ! Hit Based Tracking ERror bank
!  record_no=0
!
!  Fit parameter and Covariance matrix: (Cij)
!
!  Track# = row#  (cf. HBTR bank)
!  note these are in the sda tracking coordinate system 
!           (x=beamline, y=radially outward, z=parallel to axial wires)
!   ATTributes:
!   -----------
!COL ATT-name FMT    Min     Max   ! Comments
!
  1  q_over_p   F      0.    100.   ! q/p
  2  lambda     F    -10.     10.   ! dip angle (pi/2 - theta)
  3  phi        F    -60.     60.   ! phi
  4  d0         F   -100.    100.   ! min.distance from (x=0,y=0,z=?)   [cm]
  5  z0         F   -100.    100.   ! z position of starting point  [cm]
  6  c11        F    -10.     10.   ! element C{1,1}
  7  c12        F    -10.     10.   ! element C{1,2}
  8  c13        F    -10.     10.   ! element C{1,3}
  9  c14        F    -10.     10.   ! element C{1,4}
 10  c15        F    -10.     10.   ! element C{1,5}
 11  c22        F    -10.     10.   ! element C{2,2}
 12  c23        F    -10.     10.   ! element C{2,3}
 13  c24        F    -10.     10.   ! element C{2,4}
 14  c25        F    -10.     10.   ! element C{2,5}
 15  c33        F    -10.     10.   ! element C{3,3}
 16  c34        F    -10.     10.   ! element C{3,4}
 17  c35        F    -10.     10.   ! element C{3,5}
 18  c44        F    -10.     10.   ! element C{4,4}
 19  c45        F    -10.     10.   ! element C{4,5}
 20  c55        F    -10.     10.   ! element C{5,5}
 21  chi2       F      0.     50.   ! Chisquare for this Track
 22  layinfo1   I      0.      0.   ! layerhit info
 23  layinfo2   I      0.      0.   ! layerhit info&sector&track#in sector
! the layer hit info is stored in the following way
! for layinfo1= sum over each layer used in track(layers 1-30) Of 2^(layer#-1)
! for layinfo2 = sum of 2^(layer#-31) for (layers 31-36)
!	 	+ 256 * track# in sector + 256^3 * sector
!RELations:
!    ----------
!COL RELname  RELtype INTbank  ! Comments
!                     (COL)
!  
!
 END TABLE


