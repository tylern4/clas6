c  ic1geom.inc

      real icvol(3)
      real icvol4(3),icvol1w(5),icvol1c(5),icvol1r(5)
      real icsh1(5),icsh2(5),icsh3(3),icsh4(3),icshf(3),icshi(3)
      real icvol1w_d(5),icvol1c_d(5)
      real icvol1w_m(5),icvol1c_m(5)
      real z0p
 
C     === Hyon-Suk Jo - April 4, 2011 === Increased the outside radius of the mother volume of IC from 22cm to 35cm
C     because of the additional IC volumes newly implemented.
      data icvol /0.0, 35.0, 230.0/	! TUBE Mother volume for IC
      data icvol1w /0.6715,0.8065,0.6715,0.8065,8.0/ ! TRD2 volume for CRWR
      data icvol1w_d /0.674,0.8075,0.674,0.8075,8.0/ ! TRD2 volume for CRWR - values from design
      data icvol1w_m /0.66985,0.80535,0.66985,0.80535,8.0/ ! TRD2 volume for CRWR - average values from measurements
      data icvol1c /0.0,0.0,0.0,0.0,8.0/ ! TRD2 volume for IC1
      data icvol1c_m /0.0,0.0,0.0,0.0,8.0/ ! TRD2 volume for IC1
      data icvol1r /5.85,8.25,0.5,0.5,12.85/ ! TRD2 volume for brass plate under the IC
    
      data icsh1 /15.0, 14.5, 19.5, 5.9, 10.9/  ! CONE volume for shielding behind the calorimeter
      data icshf /19.5, 19.9, 8.4/  ! TUBE volume for shielding around the electronics
      data icsh2 /8.72, 3.96, 10.9, 3.96, 5.9/  ! CONE volume for shielding behind the calorimeter
      data icsh3 /3.96, 5.9, 9.03/  ! TUBE volume for shielding behind the calorimeter
      data icshi /1.8, 3.96, 25.25/  ! TUBE volume for shielding behind the calorimeter
      data icsh4 /3.96, 6.35, 19.165/  ! TUBE volume for shielding behind the calorimeter
