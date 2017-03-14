c     Multipole and Helicity Amplitudes

      integer wave_L		! max pi-N ang. mom. in table
      integer mwave_L		! max pi-N ang. mom. calculated
      integer max_L		! max hel wave summed
      integer method_helicity 	! =1, for Tiator's
	                      	! =2, for Burkert's
      parameter(max_L = 5)
      parameter(wave_L = 5)
	  
      complex sp(0:max_L+1), sm(0:max_L+1)
      complex ep(0:max_L+1), em(0:max_L+1)
      complex mp(0:max_L+1), mm(0:max_L+1)
      complex ap(0:max_L+1), am(0:max_L+1)
      complex bp(0:max_L+1), bm(0:max_L+1)
      complex cp(0:max_L+1), cm(0:max_L+1)

      common /mp_amplitudes/sp, sm, ep, em, mp, mm, 
     1                      ap, am, bp, bm, cp, cm
      common /method/method_helicity,mwave_L

      complex hh1,hh2,hh3,hh4,hh5,hh6 ! helicity amplitudes
      complex ff1,ff2,ff3,ff4,ff5,ff6 ! CGLN amplitudes

      common /hel_amplitudes/hh1,hh2,hh3,hh4,hh5,hh6
      common /cgln_amplitudes/ff1,ff2,ff3,ff4,ff5,ff6
      
      real pol(0:max_L+2,2)

      common /legend/ pol

      character*132 data_file

      common /input_file/data_file      

C*******************************************************************************
C For the response functions.
C*******************************************************************************
	  
      real sigma_l, sigma_t, sigma_tt, sigma_lt, sigma_ltp
      real sigma_t_y, sigma_l_y, sigma_lt_x, sigma_lt_y, sigma_lt_z
      real sigma_tt_x, sigma_tt_y, sigma_tt_z
      real sigma_ltp_x, sigma_ltp_y, sigma_ltp_z
      real sigma_ttp_x, sigma_ttp_z
	  	  
      common /dsigma_fcns/sigma_l, sigma_t,
     1                    sigma_tt, sigma_lt, sigma_ltp,
     1                    sigma_t_y, sigma_l_y,
     1                    sigma_lt_x, sigma_lt_y, sigma_lt_z,
     1                    sigma_tt_x, sigma_tt_y, sigma_tt_z,
     1                    sigma_ltp_x, sigma_ltp_y, sigma_ltp_z,
     1                    sigma_ttp_x, sigma_ttp_z
     
      real sig_l,sig_t,sig_tt,sig_lt,sig_ltp
      real sigt_t_y,sigt_l_y
      real sigt_lt_x,sigt_lt_y,sigt_lt_z
      real sigt_tt_x,sigt_tt_y,sigt_tt_z
      real sigt_ltp_x,sigt_ltp_y,sigt_ltp_z
      real sigt_ttp_x,sigt_ttp_z
      
      real sigr_lt_y,sigr_ltp_x,sigr_ttp_z
           
      common /response_fcns/sig_l,sig_t,sig_tt,sig_lt,sig_ltp,
     1sigt_t_y,sigt_l_y,sigt_lt_x,sigt_lt_y,sigt_lt_z,
     1sigt_tt_x,sigt_tt_y,sigt_tt_z,sigt_ltp_x,sigt_ltp_y,sigt_ltp_z,
     1sigt_ttp_x,sigt_ttp_z,sigr_lt_y,sigr_ltp_x,sigr_ttp_z

c for SatoLee:      NVAR1 =  77, NVAR2 = 26 ! Q2,W 
c for MAID:         NVAR1 =  81, NVAR2 = 91 ! Q2=4,W 
c for MAID:         NVAR1 = 101, NVAR2 = 92 ! Q2=5,W 
c NVAR1,NVAR2 should be larger or equal to the largest formatted
c table to be read in

      integer max_mp,nvar1,nvar2,nv1,nv2
      parameter (max_mp = 62, nvar1=101, nvar2=93)
      
      real var1(nvar1),var2(nvar2),var(nvar1+nvar2)

      integer method_spline,intopt

      common /var_maxmin/nv1,nv2,method_spline
     
      real sf(nvar1,nvar2,max_mp),d2sf(nvar1,nvar2,max_mp)
      real vec_f(max_mp)
      common /theory_mesh/var1,var2,var,sf,d2sf,vec_f,intopt

C Labeling Scheme for multipoles:
C
C                     SF1,2   = Real, Imag S0+
C                     SF3,4   = Real, Imag S1+
C                     SF5,6   = Real, Imag S2+
C                     SF7,8   = Real, Imag S3+
C                     SF9,10  = Real, Imag S4+
C                     SF11,12 = Real, Imag S5+
C
C                     SF13,14 = Real, Imag S1-
C                     SF15,16 = Real, Imag S2-
C                     SF17,18 = Real, Imag S3-
C                     SF19,20 = Real, Imag S4-
C                     SF21,22 = Real, Imag S5-
C
C                     SF23,24 = Real, Imag E0+
C                     SF25,26 = Real, Imag E1+
C                     SF27,28 = Real, Imag E2+
C                     SF29,30 = Real, Imag E3+
C                     SF31,32 = Real, Imag E4+
C                     SF33,34 = Real, Imag E5+
C
C                     SF35,36 = Real, Imag E2-
C                     SF37,38 = Real, Imag E3-
C                     SF39,40 = Real, Imag E4-
C                     SF41,42 = Real, Imag E5-
C
C                     SF43,44 = Real, Imag M1+
C                     SF45,46 = Real, Imag M2+
C                     SF47,48 = Real, Imag M3+
C                     SF49,50 = Real, Imag M4+
C                     SF51,52 = Real, Imag M5+
C
C                     SF53,54 = Real, Imag M1-
C                     SF55,56 = Real, Imag M2-
C                     SF57,58 = Real, Imag M3-
C                     SF59,60 = Real, Imag M4-
C                     SF61,62 = Real, Imag M5-


     




