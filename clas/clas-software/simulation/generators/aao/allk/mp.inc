      integer mwave_l,max_l
      parameter(max_l=5)
      parameter(mwave_l=5)
      
      complex sp(0:max_l+1), sm(0:max_l+1)
      complex ep(0:max_l+1), em(0:max_l+1)
      complex mp(0:max_L+1), mm(0:max_L+1)
      
      common /mp_amplitudes/sp, sm, ep, em, mp, mm

      complex hh1,hh2,hh3,hh4,hh5,hh6 ! helicity amplitudes
      complex ff1,ff2,ff3,ff4,ff5,ff6 ! CGLN amplitudes

c      common /hel_amplitudes/hh1,hh2,hh3,hh4,hh5,hh6
c      common /cgln_amplitudes/ff1,ff2,ff3,ff4,ff5,ff6
      
      real sig_t,sig_l,sig_tt,sig_lt,sig_ltp
      real sig_t32,sig_t12,sigt_t_y
      real sigt_tt_x,sigt_tt_y,sigt_tt_z
      real rsp(5),sigma_u,sigma_p,fkt,robs(15,2)
      common /reponse_fcns/sig_t,sig_l,sig_tt,sig_lt,sig_ltp,
     1sig_t32,sig_t12,sigt_t_y,sigt_tt_x,sigt_tt_y,sigt_tt_z,
     1rsp,sigma_u,sigma_p,fkt,robs
      
      real pol(0:max_L+2,2)
c      common /legend/ pol
