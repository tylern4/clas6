
c 
      REAL          tvsst,tag_tof_corr,coin_gate_ST_TAG
     &             ,tdiff,tdiff_test
      common /tvsst/tvsst,tag_tof_corr,coin_gate_ST_TAG
     &             ,tdiff,tdiff_test

      logical               flag_false_g,flag_good_ph
      common  /flag_false_g/flag_false_g,flag_good_ph

c TOF recalibration
       integer n_pi
       real    sl_i, t0_i
       common /tofCalib/sl_i(7,48), t0_i(7,48), n_pi(7,48)

      real*4  pnt,t01    
      data pnt,t01/0.5,-32.08/
