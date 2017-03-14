      program spp_int

      implicit none
      
      real ebeam
      
      integer  int_ctrl, theory_opt, channel_opt, resonance_opt

      int_ctrl      = 0
      theory_opt    = 0
      channel_opt   = 0
      resonance_opt = 0

 1000 write(6,*)' Enter 1 for Sato-Lee'
      write(6,*)' Enter 2 for MAID98'
      write(6,*)' Enter 3 for MAID2003'
      write(6,*)' Enter 4 for MAID2007'
      write(6,*)' Enter 5 for DMT'
      write(6,*)' Enter 6 for ETAMAID'
      write(6,*)' Enter 7 for other tables'

      read(5,*) theory_opt

      if (theory_opt.lt.1 .or. theory_opt.gt.9) then
        write(5,*) 'try again'
        goto 1000
      endif

      write(6,*)' Enter 1 for pi0 + p channel'
      write(6,*)' Enter 2 for pi- + p channel'
      write(6,*)' Enter 3 for pi+ + n channel'
      read(5,*) channel_opt

      if (theory_opt.ge.2) then
        write(6,*)' Enter  0 for all res + bg(born+rho+omega)'
        write(6,*)' Enter  1 for p33 resonance only'
        write(6,*)' Enter  2 for p11 resonance only'
        write(6,*)' Enter  3 for d13 resonance only'
        write(6,*)' Enter  4 for s11(1535) resonance only'
        write(6,*)' Enter  5 for s11(1635) resonance only'
        write(6,*)' Enter  6 for f15 resonance only'
        write(6,*)' Enter  7 for d33 resonance only'
        write(6,*)' Enter  8 for born  only'
        write(6,*)' Enter  9 for rho   only'
        write(6,*)' Enter 10 for omega only'
        write(6,*)'                        '
        write(6,*)' Enter 11 for all without p11'
        write(6,*)' Enter 12 for all without d13'
        write(6,*)' Enter 13 for all without s11(1535)'
        write(6,*)' Enter 14 for bg(born+rho+omega) only'
        read(5,*) resonance_opt
      endif
      
      if (theory_opt.eq.6) channel_opt=4
     
      write(6,*)' Enter 0 for sigma(theta, phi)'
      write(6,*)' Enter 1 for sigma(theta) integrated over phi'
      write(6,*)' Enter 2 for sigma(phi) integrated over theta'
      write(6,*)' Enter 3 for sigma(W) integrated over theta and phi'
      write(6,*)' Enter 4 for sigma(theta,phi) at nominal kinematics'
      write(6,*)' Enter 5 for sigma_binavg(theta,phi) at nominal kinematics'
      write(6,*)' Enter 6 for sigma(theta,phi) at external file kinematics'
      write(6,*)' Enter 7 to create multipole table from SAID or JANR'
      read(5,*) int_ctrl

      if (int_ctrl.eq.7) call epr
       
      write(6,*)' Enter electron beam energy (GeV)'
      read(5,*) ebeam

      if (int_ctrl.lt.0 .or. int_ctrl.gt.9) int_ctrl=0
      
      open(unit=21,file='spp_crs.out',type='unknown')

      if (int_ctrl.eq.0) then
        call int_ctrl_0(ebeam,theory_opt,channel_opt,resonance_opt)
      elseif (int_ctrl.eq.1) then
        call int_ctrl_1(ebeam,theory_opt,channel_opt,resonance_opt)
      elseif (int_ctrl.eq.2) then
        call int_ctrl_2(ebeam,theory_opt,channel_opt,resonance_opt)
      elseif (int_ctrl.eq.3) then
        call int_ctrl_3(ebeam,theory_opt,channel_opt,resonance_opt)
      elseif (int_ctrl.eq.4.or.int_ctrl.eq.6.or.int_ctrl.eq.9) then
        call int_ctrl_4(ebeam,theory_opt,channel_opt,resonance_opt,int_ctrl)
      elseif (int_ctrl.eq.5) then
        call int_ctrl_5(ebeam,theory_opt,channel_opt,resonance_opt)
      endif
      
      close(21)

      end
