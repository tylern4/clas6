      program spp_int

      implicit none

      real ebeam
c
      integer  int_ctrl, theory_opt, channel_opt, resonance_opt
c
      int_ctrl = 0
      theory_opt = 0
      channel_opt = 0
      resonance_opt = 0
c
 1000 write(6,*)' input 1 for A0'
      write(6,*)' input 2 for Latham (pi0 only)'
      write(6,*)' input 3 for Harry Lee (pi0 only)'
      write(6,*)' input 4 for DMT'
      write(6,*)' input 5 for maid98'
      write(6,*)' input 6 for maid2000'
      write(6,*)' input 7 for maid07'
      write(6,*)' input 8 for maid03'
c
      read(5,*) theory_opt
c
      if (theory_opt.lt.1 .or. theory_opt.gt.8) then
        write(5,*) 'try again'
        goto 1000
      endif
c
      if (theory_opt.eq.1) then
        write(6,*)' input 1 for pi0 + p channel'
        write(6,*)' input 3 for pi+ + n channel'
        read(5,*) channel_opt
      endif
c
      if (theory_opt.eq.3.or.theory_opt.eq.4) then
        write(6,*)' input 1 for pi0 + p channel'
        write(6,*)' input 3 for pi+ + n channel'
        read(5,*) channel_opt
c        channel_opt = 1
      endif

      if (theory_opt.gt.4.and.theory_opt.lt.9) then
        write(6,*)' input 1 for pi0 + p channel'
        write(6,*)' input 2 for pi- + p channel'
        write(6,*)' input 3 for pi+ + n channel'
        read(5,*) channel_opt
c
        write(6,*)' input  0 for all res + bg(born+rho+omega)'
        write(6,*)' input  1 for p33 resonance only'
        write(6,*)' input  2 for p11 resonance only'
        write(6,*)' input  3 for d13 resonance only'
        write(6,*)' input  4 for s11(1535) resonance only'
        write(6,*)' input  5 for s11(1635) resonance only'
        write(6,*)' input  6 for f15 resonance only'
        write(6,*)' input  7 for d33 resonance only'
        write(6,*)' input  8 for born  only'
        write(6,*)' input  9 for rho   only'
        write(6,*)' input 10 for omega only'
        write(6,*)'                        '
        write(6,*)' input 11 for all without p11'
        write(6,*)' input 12 for all without d13'
        write(6,*)' input 13 for all without s11(1535)'
        write(6,*)' input 14 for bg(born+rho+omega) only'
        read(5,*) resonance_opt
      endif
c
      write(6,*)' input 0 for sigma(theta, phi)'
      write(6,*)' input 1 for sigma(theta) integrated over phi'
      write(6,*)' input 2 for sigma(phi) integrated over theta'
      write(6,*)' input 3 for sigma(W) integrated over theta and phi'
      write(6,*)' input 4 for sigma(theta,phi) at e1a kinematics'
      write(6,*)' input 5 for sigma(phi)integrated over a range'
      write(6,*)'    in theta'
      write(6,*)' input 6 for sigma(theta) integrated over a range'
      write(6,*)'   in phi '
      write(6,*)' input 7 for sigma(W,Q) integrated over theta and a'
      write(6,*)'   range in phi '
      write(6,*)' input 8 for sigma(phi)integrated over a range in'
      write(6,*)'   theta acceptance corrected'
      write(6,*)' input 9 for sigma(theta)integrated over a range'
      write(6,*)'   in phi acceptance corrected'
      write(6,*)' input 10 for sigma(W,Q) integrated over theta and'
      write(6,*)'   a range in phi acceptance corrected'
      write(6,*)' input 11 for sigma(phi)integrated over a range in'
      write(6,*)'   theta (phi bin int) acc corr'
      write(6,*)' input 12 for sigma(theta) integrated over a range'
      write(6,*)'   in phi (theta bin int) acc corr'
      read(5,*) int_ctrl

c
      write(6,*)' input electron beam energy'
      read(5,*) ebeam
c
      if (int_ctrl.lt.0 .or. int_ctrl.gt.12) int_ctrl=0

      open(unit=21,file='spp_int.out',status='unknown')
      open(unit=22,file='spp_int_pol.out',status='unknown')

      if (int_ctrl.eq.0) then
        call int_ctrl_0(ebeam,theory_opt,channel_opt,resonance_opt)
      elseif (int_ctrl.eq.1) then
        call int_ctrl_1(ebeam,theory_opt,channel_opt,resonance_opt)
      elseif (int_ctrl.eq.2) then
        call int_ctrl_2(ebeam,theory_opt,channel_opt,resonance_opt)
      elseif (int_ctrl.eq.3) then
        call int_ctrl_3(ebeam,theory_opt,channel_opt,resonance_opt)
      elseif (int_ctrl.eq.4) then
        call int_ctrl_4(ebeam,theory_opt,channel_opt,resonance_opt)
      elseif (int_ctrl.eq.5) then
        call int_ctrl_5(ebeam,theory_opt,channel_opt,resonance_opt)
      elseif (int_ctrl.eq.6) then
        call int_ctrl_6(ebeam,theory_opt,channel_opt,resonance_opt)
      elseif (int_ctrl.eq.7) then
        call int_ctrl_7(ebeam,theory_opt,channel_opt,resonance_opt)
      elseif (int_ctrl.eq.8) then
        call int_ctrl_8(ebeam,theory_opt,channel_opt,resonance_opt)
      elseif (int_ctrl.eq.9) then
        call int_ctrl_9(ebeam,theory_opt,channel_opt,resonance_opt)
      elseif (int_ctrl.eq.10) then
        call int_ctrl_10(ebeam,theory_opt,channel_opt,resonance_opt)
      elseif (int_ctrl.eq.11) then
        call int_ctrl_11(ebeam,theory_opt,channel_opt,resonance_opt)
      elseif (int_ctrl.eq.12) then
        call int_ctrl_12(ebeam,theory_opt,channel_opt,resonance_opt)
      endif

      close(21)
      close(22)

      end
