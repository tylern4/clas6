      subroutine int_ctrl_0(ebeam,theory_opt,channel_opt,resonance_opt)

      implicit none

      include 'spp.inc'    

      integer theory_opt, channel_opt, resonance_opt
      integer iW, iQ2, iW_bin, W_bin_n
      integer csthcm_bin_n, phicm_bin_n 
      real W_calc, Q2_calc
      real W_min, W_max
      real W_bin_max, W_bin_min, W_bin, W_bin_step
      real Q2_bin_max, Q2_bin_min, Q2_bin, Q2_bin_step
      real sigma0, sigma0_total , sigma0_calc, ebeam
      real sig00,sig0e,sig0t,sig0et
      real sig00_total, sig0e_total,sig0t_total,sig0et_total
      integer Q2_n,W_n,csthcm_n,phicm_n
      data Q2_n,W_n,csthcm_n,phicm_n /10,10,40,36/

      write(6,*)' input lower and upper bounds on Q2 bin(GeV)'
      read(5,*)Q2_bin_min,Q2_bin_max

      Q2_bin = Q2_bin_max - Q2_bin_min
      Q2_bin_step  = Q2_bin / Q2_n
      Q2_calc = (Q2_bin_max + Q2_bin_min) / 2

      write(6,*)' Input W bin size of calculation (GeV)'
      read(5,*) W_bin 
      W_bin_step =  W_bin / W_n
      write(6,*)' Input starting and ending values of W (GeV)'
      read(5,*)W_min, W_max
      W_bin_n  = (W_max - W_min) / W_bin

      write(6,*)' input cos(theta_cm) and phi'
      read(5,*) csthcm, phicm
      write(6,*) ' '
      if (csthcm.lt.-1. .or. csthcm .gt. 1. .or.
     1 phicm .lt. -180. .or. phicm .gt. 180.) return
      do iW_bin = 1, W_bin_n+1   ! for each W bin
        W_bin_min = W_min + W_bin*(iW_bin-1)
        W_bin_max = W_bin_min + W_bin
        W_calc = (W_bin_min + W_bin_max) / 2.0
        sigma0_total = 0.0
        sig00_total = 0.0
        sig0e_total = 0.0
        sig0t_total = 0.0
        sig0et_total = 0.0
        write(6,*) ' '
        write(21,10) 'Q2 (GeV)= ',Q2_calc,'W (GeV)= ',W_calc 
        write(22,10) 'Q2 (GeV)= ',Q2_calc,'W (GeV)= ',W_calc 
 10     format(1X, A10, f5.3, 2X, A9, f5.3)
        write(21,20) 'csthcm ','phicm (deg)','Sigma(micro-barn)'
        write(22,20) 'csthcm ','phicm (deg)','Sigma0','Sigmae',
     1            'Sigmat','Sigmaet'
 20     format(1X, A7, 4X, A11, 4X,A17)
 25     format(1X, A7, 4X, A11, 5X, A9, 2X, A9, 2X, A9, 2X, A9)
        do iW = 1, W_n+1           ! integrating over W bin
          W = W_bin_min + W_bin_step*(iW-1)
          do iQ2 = 1, Q2_n+1       ! integrating over Q2 bin
            Q2 = Q2_bin_min + Q2_bin_step*(iQ2-1)
            sigma0=sigma0_calc(ebeam,
     1             theory_opt,channel_opt,resonance_opt,
     1             sig00,sig0e,sig0t,sig0et)
            if (iQ2.eq.1.or.iQ2.eq.Q2_n+1) then
             sigma0=0.5*sigma0
             sig00=0.5*sig00
             sig0e=0.5*sig0e
             sig0t=0.5*sig0t
             sig0et=0.5*sig0et
            endif
            if (iW.eq.1.or.iW.eq.W_n+1) then
             sigma0=0.5*sigma0
             sig00=0.5*sig00
             sig0e=0.5*sig0e
             sig0t=0.5*sig0t
             sig0et=0.5*sig0et
            endif
            sigma0_total = sigma0_total + sigma0
            sig00_total = sig00_total + sig00
            sig0e_total = sig0e_total + sig0e
            sig0t_total = sig0t_total + sig0t
            sig0et_total = sig0et_total + sig0et
          enddo  ! integrating over Q2 bin
        enddo    ! integrating over W bin
        sigma0_total = sigma0_total / W_n / Q2_n
        sig00_total = sig00_total / W_n / Q2_n
        sig0e_total = sig0e_total / W_n / Q2_n
        sig0t_total = sig0t_total / W_n / Q2_n
        sig0et_total = sig0et_total / W_n / Q2_n

        write(21,30) csthcm, phicm, sigma0_total
        write(22,40) csthcm, phicm, sig00_total,sig0e_total,
     1                        sig0t_total,sig0et_total
 30     format(f5.2,5X,f6.2,10X,f15.6)
 40     format(f5.2,5X,f6.2,10X,f15.6,f15.6,f15.6,f15.6)
      enddo

      return

      end
