      subroutine int_ctrl_1(ebeam,theory_opt,channel_opt,resonance_opt)

      implicit none

      include 'spp.inc'    
      
      integer iW, iQ2, iW_bin, W_bin_n
      integer theory_opt, channel_opt, resonance_opt
      integer icsthcm, iphicm
	  integer csthcm_bin_n, phicm_bin_n 
      real W_calc, Q2_calc, csthcm_calc
	  real csthcm_bin, phicm_bin
      real W_min, W_max
      real W_bin_max, W_bin_min, W_bin, W_bin_step
      real Q2_bin_max, Q2_bin_min, Q2_bin, Q2_bin_step
      real sigma0, sigma0_total , sigma0_calc, ebeam, asym
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
      write(6,*)' Input the bin size for cos(theta_cm)'
      read(5,*) csthcm_bin
      csthcm_bin_n = 2. / csthcm_bin

      do iW_bin = 1, W_bin_n+1   ! for each W bin
        W_bin_min = W_min + W_bin*(iW_bin-1)
        W_bin_max = W_bin_min + W_bin
        W_calc = (W_bin_min + W_bin_max) / 2.0
        write(6,*) ' '
        write(6,10) 'Q2 (GeV)= ',Q2_calc,'W (GeV)= ',W_calc 
 10     format(1X, A10, f5.3, A9, f5.3)
        write(6,20) 'csthcm ','Sigma(micro-barn)'
 20     format(1X, A7, 4X, A17)
        do icsthcm = 1, csthcm_bin_n+1 ! for each csthcm bin
          csthcm = -1. + csthcm_bin*(icsthcm-1)
	      sigma0_total = 0.0
          do iphicm = 1, phicm_n+1     ! integrating over phicm
	        phicm = 5.*(iphicm - 1)
            do iW = 1, W_n+1           ! integrating over W bin
              W = W_bin_min + W_bin_step*(iW-1)
              do iQ2 = 1, Q2_n+1       ! integrating over Q2 bin
                Q2 = Q2_bin_min + Q2_bin_step*(iQ2-1)
                sigma0=sigma0_calc(ebeam,
     1             theory_opt,channel_opt,resonance_opt,asym)
                if (iphicm.eq.1.or.iphicm.eq.phicm_n+1) 
     1            sigma0=0.5*sigma0
                if (iQ2.eq.1.or.iQ2.eq.Q2_n+1) sigma0=0.5*sigma0
                if (iW.eq.1.or.iW.eq.W_n+1) sigma0=0.5*sigma0
                sigma0_total = sigma0_total + sigma0
              enddo  ! integrating over Q2 bin
            enddo    ! integrating over W bin
          enddo      ! integrating over phicm
          sigma0_total = sigma0_total*(2*pi)/W_n/Q2_n/36
          write(6,30) csthcm,sigma0_total
 30       format(f7.2,5X,f8.3)
        enddo        ! calculating sigma for each csthcm bin
      enddo          ! calculating sigma for each W bin
     
      return
      end
