      subroutine int_ctrl_3(ebeam,theory_opt,channel_opt,resonance_opt)

      implicit none

       include 'spp.inc'         

      integer theory_opt,channel_opt,resonance_opt
      integer iW, iQ2, iW_bin, W_bin_n
      integer icsthcm, iphicm
      integer csthcm_bin_n, phicm_bin_n
       
      real W_calc, Q2_calc
      real csthcm_bin, phicm_bin
      real W_min, W_max
      real W_bin_max, W_bin_min, W_bin, W_bin_step
      real Q2_bin_max, Q2_bin_min, Q2_bin, Q2_bin_step
      real sigma0, sigma0_total, sigma0_calc, ebeam
      real sigu,sigt,sigl,sigi,sige,siget,sigpt,sig5
      
      integer Q2_n,W_n,csthcm_n,phicm_n
      
      data Q2_n,W_n,csthcm_n,phicm_n /2,2,40,36/

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

      write(6,10) 'Q2 (GeV)','W  (GeV)','Sigma(micro-barn)'
 10   format(1X, A9, 5X, A9, 5X, A17)
      open(unit=22,file='spp_crs.out',status='old')

      do iW_bin = 1, W_bin_n+1   ! for each W bin
        W_bin_min = W_min + W_bin*(iW_bin-1)
        W_bin_max = W_bin_min + W_bin
        W_calc = (W_bin_min + W_bin_max) / 2.0
        sigma0_total = 0.0
        do icsthcm = 1, csthcm_n+1 ! for each csthcm bin
          csthcm = -1. + 0.05*(icsthcm-1)
          do iphicm = 1, phicm_n+1     ! integrating over phicm
	        phicm = 5.*(iphicm - 1)
            do iW = 1, W_n+1           ! integrating over W bin
              W = W_bin_min + W_bin_step*(iW-1)
              do iQ2 = 1, Q2_n+1       ! integrating over Q2 bin
                Q2 = Q2_bin_min + Q2_bin_step*(iQ2-1)
            sigma0 =  sigma0_calc(ebeam,theory_opt,
     1                channel_opt,resonance_opt,
     1                sigu,sigt,sigl,sigi,sige,siget,sigpt,sig5)
                sigma0 = sig5
                if (iphicm.eq.1.or.iphicm.eq.phicm_n+1)
     1            sigma0=0.5*sigma0
                if (iQ2.eq.1.or.iQ2.eq.Q2_n+1) sigma0=0.5*sigma0
                if (iW.eq.1.or.iW.eq.W_n+1) sigma0=0.5*sigma0
                sigma0_total = sigma0_total + sigma0
              enddo  ! integrating over Q2 bin
            enddo    ! integrating over W bin
          enddo      ! integrating over phicm
        enddo        ! integrating over each csthcm
        sigma0_total = sigma0_total/W_n/Q2_n*(2*pi/phicm_n)*(2.0/csthcm_n)
        write(6,20) W_calc, Q2_calc, sigma0_total
        write(21,20) W_calc, Q2_calc, sigma0_total
 20     format(f8.3,6X,f8.3,8X,f8.3)
      enddo          ! calculating sigma for each W bin
     
      return
      end
