      subroutine int_ctrl_11(ebeam,theory_opt,channel_opt,resonance_opt)

      implicit none

      include 'spp.inc'    

      integer theory_opt,channel_opt,resonance_opt
      integer iW, iQ2, iW_bin, W_bin_n
      integer csthcm_bin_n
      integer phicm_bin_n,phicm_bin_nt,phicm_bin_n2
      integer icsthcm, iphicm, iphicm2
      real W_calc, Q2_calc, phicm_calc
      real csthcm_bin
      real phicm_bin,phicm_bin2,phicm_min,phicm1
      real W_min, W_max
      real W_bin_max, W_bin_min, W_bin, W_bin_step
      real Q2_bin_max, Q2_bin_min, Q2_bin, Q2_bin_step
      real costh_bin_max, costh_bin_min, costh_bin ,costh_bin_step
      real sigma0, sigma0_total , sigma0_calc, ebeam
      integer Q2_n,W_n,csthcm_n,phicm_n
      data Q2_n,W_n,csthcm_n,phicm_n,phicm_bin_n2 /10,10,40,36,5/

      real thecm,ac


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
      write(6,*)' input lower and upper bounds on cos(theta)'
      read(5,*)costh_bin_min,costh_bin_max
      costh_bin = costh_bin_max - costh_bin_min
      costh_bin_step  = costh_bin / csthcm_n
      write(6,*)' Input the number of bins for phicm'
      read(5,*) phicm_bin_nt
      phicm_bin = 360.0/phicm_bin_nt
      phicm_bin_n = ifix(0.5 + phicm_bin_nt/2.0)
      write(21,*) phicm_bin_n
C      OLD PART OF CODE
C      phicm_bin_nt = ifix(0.5 + 360.0/phicm_bin)
C      write(21,*) ifix(0.5 + phicm_bin_nt/2.0)
      if ( mod(phicm_bin_nt,2).eq.1 ) then
        phicm_min = -phicm_bin/2.0
      else
        phicm_min=0.0
      endif
      phicm_bin2 = phicm_bin/phicm_bin_n2

      do iW_bin = 1, W_bin_n+1   ! for each W bin
        W_bin_min = W_min + W_bin*(iW_bin-1)
        W_bin_max = W_bin_min + W_bin
        W_calc = (W_bin_min + W_bin_max) / 2.0
        write(6,*) ' '
        write(21,10) 'Q2 (GeV)= ',Q2_calc,' W (GeV)= ',W_calc 
 10     format(1X, A10, f5.3, A10, f5.3)
        write(21,20) 'phicm (deg)','Sigma(micro-barn)'
 20     format(1X, A11, 4X, A17)


        do iphicm = 1, phicm_bin_n ! for each phicm bin
          phicm1 = phicm_min + phicm_bin/2.0 + phicm_bin*(iphicm-1)
	  sigma0_total = 0.0
          do iphicm2 = 1, phicm_bin_n2+1 ! integrate over phicm bin
            phicm = phicm1 - phicm_bin/2.0 + phicm_bin2*(iphicm2-1)
            do icsthcm = 1, csthcm_n+1     ! integrating over csthcm
              csthcm = costh_bin_min + costh_bin_step*(icsthcm - 1)
              if (csthcm.GE.1.0) then
                thecm=0.
	      elseif(csthcm.LE.-1.0) then
		thecm=180
              else
                thecm=acos(csthcm)*180./3.14159265359
              endif
              do iW = 1, W_n+1           ! integrating over W bin
                W = W_bin_min + W_bin_step*(iW-1)
                do iQ2 = 1, Q2_n+1       ! integrating over Q2 bin
                  Q2 = Q2_bin_min + Q2_bin_step*(iQ2-1)
                  sigma0=sigma0_calc(ebeam,
     1               theory_opt,channel_opt,resonance_opt)
c                  call acc(ebeam,Q2,W,phicm,thecm,ac)
		  write(*,*)'add here the call to your acceptance function'
                  if (iphicm2.eq.1.or.iphicm2.eq.phicm_bin_n2+1)
     1              sigma0=0.5*sigma0
                  if (icsthcm.eq.1.or.icsthcm.eq.csthcm_n+1) 
     1              sigma0=0.5*sigma0
                  if (iQ2.eq.1.or.iQ2.eq.Q2_n+1) sigma0=0.5*sigma0
                  if (iW.eq.1.or.iW.eq.W_n+1) sigma0=0.5*sigma0
                  if(ac.ge.0.000000001) then
                    sigma0_total = sigma0_total + sigma0
                  endif
                enddo  ! integrating over Q2 bin
              enddo    ! integrating over W bin
            enddo      ! integrating over csthcm
          enddo        ! integrating over the phi-bin
          sigma0_total = sigma0_total*
     1                   (pi*phicm_bin2/180.0)*(costh_bin/csthcm_n)/
     1                   W_n/Q2_n/phicm_bin_n2
          write(21,30) '+/-', phicm1,sigma0_total
 30       format(A3,f7.2,5X,f8.3)
        enddo        ! calculating sigma for each phicm bin
      enddo          ! calculating sigma for each W bin
     
      return
      end
