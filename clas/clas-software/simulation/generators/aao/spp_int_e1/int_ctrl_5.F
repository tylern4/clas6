      subroutine int_ctrl_5(ki_mag,theory_opt,channel_opt,resonance_opt)

      implicit none

      include 'spp.inc'    
      include 'helamp.inc'  
  
      real sigma0, sigma0_calc
      real siguu,sigu,sigt,sigl,sigi,sige,siget,sigpt,sig5,rltp
      real ki_mag
      real asym, sigmap, sigmam
      integer iw, ic, ip
      integer jq, jw, jc, jp
      integer nw, nc, np, nbin
      integer theory_opt, channel_opt, resonance_opt
      real q2_min, q2_max
      real q2_bin, w_bin, csthcm_bin, phicm_bin
      real w_min, csthcm_min, phicm_min, wmin
      real q2_center, w_center, csthcm_center, phicm_center
      real sigma0_avg, sigma0_corr, sigma0_center, epsilon_center
      character*132 fname
      
      data w_bin, csthcm_bin, phicm_bin /0.02, 0.2, 15.0/
      data nw, nc, np, nbin /13, 10, 24, 10/

      print *, 'Enter q**2 (GeV)^2, min q**2, max q**2: '
      read(5,*) q2_center, q2_min, q2_max      
      q2_bin = q2_max - q2_min
      
      print *, 'Enter pol_elec,pol_targ: '
      read(5,*) pol_e,pol_t
      
      print *, 'data points in the bin'
      read(5,*) nbin

      print *, 'output file name:'
      read(5,'(a)') fname

      open(unit=21,file=fname,type='unknown')

      wmin=1.09
      if (channel_opt.eq.4) wmin=1.50

      do iw = 1, nw
        w_center = wmin + w_bin/2.0 +(iw-1)*w_bin
        do ic = 1, nc
          csthcm_center = -1.0 + csthcm_bin/2.0 +(ic-1)*csthcm_bin
          do ip = 1, np
            phicm_center = phicm_bin/2.0 +(ip-1)*phicm_bin
            q2 = q2_center
            w = w_center
            csthcm = csthcm_center
            phicm  = phicm_center
c
            if (theory_opt.eq.3.and.iw.gt.13) then
              sigma0_center = 1.0
              sigma0_corr   = 1.0
              go to 22
            endif
c
            sigma0 =  sigma0_calc(ki_mag,theory_opt,
     1                channel_opt,resonance_opt,
     1                sigu,sigt,sigl,sigi,sige,siget,sigpt,sig5,rltp)
            sigma0_center = sigma0
            epsilon_center = epsilon
            w_min = w_center - w_bin/2.0
            csthcm_min = csthcm_center - csthcm_bin/2.0
            phicm_min  = phicm_center - phicm_bin/2.0
            sigma0_avg = 0.0

            do jq = 1, nbin+1           
              q2 = q2_min + (jq-1)*q2_bin/nbin
              do jw = 1, nbin+1 
                w = w_min + (jw-1)*w_bin/nbin
                if (w.lt.1.1) w = 1.1
                do jc = 1, nbin+1
                  csthcm = csthcm_min + (jc-1)*csthcm_bin/nbin
                  do jp = 1, nbin+1
                    phicm = phicm_min + (jp-1)*phicm_bin/nbin 
                    sigma0 = sigma0_calc(ki_mag,theory_opt,
     1                       channel_opt,resonance_opt,
     1                sigu,sigt,sigl,sigi,sige,siget,sigpt,sig5,rltp)
                    sigma0_avg = sigma0_avg + sigma0
                  enddo
                enddo
              enddo    
            enddo
            sigma0_avg  = sigma0_avg/((nbin+1)**4)
            sigma0_corr = sigma0_center / sigma0_avg
            if (iw.eq.1) sigma0_corr = 1.0
 22         write(6,100) w_center,q2_center,epsilon_center,csthcm_center,
     1                   phicm_center,sigma0_corr,sigma0_center
            write(21,100) w_center,q2_center,epsilon_center,csthcm_center,
     1                   phicm_center,sigma0_corr,sigma0_center 
          enddo
        enddo
      enddo

 100  format(5F10.3,2E12.3)      
      end
