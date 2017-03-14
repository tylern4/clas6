      subroutine int_ctrl_4(ki_mag,theory_opt,channel_opt,resonance_opt,tbl_opt)

      implicit none

      include 'spp.inc'  
      include 'helamp.inc'  
  
      real sigtot, sigma0, sigma0e, sigma0_calc, asym, dum
      real sigup,sigu,sigt,sigl,sigi,sige,siget,sigpt,sig5,rltp
      real ki_mag,th,cdat(8),wmin
      integer iw, ic, ip
      integer theory_opt, channel_opt, resonance_opt, tbl_opt, i,iopt
      character*80 filename
      
      data cdat/-0.875,-0.625,-0.375,-0.125,0.125,0.375,0.625,0.875/
      
      wmin=1.10
      if (channel_opt.eq.4) wmin=1.49
      
      if (tbl_opt.eq.4) then
      
      print *, 'Enter q**2 (GeV)^2: '
      read(5,*) q2
      
      print *, 'Enter pol_elec,pol_targ: '
      read(5,*) pol_e,pol_t
     
      do iw = 1, 30
        w = wmin+(iw-1)*0.02
        iopt = 1
        do ic = 1,10
          csthcm = -0.9+(ic-1)*0.2
          do ip = 1,36
            phicm = 5.0+(ip-1)*10
            sigtot =  sigma0_calc(ki_mag,theory_opt,
     1                channel_opt,resonance_opt,iopt,
     1                sigu,sigt,sigl,sigi,sige,siget,sigpt,sig5,rltp)
            iopt = 0
            sigup = sigu+sigl+sigt+sigi
            asym = sige/sigup
            write(21,100) w,q2,epsilon,csthcm,phicm,ki_mag,sigu,sigl,sigt,sigi,sige,siget,sigpt
c            write(21,100) w,q2,epsilon,csthcm,phicm,sigup,sigup*0.1,asym,rltp,sigpt
          enddo
            write(22,101) w,q2,epsilon,csthcm,reph,ieph
        enddo
      enddo
      
      if (channel_opt.eq.333) then
      
      do iw = 1, 30
        w = 1.115+(iw-1)*0.03+0.0001
        iopt = 1
        do ic = 8,1,-1
          csthcm = cos((10+(ic-1)*20)*3.14159/180.) 
          do ip = 1,18
            phicm = (ip-1)*20
            sigtot =  sigma0_calc(ki_mag,theory_opt,
     1                channel_opt,resonance_opt,iopt,
     1                sigu,sigt,sigl,sigi,sige,siget,sigpt,sig5,rltp)
            iopt = 0
            sigu = sigu+sigl+sigt+sigi
            asym = sige/sigu
            write(21,100) w,q2,epsilon,csthcm,phicm,sigu,sigu*0.1,asym,rltp,dum
          enddo
        enddo
      enddo
      
      endif
      
      elseif (tbl_opt.eq.9) then
      
      open(unit=21,file='spp_int_w.tbl',status='new')
      open(unit=22,file='spp_int_c.tbl',status='new')
      
      print *, 'Enter q**2 (GeV)^2: '
      read(5,*) q2
      
      print *, 'Enter pol_elec,pol_targ: '
      read(5,*) pol_e,pol_t
      
      do iw = 1, 61
        w = wmin+(iw-1)*0.01
        iopt = 1
        do ic = 1,8
          csthcm = cdat(ic)
          phicm = 90.0
            sigtot =  sigma0_calc(ki_mag,theory_opt,
     1                channel_opt,resonance_opt,iopt,
     1                sigu,sigt,sigl,sigi,sige,siget,sigpt,sig5,rltp)
            iopt = 0
            sigu = sigu+sigl+sigt+sigi
            asym = sige/sigu
            write(21,100) w,q2,epsilon,csthcm,phicm,sigu,sigu*0.1,asym,rltp,dum
        enddo
      enddo
      close(21)
      do iw = 1, 15
        w = wmin+(iw-1)*0.04
        iopt = 1
        do th = 1,179,3
          csthcm = -cos(th*3.1415926/180.)
          phicm = 90.0
            sigtot =  sigma0_calc(ki_mag,theory_opt,
     1                channel_opt,resonance_opt,iopt,
     1                sigu,sigt,sigl,sigi,sige,siget,sigpt,sig5,rltp)
            iopt = 0
            sigu = sigu+sigl+sigt+sigi
            asym = sige/sigu
            write(22,100) w,q2,epsilon,csthcm,phicm,sigu,sigu*0.1,asym,rltp,dum
        enddo
      enddo
      close(22)
      
      elseif (tbl_opt.eq.6) then
      
      print *, 'Enter pol_elec,pol_targ: '
      read(5,*) pol_e,pol_t
      
      print *, 'Enter data table containing kinematics: '
      read(*,'(A)') filename
      print *, 'Opening ',filename
      open(unit=22,file=filename,status='old')
c      open(unit=23,file='junk.dat',status='new')

      iopt = 1
      do i = 1,8500
        read(22,*,end=99) w,q2,epsilon,csthcm,phicm,sigma0,sigma0e,dum,dum,dum
        if (csthcm.gt.1) csthcm=cos(csthcm*3.14159/180.)
        if (csthcm.gt.0.998) csthcm=0.999
        if (csthcm.lt.-0.998) csthcm=-0.999
        sigtot =  sigma0_calc(ki_mag,theory_opt,
     1            channel_opt,resonance_opt,iopt,
     1            sigu,sigt,sigl,sigi,sige,siget,sigpt,sig5,rltp)
        sigu = sigu+sigl+sigt+sigi
        asym = sige/sigu
        write(21,100) w,q2,epsilon,csthcm,phicm,sigu,sigu*0.1,asym,rltp,dum
      enddo
      
      endif
     
  99  close(22)
      close(21)
c      close(23)
        
 100  format(6F10.3,7E12.3)  
 101  format(5F10.3,12E12.3)  
     
      end

