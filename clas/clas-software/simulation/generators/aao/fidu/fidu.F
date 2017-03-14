      program fidu
      implicit none
      
#include "bcs.inc"
#include "mc.inc"
c
      common /pawc/h(500000)
c
      character*132 hname       ! histogram file name
c
      real the_e, the_ed, phi_e, phi_ed, kf_mag
      real the_h, the_hd, phi_h, phi_hd, pf_mag
      real phi_edn, phi_edn1, phi_hdn, phi_hdn1
      real kf(3), pf(3)
      real dot, newphi, accvb1, accvb2
      real toru
c 
      integer acc1, acc2, h, lrecl, istat, icycle
      integer devent, nevent, nevent_max, ierr, ntries
c 
      data toru/2250/
c
      write(6,*) 'Input the number of events to process'
      read(5,*) nevent_max

      write(6,*) 'Input the number of particle'
      read(5,*) npart 
c
c     Initialize BOS
c
      call bos_ini
c
c     Initialize histogram file and historams
c
      call getenvf('HIST',hname)
      lrecl=1024
      call hlimit(100000)
c
      WRITE(6,*)'Opened histogram file: ', hname(1:132)
      call hropen(31,'fidu',hname,'n',lrecl,istat)
      call hbook2(100,'[q] vs [f] for electrons with tight cut'
     1      ,200,-50.,350.,80,20.,60.,0.)
      call hbook2(200,'[q] vs [f] for electrons with loose cut'
     1      ,200,-50.,350.,80,20.,60.,0.)
      call hbook2(300,'[q] vs [f] for hadrons with tight cut'
     1      ,200,-50.,350.,140,0.,70.,0.)
      call hbook2(400,'[q] vs [f] for hadrons with loose cut'
     1      ,200,-50.,350.,140,0.,70.,0.)
c
c
c Initialize ntries, ievt_500
c
      ntries = 1
      nevent = 1
      devent = 1
  
      do while (ntries.le.nevent_max)
C 
        ierr = 0
        call read_bank(ierr)
        if(ierr.eq.1) goto 10
c
        kf(1)    = p(1,1)
        kf(2)    = p(1,2)
        kf(3)    = p(1,3)
        pf(1)    = p(2,1)
        pf(2)    = p(2,2)
        pf(3)    = p(2,3)
c
        call extractang(the_e,the_ed,phi_e,phi_ed,kf)
        call extractang(the_h,the_hd,phi_h,phi_hd,pf)
        kf_mag = sqrt(dot(kf,kf))
        pf_mag = sqrt(dot(pf,pf))
c
        phi_edn1 =  phi_ed
        phi_hdn1 =  phi_hd
        if(phi_ed.lt.0.) phi_edn1 = phi_ed + 360.
        phi_edn = newphi(phi_edn1)
        if(phi_hd.lt.0.) phi_hdn1 = phi_hd + 360.
        phi_hdn = newphi(phi_hdn1)
        if(phi_edn1.gt.330.) phi_edn1 = phi_edn1 - 360.
        if(phi_hdn1.gt.330.) phi_hdn1 = phi_hdn1 - 360.
        acc1    = 0
        acc2    = 0
        acc1 	= accvb1(0,kf_mag,the_ed,phi_edn,toru)
        acc2 	= accvb1(1,pf_mag,the_hd,phi_hdn,toru)
        if(acc1.eq.1.0.and.acc2.eq.1.0) then
          call hf2(100,phi_edn1,the_ed,1.)
          call hf2(300,phi_hdn1,the_ed,1.)
        endif
        acc1    = 0.
        acc2    = 0.
        acc1 	= accvb2(0,kf_mag,the_ed,phi_edn,toru)
        acc2 	= accvb2(1,pf_mag,the_hd,phi_hdn,toru)
        flag(1) = -3
        flag(2) = -3
        if(acc1.eq.1.0.and.acc2.eq.1.0) then
          flag(1) = 1
          flag(2) = 1
        endif
        if(acc1.eq.1.0.and.acc2.eq.1.0) then
          call hf2(200,phi_edn1,the_ed,1.)
          call hf2(400,phi_hdn1,the_ed,1.)
        endif
c        
c
c
        call bos_out ! Pack the BOS banks and write out to file 
c
        if(devent.eq.5000) then
          write(6,*) ntries, nevent
          devent = 0
        endif
        nevent =  nevent + 1
        devent =  devent + 1
 10     continue
        ntries =  ntries + 1 
      enddo
c
c     Close out the n-tuple file
c
      call hrout(0,icycle,' ')
      call hrend('fidu')
c
      CALL bos_end
c      
      END
