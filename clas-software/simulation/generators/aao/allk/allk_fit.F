      subroutine allk_fit_run(qinp,qout,eout)
      
      double precision arg1,arg2,arg3,arg4,ar_list(35)
      real fputil,qinp,qout,eout
      
      include 'allk_fit.inc'
      
      integer i,erf
      external allk_fmin
      data lmini/5/lmino/66/lminf/11/linput/21/fputil/0./f_out/49/
      
      print *, 'ALLK_FIT: Reading slfit_inp.dat'
      open (unit=linput,file='slfit_inp.dat',status='OLD')
      
      read (linput,*) inffile
      read (linput,*) maxcalls
      read (linput,*) tolorance 
      read (linput,*) errorup
      read (linput,*) parms
      read (linput,*) obsv2fit     
      read (linput,*) ebeam
      read (linput,*) wmin,wmax,wstep
      read (linput,*) q2min,q2max,q2step
      read (linput,*) cosmin,cosmax,cosstep
      read (linput,*) phimin,phimax,phistep
      read (linput,*) parofile      
      read (linput,*) datform
      read (linput,*) chimax
      read (linput,*) fname(1)
      read (linput,*) fname(2)
      read (linput,*) fname(3)
      read (linput,*) fname(4)
     
      close(linput)
      
      qout = 0.5*(q2min+q2max)
      eout = ebeam

      if (qinp.eq.-4) return
      
      print *, 'ALLK_FIT: Initializing Minuit'     
c     Initialize Minuit, Define I/O
      CALL mninit(lmini,lmino,lminf) 
                
c     Define the title of the Job
      CALL mnseti('Sato-Lee Analysis of Single Pion Production')
          
      print *, 'ALLK_FIT: Defining parameters'
c     Define parameters, Set the initial values
      fix_param=0.
      do i=1,parms
        arg1 = pval(i)
        arg2 = pstp(i)
        arg3 = pmin(i)
        arg4 = pmax(i)
        call mnparm(plist(i),pname(i),arg1,arg2,arg3,arg4,erf,fputil)
        if(erf.ne.0)then
        write(6,*)'Unable to define parameter  ',pname(i),
     +  i,' Error',erf
        endif
      enddo
      
      if (qout.eq.0) pstat(3)=2	! Override fitting status of gdc2 if Q2=0
      
      ar_list(1)=1.
      CALL mnexcm(allk_fmin,'CALL UFC',ar_list,1,erf,fputil)
      
      print *, 'ALLK_FIT: SCAN or FIX parameters'
      do i=1,parms
        ar_list(1)=i
c       ar_list(2)=100
        ar_list(2)=10
        if(abs(pstat(i)).eq.1)then
          print *,'ALLK_FIT: Scaning parameter ',i
          call mnexcm(allk_fmin,'SCAN',ar_list,2,erf,fputil)
        endif
        if(pstat(i).gt.1)then
          print *,'ALLK_FIT: Fixing parameter ',i
          fix_param=fix_param+1.
          ar_list(1)=plist(i)
          CALL mnexcm(allk_fmin,'FIX',ar_list,1,erf,fputil)
          if(erf.ne.0)then
            write(6,*)'Unable to Fix the parameter  ',pname(i),
     +      i,' Error',erf
          endif
        endif
      enddo
   
      print *,'ALLK_FIT: Number of parameters = ',parms
      print *,'ALLK_FIT: Number of fixed parameters = ',fix_param

      fputil=0.
      ar_list(1)=1.
      CALL mnexcm(allk_fmin,'SET PRINT',ar_list,1,erf,fputil)
     
      ar_list(1)=errorup
      CALL mnexcm(allk_fmin,'SET ERRORDEF',ar_list,1,erf,fputil)
     
      ar_list(1)=2.
      CALL mnexcm(allk_fmin,'CALL UFC',ar_list,1,erf,fputil)
     
      ar_list(1)=maxcalls
      ar_list(2)=tolorance
      write(6,*)'ALLK_FIT: Starting fit', ar_list(1), ar_list(2)
      CALL mnexcm(allk_fmin,'MIG',ar_list,2,erf,fputil)
      
      write(6,*),'ALLK_FIT: MIGRAD',erf
      write(lmino,*)'MIGRAD',erf
     
      if(erf.ne.0)then
        ar_list(1)=tolorance
        ar_list(2)=1
        ar_list(3)=2
        ar_list(4)=3
        CALL mnexcm(allk_fmin,'MINOS',ar_list,1,erf,fputil)
        write(6,*),'MINOS',erf
        write(lmino,*),'MINOS',erf
      endif
  
      print *,'ALLK_FIT: Sending STOP to MINUI '
      CALL mnexcm(allk_fmin,'STOP',0,0,erf,fputil)
      
c   77 call hjanr_plot 
c      call hjanr_pick
c      go to 77
      
      end
      
      subroutine allk_fmin(npar,grad,fval,xval,iflag,futil)
      save
      parameter (maxpoints=2000)
      double precision grad,xval,fval 
      dimension grad(37),xval(37)
      integer obsv,fitop,point_stat(maxpoints)
      real xx(maxpoints,10),nb,q2 
      logical bad_xsec,bad_data,bad_acc,bad_stat,iupd,tst_obs
      data lmini/5/lmino/66/lminf/11/linput/21/fputil/0./f_out/49/
     
      include 'mp.inc'
      include 'allk_fit.inc'
      
      print *, 'ALLK_FMIN: iflag',iflag

c Read in data file  

      IF ( iflag .eq. 1 ) THEN
        open(unit=f_out,file=inffile,status='UNKNOWN')
        call timex(timdd2)
        time_add   = 0.
        fitop      = 1
        itr_number = 0
        itr_count  = 0
        Npoints_max= 0
        N_zero     = 0
        N_bad      = 0
        N_good     = 0
        nhic       = 0
        
        do if=1,4
        if (fname(if).ne.'JUNK'.and.fname(if).ne.'junk') then
          print *, 'ALLK_FMIN: Opening file:',fname(if)
          
          open(unit=44,file=fname(if),status='OLD')
          
          do i=1,maxpoints
            
            if(datform.eq.1)then
            
              READ(44,*,err=999,end=998)W,Q2,epsilon,costh,phi,sec,err1
              bad_data    = sec.le.0..and.err1.le.0
                            
            elseif(datform.eq.2)then	! e1e data file
            
              READ(44,*,err=999,end=998)W,Q2,epsilon,costh,phi,sec,err1,
     +        nb,ac,rad,pi_type,obsv
     
              if (costh.gt.1.0) costh=cos(costh*3.1415926/180.)
              bad_xsec     = sec.le.0.or.err1.le.0
c              bad_acc      = ac.le.0.05.or.(ac.lt.0.1.and.nb.lt.10)
              bad_acc      = ac.le.0.05
              tst_obs      = pi_type.eq.1.and.obsv.eq.1
              bad_data	   = tst_obs.and.(bad_xsec.or.bad_acc)
     
            elseif(datform.eq.3)then	! e1b data file

              READ(44,*,err=999,end=998)W,Q2,epsilon,costh,phi,sec,err1,
     +        nb,ac,rad,dum,pi_type,obsv
     
              bad_xsec     = sec.le.0..and.err1.le.0
              bad_acc      = ac.lt.0.05.or.(ac.lt.0.1.and.nb.lt.10)
              bad_data	   = pi_type.eq.1.and.(bad_xsec.or.bad_acc)
                            
            endif
            
            Npoints_max  = Npoints_max+1
            
            if(bad_data)then
              point_stat(Npoints_max) = 1
              N_zero = N_zero+1
            else
              point_stat(Npoints_max) = 0
            endif
              
            if(w.lt.wmin.or.w.gt.wmax)             point_stat(Npoints_max)=1
            if(q2.lt.q2min.or.q2.gt.q2max)         point_stat(Npoints_max)=1
            if(costh.lt.cosmin.or.costh.gt.cosmax) point_stat(Npoints_max)=1
            if(phi.lt.phimin.or.phi.gt.phimax)     point_stat(Npoints_max)=1
            
            if (.not.btest(obsv2fit,pi_type-1))    point_stat(Npoints_max)=1
            if (.not.btest(obsv2fit,obsv+1))       point_stat(Npoints_max)=1
            
            xx(Npoints_max,1)     = w
            xx(Npoints_max,2)     = q2
            xx(Npoints_max,3)     = epsilon
            xx(Npoints_max,4)     = costh
            xx(Npoints_max,5)     = phi
            xx(Npoints_max,6)     = sec
            xx(Npoints_max,7)     = err1
            xx(Npoints_max,8)     = pi_type
            xx(Npoints_max,9)     = obsv
            
            if (point_stat(Npoints_max).eq.0) N_good = N_good+1
            
          enddo   
        endif
         
 998    close(44)
 
        enddo
          
        OPEN (UNIT=53,FILE='start_tab',status='UNKNOWN')
          
        write(lmino,*)'Number of data points =  ',Npoints_max
        write(lmino,*)'Number of good data points =  ',Npoints_max-N_zero
        write(lmino,*)'Number of fitted data points =  ',N_good
          
        write(f_out,*)'Kinematics:   W  =',wmin,' - ',wmax
        write(f_out,*)'              Q2 =',q2min,' - ',q2max
        write(f_out,*)'Number of data points =  ',Npoints_max
        write(f_out,*)'Number of good data points =  ',Npoints_max-N_zero
        write(f_out,*)'Number of fitted data points =  ',N_good
          
        write(lmino,*)'Number of data points for first iteration =  ',Npoints_max-N_zero 
          
        write(6,111)(pname(i),xval(i),i=1,parms)
111     format('Starting parameter values  ',/,
     +  37(a9,'=',f10.4/))
     
      ENDIF	! IFLAG.eq.1
      
      IF ( iflag .eq. 2 ) THEN
        print *,'Changing option'
        fitop=0
      ELSEIF ( iflag .EQ. 3 ) THEN
        write(f_out,*)'Number of data points to fit =  ',ndf
        write(f_out,*)'Number of iterations = ',itr_number,'  Chi2/point',fval_last/float(ndf)
        rq2=0.5*(q2min+q2max)
        call prterr(rq2)
        itr_number=0
      ENDIF

      IF(iflag.eq.1.or.iflag.ge.3)THEN
        timdd1   = timdd2
        call timex(timdd2)
        timdd    = timdd2-timdd1
        time_add = time_add+timdd
        write(6,*)
        write (*,*)'Iteration time = ',timdd,'  Total time = ',time_add
        fitted_points=0
        fval=0.
        
        call parput(xval)
        
        iupd = .true.
        
        DO i_point=1,Npoints_max
        
          if(point_stat(i_point).eq.0)then
           
            W       = xx(i_point,1)
            Q2      = xx(i_point,2)
            epsilon = xx(i_point,3)
            costh   = xx(i_point,4)
            phi     = xx(i_point,5)
            pi_type = xx(i_point,8)
            obsv    = xx(i_point,9)
            
            call allk_run(q2,w,epsilon,costh,phi,pi_type,iupd,0)
            
            IF(ierr.eq.0) THEN
            
              var_calc = robs(obsv,pi_type)
              c2weight = 1.
                            
              write(6,120) w,costh,var_calc,xx(i_point,6),xx(i_point,7) 
120      format(5f10.3)
              diff=c2weight*((var_calc-xx(i_point,6))/xx(i_point,7))**2
              
              if(diff.gt.chimax.and.itr_number.eq.0.and.fitop.eq.0)then
                nhic = nhic+1
                point_stat(i_point) = 1
                write(55,134)nhic,i_point,W,Q2,costh,phi,xx(i_point,6),var_calc,diff
                itr_bad = nhic
              else
                if(itr_number.eq.0.and.(iflag.eq.1.or.iflag.eq.3))then
                write(53,133)i_point,W,Q2,epsilon,costh,phi,xx(i_point,6),
     +xx(i_point,7),var_calc,diff
                endif
                
                fitted_points = fitted_points + 1.
                fval          = fval + diff
                fval_last     = fval
              endif
            Else
              write(6,*)'Bad calculation',w,q2,costh,phi,i_point
            ENDIF
          endif
        ENDDO

c *** lcs begin hjanr plugin for interactive plots        
c        do i=1,30
c          xnew(i)=sngl(xval(i))
c        enddo
c        call hjanr_plot 
c *** lcs end hjanr plugin for interactive plots        
        
        if(itr_number.eq.0)then
          if(iflag.eq.1) then
            close(53)
            ndf=fitted_points
          else
            ndf=fitted_points-(parms-fix_param)
          endif
          write(lmino,*)'Number of degrees of freedom for fit =  ',ndf
          print *, 'ALLK_FMIN: Number of degrees of freedom for fit =  ',ndf
        endif
         
        if(iflag.ne.1)itr_number=itr_number+1
        if(iflag.ne.1)itr_count=itr_count+1
        
        write(6,*)'Iteration = ',itr_number,' Value to minimize',fval,' Reduced Chi2',fval/float(ndf)
        write(6,*)'No. fitted param.',int(parms-fix_param),' No. fitted points',int(fitted_points)
        write(26,*)'Iteration = ',itr_number,' Value to minimize',fval,' Reduced Chi2',fval/float(ndf)
        
        do ip=1,parms
          if((pstat(ip).le.0.and.fitop.eq.0).or.(abs(pstat(ip)).eq.1.and.fitop.eq.1))then
            write(6,*)pname(ip),'  ',sngl(xval(ip))
            write(26,*)pname(ip),'  ',xval(ip)
          endif
        enddo
      
      ENDIF
 133  format(i7,8f9.4,f10.5)
 134  format(2i7,7f8.3)
        
      RETURN
      
 999  WRITE(6,*) 'Error while reading the data file',W,Q2,epsilon,costh,phi,sec,err1,
     +        nb,ac,rad,dum,pi_type,obsv      
      end
      
      subroutine prterr(rq2)

      double precision fmin,fedm,errdef,eplus,eminus,eparab,globcc,parv,parerr,bn1,bn2
      integer npari,nparx,istat,i,iv
      character*10 pnamei(35)
      
      include 'allk_fit.inc'

      OPEN (UNIT=27,FILE=parofile,STATUS='UNKNOWN')
      
      call mnstat(fmin,fedm,errdef,npari,nparx,istat)
      write(6,*)
      write(6,*) 'Minimized function=',fmin,'Reduced Chi2=',fmin/ndf
      write(6,*) 'Estimated distance to minimum',fedm
      write(6,*)
      write(6,*) 'Parameters and errors'

      do i=1,parms
         call mnpout(i,pnamei(i),parv,parerr,bn1,bn2,iv)
         if(pstat(i).le.0)then
            call mnerrs(-i,eplus,eminus,eparab,globcc)
            write(6,*)i,'  ',pnamei(i),parv,parerr,bn1,bn2
            write(6,*)i,eplus,eminus,eparab,globcc
            write(f_out,*)i,'  ',pnamei(i),parv,parerr
            write(27,53)plist(i),pname(i),parv,parerr,
     +         pstat(i),pmin(i),pmax(i)
         else
            write(27,53)plist(i),pname(i),pval(i),pstp(i),
     +         pstat(i),pmin(i),pmax(i)
         endif
      enddo
      
      close(27)
      
53    format(i2,1x,A10,2f10.4,i10,2f10.4)

      return
      end
      
      subroutine allk_run(q2,w,eps,costh,phi,k,iupd,iwndx)
      
      include 'mp.inc'
      real m_p,mpi2,m_pi(2),q2,qcm
      real*8 win,qin
      logical ok_wsave,ok_qsave,ok_csave,skip,iupd
      data m_p/0.93828/
      data m_pi/0.1349764,0.13957/
      data pi/3.14159/
      save wsave,qsave,csave
      
      if (iupd) then
        wsave = 0.0
        qsave = 0.0
        csave = -2
        iupd = .false.
      endif
      
      ok_wsave	= w.eq.wsave
      ok_qsave	= q2.eq.qsave
      ok_csave 	= costh.eq.csave
      skip      = ok_wsave.and.ok_qsave
      
      wsave 	= w
      qsave	= q2
      csave	= costh
      
      win        = w*1000
      qin	 = q2
      
      if (.not.skip) then
        if (iwndx.eq.0) then	! Recalculate multipoles
          call allkqw(win,qin,1)
          n=1
        else			! Use precalculated table
          n=iwndx
        endif
        call mul_amp(n,k)
      endif
      
      skip = skip.and.ok_csave
      if (skip) go to 10
      
      mpi2 = m_pi(k)**2
      ecm  = (w*w + mpi2 - m_p**2)/(2.*w)
      pcm  = ecm**2 - mpi2
      pcm  = sqrt(pcm)
      fkt  = 2.0*w*pcm/(w**2-m_p**2)
      qcm  = ((w**2 + q2 + m_p**2)/2.0/w)**2 - m_p**2
      ekin = sqrt(q2/qcm)
      
      vl   = eps
      vtt  = eps
      vlt  = sqrt(2*eps*(1+eps))
      vltp = sqrt(2*eps*(1-eps))
            
      call hel_amp(costh)
      
      rsp(1) = fkt*sig_t
      rsp(2) = fkt*sig_l*vl*ekin**2
      rsp(3) = fkt*sig_tt
      rsp(4) = fkt*sig_lt
      rsp(5) = fkt*sig_ltp 
      
10    phir = phi*pi/180.0      
      cs2f = cos(2*phir)
      csf  = cos(phir)
      snf  = sin(phir)
      
      sigma_u  =     rsp(1)  
     1         +     rsp(2)  
     1         + vtt*rsp(3)*cs2f
     1         + vlt*rsp(4)*ekin*csf 
     
      sigma_lt  =  vlt*rsp(4)*ekin*csf 
      sigma_ltp = vltp*rsp(5)*ekin*snf 
      
      robs(1,k) = sigma_u
      robs(2,k) = sigma_ltp/sigma_u
      robs(3,k) = sigma_lt/sigma_u
      robs(4,k) = -sig_tt/sig_t
      robs(5,k) = rsp(1)
      robs(6,k) = rsp(1)+rsp(2)
      robs(7,k) = rsp(3)
      robs(8,k) = rsp(4)*ekin
      robs(9,k) = rsp(5)*ekin
      robs(10,k) = fkt*(sig_t32-sig_t12)
      robs(11,k) = sigt_t_y/sig_t	! T
      robs(12,k) = -sigt_tt_y/sig_t	! P
      robs(13,k) = -sigt_tt_z/sig_t	! G
      robs(14,k) = -sigt_tt_x/sig_t	! H
      robs(15,k) =  (rsp(1)+rsp(2))/fkt	! Bates R0
      return
      end
      
      subroutine mul_amp(n,k)
      
      implicit real*8(a-b,d-h,o-z)
      implicit complex*16(c)
      parameter(ndim=20,maxpot=8,nedim=50,npdim=20,nbmas=10,
     &          ladim=10)
      common /mult/ cdmul(nedim,4,0:ladim,8),cbmul(nedim,4,0:ladim,8)
     & ,cbrmul(nedim,4,0:ladim,8),cbdmul(nedim,4,0:ladim,8)
     & ,ctmul (nedim,4,0:ladim,8)
     & ,cxb(4,0:ladim,8),cxbr(4,0:ladim,8),cxbd(4,0:ladim,8)
     & ,cxd(4,0:ladim,8),cxt(4,0:ladim,8)
      common / c2001 / wcmmev(1000),qsgev(1000),fact(1000),
     &ebeam,qmin,wmin,delq,delw,meshq,meshw,imaxdt
      include 'mp.inc'
      
      factor = 0.141383e0     ! Convert from 10**-3/m_pi+ to sqrt(microbarns)
      
      do i=0,5
        sp(i)=ctmul(n,k,i,7)*fact(n)*factor
        sm(i)=ctmul(n,k,i,8)*fact(n)*factor
        ep(i)=ctmul(n,k,i,1)*fact(n)*factor
        em(i)=ctmul(n,k,i,2)*fact(n)*factor
        mp(i)=ctmul(n,k,i,3)*fact(n)*factor
        mm(i)=ctmul(n,k,i,4)*fact(n)*factor
      enddo
      
c      c12=(ctmul(n,1,1,4)-sqrt(2.)*ctmul(n,2,1,4))/3.
c      c32=(ctmul(n,1,1,4)+ctmul(n,2,1,4)/sqrt(2.))
c      c32=c32*0.8
c      mm(1)=(c12+2*c32/3.)*fact(n)*factor

      end
      
      subroutine hel_amp(x)

      include 'mp.inc'

      real x
      data root2/1.41421356/

      pol(0,1) = 0.
      pol(1,1) = 1.
      pol(2,1) = 3.*x 
      pol(3,1) = (15.*x*x-3.)/2.
      pol(4,1) = (35.*x*x*x-15.*x)/2.
      pol(5,1) = (315.*x*x*x*x-210.*x*x+15.)/8.
      pol(6,1) = (693.*x*x*x*x*x-630.*x*x*x +105.*x)/8.
      pol(7,1) = (3003.*x*x*x*x*x*x-3465.*x*x*x*x+945*x*x-35.)/16.
      pol(0,2) = 0.
      pol(1,2) = 0.
      pol(2,2) = 3.
      pol(3,2) = 15.*x 
      pol(4,2) = (105.*x*x-15.)/2.
      pol(5,2) = (315.*x*x*x-105.*x)/2.
      pol(6,2) = (3465.*x*x*x*x-1890.*x*x+105.)/8.  
      pol(7,2) = (9009.*x*x*x*x*x-6930.*x*x*x +945.*x)/8.
      
      ff1 = 0.0
      ff2 = 0.0
      ff3 = 0.0
      ff4 = 0.0
      ff5 = 0.0
      ff6 = 0.0
      
      do l = 0, mwave_L
        if(l.lt.2) then
          ff1 = ff1 + (    l*mp(l) +  ep(l))*pol(l+1,1)
        else
          ff1 = ff1 + (    l*mp(l) +  ep(l))*pol(l+1,1)
     1              + ((l+1)*mm(l) +  em(l))*pol(l-1,1)
        endif
      enddo

      do l = 1, mwave_L
        ff2 = ff2 + ((l+1)*mp(l) + l*mm(l))*pol(l,1)
        if(l.lt.2) then
           ff3 = ff3 + (   ep(l) -   mp(l))*pol(l+1,2)
        else
           ff3 = ff3 + (   ep(l) -   mp(l))*pol(l+1,2) +
     1                 (   em(l) +   mm(l))*pol(l-1,2)
        endif
      enddo

      do l = 2, mwave_L
        ff4 = ff4 + (mp(l) - ep(l) -mm(l) - em(l))* pol(l,2)
      enddo

      do l = 0, mwave_L
        if(l.lt.2) then
          ff5 = ff5 +  (l+1)*sp(l)*pol(l+1,1)
        else
          ff5 = ff5 + ((l+1)*sp(l)*pol(l+1,1) - l*sm(l)*pol(l-1,1))
        endif
      enddo

      do l = 1, mwave_L
        ff6 = ff6 + (l*sm(l) - (l+1)*sp(l))*pol(l,1)
      enddo

      theta_cm	= acos(x)
      s		= sin(theta_cm)
      c		= cos(theta_cm)
      s2	= sin(theta_cm/2.0)
      c2	= cos(theta_cm/2.0)
      
      hh1 = -s*c2*(ff3 + ff4)/root2
      hh2 =  c2*((ff2 - ff1) + 0.5*(1 - c)*(ff3 - ff4))*root2
      hh3 =  s*s2*(ff3 - ff4)/root2
      hh4 =  s2*((ff2 + ff1) + 0.5*(1 + c)*(ff3 + ff4))*root2
      hh5 =  c2*(ff6 + ff5)
      hh6 =  s2*(ff6 - ff5)
      
      sig_t  = (cabs(hh1)**2+cabs(hh2)**2
     1         +cabs(hh3)**2+cabs(hh4)**2)/2.
      sig_l  =  cabs(hh5)**2+cabs(hh6)**2
      sig_tt =  real(-conjg(hh1)*hh4+conjg(hh2)*hh3)
      
      sig_lt =    real(conjg(hh5)*(hh1-hh4)  + 
     1                 conjg(hh6)*(hh2+hh3))/root2      
      sig_ltp = -aimag(conjg(hh5)*(hh1-hh4)  + 
     1                 conjg(hh6)*(hh2+hh3))/root2
     
      sig_t12 = (cabs(hh2)**2+cabs(hh4)**2)
      sig_t32 = (cabs(hh1)**2+cabs(hh3)**2)
      
      sigt_t_y   = -aimag(conjg(hh1)*hh2+conjg(hh3)*hh4)	! T
      sigt_tt_y  = -aimag(conjg(hh1)*hh3+conjg(hh2)*hh4)	! P
      sigt_tt_z  = -aimag(conjg(hh1)*hh4+conjg(hh2)*hh3)	! G
      sigt_tt_x  = -aimag(conjg(hh1)*hh3-conjg(hh2)*hh4)	! H
           
      end
      
      subroutine sigtot(sigt,sig32)
      
      include 'mp.inc'
      
      sum1 = 0.
      sum2 = 0.
      do l = 0, mwave_L
        a=(l+2)*(cabs(ep(l))**2+cabs(mm(l+1))**2)
        b=    l*(cabs(mp(l))**2+cabs(em(l+1))**2)
        c=real(conjg(ep(l))*mp(l))-real(conjg(em(l+1))*mm(l+1))
        sigt=(a+b)*(l+1)**2
        sig32=(b-a-2*l*(l+2)*c)*(l+1)
        sum1=sum1+sigt
        sum2=sum2+sig32
      enddo
      sigt = sum1*fkt*2*3.14159
      sig32= sum2*fkt*4*3.14159
      
      end


