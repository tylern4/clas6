      SUBROUTINE JANR_FMIN(npar,grad,fval,xval,iflag,futil)
      IMPLICIT NONE
      SAVE

#include "res_func.inc"
#include "fit_par.inc"
#include "janr_data.inc"
#include "janrtab.inc"
#include "hjanr_val.inc"
#include "inpar.inc"

      DOUBLE PRECISION grad,xval,fval
      dimension grad(36),xval(36)
      REAL  futil
      INTEGER iflag, npar
      INTEGER i,itr_number,Npoints_max,i_point,ierr,if
      integer itr_count,nhic,itr_bad,ip,N_zero,N_bad,N_good,jitr
      integer  obsv
      REAL xx(MAXpoints,10),diff,cross_calc,fitted_points
      REAL W,costh,Q2,epsilon,phi,sec,err1,err2,fval_last
      REAL timdd, timdd1, timdd2, time_add, ac, rad,nb,dum
      integer fitop
      logical bad_xsec,bad_data,bad_acc,tst_obs
      real amod
      real var_calc, var_meas , sigma_total
      real c2weight,rq2
      integer arg0, arg1, argt
      double precision norm_scale(10) 
      data norm_scale / 1., 1., 1., 1., 1., 1., 1., 1., 1., 1. /
      
c Read in data file  
c      print *, 'JANR_FMIN: iflag',iflag

      IF ( iflag .eq. 1 ) THEN
        OPEN (UNIT=f_out,FILE=inffile,status='UNKNOWN')
        CALL TIMEX(timdd2)
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
        IF(fname(if).NE.'JUNK'.AND.fname(if).NE.'junk' ) THEN
          print *, 'JANR_FMIN: Opening file:',fname(if)
          
          OPEN (UNIT=44,FILE=fname(if),status='OLD')
          
          do i=1,MAXpoints
          
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
          
 998      close(44)
 
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
          
        if (nbad.gt.0) then
        DO I_point=1,Npoints_max
          do i=1,Nbad
            if(i_point.gt.Ibad(1,i).and.I_point.lt.Ibad(2,i)) then
              point_stat(I_point) = 1
              N_bad = N_bad+1
            endif
          enddo  
        ENDDO
        endif
          
        write(lmino,*)'Number of bad data points =  ',N_bad
        write(lmino,*)'Number of data points for first iteration =  ',Npoints_max-N_zero-N_bad
          
        write(6,111)(xval(i),i=1,parms)
111     format(1x,'JANR_FMIN: Value of parameters at the begining => 1-30 ',/,
     +'   cM1 =',f9.5,'   cE1 =',f9.5,'   cS1 =',f9.5,/,
     +'   cMP1 =',f9.5,'  cSP1 =',f9.5,'  cEP2 =',f9.5,'   cSP2 =',f9.5,/,
     +'   cMP3 =',f9.5,'  cEP3 =',f9.5,'  cSP3 =',f9.5,/,
     +'   cEP4 =',f9.5,'  cSP4 =',f9.5,'  cEP5 =',f9.5,'   cSP5 =',f9.5,/,
     +'   cMP6 =',f9.5,'  cEP6 =',f9.5,'  cSP6 =',f9.5,/,
     +'   cMP7 =',f9.5,'  cEP7 =',f9.5,'  cSP7 =',f9.5,/,
     +'   cMP8 =',f9.5,'  cEP8 =',f9.5,'  cSP8 =',f9.5,/,
     +'   cMP9 =',f9.5,'  cEP9 =',f9.5,'  cMP10 =',f9.5,'  cEP10 =',f9.5,/,  
     +'   cMP11 =',f9.5,' cEP11 =',f9.5,/,
     +'   cSPA =',f9.5,/)     
     
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
        CALL TIMEX(timdd2)
        timdd    = timdd2-timdd1
        time_add = time_add+timdd
        fitted_points=0
        fval=0.
        cM1  =sngl(xval(1))
        cE1  =sngl(xval(2))
        cS1  =sngl(xval(3))
        cMP1 =sngl(xval(4))
        cSP1 =sngl(xval(5))
        cEP2 =sngl(xval(6))
        cSP2 =sngl(xval(7))
        cMP3 =sngl(xval(8))
        cEP3 =sngl(xval(9))
        cSP3 =sngl(xval(10))
        cEP4 =sngl(xval(11))
        cSP4 =sngl(xval(12))
        cEP5 =sngl(xval(13))
        cSP6 =sngl(xval(14))
        cMP6 =sngl(xval(15))
        cEP6 =sngl(xval(16))
        cSP6 =sngl(xval(17))
        cMP7 =sngl(xval(18))
        cEP7 =sngl(xval(19))
        cSP7 =sngl(xval(20))
        cMP8 =sngl(xval(21))
        cEP8 =sngl(xval(22))
        cSP8 =sngl(xval(23))
        cMP9 =sngl(xval(24))
        cEP9 =sngl(xval(25))
        cMP10=sngl(xval(26))
        cEP10=sngl(xval(27))
        cMP11=sngl(xval(28))
        cEP11=sngl(xval(29))
        cM2  =sngl(xval(30))
        cE2  =sngl(xval(31))
        cS2  =sngl(xval(32))
        cM3  =sngl(xval(33))
        cE3  =sngl(xval(34))
        cS3  =sngl(xval(35))
        cSPA =sngl(xval(36))

        DO i_point=1,Npoints_max
        
          if(point_stat(i_point).eq.0)then
           
            W       = xx(i_point,1)
            Q2      = xx(i_point,2)
            epsilon = xx(i_point,3)
            costh   = xx(i_point,4)
            phi     = xx(i_point,5)
            pi_type = xx(i_point,8)
            obsv    = xx(i_point,9)
            
            CALL JANR_RUN(W,Q2,epsilon,costh,phi,.FALSE.)
            
            IF(ierr.eq.0) THEN
            
              var_calc = robs(obsv,pi_type)
c              print *, w,q2,costh,phi,var_calc,xx(i_point,6)
              c2weight = 1.
              
              if (obsv.eq.7.and.itr_count.eq.49) 
     1        print *, costh,var_calc,xx(i_point,6),xx(i_point,9)
              
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

        if(itr_number.eq.0)then
          if(iflag.eq.1)close(53)
          ndf=fitted_points-parms+fix_param
          write(lmino,*)'Number of degrees of freedom for fit =  ',ndf
        endif
         
        if(iflag.ne.1)itr_number=itr_number+1
        if(iflag.ne.1)itr_count=itr_count+1
        
        if (itr_number.lt.50.or.itr_count.eq.50) then
        
c *** lcs begin hjanr plugin for interactive plots 
        if (grfopt.ne.'N') then       
        do i=1,30
          xnew(i)=sngl(xval(i))
        enddo
        call hjanr_plot 
        endif
c *** lcs end hjanr plugin for interactive plots        
        
        write(6,*)
        write (*,*)'Iteration time = ',timdd,'  Total time = ',time_add
        
        write(6,*)'Iteration = ',itr_number,' Value to minimize',fval,' Reduced Chi2',fval/float(ndf)
        write(6,*)'No. fitted param.',int(parms-fix_param),' No. fitted points',int(fitted_points)
        write(26,*)'Iteration = ',itr_number,' Value to minimize',fval,' Reduced Chi2',fval/float(ndf)
        
        do ip=1,parms
          if((par_stat(ip).le.0.and.fitop.eq.0).or.(abs(par_stat(ip)).eq.1.and.fitop.eq.1))then
            write(6,*)pname(ip),'  ',sngl(xval(ip))
            write(26,*)pname(ip),'  ',xval(ip)
          endif
        enddo
        if (itr_count.eq.50) itr_count=0
        endif
        
      ENDIF
 133  format(i7,8f9.4,f10.5)
 134  format(2i7,7f8.3)
        
      RETURN
      
 999  WRITE(6,*) 'Error while reading the data file',W,Q2,epsilon,costh,phi,sec,err1,
     +        nb,ac,rad,dum,pi_type,obsv
 
      RETURN
      END
      
      subroutine prterr(rq2)

      implicit none
#include "fit_par.inc"
#include "janr_data.inc"
#include "amplitudes.inc"
      double precision fmin,fedm,errdef,eplus,eminus,eparab,globcc,parv,parerr,bn1,bn2
      integer npari,nparx,istat,i,iv
      CHARACTER*10 pnamei(35)
      character*9 rnam(9)
      real dum,rq2
      data rnam/'P33(1232)','P11(1440)','S11(1535)','D13(1520)','S11(1650)','S31(1620)',
     1'F15(1680)','D33(1700)','P13(1720)'/

      OPEN (UNIT=27,FILE=parofile,STATUS='UNKNOWN')
      
      call mnstat(fmin,fedm,errdef,npari,nparx,istat)
      write(6,*)
      write(6,*) 'Minimized function=',fmin,'Reduced Chi2=',fmin/ndf
      write(6,*) 'Estimated distance to minimum',fedm
      write(6,*)
      write(6,*) 'Parameters and errors'

      do i=1,parms
         call mnpout(i,pnamei(i),parv,parerr,bn1,bn2,iv)
         if(par_stat(i).le.0)then
            call mnerrs(-i,eplus,eminus,eparab,globcc)
            write(6,*)i,'  ',pnamei(i),parv,parerr,bn1,bn2
            write(6,*)i,eplus,eminus,eparab,globcc
            write(f_out,*)i,'  ',pnamei(i),parv,parerr
            write(27,53)plist(i),pname(i),parv,parerr,
     +         par_stat(i),low_lim(i),up_lim(i)
         else
            write(27,53)plist(i),pname(i),start_value(i),step_size(i),
     +         par_stat(i),low_lim(i),up_lim(i)
         endif
      enddo

      close(27)
      write(f_out,40) (rnam(i),i=1,9)
40    format(4x,9(1x,a9))
      write(f_out,41) 'AM=',am3(1),am1(1,1),dum,am1(1,3),dum,dum,am1(1,6),am3(2),am1(1,7)
      write(f_out,41) 'AE=',ae3(1),dum,ae1(1,2),ae1(1,3),ae1(1,5),ae3(5),ae1(1,6),ae3(2),ae1(1,7)
      write(f_out,41) 'AS=',as3(1),as1(1,1),as1(1,2),as1(1,3),as1(1,5),as3(5),
     1                as1(1,6),as3(2),as1(1,7)
      call hjanr_loadpar(5)
      call janr_ini_point(rq2)
      write(f_out,41) 'AM=',am3(1),am1(1,1),dum,am1(1,3),dum,dum,am1(1,6),am3(2),am1(1,7)
      write(f_out,41) 'AE=',ae3(1),dum,ae1(1,2),ae1(1,3),ae1(1,5),ae3(5),ae1(1,6),ae3(2),ae1(1,7)
      write(f_out,41) 'AS=',as3(1),as1(1,1),as1(1,2),as1(1,3),as1(1,5),as3(5),
     1                as1(1,6),as3(2),as1(1,7)
41    format(A3,9(4x,f6.3))
53    format(i2,1x,A10,2f10.4,i10,2f10.4)

      return
      end






