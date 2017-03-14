      SUBROUTINE DASPE_FMIN(npar,grad,fval,xval,iflag,futil)
c     
c     _begin_doc
c     RCS ID string
c     $Id$
c     
c     Documentation for subroutine DASPE_FMIN
c     
c     Purpose:
c     --------
c     
c     Input Parameters:  (Name - Type - Meaning)
c     ----------------
c     
c     Output Parameters:  (Name - Type - Meaning)
c     -----------------
c     
c     Other routines:
c     -------------
c     
c     Notes:
c     ------
c     
c     Author:  S. Stepanyan       Created:  Wed Jun 23 17:16:39 EST 1999
c     -------
c     
c     Major revisions:
c     ----------------
c     
c     
c     _end_doc
c     
      IMPLICIT NONE
      SAVE
c     
c     _begin_inc
c     include files :
c     ---------------------
#include "cross_sec.inc"
#include "fit_par.inc"
#include "daspe_data.inc"
#include "daspetab.inc"
c     _end_inc
c     
c     _begin_var
c     input/output variables:
c     -----------------------
      DOUBLE PRECISION grad,xval,fval
      dimension grad(50),xval(50)
      REAL  futil
      INTEGER iflag, npar
c     
c     Local pre-defined variables:
c     ---------------------------
c     RCS information: 
      CHARACTER*132  CFILE, CREVIS, CSTATE, CDATE, CAUTHO, CRCSID
      PARAMETER (CFILE = '$RCSfile$')
      PARAMETER (CREVIS = '$Revision$')
      PARAMETER (CSTATE = '$State$')
      PARAMETER (CDATE = '$Date$')
      PARAMETER (CAUTHO = '$Author$')
      DATA CRCSID/   
     1     '$Id$'   
     2     /   
c     Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      PARAMETER (CRNAME='DASPE_FMIN`')
      PARAMETER (CRAUTH='S.Stepanyan')
c     
c     Local User defined variables:
c     -----------------------------
c     
      INTEGER i,itr_number,Npoints_max,i_point,ierr
      integer itr_count,nhic,itr_bad,ip,N_zero,N_bad,fitop
      integer  obsv
      REAL diff,cross_calc,fitted_points
      REAL W,costh,Q2,epsilon,phi,sec,errors,syst
      REAL timdd, timdd1, timdd2, time_add, ac, rad,nb
      logical bad_xsec,bad_rad,bad_acc,bad_stat
      real amod
      real var_calc, var_meas , sigma_total
      real c2weight
      integer arg0, arg1, argt
      double precision norm_scale(10) 
      data norm_scale / 1., 1., 1., 1., 1., 1., 1., 1., 1., 1. /
c     _end_var
c     
c     executable code for routine DASPE_RUN:
c---- 6----------------------------------------------------------------72
      IF ( iflag .EQ. 1 ) THEN
         CALL TIMEX(timdd2)
         time_add=0.
         f_out=74
         OPEN (UNIT=f_out,FILE='minimise.inf',status='UNKNOWN')
         itr_number=0
         itr_count=0
         fval=0.
         Npoints_max=0
         N_zero=0
         N_bad=0
         fitop=1
         nhic=0
         IF( Datafile.NE.'JUNK' .OR. Datafile.NE.'junk' ) THEN
            OPEN (UNIT=44,FILE=Datafile,status='OLD')
            do i=1,MAXpoints
               if(datform.eq.'o'.or.datform.eq.'O')then
                  READ(44,*,err=999,end=998)W,Q2,epsilon,costh,
     +                 phi,sec,errors
                  obsv = 1
                  pi_type = pi_type2fit 
                  Npoints_max=Npoints_max+1
                  bad_xsec=sec.le.0..and.errors.le.0
                  if(bad_xsec)then
                     point_stat(Npoints_max)=1
                     N_zero=N_zero+1
                  else
                     point_stat(Npoints_max)=0
                  endif
c                  print *,i,n_zero,
               elseif(datform.eq.'n'.or.datform.eq.'N')then
                  READ(44,*,err=999,end=998)W,Q2,epsilon,costh,
     +              phi,sec,errors,nb,ac,rad,syst,pi_type,obsv
	errors=sqrt(errors**2+syst**2)
                  Npoints_max=Npoints_max+1
                  bad_xsec=sec.le.0..and.errors.le.0
                  bad_stat=nb.lt.5
                  bad_acc=ac.lt.0.05.or.(ac.lt.0.1.and.nb.lt.10)
                  bad_rad=rad.lt.1.
                  if(bad_xsec)then
c                  if(bad_xsec.or.bad_stat)then
                     point_stat(Npoints_max)=1
                     N_zero=N_zero+1
                  else
                     point_stat(Npoints_max)=0
                  endif
                  arg0 = 10**(obsv) 
                  arg1 = 10**(obsv-1)
                  pi_type2fit =  amod( float(obsv2fit), float(arg0) ) / arg1
               endif
c
               if(w.lt.wmin.or.w.gt.wmax)point_stat(Npoints_max)=1
               if(q2.lt.q2min.or.q2.gt.q2max)point_stat(Npoints_max)=1
               if(costh.lt.cosmin.or.costh.gt.cosmax)point_stat(Npoints_max)=1
               if(phi.lt.phimin.or.phi.gt.phimax)point_stat(Npoints_max)=1
C
               if ( ( ( pi_type .ne. pi_type2fit ) .AND. ( pi_type2fit .ne. 9 ) ) .OR. 
     |              ( pi_type2fit .eq. 0 ) ) then
                  point_stat(Npoints_max)=1 
               endif
C               
               Wd(Npoints_max)=W
               Q2d(Npoints_max)=Q2
               epsilond(Npoints_max)=epsilon
               costhd(Npoints_max)=costh
               phi_str(Npoints_max)=phi
               cross_sec(Npoints_max)=sec
               st_error(Npoints_max)=errors
               pi_typed(Npoints_max)=pi_type
               obsvd(Npoints_max)=obsv
            enddo    
 998        continue
            close(44)
            OPEN (UNIT=53,FILE='start_tab',status='UNKNOWN')
            Write(lmino,*)'Number of data points =  ',Npoints_max
            Write(lmino,*)'Number of NON zero data points =  ',Npoints_max-N_zero
            Write(f_out,*)'Number of data points =  ',Npoints_max
            Write(f_out,*)'Number of NON zero data points =  ',Npoints_max-N_zero
c     Write(6,*)
c     +'Order is -> W, Q2, epsilon, cos(*), phi*, sigma, err_st'
c     do i=1, Npoints_max       
c     write(6,*)Wd(i),Q2d(i),epsilond(i),costhd(i),
c     +azim_gradd(i),cross_sec(i),st_error(i)
c     enddo
C     
            DO I_point=1,Npoints_max
               do i=1,Nbad
                  if(i_point.gt.Ibad(1,i).and.I_point.lt.Ibad(2,i))then
                     point_stat(I_point)=1
                     N_bad=N_bad+1
                  endif
               enddo  
            ENDDO
            Write(lmino,*)'Number of data points for first iteration =  ',Npoints_max-N_zero-N_bad
            Write(f_out,*)'Number of data points for first iteration =  ',Npoints_max-N_zero-N_bad
         ENDIF
         write(6,111)(xval(i),i=1,parms)
         write(f_out,111)(xval(i),i=1,parms)
 111     format(20x,'Value of parameters at the begining => 1-49 ',/,
     +'   cM =',f9.5,'   cE =',f9.5,'   cS =',f9.5,/,
     +'   ap11 =',f9.5,' sp11 =',f9.5,'  as11 =',f9.5,' ss11 =',f9.5,/,
     +'   a1d13 =',f9.5,'   a3d13 =',f9.5,'   sd13 =',f9.5,/,
     +'   cas11 =',f9.5,' css11 =',f9.5,' cas31 =',f9.5,' css31 =',f9.5,/,
     +'   a1f15 =',f9.5,'   a3f15 =',f9.5,'   sf15 =',f9.5,/,
     +'   ca1d33 =',f9.5,'   ca3d33 =',f9.5,'   csd33 =',f9.5,/,
     +'   ca1d13 =',f9.5,'   ca3d13 =',f9.5,'   csd13 =',f9.5,/,
     +'   cap11 =',f9.5,'   csp11 =',f9.5,/,
     +'   a1d15 =',f9.5,'   a3d15 =',f9.5,'   sd15 =',f9.5,/,
     +'   a1p13 =',f9.5,'   a3p13 =',f9.5,'   sp13 =',f9.5,/,
     +'   a1p33 =',f9.5,'   a3p33 =',f9.5,'   sp33 =',f9.5,/,
     +     '   cw1 =',f9.5,'   cw2 =',f9.5,'   cw3 =',f9.5,/,
     +     '   cw4 =',f9.5,'   cw5 =',f9.5,'   cw6 =',f9.5,/,
     +     '   cw7 =',f9.5,'   cw8 =',f9.5,'   cw9 =',f9.5,/,
     +     '   cw10 =',f9.5,'   cw11 =',f9.5,'   cw12 =',f9.5,/,
     +     '   cw13 =',f9.5,'   cw14 =',f9.5,'   cw15 =',f9.5,/)      
	ELSEIF( iflag .EQ. 2 ) THEN
         fitop=0
      ELSEIF( iflag .EQ. 3 ) THEN
         call prterr
      ENDIF
         fval=0.0
         timdd1=timdd2
         CALL TIMEX(timdd2)
         timdd=timdd2-timdd1
         time_add=time_add+timdd
         write(6,*)
c         write (*,*)'time for iteration = ',timdd,'  Total time = ',time_add
         fitted_points=0
         cM=sngl(xval(1))
         cE=sngl(xval(2))
         cS=sngl(xval(3))
         ap11=sngl(xval(4))
         sp11=sngl(xval(5))
         as11=sngl(xval(6))
         ss11=sngl(xval(7))
         a1d13=sngl(xval(8))
         a3d13=sngl(xval(9))
         sd13=sngl(xval(10))
         cas11=sngl(xval(11))
         css11=sngl(xval(12))
         cas31=sngl(xval(13))
         css31=sngl(xval(14))
         a1f15=sngl(xval(15))
         a3f15=sngl(xval(16))
         sf15=sngl(xval(17))
         ca1d33=sngl(xval(18))
         ca3d33=sngl(xval(19))
         csd33=sngl(xval(20))
         ca1d13=sngl(xval(21))
         ca3d13=sngl(xval(22))
         csd13=sngl(xval(23))
         cap11=sngl(xval(24))
         csp11=sngl(xval(25))
         a1d15=sngl(xval(26))
         a3d15=sngl(xval(27))
         sd15=sngl(xval(28))
         a1p13=sngl(xval(29))
         a3p13=sngl(xval(30))
         sp13=sngl(xval(31))
         a1p33=sngl(xval(32))
         a3p33=sngl(xval(33))
         sp33=sngl(xval(34))
         cw1=sngl(xval(35))
         cw2=sngl(xval(36))
         cw3=sngl(xval(37))
         cw4=sngl(xval(38))
         cw5=sngl(xval(39))
         cw6=sngl(xval(40))
         cw7=sngl(xval(41))
         cw8=sngl(xval(42))
         cw9=sngl(xval(43))
         cw10=sngl(xval(44))
         cw11=sngl(xval(45))
         cw12=sngl(xval(46))
         cw13=sngl(xval(47))
         cw14=sngl(xval(48))
         cw15=sngl(xval(49))
c     
         DO I_point=1,Npoints_max
            if(point_stat(I_point).eq.0)then
               W=Wd(i_point)
               Q2=Q2d(i_point)
               epsilon=epsilond(i_point)
               costh=costhd(i_point)
               phi=phi_str(i_point)
               pi_type = pi_typed(i_point)
               obsv = obsvd(i_point)
               CALL DASPE_RUN(W,Q2,epsilon,costh,phi,ierr)
               IF(ierr.eq.0) THEN

                  if ( obsv .eq. 1 ) then
C     Cross section point
                     var_calc = sigma(pi_type)
                     var_meas = norm_scale(pi_type) * cross_sec(I_point)
                     diff=(( var_calc - var_meas )/st_error(i_point))**2
C     
                  else if ( obsv .eq. 2 ) then
C     5-th structure function point ( observable )
                     sigma_total  = sigma(pi_type)
                     var_calc = sigmaTLP(pi_type)   *
     *    sqrt( 2. * epsilon * (1.-epsilon) ) * sin(phi*acos(0.)/90.)/sigma_total
                     var_meas = cross_sec(I_point)
c                     write(6,*) 'Calc= ', var_calc, 'measured= ',   var_meas
                     c2weight = 3.
                     diff= c2weight * (( var_calc - var_meas )/st_error(i_point))**2
C
C     
                  else 
C     Wrong observable type
                  endif

                  if(diff.gt.chimax.and.itr_number.eq.0)then
                     nhic=nhic+1
                     point_stat(I_point)=1
                     write(55,133)nhic,I_point,W,Q2,costh,phi,cross_calc,cross_sec(I_point),diff
                  else
                     if(itr_number.eq.0)then
c                        write(53,133)I_point,W,Q2,epsilon,costh,phi,cross_calc,
                        write(53,133)I_point,W,Q2,epsilon,costh,phi,
     +cross_sec(I_point),st_error(i_point),var_calc,diff
                     endif
                     fitted_points=fitted_points+1.
                     fval=fval+diff
                  endif
               ENDIF
            endif
         ENDDO
         if(itr_number.eq.0)then
            Write(lmino,*)'The number of data points for fit =  ',Npoints_max-N_zero-N_bad-nhic
            Write(f_out,*)'The number of data points for fit =  ',Npoints_max-N_zero-N_bad-nhic
            close(53)
c            ndf=fitted_points-parms+fix_param
            ndf=fitted_points
         endif
         itr_number=itr_number+1
         itr_count=itr_count+1
         itr_bad=nhic
c     fval=fval/(fitted_points-float(parms)+fix_param)
c     if(itr_count.eq.10)then
         write(6,*)'Iteration = ',itr_number,'  Chi2 / NDF  ',fval/ndf
         write(6,*)' Number of data points ',int(fitted_points)
         write(f_out,*)'Iteration = ',itr_number,'    Value to minimize',fval,int(parms-fix_param),parms,int(fitted_points)
         write(26,*)'Iteration = ',itr_number,'    Value to minimize',fval,'  Chi2/point',fval/ndf
         do ip=1,parms
            if((par_stat(ip).le.0.and.fitop.eq.0).or.(abs(par_stat(ip)).eq.1.and.fitop.eq.1))then
               write(6,*)pname(ip),'  ',xval(ip)
               write(f_out,*)pname(ip),'  ',xval(ip)
               write(26,*)pname(ip),'  ',xval(ip)
            else
               write(6,*)pname(ip),'  ',xval(ip)
            endif
         enddo
c     itr_count=0
c     endif
c      ENDIF
 133  format(i7,9f8.3)
      RETURN
 999  continue
      write(6,*)'Error while reading the data file'
      RETURN
      END
c
c------------------------------------------------------------------------------




