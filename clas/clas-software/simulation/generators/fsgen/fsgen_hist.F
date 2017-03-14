      SUBROUTINE fsgen_hist(itest,nevent,fiducial)
c  ---------------
c
c  Notes:
c  ------
c
c  Author:    S.Stepanyan     Created:  Tue Mar 25 14:32:25 EST 1997
c  -------
c
c  Major revisions:
c  ----------------
c     
C
      IMPLICIT NONE
#include "parm2.inc"
#include "parm3.inc"
#include "parm1.inc"
#include "leptou.inc"
#include "ludat.inc"
#include "lujet.inc"
#include "pawc.inc"
#include "gbd.inc"
C
      REAL plu
C
      INTEGER lrecl
      INTEGER istat
      INTEGER icycle
      INTEGER ip,itest,nevent
c      PARAMETER (lrecl = 1024)
      PARAMETER (lrecl = 8191)
      LOGICAL fiducial
      real jp_xs
      external jp_xs
C
      INTEGER nevt, evstat
      REAL wp,q2g,t_T,m_T,Eg,wgt
      COMMON /mcevt0/ nevt,evstat,Eg,wp,q2g,t_T,m_T,wgt
      INTEGER max_part
      PARAMETER (max_part = 20)
      INTEGER gnpart
      INTEGER stcod(max_part)
      INTEGER pid(max_part)
      INTEGER prntid(max_part)
      REAL theta(max_part)
      REAL phi(max_part)
      REAL pm(max_part)
      REAL m(max_part)
      COMMON /mcevt2/ gnpart,stcod,pid,prntid,
     +theta,phi,pm,m
C
      IF (hnam.ne.'NO'.or.hnam.ne.'no')THEN
         IF ( itest.eq.1 ) THEN
C     ********************************************************
C     Booking histograms
C     ********************************************************
            print *,'Opening N-tuple file ',hnam
            CALL hropen(jhist,'hien',hnam,'N',lrecl,istat)
      call hbook2(101,' ',100,2.,5.,100,0.,2.,0.)
      call hbook1(102,' ',200,0.,10.,0.)
      call hbook1(103,' ',200,0.,10.,0.)
      call hbook1(104,' ',200,0.,10.,0.)
      call hbook1(201,' ',500,0.,1.,0.)
      call hbook1(211,'Cos(th)CM',100,-1.,1.,0.)
      call hbook1(301,' ',200,0.,4.,0.)
C     
            CALL hbnt(10,'hien',' ')
            CALL hbname(10,' ',0,'$clear')
            CALL hbname(10,'Kine',nevt,
     +        'nevt[0,20000000]:I,evstat:I,Eg:R,Wp:R,Q2g:R,t_T:R,M_T:R,
     +        wgt:R')
            CALL hbname(10,'gener',gnpart,
     +           'gnpart[0,20]:I,'//
     +           'stcod(gnpart)[0,100]:I,pid(gnpart)[-20000,20000]:I,
     +           prntid(gnpart)[0,20]:I,'//
     +           'theta(gnpart):R,phi(gnpart):R,pm(gnpart):R,m(gnpart):
     +           R')
         ELSEIF ( itest.eq.2 ) THEN
C     ********************************************************
C     Filling histograms
C     ********************************************************
            nevt=nevent
            gnpart=line
            wp=sqrt(w2)
            q2g=q2
            m_T=rmass
            t_T=t
            Eg = qv
            evstat=1
            if(t_slop.le.0.00001)then
               wgt=wt/wt0
            else
               wgt=0.
c               wgt=jp_xs(eg,t)
            endif
            if(.not.fiducial)evstat=-1
c            print *,nevt,eg,wp,q2g,t_t,m_t,fiducial
            do ip=1,line
               pm(ip)=plu(ip,8)
               theta(ip)=plu(ip,13)
               phi(ip)=plu(ip,15)
               stcod(ip)=k(ip,1)
               pid(ip)=k(ip,2)
               prntid(ip)=k(ip,3)
               m(ip)=p(ip,5)
c               print *,ip,pm(ip),theta(ip),phi(ip),stcod(ip),pid(ip),
c     &             prntid(ip),m(ip)
            enddo
            CALL hfnt(10)
         ELSEIF ( itest.eq.3 ) THEN
C     ********************************************************
C     Storing histograms
C     ********************************************************
            CALL hprnt(10)
            CALL hrout(0,icycle,' ')
            call hrend('hien')
            CLOSE(jhist)
         ENDIF
      ENDIF
C     
      RETURN
      END
