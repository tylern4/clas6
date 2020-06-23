      Subroutine fsgen_evnt(ierr,nevent)
c     ---------------
c     
c     Notes:
c     ------
c     
c     Author:    S.Stepanyan     Created:  Tue Mar 25 14:32:25 EST 1997
c     -------
c     
c     Major revisions:
c     ----------------
c     
      IMPLICIT NONE
      SAVE
C     
#include "parm1.inc"
#include "parm3.inc"
#include "leptou.inc"
#include "ludat.inc"
#include "lujet.inc"
#include "gbd.inc"
      REAL CM_MOM(4)
      REAL PI,Tgen,user_tdep,user_costhcm,dual_tgen
      external  user_tdep,Tgen,user_costhcm,dual_tgen
      double precision f_lambda
      data pi /3.14159265/
      REAL ecm_t, pcm_t
      REAL PCM_1(4),PCM1(4),tpart(10)
      REAL e_beam, p_beam, e_targ, p_targ
      REAL ts, t_dip, a1, a2, rmass2
      INTEGER i,i1,i_ve,locr,lineold
      REAL getrand, costhcm,phicm,tmin,tmax,wtr,tt,tcheck
      INTEGER ierr, nevent, itype
      Logical phipass
      external phipass
C     
C     ****  Calculate CM four-momenta of incoming particles
C           (proton+photon(virt))
C     
c      kgenev=2   
      CALL A4(TECM**2,PHMASS,TMASS,E_BEAM,P_BEAM)
      E_TARG=SQRT(P_BEAM**2+TMASS**2)
      P_TARG=P_BEAM
C     - Get CM momentum and energy
      CM_MOM(4)=U+etarget
      CM_MOM(1)=(qx+ptarg(1))
      CM_MOM(2)=(qy+ptarg(2))
      CM_MOM(3)=(qz+ptarg(3))
C     
      locr=0
      lineold=line
      If(T_SLOP.LE.0.00001.or.npart.eq.1)Then
c     Phase space
 776     continue
         ierr=1
         if(locr.ge.10)return
         locr=locr+1
         CALL GBD(nevent)
         call hf2(101,tecm,wt/wt0,1.)
c     
         wtr=getrand(-1.)*wt0
         if(kgenev.gt.0.and.wtr.gt.wt)return
         if(kgenev.lt.0.and.wtr.lt.wt)return
c         if(wtr.gt.wt.and.t_slop.eq.0.00001)return
         A1=(E_BEAM+E_TARG-PCM(4,1))**2
         A2=PCM(5,1)**2
         RMASS2=A1-A2
         if(rmass.lt.0.)then
            goto776
            print *,rmass,a1,e_beam,e_targ,pcm(4,1),pcm(5,1)
         endif
         rmass=sqrt(RMASS2)
         T=TMASS**2+amass(1)**2-2.*(PCM(4,1)*E_TARG-P_TARG*PCM(3,1))
         tmin=tmass**2+amass(1)**2-2.*E_TARG*pcm(4,1)+2.*abs(P_TARG)
     &       *pcm(5,1)
         tmax=tmass**2+amass(1)**2-2.*E_TARG*pcm(4,1)-2.*abs(P_TARG)
     &       *pcm(5,1)
         if(t_min.lt.0.and.t_max.lt.0.and.t_max.lt.t_min)then
            if(t_min.lt.tmin)tmin=t_min
            if(t_max.gt.tmax)tmax=t_max
         endif
         If(T_SLOP.lt.0)then
            ts=t_slop
            tt=tgen(abs(tmin),abs(tmax),ts)
            if(abs(t).gt.tt)then
               goto 776
               ierr=1
            endif
         endif
         if(t.gt.tmin.or.t.lt.tmax)then
            goto 776
         endif
         do i=2,npart
            tpart(i)=amass(i)**2-2.*P_BEAM*(PCM(4,i)-PCM(3,i))
            if(tdep_slop(i).lt.0)then
               if(t_min.lt.0.and.t_max.lt.0.and.t_max.lt.t_min)then
                  tmin=t_min
                  tmax=t_max
               else
                  tmin=amass(i)**2-2.*P_BEAM*(PCM(4,i)-PCM(5,i))
                  tmax=amass(i)**2-2.*P_BEAM*(PCM(4,i)+PCM(5,i))
               endif
               ts=tdep_slop(i)
               tt=tgen(abs(tmin),abs(tmax),ts)
               call hf1(103,abs(tpart(i)),1.)
               if(abs(tpart(i)).ge.10.or.abs(tpart(i)).gt.tt)then
                  goto 776
               endif
               call hf1(104,abs(tpart(i)),1.)
            endif
         enddo
         ierr=0
         call repart(cm_mom,1,npart,0)
      ElseIf(T_SLOP.GT.0.)Then
c     - This assums coherent scattering off the target
         rmass=amass(2)
         if(npart.gt.2)then
            do i=3,npart
               rmass=rmass+amass(i)
            enddo
            rmass=rmass+getrand(-1.)*(tecm-amass(1)-rmass)
         endif
c     
         pcm(5,1)=sngl(dsqrt(f_lambda(tecm**2,rmass**2,amass(1)**2)))
     &       /2./tecm
         pcm(4,1)=(tecm**2+amass(1)**2-rmass**2)/2./tecm
         if(t_slop.eq.200.)then
            costhcm=user_costhcm(u)
            t=tmass**2+amass(1)**2-2.*(E_TARG*pcm(4,1)-abs(P_TARG)
     &          *pcm(5,1)*costhcm)
         else
            tmin=tmass**2+amass(1)**2-2.*E_TARG*pcm(4,1)+2.*abs(P_TARG)
     &          *pcm(5,1)
            tmax=tmass**2+amass(1)**2-2.*E_TARG*pcm(4,1)-2.*abs(P_TARG)
     &          *pcm(5,1)
            if(t_min.lt.0.and.t_max.lt.0.and.t_max.lt.t_min)then
               if(t_min.lt.tmin)tmin=t_min
               if(t_max.gt.tmax)tmax=t_max
            endif
            if(t_slop.eq.100.)then
               ts=user_tdep(u)
               t=-tgen(abs(tmin),abs(tmax),ts)
            elseif(t_slop.eq.300)then
               t=-dual_tgen(abs(tmin),abs(tmax))
            else
               ts=t_slop
               t=-tgen(abs(tmin),abs(tmax),ts)
            endif
            itype=1
            if(t.ge.tmin.or.t.lt.tmax)then
               ierr=1
               return
            endif
            costhcm=(t-(tmass**2+amass(1)**2-2.*E_TARG*pcm(4,1)))
     &          /2./abs(P_TARG)/pcm(5,1)
         endif
         call hf1(211,costhcm,1.)
         phicm=pi-2.*getrand(-1.)*pi
         pcm(1,1)=pcm(5,1)*sqrt(1.-costhcm**2)*cos(phicm)
         pcm(2,1)=pcm(5,1)*sqrt(1.-costhcm**2)*sin(phicm)
         pcm(3,1)=-pcm(5,1)*costhcm
         tcheck=tmass**2+amass(1)**2-2.
     &       *(E_TARG*pcm(4,1)-p_targ*pcm(3,1))
         call repart(cm_mom,1,1,0)
c     
         pcm(1,2)=-pcm(1,1)
         pcm(2,2)=-pcm(2,1)
         pcm(3,2)=-pcm(3,1)
         pcm(5,2)=pcm(5,1)
         pcm(4,2)=sqrt(pcm(5,2)**2+rmass**2)
         phicm=phicm+pi
         if(phicm.gt.pi)phicm=phicm-2.*pi
         if(npart.eq.2)then
            call repart(cm_mom,2,npart,0)
         elseif(npart.gt.2)then
            do i=2,npart
               amass(i-1)=amass(i)
            enddo
            np=npart-1
            DO I1 = 1,4
               PCM_1(I1)=PCM(I1,2)
            ENDDO
c            CALL LOREN4(CM_MOM,PCM_1,PCM1)
            CALL LORENB(tecm,CM_MOM,PCM_1,PCM1)
            do i=1,3
               cm_mom(i)=pcm1(i)
            enddo
            cm_mom(4)=pcm1(4)
            tecm=rmass
            CALL GBD(nevent)
            call repart(cm_mom,2,npart,1)
         endif
      EndIf
c     
      return
      end




