      SUBROUTINE EVNT_2_BOS ( )
c
c_begin_doc
c  RCS ID string
c  $Id: evnt_2_bos.F,v 1.47 2008/03/16 04:33:22 fklein Exp $
c
c  Documentation for subroutine EVNT_2_BOS
c
c  Purpose:  Fills output BOS banks after event building.
c  --------
c
c  Input Parameters:   NON
c  ----------------
c
c  Output Parameters:  NON
c  -----------------
c
c  Other routines:
c  ---------------
c
c  Notes:
c  ------
c
c  Author:    S. Stepanyan     Created:  Sun Mar 23 20:53:13 EST 1997
c  -------
c
c  Major revisions:
c  ----------------
c
c
c_end_doc
c
      IMPLICIT NONE
      SAVE
c
c_begin_inc
c  include files :
c  ---------------------
c BOS common block  uncomment the next line for BOS include file
#include "bcs.inc"
#include "clasmdl.inc"
#include "evnt_general.inc"
#include "echits.inc"
#include "sthits.inc"
#include "particle_out.inc"
#include "hbt_planes.inc"
#include "dc_inf.inc"
#include "cc_inf.inc"
#include "sc_inf.inc"
#include "ec_inf.inc"
#include "tbtinf.inc"
#include "lec_inf.inc"
#include "st_inf.inc"
#include "tag_inf.inc"
#include "piddef.inc"
#include "pdgid.inc"
#include "ecifl.inc"

c_end_inc
c
c_begin_var
c  input/output variables:
c  -----------------------
c
c  Local pre-defined variables:
c  ---------------------------
c  RCS information:
      CHARACTER*132  CFILE, CREVIS, CSTATE, CDATE, CAUTHO, CRCSID
      PARAMETER (CFILE = '$RCSfile: evnt_2_bos.F,v $')
      PARAMETER (CREVIS = '$Revision: 1.47 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 2008/03/16 04:33:22 $')
      PARAMETER (CAUTHO = '$Author: fklein $')
      DATA CRCSID/
     1'$Id: evnt_2_bos.F,v 1.47 2008/03/16 04:33:22 fklein Exp $'
     2/
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='EVNT_2_BOS')
      PARAMETER (CRAUTH='Stepan Stepanyan')
c
c  Local User defined variables:
c  -----------------------------
      REAL pi, ab
      DATA pi/3.1415926/
      INTEGER NWRITE, UNUSED, ITGBI, ib16, ib15,ibset(16)
      data ITGBI,ib16,ib15/0,32768,16384/
      INTEGER AS, DS, J,itr,ihlst,iclc,evntcls
      INTEGER i,ia,ic,indx,ind,nrow,ind1,indx1,indv,indhevt
      INTEGER ihd,indh
      INTEGER mbank, mamind, MAX, scpln, namivr, ISHFT
      EXTERNAL mbank, mamind
      integer oi,oj,wi,wj,ii,ij,ngdc,ivrt1,ivrt2,id_vr(MaxParticles),stat2ec(MaxParticles),id2ec(MaxParticles)
c      real eioi,eioj,deioi
      real vert_x(MaxParticles),vert_y(MaxParticles),vert_z(MaxParticles),vert_r(MaxParticles),angl
c     _end_var

c
c     executable code for routine EVNT_2_BOS:
c---- 6----------------------------------------------------------------72
c
      IF (NWRITE .LT. 1) THEN
         NWRITE = NWRITE + 1
         CRMESS='Writing out event BOS banks (mDST).'
         CALL RECMES(CRNAME,'I',CRMESS)
         ITGBI=MAMIND(IW,'TGBI')
      ENDIF
c- Get Helicity bits from TGBI bank
c
      if(iTGBI.gt.0)ind=IW(ITGBI)
      if(ind.gt.0)then
         do i=0,15
c            ibset(i+1)=BTEST(iw(ind+1),i)
c            BTEST was replaced by ISHFT/AND here due to
c            dependency issues. (Johann Goetz Aug 2008)
           if(AND(iw(ind+1),ISHFT(1,i)).NE.0) then
             ibset(i+1) = 1
           else
             ibset(i+1) = 0
           endif
c            print *,i,ibit
         enddo
         ihlst=int(iw(ind+1)/ib16)
         iclc=int(mod(iw(ind+1),ib16)/ib15)
         if (iclc.eq.0)then     ! either clock is broken, or GSIM data
            ihd=0
            ihd=mamind(iw,'HEAD') ! check event type
            if(ihd.gt.0)indh=IW(ihd)
            if(iw(indh+5).lt.0)then ! simulated data!
               iclc=1           ! make non-zero so that ntuple data shows helicity
            endif
         endif
         itr=int(iw(ind+1)-ihlst*ib16-iclc*ib15-ibset(13)*(2)**12-ibset(14)*(2)**13)
         evntcls=(10000*iclc+itr)*(-1)**ihlst
c         print *,'TGBI',iw(ind+1),ihlst,iclc,itr
      endif


C     - Begin with HEVT
      ind = MBANK(IW,'HEVT',0,16,1)
      IF(ind.LE.0) THEN
         WRITE(CRMESS,10) ind
         CALL RECMES(CRNAME,'F',CRMESS)
         GO TO 999
      ENDIF
c
      IW(ind + 1) = EVENT_STAT
      IW(ind + 2) = NCCRUN
      IW(ind + 3) = NEVENT
      IW(ind + 4) = EVTYPE
      IW(ind + 5) = NPARTICLES*100+GPARTICLES
c     Helicity information in EVTWGHT are in bits 14 and 15
      IW(ind + 6) = EVNTCLS
      RW(ind + 7) = Q_t
      RW(ind + 8) = Q_t_l
      RW(ind + 9) = T_l_T
      RW(ind + 10)= TTIME
      RW(ind + 11)= grf
      IW(ind + 16)= EVTIME
      indhevt=ind
C     - HEVT filled
C     - Go to EVNT
      indx=0
      nrow = MAX(nparticles,gparticles)
      IF ( nrow.GT.0 ) THEN
         ind = MBANK(IW,'EVNT',0,18,nrow)
         IF(ind.LE.0) THEN
            WRITE(CRMESS,11) ind
            CALL RECMES(CRNAME,'F',CRMESS)
            GO TO 999
         ENDIF
         ivrt1=0
         angl=1.
c
         DO i=1,NPARTICLES
            indx=18*(i-1)+ind
            IW(indx+1) = evID(i)
            RW(indx+2) = evp(i)
            RW(indx+3) = evm(i)
            IW(indx+4) = evq(i)
            RW(indx+5) = evbetta(i)
            RW(indx+6) = evCX(i)
            RW(indx+7) = evCY(i)
            RW(indx+8) = evCZ(i)
            RW(indx+9) = evVERTX(i)
            RW(indx+10) = evVERTY(i)
            RW(indx+11) = evVERTZ(i)
            IW(indx+12) = evSTDC(i)
            IW(indx+13) = evSTCC(i)
            IW(indx+14) = evSTSC(i)
            IW(indx+15) = evSTEC(i)
            IW(indx+16) = evSTLEC(i)
            IW(indx+17) = evSTST(i)
            IW(indx+18) = evSTATUS(i)
            If(evSTDC(i).gt.0)Then
               if(dcstatus(evSTDC(i)).gt.0)then
                  if(i.eq.1)then
                     ivrt1=dcitrk(evSTDC(i))
                  elseif(i.ne.1.and.abs(evCZ(i)).lt.angl)then
                     angl=abs(evCZ(i))
                     ivrt2=dcitrk(evSTDC(i))
                  endif
               endif
            EndIf
            If( evSTEC(i).gt.0 ) Then
               stat2ec(evSTEC(i))=evSTATUS(i)
	       id2ec(evSTEC(i))=evID(i)
            EndIf
         ENDDO
      ENDIF
c     Get the vertex
      call bdrop(IW,'VERT')
      call vertex_tbtr
      INDV=0
      ngdc=0
      IF(NAMIVR.EQ.0)NAMIVR=MAMIND(IW,'VERT')
      IF(NAMIVR.GT.0)INDV=IW(NAMIVR)
      IF(INDV.GT.0)THEN
         Do i=1,IW(indv),iw(indv-5)
            indx=indv+i-1
            if(iw(indx+3).gt.0)Then
               vert_x(iw(indx+3))=rw(indx+4)
               vert_y(iw(indx+3))=rw(indx+5)
               vert_z(iw(indx+3))=rw(indx+6)
               vert_r(iw(indx+3))=rw(indx+7)
               ngdc=ngdc+1
               id_vr(ngdc)=iw(indx+3)
            endif
         EndDo
      ENDIF
c      If(ivrt1.gt.0)then
c         RW(indhevt + 12)= vert_r(ivrt1)
c         RW(indhevt + 13)= vert_x(ivrt1)
c         RW(indhevt + 14)= vert_y(ivrt1)
c         RW(indhevt + 15)= vert_z(ivrt1)
c      EndIf
c
      ia=0
      IF (NPARTICLES.LT.GPARTICLES)THEN
         DO 100 i=1,GPARTICLES
            ic=1
            DO WHILE(NPARTICLES.GT.0.and.ic.le.NPARTICLES)
               IF(i.eq.gindex(ic)) go to 100
               ic=ic+1
            ENDDO
            ia=ia+1
            if(NPARTICLES+ia.GT.GPARTICLES)then
               WRITE(CRMESS,21)NPARTICLES,ia,GPARTICLES
               CALL RECMES(CRNAME,'F',CRMESS)
               GO TO 990
            endif
            indx=ind+18*(NPARTICLES+ia-1)
            IW(indx+1) = partID(i)
            RW(indx+2) = partp(i)
            RW(indx+3) = partm(i)
            IW(indx+4) = partq(i)
            RW(indx+5) = partbetta(i)
            RW(indx+6) = partCX(i)
            RW(indx+7) = partCY(i)
            RW(indx+8) = partCZ(i)
            RW(indx+9) = partVERTX(i)
            RW(indx+10) = partVERTY(i)
            RW(indx+11) = partVERTZ(i)
            IW(indx+12) = partSTDC(i)
            IW(indx+13) = partSTCC(i)
            IW(indx+14) = partSTSC(i)
            IW(indx+15) = partSTEC(i)
            IW(indx+16) = partSTLEC(i)
            IW(indx+17) = partSTST(i)
            IW(indx+18) = partSTATUS(i)
 100     CONTINUE
      ENDIF
 990  CONTINUE
C     - Start **PB banks for all detectors.
      IF (DPARTICLES.GT.0) THEN
         ind = MBANK(IW,'DCPB',0,13,DPARTICLES)
         ind1 = MBANK(IW,'TRPB',0,8,DPARTICLES)
         IF(ind.LE.0) THEN
            WRITE(CRMESS,12) ind
            CALL RECMES(CRNAME,'W',CRMESS)
            GO TO 991
         ENDIF
         IF(ind1.LE.0) THEN
            WRITE(CRMESS,12) ind1
            CALL RECMES(CRNAME,'W',CRMESS)
         ENDIF
         DO i=1,DPARTICLES
            indx=13*(i-1)+ind
            IW(indx+1) = dcSECTOR(i)*100+dcITRK(i)
            scpln=0
            if(partSTSC(DCPART(i)).gt.0)scpln=sctrk_pln(partSTSC(DCPART(i)))
            IF(scpln.GT.0.and.scpln.lt.9)then
               RW(indx+2) = tbt_x(scpln,dcitrk(i))
               RW(indx+3) = tbt_y(scpln,dcitrk(i))
               RW(indx+4) = tbt_z(scpln,dcitrk(i))
               RW(indx+5) = tbt_cx(scpln,dcitrk(i))
               RW(indx+6) = tbt_cy(scpln,dcitrk(i))
               RW(indx+7) = tbt_cz(scpln,dcitrk(i))
            ELSE
               RW(indx+2) = 0.
               RW(indx+3) = 0.
               RW(indx+4) = 0.
               RW(indx+5) = 0.
               RW(indx+6) = 0.
               RW(indx+7) = 0.
            ENDIF
            RW(indx+8) = tbt_x(9,dcitrk(i))
            RW(indx+9) = tbt_y(9,dcitrk(i))
            RW(indx+10) = tbt_z(9,dcitrk(i))
            RW(indx+11) = 0.
            if(sqrt(tbt_x(4,dcitrk(i))**2+tbt_y(4,dcitrk(i))**2+tbt_z(4,dcitrk(i))**2).gt.1.)then
               RW(indx+11) = tbt_z(4,dcitrk(i))/
     +              sqrt(tbt_x(4,dcitrk(i))**2+tbt_y(4,dcitrk(i))**2+tbt_z(4,dcitrk(i))**2)
            endif
c
            Do j=1,ngdc
               if(dcSTATUS(i).gt.0.and.id_vr(j).eq.dcITRK(i))then
                  RW(indx+8) = vert_x(id_vr(j))
                  RW(indx+9) = vert_y(id_vr(j))
                  RW(indx+10) = vert_z(id_vr(j))
                  RW(indx+11) = vert_r(id_vr(j))
               endif
            EndDo
            RW(indx+12) = dcCHI2(i)
            IW(indx+13) = dcSTATUS(i)
c     Layer 1 information.
            if(ind1.gt.0)then
               indx1=8*(i-1)+ind1
               IW(indx1+1) = tbt_lst(dcitrk(i))
               RW(indx1+2) = tbt_x(11,dcitrk(i))
               RW(indx1+3) = tbt_y(11,dcitrk(i))
               RW(indx1+4) = tbt_z(11,dcitrk(i))
               RW(indx1+5) = tbt_cx(11,dcitrk(i))
               RW(indx1+6) = tbt_cy(11,dcitrk(i))
               RW(indx1+7) = tbt_cz(11,dcitrk(i))
               RW(indx1+8) = tbt_r(11,dcitrk(i))
            endif
         ENDDO
      ENDIF
 991  CONTINUE
      IF (EPARTICLES.GT.0) THEN
         ind = MBANK(IW,'ECPB',0,16,EPARTICLES)
         IF(ind.LE.0) THEN
            WRITE(CRMESS,13) ind
            CALL RECMES(CRNAME,'W',CRMESS)
            GO TO 992
         ENDIF
         DO i=1,EPARTICLES
            If(stat2ec(i).gt.0.and.EPARTICLES.Gt.1.and.i.ne.EPARTICLES)Then
               oi=int(ecstatus(i)/10000)
               ii=int((ecstatus(i)-oi*10000)/100)
               wi=int(ecstatus(i)-oi*10000-ii*100)
               Do j = i+1,EPARTICLES
                  oj=int(ecstatus(j)/10000)
                  ij=int((ecstatus(j)-oj*10000)/100)
                  wj=int(ecstatus(j)-oj*10000-ij*100)
                  If(stat2ec(j).gt.0.and.ecSECTOR(i).eq.ecSECTOR(j).and.wj.eq.wi)then
                     if(ii.eq.ij.and.oi.ne.oj)then
                        ece_tot(i)=ece_tot(i)-ece_out(j)
                        ece_tot(j)=ece_tot(j)-ece_out(i)
                     elseif(oi.eq.oj.and.ii.ne.ij)then
                        ece_tot(i)=ece_tot(i)-ece_in(j)
                        ece_tot(j)=ece_tot(j)-ece_in(i)
                     endif
                  EndIf
               EndDo
            EndIf
            indx=16*(i-1)+ind
            IW(indx+1) = ecSECTOR(i)*100+echit(WHOLE,i)
            RW(indx+2) = ecE_TOT(i)
            RW(indx+3) = ecE_IN(i)
            RW(indx+4) = ecE_OUT(i)
            RW(indx+5) = ecTIME(i)
            RW(indx+6) = ecR_TRK(i)
            RW(indx+7) = eCX(i)
            RW(indx+8) = eCY(i)
            RW(indx+9) = eCZ(i)
            RW(indx+10) = sqrt(Ec_peak(mom2,EChit(whole,i),Ec_U,whole,ecsector(i))**2+
     +           Ec_peak(mom2,EChit(whole,i),Ec_V,whole,ecsector(i))**2+
     +           Ec_peak(mom2,EChit(whole,i),Ec_W,whole,ecsector(i))**2)
            RW(indx+11) =sqrt(Ec_peak(mom3,EChit(whole,i),Ec_U,whole,ecsector(i))**2+
     +           Ec_peak(mom3,EChit(whole,i),Ec_V,whole,ecsector(i))**2+
     +           Ec_peak(mom3,EChit(whole,i),Ec_W,whole,ecsector(i))**2)
            RW(indx+12) =sqrt(Ec_peak(mom4,EChit(whole,i),Ec_U,whole,ecsector(i))**2+
     +           Ec_peak(mom4,EChit(whole,i),Ec_V,whole,ecsector(i))**2+
     +           Ec_peak(mom4,EChit(whole,i),Ec_W,whole,ecsector(i))**2)
            IW(indx+13)=0
            if(EChit(inner,i).gt.0)then
               IW(indx+13) =ecfit_nhitU(EChit(inner,i),Inner,ecsector(i))
     +              +ecfit_nhitV(EChit(inner,i),Inner,ecsector(i))*100
     +              +ecfit_nhitW(EChit(inner,i),Inner,ecsector(i))*10000
            endif
            IW(indx+14)=0
            if(EChit(outer,i).gt.0)then
               IW(indx+14) =ecfit_nhitU(EChit(outer,i),Outer,ecsector(i))
     +              +ecfit_nhitV(EChit(outer,i),Outer,ecsector(i))*100
     +              +ecfit_nhitW(EChit(outer,i),Outer,ecsector(i))*10000
            endif
            RW(indx+15) = partCHI2EC(whole,ECPART(i))
c******************************************************************
c         Discrimination between gamma and pi0->2gamma  hits.
c  If overlapping of two clusters in EC is found, ecSTATUS is negative.
c  Valid only for photon/pi0 hits. This procedure is also appied to
c  neutron (id=2112)  or unrecognised (id=0) hits assuming
c  that some photons may be misidentified.

	   if(ifl(EChit(whole,i),ecsector(i)).gt.0.and.
     &     (id2ec(i).eq.22.or.id2ec(i).eq.2112.or.id2ec(i).eq.0))
     &     ecSTATUS(i)=-ecSTATUS(i)

c******************************************************************

            IW(indx+16) = ecSTATUS(i)
         ENDDO
      ENDIF
 992  CONTINUE
      IF (SPARTICLES.GT.0) THEN
         ind = MBANK(IW,'SCPB',0,6,SPARTICLES)
         IF(ind.LE.0) THEN
            WRITE(CRMESS,14) ind
            CALL RECMES(CRNAME,'W',CRMESS)
            GO TO 993
         ENDIF
         DO i=1,SPARTICLES
            indx=6*(i-1)+ind
            IW(indx+1) = scSECTOR(i)*10000+scpdid(i)*100+scIHIT(i)
            RW(indx+2) = scE_DEP(i)
            RW(indx+3) = scTIME(i)
            RW(indx+4) = scR_TRK(i)
            RW(indx+5) = partCHI2SC(scPART(i))
            IW(indx+6) = scSTATUS(i)
         ENDDO
      ENDIF
 993  CONTINUE
      IF (CPARTICLES.GT.0) THEN
         ind = MBANK(IW,'CCPB',0,6,CPARTICLES)
         IF(ind.LE.0) THEN
            WRITE(CRMESS,15) ind
            CALL RECMES(CRNAME,'W',CRMESS)
            GO TO 994
         ENDIF
         DO i=1,CPARTICLES
            indx=6*(i-1)+ind
            IW(indx+1) = ccSECTOR(i)*100+ccIHIT(i)
            RW(indx+2) = ccNPHE(i)
            RW(indx+3) = ccTIME(i)
            RW(indx+4) = ccR_TRK(i)
            RW(indx+5) = partCHI2CC(ccPART(i))
            IW(indx+6) = ccSTATUS(i)
         ENDDO
      ENDIF
 994  CONTINUE
      IF (LPARTICLES.GT.0) THEN
         ind = MBANK(IW,'LCPB',0,10,LPARTICLES)
         IF(ind.LE.0) THEN
            WRITE(CRMESS,13) ind
            CALL RECMES(CRNAME,'W',CRMESS)
            GO TO 995
         ENDIF
         DO i=1,LPARTICLES
            indx=10*(i-1)+ind
            IW(indx+1) = lecSECTOR(i)*100+lechit(1,i)
            RW(indx+2) = lecE(i)
            RW(indx+3) = lecTIME(i)
            RW(indx+4) = lecR_TRK(i)
            RW(indx+5) = lecX(i)
            RW(indx+6) = lecY(i)
            RW(indx+7) = lecZ(i)
            RW(indx+8) = partchi2lec(lecPART(i))
            IW(indx+9) = lecSTATUS(i)
            RW(indx+10) = lecEi(i)
         ENDDO
      ENDIF
 995  CONTINUE
      IF (STPARTICLES.GT.0) THEN
         ind = MBANK(IW,'STPB',0,5,STPARTICLES)
         IF(ind.LE.0) THEN
            WRITE(CRMESS,17) ind
            CALL RECMES(CRNAME,'W',CRMESS)
            GO TO 996
         ENDIF
         DO i=1,STPARTICLES
            indx=5*(i-1)+ind
            if(sttype.eq.0) then
               IW(indx+1) = stSECTOR(i)*100+stIHIT(i)
            else
               IW(indx+1) = stid(i)
            endif
            RW(indx+2) = stTIME(i)
            RW(indx+3) = stR_TRK(i)
            IW(indx+4) = sttrkno(i)
            IW(indx+5) = stSTATUS(i)
         ENDDO
      ENDIF
 996  CONTINUE
c
      IF (GOODTAG.GT.0) THEN
         ind = MBANK(IW,'TGPB',0,4,GOODTAG)
         IF(ind.LE.0) THEN
            WRITE(CRMESS,18) ind
            CALL RECMES(CRNAME,'W',CRMESS)
            GO TO 997
         ENDIF
         DO i=1,GOODTAG
            indx=4*(i-1)+ind
            IW(indx+1) = tagptr(i)
            RW(indx+2) = tagtime(i)
            RW(indx+3) = tagenergy(i)
            RW(indx+4) = dt_sttag(i)
         ENDDO
      ENDIF
 997  CONTINUE

 10   FORMAT('Error in MBANK for HEVT, ind=',I6)
 11   FORMAT('Error in MBANK for EVNT, ind=',I6)
 12   FORMAT('Error in MBANK for DCPB, ind=',I6)
 13   FORMAT('Error in MBANK for ECPB, ind=',I6)
 14   FORMAT('Error in MBANK for SCPB, ind=',I6)
 15   FORMAT('Error in MBANK for CCPB, ind=',I6)
 16   FORMAT('Error in MBANK for UNUS, ind=',I6)
 17   FORMAT('Error in MBANK for STPB, ind=',I6)
 18   FORMAT('Error in MBANK for TGPB, ind=',I6)
 21   FORMAT('Error on number of particles',3I6)
      RETURN
 999  CONTINUE
      RETURN
      END
c
c------------------------------------------------------------------------------




