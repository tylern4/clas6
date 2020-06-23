      SUBROUTINE TRIG_TIME(trkstat)
c     
c     _begin_doc
c     RCS ID string
c     $Id: trig_time.F,v 1.37 2008/06/12 15:18:53 devita Exp $
c     
c     Documentation for subroutine TRIG_TIME
c     
c     Purpose: To finde a trigger time (for electron scattering = electron time).
c     --------
c     
c     Input Parameters:   
c     ----------------      
c     
c     
c     Output Parameters: NPART  -  Is =1 if triggered particle was found
c     -----------------  Fills event structur's first line.
c     
c     Other routines:
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
c     
c     _end_doc
c     
      IMPLICIT NONE
      SAVE
c     
c     _begin_inc
c     include files :
c     ---------------------
c     BOS common block  uncomment the next line for BOS include file
c#include "bcs.inc"
#include "evnt_general.inc"
#include "echits.inc"
#include "particle_out.inc"
#include "piddef.inc"
#include "dc_inf.inc"
#include "cc_inf.inc"
#include "sc_inf.inc"
#include "ec_inf.inc"
#include "lec_inf.inc"
#include "pdgid.inc"
#include "evnt_par.inc"
c     _end_inc
c     
c     _begin_var
c     input/output variables:
c     -----------------------
c     
c     Local pre-defined variables:
c     ---------------------------
c     RCS information: 
      CHARACTER*132  CFILE, CREVIS, CSTATE, CDATE, CAUTHO, CRCSID
      PARAMETER (CFILE = '$RCSfile: trig_time.F,v $')
      PARAMETER (CREVIS = '$Revision: 1.37 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 2008/06/12 15:18:53 $')
      PARAMETER (CAUTHO = '$Author: devita $')
      DATA CRCSID/   
     1     '$Id: trig_time.F,v 1.37 2008/06/12 15:18:53 devita Exp $'   
     2     /   
c     Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='TRIG_TIME')
      PARAMETER (CRAUTH='Stepan Stepanyan')
c     
c     Local User defined variables:
c     -----------------------------
      INTEGER NWRITE,ierr
      INTEGER iprt,np_dc,np_cc,np_sc,np_ec,np_lc,indx,found_electron
      INTEGER foundelectron
      REAL pel, vel, bcc, bec, td_time, tdr_trk, drftel
      REAL stime, etime, gt_rf_seb, get_gt_rf
      REAL tcx, tcy, tcz
      REAL m_trigg, q_trigg
      INTEGER iel, nel, trigger_status, trig_stat,trkstat,nfind
      LOGICAL bettertrigger
      external get_gt_rf
c     _end_var
c     
c     executable code for routine TRIG_TIME:
c---- 6----------------------------------------------------------------72
c     
      IF (NWRITE .LT. 1) THEN
         NWRITE = NWRITE + 1
         CRMESS='This routine determines a trigger time.'
         CALL RECMES(CRNAME,'I',CRMESS)
      ENDIF
c     - Get the triggered particle time  
c     - For electron scattering experiments - electron time.
c     - For photon beam experiments ... Go head.
      nparticles = 0
      evid(1) = 0
      evstatus(1) = 0
      event_stat = 0
      Do indx=1,maxparticles
         gindex(indx)=0
      EndDo
      nfind=0
      q_trigg=qtrigg
      m_trigg=mtrigg
c     
 899  nfind=nfind+1
      trigger_status = 0
      foundelectron=0
      nel = 0
      pel = 0.
      tel = 0.
      DO iprt = 1,gparticles
         np_dc = partstdc(iprt)
         np_cc = partstcc(iprt)
         np_sc = partstsc(iprt)
         np_ec = partstec(iprt)
         np_lc = partstlec(iprt)
         IF (q_trigg.ne.0..and.np_dc.GT.0.AND.(np_ec.GT.0.OR.np_sc.GT.0)) THEN
            If (dcq(np_dc)*q_trigg.gt.0)Then
               bettertrigger=.false.
               found_electron=0
               if(trkstat*dcstatus(np_dc).gt.0) then
                  trig_stat=1
	       else
	          trig_stat=0
               endif
               nel = nel +1
               if(np_cc.GT.0) then
                  trig_stat=trig_stat+1
                   td_time = cctime(np_cc)
                  tdr_trk = ccr_trk(np_cc)
               endif
               if(np_ec.GT.0) then
                  td_time = ectime(np_ec)
                  tdr_trk = ecr_trk(np_ec)
                  trig_stat=trig_stat+1
                  If (ecid(np_ec).Eq.electron.and.np_cc.gt.0)found_electron=1
               endif
               if(np_sc.GT.0) then
                  trig_stat=trig_stat+1
                  td_time = sctime(np_sc)
                  tdr_trk = scr_trk(np_sc)
                  If (trigger_particle.Ne.electron)found_electron=1
               endif
c     - The folowing comented lines will be opened after time colibration is done. 
c     bcc = (scr_trk(np_sc)-ccr_trk(np_cc))/
c     /            (sctime(np_sc)-cctime(np_cc))/vlight
c     bec = (scr_trk(np_sc)-ecr_trk(np_ec))/
c     /            (sctime(np_sc)-ectime(np_ec))/vlight
               If (nel.gt.maxel) Go To 999
               if(foundelectron.eq.0.and.found_electron.eq.1)then
                  bettertrigger=.true.
               elseif(foundelectron.eq.1.and.found_electron.eq.0)then
                  bettertrigger=.false.
               elseif(td_time.gt.0.and.(trig_stat.gt.trigger_status.or.
     +               (trig_stat.eq.trigger_status.and.dcp(np_dc).gt.pel)))then
                  bettertrigger=.true.
               endif
               if(bettertrigger)then
                  if(trig_stat.gt.trigger_status)nel=0
                  pel=dcp(np_dc)
                  vel=vlight*pel/sqrt(pel**2+m_trigg**2)
                  tel=td_time-tdr_trk/vel
                  iel=iprt      
                  foundelectron=found_electron
                  trigger_status=trig_stat
                  tcx = dccx(np_dc)
                  tcy = dccy(np_dc)
                  tcz = dccz(np_dc)
                  tvertx = dcvertx(np_dc)
                  tverty = dcverty(np_dc)
                  tvertz = dcvertz(np_dc)
               endif  
            EndIf
         ELSEIF(q_trigg.eq.0..and.(np_ec.GT.0.or.np_lc.gt.0).and.np_dc.eq.0)THEN
            found_electron=1
            If(np_ec.GT.0) Then
               td_time = ectime(np_ec)
               tdr_trk = ecr_trk(np_ec)
               if(td_time.gt.0.and.ecr_trk(np_ec).gt.0.and.ece_tot(np_ec).gt.pel)then
                  trig_stat=1
                  pel=ece_tot(np_ec)
                  tcx = ecx(np_ec)/ecr_trk(np_ec)
                  tcy = ecy(np_ec)/ecr_trk(np_ec)
                  tcz = ecz(np_ec)/ecr_trk(np_ec)
               endif
            ElseIf(np_lc.GT.0) Then
               td_time = lectime(np_lc)
               tdr_trk = lecr_trk(np_lc)
               if(td_time.gt.0.and.lecr_trk(np_lc).gt.0.and.lece(np_lc).gt.pel)then                
                  trig_stat=1
                  pel=lece(np_lc)
                  tcx = lecx(np_lc)/lecr_trk(np_lc)
                  tcy = lecy(np_lc)/lecr_trk(np_lc)
                  tcz = lecz(np_lc)/lecr_trk(np_lc)
               endif
            EndIf
            If(trig_stat.gt.trigger_status) Then
               vel=vlight*pel/sqrt(pel**2+m_trigg**2)
               tel=td_time-tdr_trk/vel
               iel=iprt
               trigger_status=trig_stat
               tvertx = 0.
               tverty = 0.
               tvertz = bt_z_center
            EndIf  
            If(np_sc.gt.0.and.trig_stat.gt.0)Then
               trigger_status=trigger_status+1
            EndIf
         ENDIF 
      ENDDO
c     
      IF (trigger_status.EQ.0) THEN
         If(trigger_particle_s.ne.0.and.nfind.lt.2)Then
            q_trigg=qtrigg_s
            m_trigg=mtrigg_s
            GO TO 899
         Else
            GO TO 998
         Endif
      ENDIF
c     
      event_stat = 1
      nparticles = 1
      tparticles = nparticles
c     - Start RF correction to the electron start time.
      IF(RF_STATUS.gt.0)THEN
         gt_rf_seb=get_gt_rf()
         gRF=0.
         gRF=gt_rf_seb
         dRFTEL = tel-gt_rf_seb+dtrf*RF_struct-(tvertz-bt_Z_center)/vlight
         RFCOR=AMOD(dRFTEL,RF_struct)-RF_struct/2.
c         print *,gt_rf_seb,dRFTEL,RFCOR,tel
         call hcdir('//PAWC/SEB',' ')
         call hf1(3103,dRFTEL,1.)
         call hf1(3104,RFCOR,1.)
      ELSE
         RFCOR=0.
         if(NWRITE.lt.4)then
            NWRITE = NWRITE + 1
            write(6,*)'NO RF correction  ',rf_time(1),rf_Time(2)
         endif
      ENDIF
c     
 771  TTIME=tel-RFCOR
      np_dc = partstdc(iel)
      partid(iel) = trigger_particle*foundelectron
      partp(iel) = pel
      partq(iel) = q_trigg
      partcx(iel) = tcx
      partcy(iel) = tcy
      partcz(iel) = tcz
      partvertx(iel) = tvertx
      partverty(iel) = tverty
      partvertz(iel) = tvertz
      partbetta(iel) = vel/vlight
      partm(iel) = m_trigg**2
      partstatus(iel) = -trigger_status
      gindex(nparticles) = iel
c     - At current level only electrons as triggered particles are identifyed.  
      DO iprt = 1,gparticles
         np_dc = partstdc(iprt)
         np_cc = partstcc(iprt)
         np_sc = partstsc(iprt)
         np_ec = partstec(iprt)
         IF(iprt.NE.gindex(1)) THEN
            If(np_dc.Gt.0.AND.np_sc.GT.0) Then
               stime = sctime(np_sc) - ttime ! Time from target to th
               if (stime.LT.5..or.stime.gt.100.) then
                  partbetta(iprt) = 0.
               else
                  partbetta(iprt) = 
     =                 scr_trk(np_sc)/stime/vlight
               endif
            ElseIf(np_dc.Gt.0.AND.np_ec.GT.0) Then
               etime = ectime(np_ec) - ttime
               if (etime.LT.1..or.etime.gt.150.) then
                  partbetta(iprt) = 0.
               else
                  partbetta(iprt) = 
     =                 ecr_trk(np_ec)/etime/vlight
               endif
            Else
               partbetta(iprt) = 0.
            EndIf
         ENDIF
      ENDDO
      RETURN
 998  CONTINUE
      CRMESS='No triggered particle is found, event is skipped'
c     CALL RECMES(CRNAME,'I',CRMESS)
      event_stat = 0
      RETURN
 999  CONTINUE
      CRMESS='To many electrons, event is skipped'
      CALL RECMES(CRNAME,'I',CRMESS)
      event_stat = 0
      RETURN
      END
c
c------------------------------------------------------------------------------
