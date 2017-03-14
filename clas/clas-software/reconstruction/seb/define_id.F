      SUBROUTINE DEFINE_ID (nprt,q,np_dc,np_cc,np_sc,np_ec,np_lec)
c
c_begin_doc
c  RCS ID string
c  $Id: define_id.F,v 1.25 2006/03/14 14:43:40 stepanya Exp $
c
c  Documentation for subroutine DEFINE_ID
c
c  Purpose: Define particle ID based on time and momenta.
c  --------
c
c  Input Parameters:  INTEGER nprt - Current number in the record PARTICLES.
c  ----------------   REAL q       - Charge of the particle
c                     INTEGER np_dc- current number in the recor DCST 
c                     INTEGER np_cc- current number in the recor CCST 
c                     INTEGER np_sc- current number in the recor SCST 
c                     INTEGER np_ec- current number in the recor ECST 
c
c  Output Parameters:  Update values of PARTICLES record
c  -----------------
c
c  Other routines:
c  ---------------
c
c  Notes:
c  ------
c
c  Author:  S. Stepanyan       Created:  Wed Mar 26 10:48:25 EST 1997
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
#include "evnt_general.inc"
#include "echits.inc"
#include "particle_out.inc"
#include "dc_inf.inc"
#include "cc_inf.inc"
#include "sc_inf.inc"
#include "ec_inf.inc"
#include "lec_inf.inc"
#include "hbt_planes.inc"
#include "tbtinf.inc"
#include "pdgid.inc"
#include "evnt_par.inc"
c_end_inc
c
c_begin_var
c  input/output variables:
c  -----------------------
      INTEGER nprt,np_dc,np_cc,np_sc,np_ec,np_lec,q
c
c  Local pre-defined variables:
c  ---------------------------
c  RCS information: 
      CHARACTER*132  CFILE, CREVIS, CSTATE, CDATE, CAUTHO, CRCSID
      PARAMETER (CFILE = '$RCSfile: define_id.F,v $')
      PARAMETER (CREVIS = '$Revision: 1.25 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 2006/03/14 14:43:40 $')
      PARAMETER (CAUTHO = '$Author: stepanya $')
      DATA CRCSID/   
     1'$Id: define_id.F,v 1.25 2006/03/14 14:43:40 stepanya Exp $'   
     2/   
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='DEFINE_ID')
      PARAMETER (CRAUTH='Stepan Stepanyan')
c
c  Local User defined variables:
c  -----------------------------
      INTEGER NWRITE
      REAL p, photon_sampling_ec
      REAL stime,ctime,etime,ctime_s,etime_s,ctime_e,stime_e
      REAL sc_v, ec_vel
      REAL b_part(10)
      REAL bdiff, tbtstat
      REAL neut_phot_b
      data neut_phot_b/0.95/
      INTEGER ipr,jpr,maxloop,minloop,jjj
      LOGICAL OK_SCEC,OK_SCCC,OK_ECCC,OK_ECSC,BADECT
      external photon_sampling_ec
c_end_var
c
c  executable code for routine DEFINE_ID:
c----6----------------------------------------------------------------72
c
      IF (NWRITE .LT. 1) THEN
        NWRITE = NWRITE + 1
        CRMESS='First steps for PID. '
        CALL RECMES(CRNAME,'I',CRMESS)
      ENDIF
c
c     clear flags
c
      OK_SCEC = .false.
      OK_SCCC = .false.
      OK_ECCC = .false.
      OK_ECSC = .false.
c
      IF (q.NE.0) THEN
        tbtstat = dcstatus(np_dc) / abs(dcstatus(np_dc))
        maxloop = 9
        if(q.lt.0)maxloop = 4
c- For charge's ID defined by SC if no SC then ID is that determined by EC.
        p = partp(nprt)
        minloop = 3
cStep        if(p.lt.0.1)minloop=2     
  11    continue
        If (np_sc.ne.0)then
          if(tbtstat.gt.0)scr_trk(np_sc)=tbt_r(sctrk_pln(np_sc),dcstatus(np_dc))
          stime = sctime(np_sc) - ttime   ! Time from target to the SC plane
          if (stime.LT.5..OR.stime.gt.100.) then
             np_sc = 0
             goto 11
          endif
          sc_v = scr_trk(np_sc) / stime   ! Particle velocity
          if (sc_v .lt. 1.0e-6) then
            np_sc = 0
            goto 11
          endif
          bdiff=CUT_B_SC
          jpr=0
          Do ipr = minloop, maxloop
            b_part(ipr) = p / sqrt(p**2+p_mass(ipr)**2)  ! Betta according to DC
c- Take the closest by betta particle
            if(ABS(sc_v/vlight-b_part(ipr)).lt.bdiff)then
              bdiff=ABS(sc_v/vlight-b_part(ipr))
              scid(np_sc)=p_id(ipr)*q
              jpr=ipr
            endif
          EndDo
          if(jpr.eq.0) then ! There was not matched particle
            np_sc = 0
            goto 11
          endif
c- Fill particles record (id, status, mass and betta).
          partid(nprt) = scid(np_sc)
          if(partid(nprt).eq.neutron)partid(nprt)=proton
          partstatus(nprt) = 2*tbtstat
          partbetta(nprt) = sc_v/vlight
          partm(nprt) = p**2*(1.-partbetta(nprt)**2)/partbetta(nprt)**2
c- 
          OK_SCCC = .false.
          if (np_cc.ne.0)then
c- See if CC hit in time.
            ctime = cctime(np_cc) - ttime
            ctime_s = ccr_trk(np_cc) / sc_v
            OK_SCCC = ABS(ctime_s-ctime).LT.CUT_T_SCCC
          endif
          OK_SCEC = .false.
          if (np_ec.ne.0)then
c- See if EC hit in time.
            etime = ectime(np_ec) - ttime
            etime_s = ecr_trk(np_ec) / sc_v
            OK_SCEC = ABS(etime_s-etime).LT.CUT_T_SCEC
          endif
          if(OK_SCCC )then
c- If there was CC hit then it should be ether electron or positron.
            if(OK_SCEC)then
              partstatus(nprt) = partstatus(nprt) + 2*tbtstat
              if(ecid(np_ec).eq.electron
     &    .and.abs(b_part(jpr)-1).lt.0.05)partid(nprt) = positron*q
            else
              np_ec=0
              partstatus(nprt) = partstatus(nprt) + 1*tbtstat
            endif
          else
            np_cc=0
            if(OK_SCEC)then
              partstatus(nprt) = partstatus(nprt) + 1*tbtstat
            else  
              np_ec=0
            endif  
          endif  
        ElseIf(np_ec.gt.0)Then
c- Track was matched with EC ( no SC) or SC could not fined ID. 
          if(dcstatus(np_dc).gt.0)ecr_trk(np_ec)=tbt_r(fec_pln,dcstatus(np_dc))
          etime = ectime(np_ec) - ttime
          if (etime.LT.1..OR.etime.gt.150.) then
             np_ec = 0
             goto 999
          endif
          ec_vel = ecr_trk(np_ec) / etime
          if (ec_v .lt. 1.0e-06) then
            np_ec = 0
            goto 999
          endif
          bdiff=CUT_B_EC
          jpr=0
c- Take the closest by betta particle
          Do 22 ipr = minloop, maxloop 
            b_part(ipr) = p / sqrt(p**2+p_mass(ipr)**2)
            if(ABS(ec_vel/vlight-b_part(ipr)).lt.bdiff)then
              bdiff=ABS(ec_vel/vlight-b_part(ipr))
              ecid(np_ec)=p_id(ipr)*q
              jpr=ipr
            endif
  22      continue
          if(jpr.eq.0) goto 999 ! Too bad, sorry.
c- Fill particles record (id, status, mass and betta).
          partid(nprt) = ecid(np_ec)
          if(partid(nprt).eq.neutron)partid(nprt)=proton
          partstatus(nprt) = 2*tbtstat
          partbetta(nprt) = ec_vel/vlight
          partm(nprt) = p**2*(1.-partbetta(nprt)**2)/partbetta(nprt)**2
          OK_ECCC = .false.
          if (np_cc.ne.0)then
c- See if CC hit in time.
            ctime = cctime(np_cc) - ttime
            ctime_e = ccr_trk(np_cc) / ec_vel
            OK_ECCC = ABS(ctime_e-ctime).LT.CUT_T_ECCC
          endif
          if(OK_ECCC)then
c- If there was CC hit then it should be ether electron or positron.
            partstatus(nprt) = partstatus(nprt) + 1*tbtstat
            if(abs(ecid(np_ec)).eq.electron
     &    .and.abs(b_part(jpr)-1.).lt.0.05)then
              partid(nprt) = positron*q
            else  
c              goto 999  ! Too bad, sorry.
            endif
          else
            np_cc=0
          endif  
        EndIf
      ELSEIF (q.EQ.0) THEN
c- No DC track for this particle.
c- Candidate for neutral particle (in the forward or large angle calorimeter)
        If(np_ec.gt.0)then
          badect=.false.
          etime = ectime(np_ec) - ttime
          if (etime.lt.10.) then
             ec_vel=2.*vlight
             badect=.true.
          else
             ec_vel = ecr_trk(np_ec) / etime
          endif
          OK_ECCC = .false.
          if (np_cc.ne.0)then
c- See if CC hit in time.
            ctime = cctime(np_cc) - ttime
            ctime_e = ccr_trk(np_cc) / ec_vel
            OK_ECCC = ABS(ctime_e-ctime).LT.CUT_T_ECCC
          endif
        ElseIf(np_lec.gt.0)then
          badect=.false.
          etime = lectime(np_lec) - ttime
          if (etime.lt.1.) then
             ec_vel=2.*vlight
             badect=.true.
          else
             ec_vel = lecr_trk(np_lec) / etime
          endif
        EndIf
        OK_ECSC = .false.
        if (np_sc.ne.0)then
c- See if SC hit in time.
          stime = sctime(np_sc) - ttime
          if (ec_vel.GT.0.01) then
             stime_e = scr_trk(np_sc) / ec_vel
             OK_ECSC = ABS(stime_e-stime).LT.CUT_T_SCEC
             if(sce_dep(np_sc).lt.4.)OK_ECSC=.false.
          else
             OK_ECSC=.TRUE.
          endif
        endif
        if(OK_ECCC.or.OK_ECSC.or.badect) then
          partstatus(nprt)=0
          if(.NOT.OK_ECCC)np_cc=0
          if(.NOT.OK_ECSC)np_sc=0
        else
          np_cc=0
          np_sc=0
        endif
        partbetta(nprt) = ec_vel/vlight
        if(partbetta(nprt).ge.neut_phot_b.and.partbetta(nprt).lt.1.99) then
c- Here is photon
          If(np_ec.gt.0)then
            ecid(np_ec)=photon
            ecq(np_ec)=q
            if(sampl_frac.gt.0.5)then
               partp(nprt) = ece_tot(np_ec)/sampl_frac/Photon_sampling_ec(ece_tot(np_ec),1)
            else
               partp(nprt) = ece_tot(np_ec)/sampl_frac
            endif
          ElseIf(np_lec.gt.0)then
            lecid(np_lec)=photon
            partp(nprt) = lece(np_lec)/sampl_frac
          EndIf
          partq(nprt) = q
          if(np_cc.eq.0.and.np_sc.eq.0)partstatus(nprt) = 1
          partm(nprt) = mphoton
          partid(nprt) = photon
        elseif(partbetta(nprt).gt.0..and.partbetta(nprt).lt.neut_phot_b)then
c- Here is neutron
          If(np_ec.gt.0)then
            ecid(np_ec)=neutron
            ecq(np_ec)=q
          ElseIf(np_lec.gt.0)then
            lecid(np_lec)=neutron
          EndIf
          partq(nprt) = q
          if(np_cc.eq.0.and.np_sc.eq.0)partstatus(nprt) = 1
          partm(nprt) = mneutron**2
          partid(nprt) = neutron
          partp(nprt) = 
     =    mneutron*partbetta(nprt)/sqrt(1.-partbetta(nprt)**2)
        elseif(partbetta(nprt).ge.1.99)then
           GO TO 999
        endif
      ENDIF
      RETURN
 999  CONTINUE
      partstatus(nprt)=0
      partid(nprt)=0
      partp(nprt) = 0.
      RETURN
      END
c
c------------------------------------------------------------------------------




