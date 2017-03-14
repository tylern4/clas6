      SUBROUTINE EVENT_BLD( )
c
c_begin_doc
c  RCS ID string
c  $Id: event_bld.F,v 1.24 2006/06/07 21:18:19 stepanya Exp $
c
c  Documentation for subroutine EVENT_BLD
c
c  Purpose: Get the PID after time analyses and build an event.
c  --------
c
c  Input Parameters:  NON
c  ----------------
c
c  Output Parameters: NON 
c  -----------------
c
c  Other routines:
c  ---------------
c
c  Notes:
c  ------
c
c  Author:   S.Stepanyan      Created:  Wed Feb 26 23:17:43 EST 1997
c  -------
c
c  Major revisions:
c  ----------------
c     - add in start counter info. :burin 4/7/98
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
c#include "bcs.inc"
#include "evnt_general.inc"
#include "echits.inc"
#include "particle_out.inc"
#include "dc_inf.inc"
#include "cc_inf.inc"
#include "sc_inf.inc"
#include "ec_inf.inc"
#include "lec_inf.inc"
#include "pdgid.inc"
#include "photon_stuff.inc"
#include "evnt_par.inc"
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
      PARAMETER (CFILE = '$RCSfile: event_bld.F,v $')
      PARAMETER (CREVIS = '$Revision: 1.24 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 2006/06/07 21:18:19 $')
      PARAMETER (CAUTHO = '$Author: stepanya $')
      DATA CRCSID/   
     1'$Id: event_bld.F,v 1.24 2006/06/07 21:18:19 stepanya Exp $'   
     2/   
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='EVENT_BLD')
      PARAMETER (CRAUTH='Stepan Stepanyan')
c
c  Local User defined variables:
c  -----------------------------
      INTEGER NWRITE, iprt, ip, np
      INTEGER sdc(Maxparticles),scc(Maxparticles),sst(Maxparticles)
      INTEGER ssc(Maxparticles),sec(Maxparticles),slec(Maxparticles)
      INTEGER nprt,np_dc,np_cc,np_sc,np_ec,np_lec,dc_q,np_st
c_end_var
c
c  executable code for routine EVENT_BLD:
c----6----------------------------------------------------------------72
c
      IF (NWRITE .LT. 1) THEN
        NWRITE = NWRITE + 1
        CRMESS='Building an event.'
        CALL RECMES(CRNAME,'I',CRMESS)
      ENDIF
c- 
c- For each entry in PARTICLES based on the information in 
c- DCST, CCST, SCST and ECST build the event considering this:
c-  1. For positive particles (partq>0) look for
c-     protons, pi+ and K+ and e+.
c-  2. For negative particles (partq<0) look for
c-     electrons, pi- and K-.
c-  3. For neutral particles look for 
c-     photons and neutrons.
c- Then put the information on each identifyed particle according to the
c- structure /particle/ in the record EVENT. The sequence of particles in 
c- the record EVENT (for electron scattering experiments) will be:
c-                    Electron (at this point it alredy there)
c-                    Nucleon (proton or neutron) or nuclei
c-                    Kaons and Pions
c-                    Photons
c-                    Leptons (additional electrons and positrons)
c-  
      IF(YES_PHOTON) NPARTICLES = 0  ! For photon run 	
                  
      DO nprt = 1,gparticles         ! Loop over geometrically found particles.
        np_dc = partstdc(nprt)
        np_cc = partstcc(nprt)
        np_sc = partstsc(nprt)
        np_ec = partstec(nprt)
        np_lec = partstlec(nprt)
        np_st = partstst(nprt)

c  Skip ones that in the trigger  - electron run
c  Treat all particles the same   - photon run
      IF (nprt.NE.gindex(1).OR.YES_PHOTON) THEN 
        partid(nprt) = 0
        partm(nprt) = 0.
        partbetta(nprt) = 0.
c
c
c- First identify charged particles. 
c
        IF (np_dc.gt.0) THEN   ! This happens if there was corresponding
c-                             ! to the DC track hits in SC and/or EC.
c
c- Move DC found properties to the particle record and then get the ID.
          partp(nprt) = dcp(np_dc)
          partq(nprt) = dcq(np_dc)
          partcx(nprt) = dccx(np_dc)
          partcy(nprt) = dccy(np_dc)
          partcz(nprt) = dccz(np_dc)
          partvertx(nprt) = dcvertx(np_dc)
          partverty(nprt) = dcverty(np_dc)
          partvertz(nprt) = dcvertz(np_dc)
          partchi2dc(nprt)= dcchi2(np_dc)
          dc_q = partq(nprt)
c
          If(event_stat.Gt.0) Then         ! If trigger is right.
            CALL DEFINE_ID(nprt,dc_q,np_dc,np_cc,np_sc,np_ec,np_lec)
            if(partstatus(nprt).gt.0) then
              NPARTICLES = NPARTICLES+1
              gindex(NPARTICLES) = nprt
              sdc(NPARTICLES) = np_dc
              scc(NPARTICLES) = np_cc
              ssc(NPARTICLES) = np_sc
              sec(NPARTICLES) = np_ec
              slec(NPARTICLES) = np_lec
              sst(NPARTICLES) = np_st
            endif  
          EndIf
c
c- Here is the case when hit in the EC was not assiociated with any of tracks.
c
        ELSE IF (np_ec.ne.0.and.np_ec.le.EPARTICLES.and.ecr_trk(np_ec).gt.100.) THEN
          partp(nprt) = 0.
          partq(nprt) = ecq(np_ec)
          partid(nprt) = ecid(np_ec)
          ecr_trk(np_ec)=
     +         sqrt((ecx(np_ec)-tvertx)**2+(ecy(np_ec)-tverty)**2+(ecz(np_ec)-tvertz)**2)
          partcx(nprt) = (ecx(np_ec)-tvertx)/ecr_trk(np_ec)
          partcy(nprt) = (ecy(np_ec)-tverty)/ecr_trk(np_ec)
          partcz(nprt) = (ecz(np_ec)-tvertz)/ecr_trk(np_ec)
          partvertx(nprt) = tvertx
          partverty(nprt) = tverty
          partvertz(nprt) = tvertz
          If(event_stat.Gt.0) Then         ! If trigger is right.
            CALL DEFINE_ID(nprt,0,np_dc,np_cc,np_sc,np_ec,np_lec)
            if(partstatus(nprt).gt.0) then
              NPARTICLES = NPARTICLES+1
              gindex(NPARTICLES) = nprt
              sdc(NPARTICLES) = np_dc
              scc(NPARTICLES) = np_cc
              ssc(NPARTICLES) = np_sc
              sec(NPARTICLES) = np_ec
              slec(NPARTICLES) = np_lec
              sst(NPARTICLES) = np_st
            endif
          EndIf
        ELSE IF (np_lec.ne.0.and.np_lec.le.LPARTICLES.and.lecr_trk(np_lec).gt.100.) THEN
          partp(nprt) = 0.
          partq(nprt) = 0
          partid(nprt) = lecid(np_lec)
          lecr_trk(np_lec)=
     +         sqrt((lecx(np_lec)-tvertx)**2+(lecy(np_lec)-tverty)**2+(lecz(np_lec)-tvertz)**2)
          partcx(nprt) = (lecx(np_lec)-tvertx)/lecr_trk(np_lec)
          partcy(nprt) = (lecy(np_lec)-tverty)/lecr_trk(np_lec)
          partcz(nprt) = (lecz(np_lec)-tvertz)/lecr_trk(np_lec)
          partvertx(nprt) = tvertx
          partverty(nprt) = tverty
          partvertz(nprt) = tvertz
          If(event_stat.Gt.0) Then         ! If trigger is right.
            CALL DEFINE_ID(nprt,0,np_dc,np_cc,np_sc,np_ec,np_lec)
            if(partstatus(nprt).gt.0) then
              NPARTICLES = NPARTICLES+1
              gindex(NPARTICLES) = nprt
              sdc(NPARTICLES) = np_dc
              scc(NPARTICLES) = np_cc
              ssc(NPARTICLES) = np_sc
              sec(NPARTICLES) = np_ec
              slec(NPARTICLES) = np_lec
              sst(NPARTICLES) = np_st
            endif
          EndIf
        ENDIF
      ELSE
c- For triggered particle renew the information (TBT).
        if(qtrigg.ne.0.and.np_dc.gt.0)then
          partp(nprt) = dcp(np_dc)
          partq(nprt) = dcq(np_dc)
          partcx(nprt) = dccx(np_dc)
          partcy(nprt) = dccy(np_dc)
          partcz(nprt) = dccz(np_dc)
          partvertx(nprt) = dcvertx(np_dc)
          partverty(nprt) = dcverty(np_dc)
          partvertz(nprt) = dcvertz(np_dc)
          partchi2dc(nprt)= dcchi2(np_dc)
          if(dcstatus(np_dc).gt.0)partstatus(nprt)= abs(partstatus(nprt))
        elseif(qtrigg.eq.0)then
          partstatus(nprt) = 1
          If(np_ec.gt.0)then
            ecid(np_ec)=photon
            ecq(np_ec)=qtrigg
            partp(nprt) = ece_tot(np_ec)/sampl_frac
          ElseIf(np_lec.gt.0)then
            lecid(np_lec)=photon
            partp(nprt) = lece(np_lec)/sampl_frac
          EndIf
          if(np_cc.gt.0.or.np_sc.gt.0)partstatus(nprt) = 0
        endif
        call part_2_ev(nprt,1)
        evstdc(1) = np_dc
        evstcc(1) = np_cc
        evstsc(1) = np_sc
        evstec(1) = np_ec
        evstlec(1) = np_lec
      ENDIF
c- End of loop over geometrically found particles for time analyses and PID.
      ENDDO
c- At this point using time information status of geometri" particles were set:
c- particles(i).status = 0 No time coincidence, No ID is defined
c-                     > 0 On time and defined ID
c
c- Now move defined particles from record PARTICLES to record EVENT by ordering
c- as was described above.  
c
      IF (NPARTICLES.GE.TPARTICLES.AND.TPARTICLES.GT.0) THEN
c- First line should be alredy busy by triggered particle.
c- For electron scattering experiment first line is electron.
         If(YES_PHOTON) then
            nprt = 1			! For photon run
            np   = 1
            ip   = 1
         Else  
            nprt = TPARTICLES  + 1 ! For electron run
            np   = TPARTICLES  + 1
            ip   = 2
         EndIf  
         Do iprt=np,NPARTICLES 
c- Identifyed nucleons or nuclear folowed to triggered particle.
          if(partid(gindex(iprt)).eq.proton
     o   .or.partid(gindex(iprt)).eq.neutron
     o   .or.partid(gindex(iprt)).eq.deuteron
     o   .or.partid(gindex(iprt)).eq.hel3
     o   .or.partid(gindex(iprt)).eq.hel4)then
            call part_2_ev(gindex(iprt),nprt)
            evstdc(nprt) = sdc(iprt)
            evstcc(nprt) = scc(iprt)
            evstsc(nprt) = ssc(iprt)
            evstec(nprt) = sec(iprt)
            evstlec(nprt) = slec(iprt)
            evstst(nprt) = sst(iprt)
            nprt = nprt +1
          endif
        EndDo
        IF (nprt.GT.NPARTICLES) GO TO 999
c- Then pions and Kaons.
        Do iprt=ip,NPARTICLES 
          if(ABS(partid(gindex(iprt))).eq.pip
     &   .or.ABS(partid(gindex(iprt))).eq.kp)then
            call part_2_ev(gindex(iprt),nprt)
            evstdc(nprt) = sdc(iprt)
            evstcc(nprt) = scc(iprt)
            evstsc(nprt) = ssc(iprt)
            evstec(nprt) = sec(iprt)
            evstlec(nprt) = slec(iprt)
            evstst(nprt) = sst(iprt)
            nprt = nprt +1
          endif
        EndDo
        IF (nprt.GT.NPARTICLES) GO TO 999
c- then photons
        Do iprt=ip,NPARTICLES 
          if(partid(gindex(iprt)).eq.photon)then
            call part_2_ev(gindex(iprt),nprt)
            evstdc(nprt) = sdc(iprt)
            evstcc(nprt) = scc(iprt)
            evstsc(nprt) = ssc(iprt)
            evstec(nprt) = sec(iprt)
            evstlec(nprt) = slec(iprt)
            evstst(nprt) = sst(iprt)
            nprt = nprt +1
          endif
        EndDo
        IF (nprt.GT.NPARTICLES) GO TO 999
c- At last leptons.
        Do iprt=ip,NPARTICLES 
          if(ABS(partid(gindex(iprt))).eq.electron)then
            call part_2_ev(gindex(iprt),nprt)
            evstdc(nprt) = sdc(iprt)
            evstcc(nprt) = scc(iprt)
            evstsc(nprt) = ssc(iprt)
            evstec(nprt) = sec(iprt)
            evstlec(nprt) = slec(iprt)
            evstst(nprt) = sst(iprt)
            nprt = nprt +1
          endif
        EndDo
      ENDIF
 999  CONTINUE
      RETURN
      END
c
c------------------------------------------------------------------------------




