      SUBROUTINE HIT_2_PART (  )
c
c_begin_doc
c  RCS ID string
c  $Id: hit_2_part.F,v 1.18 2006/06/15 21:48:46 stepanya Exp $
c
c  Documentation for subroutine HIT_2_PART
c
c  Purpose: Analyzes of the hit information in DC, CC, SC and EC (after pass 0),
c  -------- in order to find a simple geometrical links between trecks and hits.
c           
c
c  Input Parameters:  
c  ----------------   
c
c  Output Parameters: Fills structures recorded as PARTICLES, 
c  -----------------                               DCST, CCST, SCST and ECST
c
c  Other routines:
c  ---------------
c
c  Notes:
c  ------
c
c  Author:    S. Stepanyan     Created:  Sun Feb 23 12:39:16 EST 1997
c  -------
c
c  Major revisions:
c  ----------------
c     - add call to trk_2_st subroutine :burin 4/7/98 
c
c_end_doc
c
      IMPLICIT NONE
      SAVE
c
c_begin_inc
c  include files :
c  ---------------------
#include "clasmdl.inc"
#include "run_control.inc"
#include "evnt_general.inc"
#include "echits.inc"
#include "lechits.inc"
#include "particle_out.inc"
#include "dc_inf.inc"
#include "cc_inf.inc"
#include "sc_inf.inc"
#include "ec_inf.inc"
#include "lec_inf.inc"
#include "hbt_planes.inc"
#include "schits.inc"
#include "cchits.inc"
#include "ecgmt.inc"
#include "scgeom.inc"
#include "st_inf.inc"
#include "sthits.inc"
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
      PARAMETER (CFILE = '$RCSfile: hit_2_part.F,v $')
      PARAMETER (CREVIS = '$Revision: 1.18 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 2006/06/15 21:48:46 $')
      PARAMETER (CAUTHO = '$Author: stepanya $')
      DATA CRCSID/   
     1'$Id: hit_2_part.F,v 1.18 2006/06/15 21:48:46 stepanya Exp $'   
     2/   
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='HIT_2_PART')
      PARAMETER (CRAUTH='Stepan Stepanyan')
c
c  Local User defined variables:
c  -----------------------------
      INTEGER NWRITE
      INTEGER npart,npart_ec,npart_lec,npart_sc,npart_cc,npart_dc,npart_st
      INTEGER  itr, ihit, ipart, sector, ipart_ec
c_end_var
c
c  executable code for routine HIT_2_PART:
c----6----------------------------------------------------------------72
c
      IF (NWRITE .LT. 1) THEN
        NWRITE = NWRITE + 1
        CRMESS='This routine identifies hits as a particles'
        CALL RECMES(CRNAME,'I',CRMESS)
      ENDIF
c
c- Some initialization
c
      npart = 0
      npart_ec = 0    ! Counts EC hits    !  
      npart_lec = 0    ! Counts EC hits    !    
      npart_sc = 0    ! Counts SC hits    ! bing involved in the identification
      npart_cc = 0    ! Counts CC hits    ! of possibale particles 
      npart_dc = 0    ! Counts DC trecks  ! 
      npart_st = 0    ! Counts ST hits    ! 
c
c- Process of identification starts with "charged particles" trecks/hits, 
c- for neutrals (and if NO DC), EC hits left from above analyses will be
c- used only (as a link hit for SC and CC).
c
      IF ( ntr_link.NE.0 ) THEN ! If DC was analyzed 
c-                                          ! and there are linked tracks.
         DO itr = 1,ntr_link
            sector = itr_sect(itr)
            partstatus(npart+1) = 0
            partid(npart+1)     = 0
c
c- Look for corresponding hits in SC plane(s) if SC was analysed 
c- at all and have hits in the sector.
c
        partstsc(npart+1) = 0
        If ( ScFit_Nhits(sector).ne.0 )
     &  CALL trk_2_SC(itr,npart_sc,npart,sector)
        if(partstsc(npart+1).gt.0)
     &  partstatus(npart+1) = partstatus(npart+1) - 1
c
c- Look for corresponding hits in EC and CC in the forward region. 
c
        partstec(npart+1) = 0
c        If ( trkp(1,45,itr).Lt.zEChi.   
c     &  And.trkp(1,45,itr).Gt.zEClow ) Then
c          
          if ( ecfit_nhit(whole,sector).GT.0  ! and there are hits.
     &   .And. (ecfit_nhit(inner,sector) 
     +       + ecfit_nhit(outer,sector)).GT.0 )
     &    CALL trk_2_EC(itr,npart_ec,npart,sector)
        if(partstec(npart+1).gt.0)
     &  partstatus(npart+1) = partstatus(npart+1) - 1
     
c for start counter
	partstst(npart+1) = 0			! for photon run
        If ( StFit_Nhits(sector).ne.0 )
     &    CALL trk_2_ST(itr,npart_st,npart,sector)
        if(partstst(npart+1).gt.0)
     &    partstatus(npart+1) = partstatus(npart+1) - 1     
c
c- Fined corresponding hit in CC - 
c- for negative trecks only that have EC response. 
c- (If one wonts to look wrong geometry of CC should make a changes here)
c
          partstcc(npart+1) = 0
         if(nrclust.ne.0 )                   ! If CC analysed and
     &    CALL trk_2_CC(itr,npart_cc,npart,sector)      ! have a clasters.
        if(partstcc(npart+1).gt.0)
     &  partstatus(npart+1) = partstatus(npart+1) - 1
c
          partstlec(npart+1) = 0
         if(lec_nhit(1,sector).ne.0 )       ! If LAC analysed and
     &    CALL trk_2_LEC(itr,npart_lec,npart,sector)      ! have a hit(s).
        if(partstlec(npart+1).gt.0)
     &  partstatus(npart+1) = partstatus(npart+1) - 1
c
c- At given treck "itr" all detector planes were 
c- scaned for corresponding hits. 
c
c
c- If a treck has a corresponding hit(s) in SC and/or EC,  
c- then make a mark and keep the parameters. 
c- This means that no particle will be associated with the 
c- treck that matches with CC only.
c
        IF ( partstsc(npart+1).NE.0.OR.partstst(npart+1).NE.0
     O .OR.  partstec(npart+1).NE.0 ) THEN
          npart = npart + 1      ! Possible particle.
          CALL trk_2_dc(itr,npart_dc,npart,sector)
          partstatus(npart) = partstatus(npart) - 1
        ENDIF
c- Here is the end of the loop over tracks (charged particles).
        IF (npart .EQ. MaxParticles) THEN
          CRMESS='To many particles (> 20) in this event.'
          CALL RECMES(CRNAME,'I',CRMESS)
          go to 999
        ENDIF
      ENDDO
c- At this point all possible "charged particles" are selected
c- based on geometrical matching of trackcs with hits in detector planes.
c-        In addition on that for the negative trecks, 
c-        in the trk_2_ec, showers where identifyed.
c-
c- No time information is analized at this point.
c-
      ENDIF
c-
c- The rest of the hits in EC and LEC that were not associated with 
c- any of trecks, will be viewed as neutrals.
c-
        IF ( npart.lt.MaxParticles ) THEN            ! If EC was analysed.
        DO sector = 1,MAXsector                      ! Loop over sectors, if
          If ( ecfit_nhit(whole,sector).GT.0         ! no valid hits in the 
     &  .AND.( ecfit_nhit(inner,sector)              ! sector go to 
     +       + ecfit_nhit(outer,sector)).GT.0 ) Then ! the next one.
c- 
            Do 10 ihit = 1,ecfit_nhit(whole,sector) ! Loop over valid hits.
              ipart = 1                                    !
              do while ( npart.ne.0.and.ipart.le.npart)    ! Skip a hit that
                ipart_ec = partstec(ipart)                 ! was already  
                ipart = 1 + ipart                          ! matched with treck.
                if(ipart_ec.gt.0)then                      !
                  if( ecsector(ipart_ec).eq.sector         !
     &           .and.echit(whole,ipart_ec).eq.ihit) goto 10   ! 
                endif                                      !
              enddo                                        !
              if(ecfit_hit(energy,ihit,whole,sector).lt.0.005) GoTo10
c- 
c- Post all detector statuses 0 for potential particle.
c-
              partstdc(npart+1) = 0
              partstcc(npart+1) = 0
              partstsc(npart+1) = 0
              partstec(npart+1) = 0
              partstlec(npart+1) = 0
              partstst(npart+1) = 0
              partstatus(npart+1) = 0
              partid(npart+1)     = 0
c- 
c- Now call the main and only one subroutine for EC based matching.
c- First it will do inter layer matching for EC, than will look for SC and CC. 
c- It assumes that particle is coming from origin and has a straight trajectory. 
c-
              CALL ec_do_match(ihit,npart,npart_cc
     ,                        ,npart_sc,npart_ec,sector)
              IF ( partstec(npart+1).NE.0 ) THEN
                npart = npart +1
              ENDIF
              IF (npart .GE. MaxParticles) THEN
                CRMESS='To many particles (> 10) in this event.'
                CALL RECMES(CRNAME,'I',CRMESS)
                go to 999
              ENDIF
  10	    Continue
          EndIF
        ENDDO
c-
        DO sector = 1,MAXsector 
c                                                   ! Loop over sectors, if
          If ( lec_nhit(1,sector).GT.0)THEN         ! no valid hits in the 
c                                                   ! sector go to 
c                                                   ! the next one.
c- 
            Do 20 ihit = 1,lec_nhit(1,sector) ! Loop over valid hits.
              ipart = 1                                    !
              do while ( npart.ne.0.and.ipart.le.npart)    ! Skip a hit that
                ipart_ec = partstlec(ipart)                ! was already  
                ipart = 1 + ipart                          ! matched with treck.
                if(ipart_ec.gt.0)then                      !
                  if( lecsector(ipart_ec).eq.sector         !
     &           .and.lechit(1,ipart_ec).eq.ihit) goto 20   ! 
                endif                                      !
              enddo                                        !
              if(lec_hit(energy,ihit,1,sector).lt.0.005) GoTo20
c- 
c- Post all detector statuses 0 for potential particle.
c-
              partstdc(npart+1) = 0
              partstcc(npart+1) = 0
              partstsc(npart+1) = 0
              partstec(npart+1) = 0
              partstlec(npart+1) = 0
              partstst(npart+1) = 0
              partstatus(npart+1) = 0
              partid(npart+1)     = 0
c- 
c- Now call the main and only one subroutine for LAC based matching.
c- It will look for SC assumming, 
c- that particle is coming from origin and has a straight trajectory. 
c-
              IF (npart .GE. MaxParticles) THEN
                CRMESS='To many particles (> 10) in this event.'
                CALL RECMES(CRNAME,'I',CRMESS)
                go to 999
              ELSEIF(lec_hit(energy,ihit,1,sector).gt.0.0001)THEN
                IF(ScFit_Nhits(sector).Gt.0)
     +          CALL lec_do_match(ihit,npart,npart_sc,npart_lec,sector)
                partchi2lec(npart+1) = 0.
                npart = npart +1
                npart_lec=npart_lec+1
                partstlec(npart)=npart_lec
                lecpart(npart_lec)=npart
                lechit(1,npart_lec)=ihit
                lece(npart_lec)=lec_hit(energy,ihit,1,sector)
                lecei(npart_lec)=lec_hit(ec_i2,ihit,1,sector)
                lecx(npart_lec)=lec_hit(ec_x,ihit,1,sector)
                lecy(npart_lec)=lec_hit(ec_y,ihit,1,sector)
                lecz(npart_lec)=lec_hit(ec_z,ihit,1,sector)
                lectime(npart_lec)=lec_hit(time,ihit,1,sector)
                lecr_trk(npart_lec)=sqrt(
     +lecx(npart_lec)**2+lecy(npart_lec)**2+lecz(npart_lec)**2)
                lecsector(npart_lec)=sector
                lecstatus(npart_lec)=1
                lecid(npart_lec)=0
              ENDIF
  20	    Continue
          EndIF
        ENDDO
        ENDIF
c- End of geometry based hitmatching.
c- Number of possible detected particles - npart.
c- Status of each detector for given particle stored 
c- in structure, recorded as a "particles".
 999  CONTINUE
      gparticles=npart
      eparticles=npart_ec
      lparticles=npart_lec
      sparticles=npart_sc
      cparticles=npart_cc
      dparticles=npart_dc
      stparticles=npart_st
      RETURN
      END
c
c------------------------------------------------------------------------------
