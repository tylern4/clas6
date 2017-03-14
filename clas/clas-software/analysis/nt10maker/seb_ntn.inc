c      include file SEB_NTN
c
c_begin_doc
c  RCS ID string
c  $Id: seb_ntn.inc,v 1.2 2001/02/21 03:21:38 stepanya Exp $
c
c  Documentation for include file SEB_NT
c
c  Purpose:
c  --------
c
c  Used in routines:
c  ----------------
c
c  Notes:
c  ------
c
c  Author:         Created:  Fri Mar  7 09:43:49 EST 1997
c  -------
c
c  Major revisions:
c  ----------------
c
c_end_doc
c
c
c  Remember NO NESTED INCLUDES
c
c  COMMON BLOCK NAME should be the same as include file name!
c
c_begin_var
c  variables for include file SEB_NT:
c  -----------------------------------
c  RCS information: 
      CHARACTER*(132)  CIPART_NT
      DATA CIPART_NT/
     1'$Id: seb_ntn.inc,v 1.2 2001/02/21 03:21:38 stepanya Exp $'
     2/
c----6----------------------------------------------------------------72
c  
c  Variables contained in PART_NT common block
c  --------------------------------------------
c
c_end_var
        INTEGER Maxparticles
        PARAMETER (Maxparticles=20)
c
        INTEGER npart, gpart      ! Nuber of final and geometrical particles 
        INTEGER evntid            ! event number 
        INTEGER evstat            ! event status
        INTEGER evntype            ! event type
        INTEGER evclas            ! trigger bits 
        REAL Q_l                  ! Live gated FC
        REAL T_l                  ! Live time
        REAL tr_time              ! trigger time
        REAL rf_time              ! RF time
        INTEGER lt1               ! Level 1 trigger latch
        INTEGER hlsc              ! Helicity scaler
        INTEGER intt              ! Interrupt time
      COMMON/HEVT_NT/evntid,evntype,evclas
     +,q_l,t_l,tr_time,rf_time,lt1,hlsc,npart,evstat,intt
C-
        INTEGER ID(Maxparticles)  ! Final PID (PDG ID)
        REAL P(Maxparticles)      ! Particle momentum (GeV/c)
        REAL M(Maxparticles)      ! Particle mass2 (GeV/c)2 (Not in N-tuple)
        INTEGER Q(Maxparticles)   ! Particle charge (+1,0,-1 in the units of e)
        REAL B(Maxparticles)      ! Particle velocity in the units of c
        REAL CX(Maxparticles)     ! X direction cosine at the origin
        REAL CY(Maxparticles)     ! Y direction cosine at the origin
        REAL CZ(Maxparticles)     ! Z direction cosine at the origin
        REAL VX(Maxparticles)     ! X coordinate of vertex (cm)
        REAL VY(Maxparticles)     ! Y coordinate of vertex (cm)
        REAL VZ (Maxparticles)    ! Z coordinate of vertex (cm)
        INTEGER DC(Maxparticles)  ! Status of DC (>0 DCPB row numb., =0 no DC ->Not in N-tuple) 
        INTEGER CC(Maxparticles)  ! Status of CC (>0 CCPB row numb., =0 no CC ->Not in N-tuple)
        INTEGER SC(Maxparticles)  ! Status of SC (>0 SCPB row numb., =0 no SC ->Not in N-tuple)
        INTEGER EC(Maxparticles)  ! Status of EC (>0 ECPB row numb., =0 no EC ->Not in N-tuple)
        INTEGER LEC(Maxparticles)  ! Status of LEC (>0 LCPB row numb., =0 no LAC ->Not in N-tuple)
        INTEGER CCST(Maxparticles)
        INTEGER ST(Maxparticles)  ! Status of ST (>0 STPB row numb., =0 no S ->Not in N-tupleT)
        INTEGER STAT(Maxparticles)! Status of particle stat*(DC+100*SC+10000*EC+1000000*LAC+ST*100000000)
C stat 1 if particle is identified, -1 that hit(s)/tracks were matched but no TB or ID, 0 - if undef.
      COMMON/EVNT_NT/gpart,id,stat,ccst,
     +p,q,b,cx,cy,cz,vx,vy,vz,m,dc,cc,sc,ec,lec,st
C-
        INTEGER ec_part              ! Nuber of EC contained particles 
        REAL etot(Maxparticles)      ! Total deposited energy (GeV)
        REAL ec_ei(Maxparticles)     ! Inner energy (GeV)
        REAL ec_eo(Maxparticles)     ! Outer energy (GeV)
        REAL ec_t(Maxparticles)      ! EC hit time
        REAL ech_X(Maxparticles)     ! X coordinate of the hit
        REAL ech_Y(Maxparticles)     ! Y coordinate of the hit
        REAL ech_Z(Maxparticles)     ! Z coordinate of the hit
        REAL ec_r(Maxparticles)      ! Length from target (cm)
        REAL ec_M2(Maxparticles)     ! Second moment of shower transvers distr.
        REAL ec_M3(Maxparticles)     ! Third moment of shower transvers distr.
        REAL ec_M4(Maxparticles)     ! Forth moment of shower transvers distr.
        REAL ec_c2(Maxparticles)     ! Qualaty of geometrical matching.
        INTEGER ec_InSt(Maxparticles)   ! Number of strips in the inner hit
        INTEGER ec_OuSt(Maxparticles)   ! Number of strips in the outer hit
        INTEGER ec_whol(Maxparticles)   ! whole hit ID (Not in the N-tuple)
        INTEGER EC_sect(Maxparticles)  ! EC hit sector (Not In the N-tuple)
        INTEGER EC_stat(Maxparticles)  ! EC hit status = Wh+100*Inh+10000*Ouh+1000000*sect
      COMMON/ecpb_nt/ec_part,ec_stat,ec_inst,ec_oust,
     +etot,ec_ei,ec_eo,ec_t,ec_r,ech_x,ech_y,ech_z,ec_m2,ec_c2,ec_sect,ec_whol,ec_m3,ec_m4
C-
        INTEGER dc_part   ! Number of DC hits involved in the particles.
        INTEGER dc_sect(Maxparticles)   ! DC track sector (not in the N-tuple)
        REAL dc_XSC(Maxparticles)     ! X coordinate  ! of the 
        REAL dc_YSC(Maxparticles)     ! Y coordinate  ! intersection point
        REAL dc_ZSC(Maxparticles)     ! Z coordinate  ! with SC plane.
        REAL dc_CXSC(Maxparticles)    ! X direction cosine ! at the 
        REAL dc_CYSC(Maxparticles)    ! Y direction cosine ! intersection point
        REAL dc_CZSC(Maxparticles)    ! Z direction cosine ! with SC plane.
        REAL dc_VX(Maxparticles)     ! DOCA X coordinate 
        REAL dc_VY(Maxparticles)     ! DOCA Y coordinate ! to the beam line Z coordinate 
        REAL dc_VZ(Maxparticles)     ! DOCA Z coordinate
        REAL dc_VR(Maxparticles)     ! Distance from the beam line 
        REAL dc_c2 (Maxparticles)     ! Chisquar of track fitting
        INTEGER DC_trk(Maxparticles)  ! DC track ID (if TB track, not in the N-tuple)
        INTEGER DC_STAT(Maxparticles) ! DC Status = TBtrID+100*DCSect if no TBT =-100*DCSect 
      COMMON/dcpb_nt/dc_part,dc_stat,
     +dc_xsc,dc_ysc,dc_zsc,dc_cxsc,dc_cysc,dc_czsc,
     +dc_vr,dc_c2,dc_sect,dc_trk,dc_vx,dc_vy,dc_vz
C-
        INTEGER sc_part            ! number of SC hits involved in particles 
        REAL EDEP(Maxparticles)          ! Energy deposited in SC (GeV)
        REAL sc_T(Maxparticles)           ! Time of SC hit (ns)
        REAL sc_R(Maxparticles)           ! Trek lenght to the SC plane (cm)
        REAL sc_c2(Maxparticles)           ! Geom.matching qual.
        INTEGER sc_SECT(Maxparticles)      ! SC hit sector (not in the N-tuple)
        INTEGER sc_HIT (Maxparticles)      ! SC hit ID (not in the N-tuple)
        INTEGER sc_pd (Maxparticles)       ! SC paddel ID (not in the N-tuple)
        INTEGER sc_stat (Maxparticles)     ! SC status = status + 100*SCPaddle + 10000*SCSect
      COMMON/scpart_nt/sc_part,sc_stat
     +,edep,sc_t,sc_r,sc_c2,sc_sect,sc_hit,sc_pd
C- Not in the N-tuple
        INTEGER cc_part            ! number of CC hits involved in particles 
        INTEGER NPHE (Maxparticles)         ! Number of photo-electron in CC hit
        REAL cc_T(Maxparticles)             ! Time of CC hit (ns)
        REAL cc_R(Maxparticles)            ! Trek lenght to the CC plane (cm)
        REAL cc_c2(Maxparticles)            ! Geom.Match qual.
        INTEGER cc_SECT(Maxparticles)        ! CC hit sector
        INTEGER cc_HIT (Maxparticles)         ! CC hit ID
        INTEGER cc_segm (Maxparticles)         ! CC hit segment
      COMMON/ccpart_nt/cc_part,cc_sect,cc_hit,cc_segm, 
     +nphe,cc_t,cc_r,cc_c2
C-
        INTEGER lac_part            ! Nuber of EC contained particles 
        REAL lec_etot(Maxparticles)      ! Total deposited energy (GeV)
        REAL lec_ein(Maxparticles)      ! Inner energy (GeV)
        REAL lec_t(Maxparticles)      ! Time (ns)
        REAL lec_X(Maxparticles)     ! X coordinate of the hit
        REAL lec_Y(Maxparticles)     ! Y coordinate of the hit
        REAL lec_Z(Maxparticles)     ! Z coordinate of the hit
        REAL lec_r(Maxparticles)     ! Length from target (cm)
        REAL lec_c2(Maxparticles)    ! Matching qual.
        INTEGER lec_hit(Maxparticles)  ! LAC hit ID (Not in the N-tuple)
        INTEGER lEC_sect(Maxparticles)  ! LAC hit sector (Not in the N-tuple)
        INTEGER lEC_stat(Maxparticles)  ! LAC hit status = hitID+100*Sector
      COMMON/lecpart_nt/lac_part,lec_stat,
     +lec_etot,lec_ein,lec_t,lec_r,lec_x,lec_y,lec_z,lec_c2,lec_sect,lec_hit
     
        INTEGER st_part                       ! number of ST hits
        INTEGER ST_SECTOR(MaxParticles)       ! ST hit sector (Not in the N-tuple)
        INTEGER ST_IHIT(MaxParticles)         ! ST hit ID (Not in the N-tuple)
        INTEGER ST_TRKNO(MaxParticles)	      ! track number defined by HBTR
        REAL ST_TIME(MaxParticles)            ! Time of ST hit (ns)
        REAL ST_RTRK(MaxParticles)            ! Trek lenght to the ST plane (cm)
        INTEGER ST_STATUS(MaxParticles)       ! Status word = hitID+100*ST_TRKNO+10000*Sector
      COMMON/stpart_nt/st_part,ST_STATUS,ST_TIME,ST_RTRK,
     & ST_SECTOR,ST_IHIT,ST_TRKNO
     

c----6----------------------------------------------------------------72




