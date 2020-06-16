c      include file SEB_NT
c
c_begin_doc
c  RCS ID string
c  $Id: seb_ntn.inc,v 1.2 2001/10/04 01:32:55 stepanya Exp $
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
     1'$Id: seb_ntn.inc,v 1.2 2001/10/04 01:32:55 stepanya Exp $'
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
        INTEGER run_num           ! run number 
        INTEGER evntid            ! event number 
        INTEGER evstat            ! event status
        INTEGER evntype           ! event type
        INTEGER evntclas          ! trigger bits 
	INTEGER evnttrip          ! trip flag
        INTEGER L1bit             ! Level 1 trigger bit
        INTEGER L2bit             ! Level 2 trigger bits (=0 if NON, <0 if fail)
        INTEGER L3bit             ! Level 3 trigger bit from TGBI
        INTEGER hlsc              ! Helicity scaler
        INTEGER*8 intt              ! Interrupt time
        REAL Q_l                  ! Live gated FC
        REAL T_l                  ! Live time
        REAL tr_time              ! trigger time
        REAL rf_time              ! RF time
        INTEGER helicity  
      COMMON/HEVT_NTN/npart,run_num,evstat,evntid,evntype,evntclas,evnttrip
     +,q_l,t_l,tr_time,rf_time,l1bit,l2bit,l3bit,hlsc,intt,helicity


        INTEGER inl1b,L1bit_v(16)            ! Level 1 trigger bit
        COMMON/L1TRG_NT/inl1b,L1bit_v
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
        INTEGER ST(Maxparticles)  ! Status of ST (>0 STPB row numb., =0 no S ->Not in N-tupleT)
        INTEGER STAT(Maxparticles)! Status of particle stat*(DC+100*SC+10000*EC+1000000*LAC+ST*100000000)
C stat 1 if particle is identified, -1 that hit(s)/tracks were matched but no TB or ID, 0 - if undef.
      COMMON/EVNT_NTN/gpart,id,stat,dc,cc,sc,ec,lec,st,
     +p,m,q,b,cx,cy,cz,vx,vy,vz
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
      COMMON/ecpb_ntn/ec_part,ec_stat,ec_sect,ec_whol,ec_inst,ec_oust,
     +etot,ec_ei,ec_eo,ec_t,ec_r,ech_x,ech_y,ech_z,ec_m2,ec_m3,ec_m4,ec_c2
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
        REAL tl1_CX(Maxparticles)    ! X direction cosine ! at DC Layer 1
        REAL tl1_CY(Maxparticles)    ! Y direction cosine ! 
        REAL tl1_CZ(Maxparticles)    ! Z direction cosine ! 
        REAL tl1_X(Maxparticles)     ! X coordinate  ! of the 
        REAL tl1_Y(Maxparticles)     ! Y coordinate  ! TBT on DC Layer 1
        REAL tl1_Z(Maxparticles)     ! Z coordinate  ! 
        REAL tl1_r(Maxparticles)     ! Path length to Layer 1. 
        REAL dc_c2 (Maxparticles)     ! Chisquar of track fitting
        INTEGER DC_trk(Maxparticles)  ! DC track ID (if TB track, not in the N-tuple)
        INTEGER DC_STAT(Maxparticles) ! DC Status = TBtrID+100*DCSect if no TBT =-100*DCSect 
        INTEGER TB_ST(Maxparticles) ! TB hit on layers
      COMMON/dcpb_ntn/dc_part,dc_sect,dc_trk,dc_stat,tb_st,
     +dc_xsc,dc_ysc,dc_zsc,dc_cxsc,dc_cysc,dc_czsc,
     +dc_vx,dc_vy,dc_vz,dc_vr,
c     +tl1_cx,tl1_cy,tl1_cz,tl1_x,tl1_y,tl1_z,tl1_r,
     +dc_c2
C-
        INTEGER sc_part                   ! number of SC hits involved in particles 
        REAL EDEP(Maxparticles)          ! Energy deposited in SC (GeV)
        REAL sc_T(Maxparticles)           ! Time of SC hit (ns)
        REAL sc_R(Maxparticles)           ! Trek lenght to the SC plane (cm)
        REAL sc_c2(Maxparticles)           ! Geom.matching qual.
        INTEGER sc_SECT(Maxparticles)      ! SC hit sector
        INTEGER sc_HIT (Maxparticles)      ! SC hit ID
        INTEGER sc_pd (Maxparticles)       ! SC paddel ID
        INTEGER sc_stat (Maxparticles)     ! SC status in the particle
      COMMON/scpart_nt/sc_part,sc_sect,sc_hit,sc_pd,sc_stat
     +,edep,sc_t,sc_r,sc_c2

C- Not in the N-tuple
        INTEGER cc_part            ! number of CC hits involved in particles 
        INTEGER NPHE (Maxparticles)         ! Number of photo-electron in CC hit
        REAL cc_T(Maxparticles)             ! Time of CC hit (ns)
        REAL cc_R(Maxparticles)            ! Trek lenght to the CC plane (cm)
        REAL cc_c2(Maxparticles)            ! Geom.Match qual.
        INTEGER cc_SECT(Maxparticles)        ! CC hit sector
        INTEGER cc_HIT (Maxparticles)         ! CC hit ID
        INTEGER cc_segm (Maxparticles)         ! CC hit segment
      COMMON/ccpart_ntn/cc_part,cc_sect,cc_hit,cc_segm, 
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
      COMMON/lecpart_ntn/lac_part,lec_stat,
     +lec_etot,lec_ein,lec_t,lec_r,lec_x,lec_y,lec_z,lec_c2,lec_sect,lec_hit
C-
        INTEGER ic_part              ! Nuber of IC particles 
        REAL et(Maxparticles)      ! Total deposited energy (GeV)
        REAL egl(Maxparticles)     ! energy of the lead module (GeV)
        REAL time(Maxparticles)      ! Time
        REAL tgl(Maxparticles)      ! Time of the lead module
        REAL ich_X(Maxparticles)     ! X coordinate of the hit
        REAL ich_Y(Maxparticles)     ! Y coordinate of the hit
        REAL ich_Z(Maxparticles)     ! Z coordinate of the hit
        REAL ich_Xgl(Maxparticles)     ! X coordinate of the lead module
        REAL ich_Ygl(Maxparticles)     ! Y coordinate of the lead module
        REAL ich_Zgl(Maxparticles)     ! Z coordinate of the lead module
        REAL ic_M2(Maxparticles)     ! Second moment of shower transvers distr.
        REAL ic_M3(Maxparticles)     ! Third moment of shower transvers distr.
        REAL ic_M4(Maxparticles)     ! Forth moment of shower transvers distr.
        INTEGER IC_stat(Maxparticles)  ! IC hit status, number of modules
      COMMON/ichb_ntn/ic_part,et,egl,time,tgl,ich_x,ich_y,ich_z
     +,ich_xgl,ich_ygl,ich_zgl,ic_m2,ic_m3,ic_m4,ic_stat
C-
        INTEGER st_part                       ! number of ST hits
        INTEGER ST_SECTOR(MaxParticles)       ! ST hit sector (Not in the N-tuple)
        INTEGER ST_IHIT(MaxParticles)         ! ST hit ID (Not in the N-tuple)
        INTEGER ST_TRKNO(MaxParticles)	      ! track number defined by HBTR
        REAL ST_TIME(MaxParticles)            ! Time of ST hit (ns)
        REAL ST_RTRK(MaxParticles)            ! Trek lenght to the ST plane (cm)
        INTEGER ST_STATUS(MaxParticles)       ! Status word = hitID+100*ST_TRKNO+10000*Sector
      COMMON/stpart_nt/st_part,ST_SECTOR,ST_IHIT,ST_TRKNO,ST_TIME,ST_RTRK,
     & ST_STATUS
     
        INTEGER taghit			! number of hits in tagger
        REAL E_gamma(30) 	! photon energy
        REAL T_gamma_noRF(30)	! time photon before RF correction
        REAL T_gamma(30)	! time photon
        INTEGER tagstat(30)	! status 
        INTEGER Tid(30)	! T counter id
        INTEGER Eid(30)	! E counter id
      COMMON/taghit_nt/taghit,E_gamma,T_gamma_noRF,T_gamma,tagstat,Tid,Eid   
      
        INTEGER TAGGOODHIT 	         ! number of tagger hits match with ST
        INTEGER TAG_PTR(Maxparticles)	 ! pointer to the track in TAGR 
        REAL VERTEX_TIME(Maxparticles)   ! event start time at interaction point(ns)
        REAL TAG_ENERGY(Maxparticles)    ! Photon Energy(GeV)
        REAL DT_ST_TAG(Maxparticles)     ! starttime_ST - starttime_TAG
        integer tag_stat(Maxparticles)
      COMMON/TAG_NTN/TAGGOODHIT,TAG_PTR,VERTEX_TIME,TAG_ENERGY,DT_ST_TAG,tag_stat

c----6----------------------------------------------------------------72




