c      include file SEB_NT
c
c_begin_doc
c  RCS ID string
c  $Id: seb_nt.inc,v 1.16 2008/03/16 04:33:10 fklein Exp $
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
     1'$Id: seb_nt.inc,v 1.16 2008/03/16 04:33:10 fklein Exp $'
     2/
c----6----------------------------------------------------------------72
c  
c  Variables contained in PART_NT common block
c  --------------------------------------------
c
c_end_var
        INTEGER Maxparts
        PARAMETER (Maxparts=40)
c
        INTEGER npart, gpart      ! Nuber of final and geometrical particles 
        INTEGER evntid            ! event number 
        INTEGER evstat            ! event status
        INTEGER evntype           ! event type
        INTEGER evntclas          ! trigger bits 
        INTEGER L2bit             ! Level 2 trigger bits (=0 if NON, <0 if fail)
        INTEGER L3bit             ! Level 3 trigger bit from TGBI
        INTEGER hlsc              ! Helicity scaler
        INTEGER intt              ! Interrupt time
        REAL Q_l                  ! Live gated FC
        REAL T_l                  ! Live time
        REAL tr_time              ! trigger time
        REAL rf_time              ! RF time
      COMMON/HEVT_NT/npart,evstat,evntid,evntype,evntclas
     +,q_l,t_l,tr_time,rf_time,l2bit,l3bit,hlsc,intt
C-
        INTEGER ID(Maxparts)  ! Final PID (PDG ID)
        REAL P(Maxparts)      ! Particle momentum (GeV/c)
        REAL M(Maxparts)      ! Particle mass (GeV/c)
        INTEGER Q(Maxparts)   ! Particle charge (+1,0,-1 in the units of e)
        REAL B(Maxparts)      ! Particle velocity in the units of c
        REAL CX(Maxparts)     ! X direction cosine at the origin
        REAL CY(Maxparts)     ! Y direction cosine at the origin
        REAL CZ(Maxparts)     ! Z direction cosine at the origin
        REAL VX(Maxparts)     ! X coordinate of vertex (cm)
        REAL VY(Maxparts)     ! Y coordinate of vertex (cm)
        REAL VZ (Maxparts)    ! Z coordinate of vertex (cm)
        INTEGER DC(Maxparts)  ! Status of DC (>0 DCST structure ID, =0 no DC is involved)
        INTEGER CC(Maxparts)  ! Status of CC (>0 CCST structure ID, =0 no CC is involved)
        INTEGER SC(Maxparts)  ! Status of SC (>0 SCST structure ID, =0 no SC is involved)
        INTEGER EC(Maxparts)  ! Status of EC (>0 ECST structure ID, =0 no EC is involved)
        INTEGER LEC(Maxparts)  ! Status of EC (>0 ECST structure ID, =0 no EC is involved)
        INTEGER ST(Maxparts)  ! Status of ST (>0 STST structure ID, =0 no ST is involved)
        INTEGER STAT(Maxparts)! Status of hits/track matching 
      COMMON/EVNT_NT/gpart,id,stat,dc,cc,sc,ec,lec,st,
     +p,m,q,b,cx,cy,cz,vx,vy,vz
C-
        INTEGER ec_part              ! Nuber of EC contained particles 
        REAL etot(Maxparts)      ! Total deposited energy (GeV)
        REAL ec_ei(Maxparts)     ! Inner energy (GeV)
        REAL ec_eo(Maxparts)     ! Outer energy (GeV)
        REAL ec_t(Maxparts)      ! Particle velocity in the units of c
        REAL ech_X(Maxparts)     ! X coordinate of the hit
        REAL ech_Y(Maxparts)     ! Y coordinate of the hit
        REAL ech_Z(Maxparts)     ! Z coordinate of the hit
        REAL ec_r(Maxparts)      ! Length from target (cm)
        REAL ec_M2(Maxparts)     ! Second moment of shower transvers distr.
        REAL ec_M3(Maxparts)     ! Third moment of shower transvers distr.
        REAL ec_M4(Maxparts)     ! Forth moment of shower transvers distr.
        REAL ec_c2(Maxparts)     ! Qualaty of geometrical matching.
        INTEGER ec_InSt(Maxparts)   ! Number of strips in the inner hit
        INTEGER ec_OuSt(Maxparts)   ! Number of strips in the outer hit
        INTEGER ec_whol(Maxparts)   ! whole hit ID
        INTEGER EC_sect(Maxparts)  ! EC hit sector
        INTEGER EC_stat(Maxparts)  ! EC hit status
      COMMON/ecpb_nt/ec_part,ec_stat,ec_sect,ec_whol,ec_inst,ec_oust,
     +etot,ec_ei,ec_eo,ec_t,ec_r,ech_x,ech_y,ech_z,ec_m2,ec_m3,ec_m4,ec_c2
C-
        INTEGER dc_part   ! Number of DC hits involved in the particles.
        INTEGER dc_sect(Maxparts)   ! DC track sector
        REAL dc_XSC(Maxparts)     ! X coordinate  ! of the 
        REAL dc_YSC(Maxparts)     ! Y coordinate  ! intersection point
        REAL dc_ZSC(Maxparts)     ! Z coordinate  ! with SC plane.
        REAL dc_CXSC(Maxparts)    ! X direction cosine ! at the 
        REAL dc_CYSC(Maxparts)    ! Y direction cosine ! intersection point
        REAL dc_CZSC(Maxparts)    ! Z direction cosine ! with SC plane.
        REAL dc_VX(Maxparts)     ! DOCA X coordinate 
        REAL dc_VY(Maxparts)     ! DOCA Y coordinate ! to the beam line Z coordinate 
        REAL dc_VZ(Maxparts)     ! DOCA Z coordinate
        REAL dc_VR(Maxparts)     ! Distance from the beam line 
        REAL tl1_CX(Maxparts)    ! X direction cosine ! at DC Layer 1
        REAL tl1_CY(Maxparts)    ! Y direction cosine ! 
        REAL tl1_CZ(Maxparts)    ! Z direction cosine ! 
        REAL tl1_X(Maxparts)     ! X coordinate  ! of the 
        REAL tl1_Y(Maxparts)     ! Y coordinate  ! TBT on DC Layer 1
        REAL tl1_Z(Maxparts)     ! Z coordinate  ! 
        REAL tl1_r(Maxparts)     ! Path length to Layer 1. 
        REAL dc_c2 (Maxparts)     ! Chisquar of track fitting
        INTEGER DC_trk(Maxparts)  ! DC track ID
        INTEGER DC_STAT(Maxparts) ! Status word
        INTEGER TB_ST(Maxparts) ! TB hit on layers
      COMMON/dcpb_nt/dc_part,dc_sect,dc_trk,dc_stat,tb_st,
     +dc_xsc,dc_ysc,dc_zsc,dc_cxsc,dc_cysc,dc_czsc,
     +dc_vx,dc_vy,dc_vz,dc_vr,tl1_cx,tl1_cy,tl1_cz,tl1_x,tl1_y,tl1_z,
     +tl1_r,dc_c2
C-
        INTEGER sc_part            ! number of SC hits involved in particles 
        REAL EDEP(Maxparts)          ! Energy deposited in SC (GeV)
        REAL sc_T(Maxparts)           ! Time of SC hit (ns)
        REAL sc_R(Maxparts)           ! Trek lenght to the SC plane (cm)
        REAL sc_c2(Maxparts)           ! Geom.matching qual.
        INTEGER sc_SECT(Maxparts)      ! SC hit sector
        INTEGER sc_HIT (Maxparts)      ! SC hit ID
        INTEGER sc_pd (Maxparts)       ! SC paddel ID
        INTEGER sc_stat (Maxparts)     ! SC status in the particle
      COMMON/scpart_nt/sc_part,sc_sect,sc_hit,sc_pd,sc_stat
     +,edep,sc_t,sc_r,sc_c2
C-
        INTEGER cc_part            ! number of CC hits involved in particles 
        INTEGER NPHE (Maxparts)         ! Number of photo-electron in CC hit
        REAL cc_T(Maxparts)             ! Time of CC hit (ns)
        REAL cc_R(Maxparts)            ! Trek lenght to the CC plane (cm)
        REAL cc_c2(Maxparts)            ! Geom.Match qual.
        INTEGER cc_SECT(Maxparts)        ! CC hit sector
        INTEGER cc_HIT (Maxparts)         ! CC hit ID
        INTEGER cc_segm (Maxparts)         ! CC hit segment
      COMMON/ccpart_nt/cc_part,cc_sect,cc_hit,cc_segm, 
     +nphe,cc_t,cc_r,cc_c2
C-
        INTEGER lac_part            ! Nuber of EC contained particles 
        REAL lec_etot(Maxparts)      ! Total deposited energy (GeV)
        REAL lec_ein(Maxparts)      ! Inner energy (GeV)
        REAL lec_t(Maxparts)      ! Time (ns)
        REAL lec_X(Maxparts)     ! X coordinate of the hit
        REAL lec_Y(Maxparts)     ! Y coordinate of the hit
        REAL lec_Z(Maxparts)     ! Z coordinate of the hit
        REAL lec_r(Maxparts)     ! Length from target (cm)
        REAL lec_c2(Maxparts)    ! Matching qual.
        INTEGER lec_hit(Maxparts)  ! LAC hit ID
        INTEGER lEC_sect(Maxparts)  ! LAC hit sector
        INTEGER lEC_stat(Maxparts)  ! LAC hit status
      COMMON/lecpart_nt/lac_part,lec_sect,lec_hit,lec_stat,
     +lec_etot,lec_ein,lec_t,lec_r,lec_x,lec_y,lec_z,lec_c2
     
        INTEGER st_part                       ! number of ST hits
        INTEGER ST_SECTOR(Maxparts)       ! ST hit sector
        INTEGER ST_IHIT(Maxparts)         ! ST hit ID
        INTEGER ST_TRKNO(Maxparts)	      ! track number defined by HBTR
        REAL ST_TIME(Maxparts)            ! Time of ST hit (ns)
        REAL ST_RTRK(Maxparts)            ! Trek lenght to the ST plane (cm)
        INTEGER ST_STATUS(Maxparts)       ! Status word (not yet defined)
      COMMON/stpart_nt/st_part,ST_SECTOR,ST_IHIT,ST_TRKNO,ST_TIME,ST_RTRK,
     & ST_STATUS
     
        INTEGER taghit			! number of hits in tagger
        REAL E_gamma(Maxparts) 	! photon energy
        REAL T_gamma_noRF(Maxparts)	! time photon before RF correction
        REAL T_gamma(Maxparts)	! time photon
        INTEGER tagstat(Maxparts)	! status 
        INTEGER Tid(Maxparts)	! T counter id
        INTEGER Eid(Maxparts)	! E counter id
      COMMON/taghit_nt/taghit,E_gamma,T_gamma_noRF,T_gamma,tagstat,Tid,Eid   
      
        INTEGER TAGGOODHIT 	         ! number of tagger hits match with ST
        INTEGER TAG_PTR(Maxparts)	 ! pointer to the track in TAGR 
        REAL VERTEX_TIME(Maxparts)   ! event start time at interaction point(ns)
        REAL TAG_ENERGY(Maxparts)    ! Photon Energy(GeV)
        REAL DT_ST_TAG(Maxparts)     ! starttime_ST - starttime_TAG
      COMMON/TAG_NT/TAGGOODHIT,TAG_PTR,VERTEX_TIME,TAG_ENERGY,DT_ST_TAG

c----6----------------------------------------------------------------72




