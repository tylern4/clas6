c      include file SEB_NT
c
c_begin_doc
c  RCS ID string
c  $Id: seb_ntn.inc,v 1.5 2008/03/16 04:33:10 fklein Exp $
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
     1'$Id: seb_ntn.inc,v 1.5 2008/03/16 04:33:10 fklein Exp $'
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
        INTEGER evntype            ! event type
        INTEGER evntclas            ! trigger bits 
        REAL Q_l                  ! Live gated FC
        REAL T_l                  ! Live time
        REAL tr_time              ! trigger time
        REAL rf_time              ! RF time
        INTEGER lt1               ! Level 1 trigger latch
        INTEGER hlsc              ! Helicity scaler
        INTEGER intt              ! Interrupt time
        INTEGER rstr1             ! First ADC for raster (IW(ind+3) of the FBPM)
        INTEGER rstr2             ! Second ADC for raster (IW(ind+6) of the FBPM)
      COMMON/HEVT_NTN/npart,evntid,evntclas
     +,q_l,t_l,tr_time,rf_time,lt1,hlsc,intt,rstr1,rstr2,evstat,evntype
C-
        INTEGER ID(Maxparts)  ! Final PID (PDG ID)
        REAL P(Maxparts)      ! Particle momentum (GeV/c)
        REAL M(Maxparts)      ! Particle mass2 (GeV/c)2 (Not in N-tuple)
        INTEGER Q(Maxparts)   ! Particle charge (+1,0,-1 in the units of e)
        REAL B(Maxparts)      ! Particle velocity in the units of c
        REAL CX(Maxparts)     ! X direction cosine at the origin
        REAL CY(Maxparts)     ! Y direction cosine at the origin
        REAL CZ(Maxparts)     ! Z direction cosine at the origin
        REAL VX(Maxparts)     ! X coordinate of vertex (cm)
        REAL VY(Maxparts)     ! Y coordinate of vertex (cm)
        REAL VZ (Maxparts)    ! Z coordinate of vertex (cm)
        INTEGER DC(Maxparts)  ! Status of DC (>0 DCPB row numb., =0 no DC ->Not in N-tuple) 
        INTEGER CC(Maxparts)  ! Status of CC (>0 CCPB row numb., =0 no CC ->Not in N-tuple)
        INTEGER SC(Maxparts)  ! Status of SC (>0 SCPB row numb., =0 no SC ->Not in N-tuple)
        INTEGER EC(Maxparts)  ! Status of EC (>0 ECPB row numb., =0 no EC ->Not in N-tuple)
        INTEGER LEC(Maxparts)  ! Status of LEC (>0 LCPB row numb., =0 no LAC ->Not in N-tuple)
c        INTEGER CCST(Maxparts)
        INTEGER ST(Maxparts)  ! Status of ST (>0 STPB row numb., =0 no S ->Not in N-tupleT)
        INTEGER STAT(Maxparts)! Status of particle stat*(DC+100*SC+10000*EC+1000000*LAC+ST*100000000)
C stat 1 if particle is identified, -1 that hit(s)/tracks were matched but no TB or ID, 0 - if undef.
      COMMON/EVNT_NTN/gpart,id,stat,
     +p,q,b,cx,cy,cz,vx,vy,vz,m,dc,cc,sc,ec,lec,st
C-
        INTEGER ec_part              ! Nuber of EC contained particles 
        REAL etot(Maxparts)      ! Total deposited energy (GeV)
        REAL ec_ei(Maxparts)     ! Inner energy (GeV)
        REAL ec_eo(Maxparts)     ! Outer energy (GeV)
        REAL ec_t(Maxparts)      ! EC hit time
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
        INTEGER ec_whol(Maxparts)   ! whole hit ID (Not in the N-tuple)
        INTEGER EC_sect(Maxparts)  ! EC hit sector (Not In the N-tuple)
        INTEGER EC_stat(Maxparts)  ! EC hit status = Wh+100*Inh+10000*Ouh+1000000*sect
      COMMON/ecpb_ntn/ec_part,ec_stat,ec_inst,ec_oust,
     +etot,ec_ei,ec_eo,ec_t,ec_r,ech_x,ech_y,ech_z,ec_m2,ec_c2,ec_m3,ec_m4,ec_sect,ec_whol
C-
        INTEGER dc_part   ! Number of DC hits involved in the particles.
        INTEGER dc_sect(Maxparts)   ! DC track sector (not in the N-tuple)
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
        INTEGER DC_trk(Maxparts)  ! DC track ID (if TB track, not in the N-tuple)
        INTEGER DC_STAT(Maxparts) ! DC Status = TBtrID+100*DCSect if no TBT =-100*DCSect 
c        INTEGER TB_ST(Maxparts) ! TB hit on layers
c      COMMON/dcpb_ntn/dc_part,dc_sect,dc_trk,dc_stat,tb_st,
c     +dc_xsc,dc_ysc,dc_zsc,dc_cxsc,dc_cysc,dc_czsc,
c     +dc_vx,dc_vy,dc_vz,dc_vr,tl1_cx,tl1_cy,tl1_cz,tl1_x,tl1_y,tl1_z,
c     +tl1_r,dc_c2
      COMMON/dcpb_ntn/dc_part,dc_stat,
     +dc_xsc,dc_ysc,dc_zsc,dc_cxsc,dc_cysc,dc_czsc,
     +dc_vr,dc_c2,
     +tl1_cx,tl1_cy,tl1_cz,tl1_x,tl1_y,tl1_z,tl1_r,dc_sect,dc_trk
C-
        INTEGER sc_part            ! number of SC hits involved in particles 
        REAL EDEP(Maxparts)          ! Energy deposited in SC (GeV)
        REAL sc_T(Maxparts)           ! Time of SC hit (ns)
        REAL sc_R(Maxparts)           ! Trek lenght to the SC plane (cm)
        REAL sc_c2(Maxparts)           ! Geom.matching qual.
        INTEGER sc_SECT(Maxparts)      ! SC hit sector (not in the N-tuple)
        INTEGER sc_HIT (Maxparts)      ! SC hit ID (not in the N-tuple)
        INTEGER sc_pd (Maxparts)       ! SC paddel ID (not in the N-tuple)
        INTEGER sc_stat (Maxparts)     ! SC status = status + 100*SCPaddle + 10000*SCSect
      COMMON/scpart_ntn/sc_part,sc_stat
     +,edep,sc_t,sc_r,sc_c2,sc_sect,sc_hit,sc_pd
C- Not in the N-tuple
        INTEGER cc_part            ! number of CC hits involved in particles 
        INTEGER NPHE (Maxparts)         ! Number of photo-electron in CC hit
        REAL cc_T(Maxparts)             ! Time of CC hit (ns)
        REAL cc_R(Maxparts)            ! Trek lenght to the CC plane (cm)
        REAL cc_c2(Maxparts)            ! Geom.Match qual.
        INTEGER cc_SECT(Maxparts)        ! CC hit sector
        INTEGER cc_HIT (Maxparts)         ! CC hit ID
        INTEGER cc_segm (Maxparts)         ! CC hit segment
      COMMON/ccpart_ntn/cc_part,cc_sect,cc_hit,cc_segm, 
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
        INTEGER lec_hit(Maxparts)  ! LAC hit ID (Not in the N-tuple)
        INTEGER lEC_sect(Maxparts)  ! LAC hit sector (Not in the N-tuple)
        INTEGER lEC_stat(Maxparts)  ! LAC hit status = hitID+100*Sector
      COMMON/lecpart_ntn/lac_part,lec_stat,
     +lec_etot,lec_ein,lec_t,lec_r,lec_x,lec_y,lec_z,lec_c2,lec_sect,lec_hit
C-
        INTEGER st_part                       ! number of ST hits
        INTEGER ST_SECTOR(Maxparts)       ! ST hit sector (Not in the N-tuple)
        INTEGER ST_IHIT(Maxparts)         ! ST hit ID (Not in the N-tuple)
        INTEGER ST_TRKNO(Maxparts)	      ! track number defined by HBTR
        REAL ST_TIME(Maxparts)            ! Time of ST hit (ns)
        REAL ST_RTRK(Maxparts)            ! Trek lenght to the ST plane (cm)
        INTEGER ST_STATUS(Maxparts)       ! Status word = hitID+100*ST_TRKNO+10000*Sector
      COMMON/stpart_ntn/st_part,ST_TIME,ST_RTRK,
     & ST_STATUS,ST_SECTOR,ST_IHIT,ST_TRKNO
     
        INTEGER taghit			! number of hits in tagger
        REAL E_gamma(Maxparts) 	! photon energy
        REAL T_gamma_noRF(Maxparts)	! time photon before RF correction
        REAL T_gamma(Maxparts)	! time photon
        INTEGER tagstat(Maxparts)	! status 
        INTEGER Tid(Maxparts)	! T counter id
        INTEGER Eid(Maxparts)	! E counter id
      COMMON/taghit_ntn/taghit,E_gamma,T_gamma_noRF,T_gamma,tagstat,Tid,Eid   
      
        INTEGER TAGGOODHIT 	         ! number of tagger hits match with ST
        INTEGER TAG_PTR(Maxparts)	 ! pointer to the track in TAGR 
        REAL VERTEX_TIME(Maxparts)   ! event start time at interaction point(ns)
        REAL TAG_ENERGY(Maxparts)    ! Photon Energy(GeV)
        REAL DT_ST_TAG(Maxparts)     ! starttime_ST - starttime_TAG
      COMMON/TAG_NTN/TAGGOODHIT,TAG_PTR,VERTEX_TIME,TAG_ENERGY,DT_ST_TAG

c----6----------------------------------------------------------------72




