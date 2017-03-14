        INTEGER Maxparticles
        PARAMETER (Maxparticles=40)
        INTEGER MaxST
        PARAMETER (MaxST=100)

        INTEGER npart, gpart      ! Nuber of final and geometrical particles 
        INTEGER evntid            ! event number 
        INTEGER evstat            ! event status
        INTEGER evtype            ! event type
        INTEGER evclas            ! trigger bits 
	INTEGER evthel	          ! helicity of event
	INTEGER evthel2           ! helicity recorded without condition on interrupt time
	INTEGER evntclas2	  ! Reads TRGPRS from bos file
        REAL Q_l                  ! Live gated FC
        REAL T_l                  ! Live time
        REAL tr_time              ! trigger time
        REAL rf_time1             ! RF time
        REAL rf_time2             ! RF time		
      COMMON/HEVT_NT/npart,evstat,evntid,evtype,evclas
     +,evthel,evntclas2,q_l,t_l,tr_time,rf_time1,rf_time2

        INTEGER ID(Maxparticles)  ! Final PID (PDG ID)
        REAL P(Maxparticles)      ! Particle momentum (GeV/c)
        REAL M(Maxparticles)      ! Particle mass (GeV/c)
        INTEGER Q(Maxparticles)   ! Particle charge (+1,0,-1 in the units of e)
        REAL B(Maxparticles)      ! Particle velocity in the units of c
        REAL CX(Maxparticles)     ! X direction cosine at the origin
        REAL CY(Maxparticles)     ! Y direction cosine at the origin
        REAL CZ(Maxparticles)     ! Z direction cosine at the origin
        REAL VX(Maxparticles)     ! X coordinate of vertex (cm)
        REAL VY(Maxparticles)     ! Y coordinate of vertex (cm)
        REAL VZ (Maxparticles)    ! Z coordinate of vertex (cm)
        INTEGER DC(Maxparticles)  ! Status of DC (>0 DCST structure ID, =0 no DC is involved)
        INTEGER CC(Maxparticles)  ! Status of CC (>0 CCST structure ID, =0 no CC is involved)
        INTEGER SC(Maxparticles)  ! Status of SC (>0 SCST structure ID, =0 no SC is involved)
        INTEGER EC(Maxparticles)  ! Status of EC (>0 ECST structure ID, =0 no EC is involved)
        INTEGER LEC(Maxparticles) ! Status of EC (>0 ECST structure ID, =0 no EC is involved)
        INTEGER STAT(Maxparticles)! Status of hits/track matching 
      COMMON/EVNT_NT/gpart,id,stat,dc,cc,sc,ec,lec,
     +p,m,q,b,cx,cy,cz,vx,vy,vz

        INTEGER ec_part              ! Nuber of EC contained particles 
        REAL etot(Maxparticles)      ! Total deposited energy (GeV)
        REAL ec_ei(Maxparticles)     ! Inner energy (GeV)
        REAL ec_eo(Maxparticles)     ! Outer energy (GeV)
        REAL ec_t(Maxparticles)      ! Particle velocity in the units of c
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
        INTEGER ec_whol(Maxparticles)   ! whole hit ID
        INTEGER EC_sect(Maxparticles)  ! EC hit sector
        INTEGER EC_stat(Maxparticles)  ! EC hit status
      COMMON/ecpb_nt/ec_part,ec_stat,ec_sect,ec_whol,ec_inst,ec_oust,
     +etot,ec_ei,ec_eo,ec_t,ec_r,ech_x,ech_y,ech_z,ec_m2,ec_m3,ec_m4,ec_c2

        INTEGER dc_part   ! Number of DC hits involved in the particles.
        INTEGER dc_sect(Maxparticles)   ! DC track sector
        REAL dc_XSC(Maxparticles)     ! X coordinate  ! of the 
        REAL dc_YSC(Maxparticles)     ! Y coordinate  ! intersection point
        REAL dc_ZSC(Maxparticles)     ! Z coordinate  ! with SC plane.
        REAL dc_CXSC(Maxparticles)    ! X direction cosine ! at the 
        REAL dc_CYSC(Maxparticles)    ! Y direction cosine ! intersection point
        REAL dc_CZSC(Maxparticles)    ! Z direction cosine ! with SC plane.
        REAL dc_XEC(Maxparticles)     ! X coordinate  ! of the 
        REAL dc_YEC(Maxparticles)     ! Y coordinate  ! intersection point
        REAL dc_ZEC(Maxparticles)     ! Z coordinate  ! with EC plane.
        REAL dc_THCC (Maxparticles)   ! strait line theta from orgn to int.point
        REAL dc_c2 (Maxparticles)     ! Chisquar of track fitting
        INTEGER DC_trk(Maxparticles)  ! DC track ID
        INTEGER DC_STAT(Maxparticles) ! Status word
      COMMON/dcpb_nt/dc_part,dc_sect,dc_trk,dc_stat,
     +dc_xsc,dc_ysc,dc_zsc,dc_cxsc,dc_cysc,dc_czsc,
     +dc_xec,dc_yec,dc_zec,dc_thcc,dc_c2

        INTEGER sc_part            ! number of SC hits involved in particles 
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

        INTEGER lac_part            ! Nuber of EC contained particles 
        REAL lec_etot(Maxparticles)      ! Total deposited energy (GeV)
        REAL lec_t(Maxparticles)      ! Time (ns)
        REAL lec_X(Maxparticles)     ! X coordinate of the hit
        REAL lec_Y(Maxparticles)     ! Y coordinate of the hit
        REAL lec_Z(Maxparticles)     ! Z coordinate of the hit
        REAL lec_r(Maxparticles)     ! Length from target (cm)
        REAL lec_c2(Maxparticles)    ! Matching qual.
        INTEGER lec_hit(Maxparticles)  ! LAC hit ID
        INTEGER lEC_sect(Maxparticles)  ! LAC hit sector
        INTEGER lEC_stat(Maxparticles)  ! LAC hit status
      COMMON/lecpart_nt/lac_part,lec_sect,lec_hit,lec_stat,
     +lec_etot,lec_t,lec_r,lec_x,lec_y,lec_z,lec_c2

        INTEGER st_part                       ! number of ST hits
        INTEGER ST_SECTOR(MaxST)       ! ST hit sector (Not in the N-tuple)
        INTEGER ST_IHIT(MaxST)         ! ST hit ID (Not in the N-tuple)
        INTEGER ST_TRKNO(MaxST)	      ! track number defined by HBTR
        REAL ST_TIME(MaxST)            ! Time of ST hit (ns)
        REAL ST_RTRK(MaxST)            ! Trek lenght to the ST plane (cm)
        INTEGER ST_STATUS(MaxST)       ! Status word = hitID+100*ST_TRKNO+10000*Sector
      COMMON/stpart_nt/st_part,ST_STATUS,ST_TIME,ST_RTRK,
     & ST_SECTOR,ST_IHIT,ST_TRKNO
        
        INTEGER TAGGOODHIT 	         ! number of tagger hits match with ST
        INTEGER TAG_PTR(MaxST)	 ! pointer to the track in TAGR 
        REAL VERTEX_TIME(MaxST)   ! event start time at interaction point(ns)
        REAL TAG_ENERGY(MaxST)    ! Photon Energy(GeV)
        REAL DT_ST_TAG(MaxST)     ! starttime_ST - starttime_TAG
      COMMON/TAG_NT/TAGGOODHIT,TAG_PTR,VERTEX_TIME,TAG_ENERGY,DT_ST_TAG

