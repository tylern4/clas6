c      include file PARTICLE_OUT
c
c_begin_doc
c  RCS ID string
c  $Id: particle_out.inc,v 1.9 2006/06/15 21:35:31 stepanya Exp $
c
c  Documentation for include file PARTICLE_OUT
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
c  Author:   S. Stepanyan      Created:  Sun Feb 23 21:51:41 EST 1997
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
c  variables for include file PARTICLE_OUT:
c  -----------------------------------
c  RCS information: 
      CHARACTER*(132)  CIPARTICLE_OUT
      DATA CIPARTICLE_OUT/
     1'$Id: particle_out.inc,v 1.9 2006/06/15 21:35:31 stepanya Exp $'
     2/
c----6----------------------------------------------------------------72
c  
c  Variables contained in PARTICLE_OUT common block
c  --------------------------------------------
c
c-
c- Identifyed particles information Structere:
c-
        INTEGER PARTID      ! Final PID (PDG ID)
        REAL PARTP          ! Particle momentum (GeV/c)
        REAL PARTM          ! Particle mass (GeV/c)
        INTEGER PARTQ       ! Particle charge (+1,0,-1 in the units of e)
        REAL PARTBETTA      ! Particle velocity in the units of c
        REAL PARTCX         ! X direction cosine at the origin
        REAL PARTCY         ! Y direction cosine at the origin
        REAL PARTCZ         ! Z direction cosine at the origin
        REAL PARTVERTX      ! X coordinate of vertex (cm)
        REAL PARTVERTY      ! Y coordinate of vertex (cm)
        REAL PARTVERTZ      ! Z coordinate of vertex (cm)
        INTEGER PARTSTDC    ! Status of DC (>0 DC area ID, =0 no DC track)
        INTEGER PARTSTCC    ! Status of CC (>0 CC area ID, =0 no CC hit)
        INTEGER PARTSTSC    ! Status of SC (>0 SC area ID, =0 no SC hit)
        INTEGER PARTSTEC    ! Status of EC (>0 EC area ID, =0 no EC hit)
        INTEGER PARTSTLEC   ! Status of LAEC (>0 LAC area ID, =0 no LAC hit)
        INTEGER PARTSTST    ! Status of ST (>0 ST area ID, =0 no ST hit)
        REAL PARTCHI2DC     ! Chisquare of track fitting
        REAL PARTCHI2CC     ! Chisquare of geometrical matching
        REAL PARTCHI2SC     ! Chisquare of geometrical matching
        REAL PARTCHI2EC     ! Chisquare of geometrical matching
        REAL PARTCHI2LEC     ! Chisquare of geometrical matching
        REAL PARTCHI2ST     ! Chisquare of geometrical matching
        INTEGER PARTSTATUS  ! Status of hits/track matching 
        REAL TVERTX,TVERTY,TVERTZ
c-          ! =1 geometrical and time matching for hits/tracks with ST**>0
c-          ! =0 no time coincidence between hits/tracks with ST**>0
c-
        INTEGER EVID      ! Final PID (PDG ID)
        REAL EVP          ! Particle momentum (GeV/c)
        REAL EVM          ! Particle mass (GeV/c)
        INTEGER EVQ       ! Particle charge (+1,0,-1 in the units of e)
        REAL EVBETTA      ! Particle velocity in the units of c
        REAL EVCX         ! X direction cosine at the origin
        REAL EVCY         ! Y direction cosine at the origin
        REAL EVCZ         ! Z direction cosine at the origin
        REAL EVVERTX      ! X coordinate of vertex (cm)
        REAL EVVERTY      ! Y coordinate of vertex (cm)
        REAL EVVERTZ      ! Z coordinate of vertex (cm)
        INTEGER EVSTDC    ! Status of DC (>0 DC area ID, =0 no DC trac)
        INTEGER EVSTCC    ! Status of CC (>0 CC area ID, =0 no CC hit)
        INTEGER EVSTSC    ! Status of SC (>0 SC area ID, =0 no SC hit)
        INTEGER EVSTEC    ! Status of EC (>0 EC area ID, =0 no EC hit)
        INTEGER EVSTLEC   ! Status of LAC (>0 EC area ID, =0 no LAC hit)
        INTEGER EVSTST    ! Status of ST (>0 ST area ID, =0 no ST hit)
        REAL EVCHI2DC     ! Chisquare of track fitting
        REAL EVCHI2CC     ! Chisquare of geometrical matching
        REAL EVCHI2SC     ! Chisquare of geometrical matching
        REAL EVCHI2EC     ! Chisquare of geometrical matching
        REAL EVCHI2LEC    ! Chisquare of geometrical matching
        REAL EVCHI2ST     ! Chisquare of geometrical matching
        INTEGER EVSTATUS  ! Status of hits/track matching 
c-          ! =1 geometrical and time matching for hits/tracks with ST**>0
c- end_var
c
      COMMON/PARTICLE_OUT/
     &EVP(MaxParticles),EVM(MaxParticles),
     &EVBETTA(MaxParticles),EVCX(MaxParticles),
     &EVCY(MaxParticles),EVCZ(MaxParticles),EVVERTX(MaxParticles), 
     &EVVERTY(MaxParticles),EVVERTZ(MaxParticles), 
     &EVCHI2DC(MaxParticles),EVCHI2CC(MaxParticles),
     &EVCHI2SC(MaxParticles),EVCHI2EC(whole:outer,MaxParticles),
     &EVCHI2LEC(MaxParticles),EVCHI2ST(MaxParticles),
     &EVID(MaxParticles),EVQ(MaxParticles),EVSTDC(MaxParticles),
     &EVSTCC(MaxParticles),EVSTSC(MaxParticles),EVSTEC(MaxParticles),
     &EVSTLEC(MaxParticles),EVSTST(MaxParticles),EVSTATUS(MaxParticles),
     &PARTP(MaxParticles),PARTM(MaxParticles),
     &PARTBETTA(MaxParticles),PARTCX(MaxParticles),
     &PARTCY(MaxParticles),PARTCZ(MaxParticles),PARTVERTX(MaxParticles), 
     &PARTVERTY(MaxParticles),PARTVERTZ(MaxParticles), 
     &PARTCHI2DC(MaxParticles),PARTCHI2CC(MaxParticles),
     &PARTCHI2SC(MaxParticles),PARTCHI2EC(whole:outer,MaxParticles),
     &PARTCHI2LEC(MaxParticles),PARTCHI2ST(MaxParticles),
     &PARTID(MaxParticles),PARTQ(MaxParticles),PARTSTDC(MaxParticles),
     &PARTSTCC(MaxParticles),PARTSTSC(MaxParticles),
     &PARTSTEC(MaxParticles),PARTSTLEC(MaxParticles),PARTSTST(MaxParticles),
     &PARTSTATUS(MaxParticles),TVERTX,TVERTY,TVERTZ
c----6----------------------------------------------------------------72




