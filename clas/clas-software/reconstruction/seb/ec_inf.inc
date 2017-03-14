c      include file EC_INF
c
c_begin_doc
c  RCS ID string
c  $Id: ec_inf.inc,v 1.6 1999/10/17 14:30:29 stepanya Exp $
c
c  Documentation for include file EC_INF
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
c  Author:         Created:  Thu Apr  3 21:54:40 EST 1997
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
c  variables for include file EC_INF:
c  -----------------------------------
c  RCS information: 
      CHARACTER*(132)  CIEC_INF
      DATA CIEC_INF/
     1'$Id: ec_inf.inc,v 1.6 1999/10/17 14:30:29 stepanya Exp $'
     2/
c----6----------------------------------------------------------------72
c  
c  Variables contained in EC_INF common block
c  --------------------------------------------
c
        INTEGER ECID(MaxParticles)      ! PID defined by EC (PDG ID)
        REAL ECE_TOT(MaxParticles)      ! Total energy loss in EC (GeV)
        REAL ECE_IN (MaxParticles)      ! energy losse (GeV) in the INNER layer
        REAL ECE_OUT(MaxParticles)      ! energy losse (GeV) in the OUTER layer
        INTEGER ECQ(MaxParticles)       ! Charge defined by EC (+1,0,-1 in the units of e)
        REAL ECX (MaxParticles)         ! X coordinate on the face of EC
        REAL ECY (MaxParticles)         ! Y coordinate on the face of EC
        REAL ECZ (MaxParticles)         ! Z coordinate on the face of EC
        REAL ECTIME(MaxParticles)       ! Time of EC inner hit (ns)
        REAL ECT_in(MaxParticles)       ! Time of EC inner hit (ns)
        REAL ECT_out(MaxParticles)      ! Time of EC outer hit (ns)
        REAL ECR_TRK(MaxParticles)      ! Trek lenght to the EC plane (cm)
        INTEGER ECSECTOR(MaxParticles)  ! EC hit sector
        INTEGER ECHIT(WHOLE:OUTER,MaxParticles)     ! EC hit ID for the layers WHOLE, INNER and OUTER 
        INTEGER ECPART(MaxParticles)    ! Particle row number for EC hit  
        INTEGER ECSTATUS(MaxParticles)  ! Status word 
c_end_var
c
       COMMON/EC_INF/ECE_TOT,ECE_IN,ECE_OUT,ECX,ECY,ECZ,ECTIME,
     &ECT_in,ECT_out,ECR_TRK,ECID,ECQ,ECSECTOR,ECHIT,ECPART,ECSTATUS
c----6----------------------------------------------------------------72




