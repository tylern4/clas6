c      include file LEC_INF
c
c_begin_doc
c  RCS ID string
c  $Id: lec_inf.inc,v 1.3 1999/07/24 21:52:04 stepanya Exp $
c
c  Documentation for include file LEC_INF
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
c  Author:   S.Stepanyan      Created:  Fri Mar  6 16:12:15 EST 1998
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
c  variables for include file LEC_INF:
c  -----------------------------------
c  RCS information: 
      CHARACTER*(132)  CILEC_INF
      DATA CILEC_INF/
     1'$Id: lec_inf.inc,v 1.3 1999/07/24 21:52:04 stepanya Exp $'
     2/
c----6----------------------------------------------------------------72
c  
c  Variables contained in LEC_INF common block
c  --------------------------------------------
c
        INTEGER LECID(MaxParticles)       ! PID defined by LAC (PDG ID)
        REAL LECE(MaxParticles)          ! Total energy loss in LAC (GeV)
        REAL LECEi(MaxParticles)          ! Energy loss in the inner part of LAC (GeV)
        REAL LECX(MaxParticles)          ! X coordinate on the face of LAC
        REAL LECY(MaxParticles)          ! Y coordinate on the face of LAC
        REAL LECZ(MaxParticles)          ! Z coordinate on the face of LAC
        REAL LECTIME(MaxParticles)       ! Time of LAC hit (ns)
        REAL LECR_TRK(MaxParticles)      ! Trek lenght to the LAC plane (cm)
        INTEGER LECSECTOR(MaxParticles)  ! LAC hit sector
        INTEGER LECHIT(3,MaxParticles)     ! LAC hit ID 
        INTEGER LECPART(MaxParticles)    ! Particle row number for LAC hit  
        INTEGER LECSTATUS(MaxParticles)  ! Status word 
c_end_var
c
       COMMON/LEC_INF/LECE,LECEi,LECX,LECY,LECZ,LECTIME,LECR_TRK,
     +LECID,LECSECTOR,LECHIT,LECPART,LECSTATUS
c----6----------------------------------------------------------------72




