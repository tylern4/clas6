c      include file SC_INF
c
c_begin_doc
c  RCS ID string
c  $Id: sc_inf.inc,v 1.6 1998/03/09 23:54:33 stepanya Exp $
c
c  Documentation for include file SC_INF
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
c  Author:         Created:  Thu Apr  3 21:54:26 EST 1997
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
c  variables for include file SC_INF:
c  -----------------------------------
c  RCS information: 
      CHARACTER*(132)  CISC_INF
      DATA CISC_INF/
     1'$Id: sc_inf.inc,v 1.6 1998/03/09 23:54:33 stepanya Exp $'
     2/
c----6----------------------------------------------------------------72
c  
c  Variables contained in SC_INF common block
c  --------------------------------------------
c
        INTEGER SCID            ! PID defined by SC (PDG ID)
        REAL SCE_DEP            ! Energy deposited in SC (GeV)
        REAL SCTIME             ! Time of SC hit (ns)
        REAL SCR_TRK            ! Trek lenght to the SC plane (cm)
        INTEGER SCSECTOR        ! SC hit sector
        INTEGER SCPDID          ! SC paddel ID
        INTEGER SCIHIT          ! SC hit ID
        INTEGER SCPART          ! Particle row number for SC hit
        INTEGER SCTRK_PLN       ! SC plane ID for swimed track.
        INTEGER SCSTATUS        ! Status word (not yet defined)
c_end_var
c
      COMMON/SC_INF/SCE_DEP(MaxParticles),SCTIME(MaxParticles),
     &SCR_TRK(MaxParticles),SCID(MaxParticles),SCSECTOR(MaxParticles),
     &SCIHIT(MaxParticles),SCPART(MaxParticles),SCSTATUS(MaxParticles),
     &SCPDID(MaxParticles),SCTRK_PLN(MaxParticles)
c----6----------------------------------------------------------------72




