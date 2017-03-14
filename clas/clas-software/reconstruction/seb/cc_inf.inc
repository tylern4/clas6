c      include file CC_INF
c
c_begin_doc
c  RCS ID string
c  $Id: cc_inf.inc,v 1.5 2001/11/16 20:04:48 vlassov Exp $
c
c  Documentation for include file CC_INF
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
c  Author:         Created:  Thu Apr  3 21:54:06 EST 1997
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
c  variables for include file CC_INF:
c  -----------------------------------
c  RCS information: 
      CHARACTER*(132)  CICC_INF
      DATA CICC_INF/
     1'$Id: cc_inf.inc,v 1.5 2001/11/16 20:04:48 vlassov Exp $'
     2/
c----6----------------------------------------------------------------72
c  
c  Variables contained in CC_INF common block
c  --------------------------------------------
c
c- CC information Structere for identifyed particles:
        REAL CCTIME             ! Time of CC hit (ns)
        REAL CCR_TRK            ! Trek lenght to the CC plane (cm)
        INTEGER CCNPHE          ! Number of photo-electron in CC hit
        INTEGER CCSECTOR        ! CC hit sector
        INTEGER CCIHIT          ! CC hit ID
        INTEGER CCPART          ! Particle row number for CC hit
        INTEGER CCSTATUS        ! Status word - now 10*(CC segment number) 
c                               ! + 1000*( 1 + phy_index).
c                               ! PHY_INDEX = -1 : left  PMT (1,3,5...35)
c                               ! PHY_INDEX = +1 : right PMT (2,4,6...36)
c          ! PHY_INDEX =  0 : both left and right (electr. near midplane)
c_end_var
c
       COMMON/CC_INF/CCTIME(MaxParticles),CCR_TRK(MaxParticles),
     & CCNPHE(MaxParticles),CCSECTOR(MaxParticles),
     & CCIHIT(MaxParticles),CCPART(MaxParticles),CCSTATUS(MaxParticles)
c----6----------------------------------------------------------------72




