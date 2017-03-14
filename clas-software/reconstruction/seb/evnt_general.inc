c      include file EVNT_GENERAL
c
c_begin_doc
c  RCS ID string
c  $Id: evnt_general.inc,v 1.12 2005/09/06 18:23:08 stepanya Exp $
c
c  Documentation for include file EVNT_GENERAL
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
c  Author:         Created:  Tue Mar 25 18:23:31 EST 1997
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
c  variables for include file EVNT_GENERAL:
c  -----------------------------------
c  RCS information: 
      CHARACTER*(132)  CIEVNT_GENERAL
      DATA CIEVNT_GENERAL/
     1'$Id: evnt_general.inc,v 1.12 2005/09/06 18:23:08 stepanya Exp $'
     2/
c----6----------------------------------------------------------------72
c  
c  Variables contained in EVNT_GENERAL common block
c  --------------------------------------------
c
c_end_var
      INTEGER MaxParticles  ! Allowed maximum nuber of particles in the event. 
      PARAMETER (MaxParticles=200)
c
      INTEGER vrt_pln
      PARAMETER (vrt_pln=1)
      INTEGER st_pln
      PARAMETER (st_pln=vrt_pln+1)
      INTEGER cc_pln
      PARAMETER (cc_pln=st_pln+2)
      INTEGER sc_pln
      PARAMETER (sc_pln=cc_pln+1)
      INTEGER fec_pln
      PARAMETER (fec_pln=sc_pln+4)
      INTEGER lec_pln
      PARAMETER (lec_pln=fec_pln+1)
      INTEGER MAXsector
      PARAMETER (MAXsector=6)
c
      REAL TTIME         ! Trigger time at the target
      REAL RF_TIME(3)    ! RF times
      REAL Q_t           ! Total Charge on FC
      REAL Q_t_l         ! Live gated Charge on FC
      REAL T_l_T         ! Live time
      REAL I_FC          ! Beam current
      REAL rftest(3)
      INTEGER TPARTICLES ! Number of trigger particles (e' =1, e'p =2 ...)
      INTEGER EVENT_STAT ! Status of built event
      INTEGER GPARTICLES ! Number of geometrically reconstructed particles 
      INTEGER NPARTICLES ! Number of final reconstructed particles
      INTEGER EPARTICLES ! Number of particles with EC hit
      INTEGER LPARTICLES ! Number of particles with EC hit
      INTEGER SPARTICLES ! Number of particles with SC hit
      INTEGER CPARTICLES ! Number of particles with CC hit
      INTEGER DPARTICLES ! Number of particles with DC track
      INTEGER STPARTICLES ! Number of particles with ST hit
      INTEGER GINDEX(Maxparticles)
      COMMON/EVNT_GENERAL/EVENT_STAT,GPARTICLES,NPARTICLES,TPARTICLES,
     &EPARTICLES,LPARTICLES,SPARTICLES,CPARTICLES,DPARTICLES,STPARTICLES,
     &GINDEX,TTIME,RF_TIME,Q_t,Q_t_l,T_l_T,I_FC,rftest

c----6----------------------------------------------------------------72




