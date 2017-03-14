      SUBROUTINE RUN_BUILDER(i_entry,TIME_DO)
c
c_begin_doc
c  RCS ID string
c  $Id: run_builder.F,v 1.28 2008/06/12 15:21:40 devita Exp $
c
c  Documentation for subroutine RUN_BUILDER
c
c  Purpose: The main subroutine for particle identification and event building.
c  -------- Should be called once for each event.
c
c  Input Parameters:  NON
c  ----------------
c
c  Output Parameters:  NON
c  -----------------
c
c  Other routines:
c  ---------------
c
c  Notes:
c  ------
c
c  Author:   S. Stepanyan      Created:  Mon Mar 24 21:10:14 EST 1997
c  -------
c
c  Major revisions:
c  ----------------
c      - add call to GTRIG_TIME, use ST or TAG to get trigger time 
c	 if STR bank exists                             :burin 4/9/98  
c
c_end_doc
c
      IMPLICIT NONE
      SAVE
c
c_begin_inc
c  include files :
c  ---------------------
c BOS common block  uncomment the next line for BOS include file
#include "bcs.inc"
#include "clasmdl.inc"
#include "run_control.inc"
#include "evnt_general.inc"
#include "evnt_par.inc"
#include "echits.inc"
#include "particle_out.inc"
#include "pdgid.inc"
#include "photon_stuff.inc"
c_end_inc
c
c_begin_var
c  input/output variables:
c  -----------------------
      INTEGER i_entry,neuttrig
      LOGICAL time_do
c
c  Local pre-defined variables:
c  ---------------------------
c  RCS information: 
      CHARACTER*132  CFILE, CREVIS, CSTATE, CDATE, CAUTHO, CRCSID
      PARAMETER (CFILE = '$RCSfile: run_builder.F,v $')
      PARAMETER (CREVIS = '$Revision: 1.28 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 2008/06/12 15:21:40 $')
      PARAMETER (CAUTHO = '$Author: devita $')
      DATA CRCSID/   
     1'$Id: run_builder.F,v 1.28 2008/06/12 15:21:40 devita Exp $'   
     2/   
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='RUN_BUILDER')
      PARAMETER (CRAUTH='Stepan Stepanyan')
c
c  Local User defined variables:
c  -----------------------------
      INTEGER NWRITE
c_end_var
c
c  executable code for routine RUN_BUILDER:
c----6----------------------------------------------------------------72
c
      IF (NWRITE .LT. 1) THEN
        NWRITE = NWRITE + 1
        CRMESS='This routine organizes the work of event builder.'
        CALL RECMES(CRNAME,'I',CRMESS)
      ENDIF
c- Initialization of geometrical and final particle counters.
      IF (i_entry.eq.1) THEN
         GPARTICLES = 0
         NPARTICLES = 0
         Neuttrig = 0
c- The general scheme i_entry=1 is following - read in reconstruction banks:
c- ECHB, CCRC, SCR(SC1), EC1R, STR, TAGR, HBTR, HDPL
c- for hits and tracks. Match the found hits and tracks, fill TRKS for TBT.
         TIME_DO=.false.
         CALL BDROP(IW,'TRKSHEVTEVNTDCPBCCPBSCPBECPBLCPBSTPBTGPBTRPB') ! First drop all
         CALL READ_RBANKS(TIME_DO)    ! Then read 
         if(.not.time_do)Return
         CALL HIT_2_PART( )     ! Geometrical matching of hits and tracks.
         If ( GPARTICLES.Gt.0 ) Then ! If matchings are found then
          If(YES_PHOTON)then         ! Get the trigger time
            CALL GTRIG_TIME(neuttrig,-1)       ! --> for photon run(use ST)
          Else
            CALL TRIG_TIME(-1)        ! --> for electron runs
          EndIf
c- If no trigger particle is found (in electron scattering case - electron)
c- then no reson(sence) to do time based tracking. 
          TIME_DO=.false.
          if (NPARTICLES.gt.0 ) then  ! If trigger is found 
            CALL FILL_TRKS( )         ! Fill bank TRKS for time tracking
            TIME_DO=.true.
          endif
        EndIf
      ELSE IF(i_entry.eq.2) THEN
c- If trigger particle (trigger time) is found try determine PID's 
c- and build an event. If no trigger just do nothing.
        if((.NOT.LTRK_DO.or.qtrigg.eq.0).and.NPARTICLES.gt.0)TIME_DO=.true.
        If((Time_do.Or.neuttrig.gt.1).And.NPARTICLES.Gt.0 )Then
          CALL time_scecst (Time_do)
          CALL TIME_TRACKING( ) 
          CALL EVENT_BLD( )          ! Build an event
c- In any case "particles" found after geometry matching will be writen 
c- out in the BOS bank with individual detector's information.
          CALL EVNT_2_BOS( )
        EndIf
      ENDIF
      RETURN
      END
c
c------------------------------------------------------------------------------




