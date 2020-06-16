      SUBROUTINE RETCL_INIT(IRET_ERR)
c
c  Documentation for subroutine RETCL_INIT
c
c  Purpose:  Initializes TCL interface for asynchronus line input
c  --------
c
c  Other routines:  CALLED from RECINI at beginning of RECSIS job
c  ---------------
c
c  Author:   Arne Freyberger      Created:  Wed Oct 25 11:34:40 EDT 1995
c  -------
c
c  Major revisions:
c  ----------------
c
c  added call to DC_TCL_INIT to link tcl and FORTRAN variables.
c                                                    gilfoyle, 12/1/96
c   
c  Modified to read data from the DD ring in addition to reading 
c  from a file.
c                                                    gilfoyle 7/97
c
      IMPLICIT NONE
      SAVE
c
c  include files :
c  ---------------------
c BOS common block  uncomment the next line for BOS include file
#include "rhist.inc"
#include "run_control.inc"
#include "reversion.inc" 
C----#include "scat_control.inc"
#include "clasmdl.inc"
c
c  Local variables:
c  ----------------
c  RCS information: 
      CHARACTER*132 CRCSID
      DATA CRCSID/   
     1'$Id: retcl_init.F,v 1.23 2004/02/23 13:31:44 clasdvcs Exp $'   
     2/   
c  Module information:
      CHARACTER*(*)  CRNAME
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='RETCL_INIT')
c
      INTEGER LENOCC, INDEXA, SCAT_LOCATEPROGRAM, tcl_eval, 
     1        scat_initialize
      EXTERNAL LENOCC, INDEXA, tcl_eval, scat_initialize,
     1         scat_locateprogram
c
      LOGICAL LEXS
      INTEGER IFILE, IRET_ERR, ICEND
      CHARACTER*(REC_STRING) CRIPT
c
c  executable code for routine RETCL_INIT:
c
c  call package routines to initialize the TCL controlable
c  variables. skip over this section if you are running the
c  online version of recsis and you are at a run change.
c
c     
      CALL RNTCL_INIT           ! initialize RECSIS run time variables
c     
      CALL USER_TCL_INIT        ! initialize USER runtime variable
c     
      call evnttcl_init         !  initialize seb variables
c     
      CALL ECTCL_INIT           ! initialize EGN runtime variables
      
      CALL EC1TCL_INIT          ! initialize Ec1 runtime variables
c   
      call ic_tcl_init
      
      CALL TRKTCL_INIT          ! initialize TRK runtime variables
      
      CALL DC_TCL_INIT          ! initialize DC runtime variables
      
      call hsttcl_init          !  initialize histogram variables
      
      call tagtcl_init          ! initialize tag runtime tcl variables
      
      CALL ST_TCL_INIT          ! initialize ST runtime variables

      CALL RFTCL_INIT           ! Initialize RF tcl variables

#ifndef MAP
      CALL CALIB_TCL_INIT           ! Initialize CalDB tcl variables
#endif
c     
c     write out welcome to the screen
c     
      CALL RECMES(CRNAME,'I','Welcome to RECSIS 3.0!')
      call recmes(crname,'n',
     1     '|     ')
      call recmes(crname,'n',
     1     '|  RECSIS will pause now,    ')
      call recmes(crname,'n',
     1     '|  to resume running enter: set lpause 0')
      call recmes(crname,'n',
     1     '|  or for help enter:       help general getting_started')
      call recmes(crname,'n',
     1     '|      ')
      call recmes(crname,'n',
     1     '|     Questions/sugestions and comment should be sent to:')
      call recmes(crname,'n',
     1     '|                                      freyberg@cebaf.gov')
      CALL RECMES(CRNAME,'I',
     1     '----------------------------------------------')
c     
      
c     
c     run the TCL initialization script, if user has entered one
c     
      cript = tcl_init
      ICEND = LENOCC(CRIPT)

      if (icend .ne. REC_STRING) then 
c     
c     Allow user to just type in TCL script without .tcl extension
c     
         if (cript(max(1,icend-3):min(rec_string,icend)) .ne. '.tcl') then
            cript(icend+1:min(rec_string,icend+4)) = '.tcl'
         endif
c
c  run the TCL initialization script, if user has entered one
c
         lexs = .false.
         inquire(file=cript(1:icend),iostat=ifile,exist=lexs)
         print *,cript
         if (lexs) then
            call tcl_recsis_evalfile(cript(1:ICEND))
         else
            crmess = 'Initialization file does not exist..using defaults'
            call recmes(crname,'w',crmess)
         endif
      else 
            crmess = 'Initialization file not entered on command line..using defaults'
            call recmes(crname,'w',crmess)
      endif
c     
      call tmpro(rec_prompt(1:lenocc(rec_prompt)))
c     
      
cscat      if (lscat) then
cscat         call scat_init
cscat      endif
      
c     
      RETURN
      END
c     
c------------------------------------------------------------------------------
