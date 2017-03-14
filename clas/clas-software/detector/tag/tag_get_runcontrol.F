	subroutine tag_get_RunControl(ok,irun)
c
c_begin_doc
c  RCS ID string
c  $Id: tag_get_runcontrol.F,v 1.7 1998/10/20 17:05:22 anciant Exp $
c
c  Documentation for subroutine tag_get_RunControl
c
c  Purpose: 
c  --------
c	Read Run Control variables.
c
c  Calling Sequence:
c  ----------------
c
c  Input Parameters:  NONE
c  ----------------
c
c  Output Parameters:  NONE
c  -----------------
c
c  Called from: tag_evnt.F
c  ------------
c
c  Other routines:
c  ---------------
c
c  Notes:
c  ------
c  Author:   Eric ANCIANT      Created:  feb 12 1998
c  -------
c
c  Major revisions:
c  ----------------
c     
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
c#include "bcs.inc"

#include "tagRunControl.CMN"
c_end_inc
c
c_begin_var
c  input/output variables:
c  -----------------------
c
      logical ok
      integer irun
c      
c  Local pre-defined variables:
c  ---------------------------
c  RCS information: 
      CHARACTER*(*)  CFILE, CREVIS, CSTATE, CDATE, CAUTHO 
      PARAMETER (CFILE=  '$RCSfile: tag_get_runcontrol.F,v $')
      PARAMETER (CREVIS= '$Revision: 1.7 $')
      PARAMETER (CSTATE= '$State: Exp $')
      PARAMETER (CDATE=  '$Date: 1998/10/20 17:05:22 $')
      PARAMETER (CAUTHO= '$Author: anciant $')
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='tag_get_RunControl')
      PARAMETER (CRAUTH='Eric Anciant')
c
c  Local tag defined variables:
c  ----------------------------

        integer NWRITE
c	character*100 comments
c	character*80 fname
c        integer Ilun,Iretb

        INTEGER run0,run1,run2,run3,run4
        character*99 fname


c_end_var
c
c  executable code for routine tag_get_runcontrol:
c  ------------------------------------
c
c      
      crmess = '  '
       
	IF (NWRITE .LT. 1) THEN
          NWRITE = NWRITE + 1
          crmess='Getting Tagger Run Control Parameters '
          CALL RECMES(CRNAME,'I',CRMESS)
        ENDIF      
      ok= .true.
      

        call revinm('CLAS_PARMS','Maps/RUN_CONTROL.map',fname)

        call map_get_float(fname,'beam','energy',1,Beam_energy,irun,run0)
        if (run0.lt.0) ok=.false.
        call map_get_float(fname,'currents','minitorus',1,PS_current,irun,run1)
        if (run1.lt.0) ok=.false.
        call map_get_float(fname,'currents','tagger',1,tagger_current,irun,run2)
        if (run2.lt.0) ok=.false.
        call map_get_float(fname,'currents','torus',1,torus_current,irun,run3)
        if (run3.lt.0) ok=.false.

        call revinm('CLAS_PARMS','Maps/TAG_CALIB.map',fname)

        call map_get_int(fname,'tag_t','do_translation',1,tagger_do_t_trans,irun,run4)
        if (run4.lt.0) then
                  ok=.false.
                  tagger_do_t_trans(1) = 0
        else

        endif
        

        if (ok) then
          CRMESS='Reading Run control parameters in the Map Manager'
          CALL RECMES(CRNAME,'I',CRMESS)
        else
          if (run0.lt.0) then
            CRMESS='Cannot get the Beam Energy from the Map Manager'
            CALL RECMES(CRNAME,'F',CRMESS)
          endif
          if (run1.lt.0) then
            CRMESS='Cannot get the Pair Spectrometer current from the Map Manager'
            CALL RECMES(CRNAME,'W',CRMESS)
          endif
          if (run2.lt.0) then
            CRMESS='Cannot get the Tagger Current from the Map Manager'
            CALL RECMES(CRNAME,'W',CRMESS)
          endif
          if (run3.lt.0) then
            CRMESS='Cannot get the Torus Current from the Map Manager'
            CALL RECMES(CRNAME,'W',CRMESS)
          endif
          if (run4.lt.0) then
            CRMESS='Cannot get the tagger T translation status from the Map Manager'
            CALL RECMES(CRNAME,'W',CRMESS)
          endif
        endif

      RETURN
      END
c
c------------------------------------------------------------------------------
