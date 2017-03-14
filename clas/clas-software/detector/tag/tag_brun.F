      SUBROUTINE TAG_BRUN(irun)
c
c_begin_doc
c  RCS ID string
c  $Id: tag_brun.F,v 1.16 2004/11/10 23:01:49 pasyuk Exp $
c
c  Documentation for subroutine TAG_BRUN
c
c  Purpose: TAGGER supplied routined called at the BEGINNING of EVERY run
c  --------
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
c  Called from:
c  ------------
c
c  Other routines:
c  ---------------
c
c  Notes:
c  ------
c
c  Author:   Francois ROUDOT      Created:  Thu Jun 19 12:00:00 EST 1997
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
#include "tag_general.PAR"
#include "tagtcl.CMN"
#include "tagRunControl.CMN"
#include "tagcal.CMN"

c_end_inc
c
c_begin_var
c  input/output variables:
c  -----------------------
c
      common/tagrun/nrun
      integer irun
      integer nrun
      logical ok


c  Local pre-defined variables:
c  ---------------------------
c  RCS information: 
      CHARACTER*(*)  CFILE, CREVIS, CSTATE, CDATE, CAUTHO 
      PARAMETER (CFILE=  '$RCSfile: tag_brun.F,v $')
      PARAMETER (CREVIS= '$Revision: 1.16 $')
      PARAMETER (CSTATE= '$State: Exp $')
      PARAMETER (CDATE=  '$Date: 2004/11/10 23:01:49 $')
      PARAMETER (CAUTHO= '$Author: pasyuk $')
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='TAG_BRUN')
      PARAMETER (CRAUTH='Francois Roudot')
c
c  Local tag defined variables:
c  ----------------------------
      INTEGER NWRITE,i
      logical ttranstable
      real EnergyInGev
c_end_var
c
c  executable code for routine TAG_BRUN:
c  -------------------------------------
c
      IF (NWRITE .LT. 1) THEN
        NWRITE = NWRITE + 1
        CRMESS='tagger begin run routine'
        CALL RECMES(CRNAME,'I',CRMESS)
      ENDIF 

c
c	 Get run control parameters  /  this as to be done FIRST 
c                    needed for the T translation table & boundaries
c  
      nrun = irun
      ok=.true.
      call tag_get_RunControl(OK,irun)
      if(.not.OK) then
        CRMESS='Problem while getting Run Control Parameters'
        CALL RECMES(CRNAME,'W',CRMESS)
      endif

      ttranstable = (tagger_do_t_trans(1).ne.0)

c
c   ---- reset flags for calibration constants
c

      call tagcal_reset(OK)

c
c
      if (Tagger_parms_input.eq.0) then
c
c	read from the Map Manager the calibration constants valid for that run
c  
        ok=.true.
        call tagcal_read_map(OK,irun,ttranstable)
        if(.not.OK) then
          CRMESS='Problem while reading Tagger calibration constants from the Map Manager'
          CALL RECMES(CRNAME,'W',CRMESS)
        endif
c
c	read second in local directory remaining calibration constants 
c      (NOT ANYMORE, if all the constants are not found in the Map Manager = Fatal error)  E.A May, 27 1999 
c
c        ok=.true.
c        call tagcal_read_local_file(OK,ttranstable)
c        if(.not.OK) then
c          CRMESS='Problem while reading Tagger calibration constants from local files'
c          CALL RECMES(CRNAME,'W',CRMESS)
c        endif
c
      else
c
c ----------------- Read E & T energy boundaries & E-T Map locally
c
          EnergyInGev = Beam_energy(1)/1000.  ! Beam_energy is in tagRunControl.CMN common ( in Mev)
          call tag_read_bounds(ok,1,EnergyInGev) ! 1 is local

c
c
c	read first in local directory calibration constants 
c  

        ok=.true.
        call tagcal_read_local_file(OK,ttranstable)
        if(.not.OK) then
          CRMESS='Problem while reading Tagger calibration constants from local files'
          CALL RECMES(CRNAME,'W',CRMESS)
        endif


c
c	read second from the Map Manager remaining calibration constants 
c  
        ok=.true.
        call tagcal_read_map(OK,irun,ttranstable)
        if(.not.OK) then
          CRMESS='Problem while reading Tagger calibration constants from the Map Manager'
          CALL RECMES(CRNAME,'W',CRMESS)
        endif
c
       endif
c
c  ------------ if we were unable to load a T translation table, don't use it.
c TAGTTranslOK is in tagcal.CMN, tagger_do_t_trans(1) in tagRunControl.CMN

         if (TAGTTranslOK.eq.0) tagger_do_t_trans(1) = 0

c
c	read in CLAS_PARMS remaining calibration constants 
c      (not anymore, if required constants are not found in the Map, this is a Fatal Error) E.A May, 27 1999
c
c      ok=.true.
c      call tagcal_read_file(OK)
c      if(.not.OK) then
c        CRMESS='Problem while reading Tagger calibration constants from files in the CLAS_PARMS area'
c        CALL RECMES(CRNAME,'W',CRMESS)
c      endif
c
c ----------------- Read E & T energy boundaries & E-T Map in CLAS PARMS (if not already read locally)
c
          EnergyInGev = Beam_energy(1)/1000.  ! Beam_energy is in tagRunControl.CMN cpmmon (in Mev)
          call tag_read_bounds(ok,0,EnergyInGev) ! 0 (<>1) is CLAS_PARMS
	

c
c	read from file the RF calibration constants valid for that run.
c  
c
c      ok=.true.
c      call tagcal_RF_read_file(OK)
c      if(.not.OK) then
c        CRMESS='Problem while reading RF calibration constants from file'
c        CALL RECMES(CRNAME,'W',CRMESS)
c      endif
      
c
c	read from BOS the calibration constants valid for that run.
c       

      ok=.true.
      call tagcal_read_from_BOS(OK)
      if(.not.OK) then
        CRMESS='Problem while reading Tagger calibration constants from BOS BANKS'
        CALL RECMES(CRNAME,'W',CRMESS)
      endif

c
c    Calculate mean value of Ci, for better alignment of DSD iwht tagger
c
	TagTCiMean=0
	do i=1,121
	  TagTCiMean = TagTCiMean + TagTCi(i)
	enddo
	TagTCiMean = TagTCiMean/121.

c
c	create permanent scaler summed BOS banks
c     
      ok=.true.
      call tag_brun_scaler(OK)
      if(.not.OK) then
        CRMESS='Problem while initializing scaler BOS banks'
        CALL RECMES(CRNAME,'W',CRMESS)
      endif
      
      RETURN
      END
c
c------------------------------------------------------------------------------




