      SUBROUTINE ST_BRUN(irun)
c
c_begin_doc
c  RCS ID string
c  $Id: st_brun.F,v 1.6 2004/05/25 20:37:10 devita Exp $
c
c  Documentation for subroutine ST_BRUN
c
c  Purpose: ST supplied routined called at the BEGINNING of EVERY run
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
#include "st_general.PAR"
#include "sthit.CMN"
#include "stcal.CMN"
c_end_inc
c
c_begin_var
c  input/output variables:
c  -----------------------
c      
c
c  Local pre-defined variables:
c  ---------------------------
c  RCS information: 
      CHARACTER*(*)  CFILE, CREVIS, CSTATE, CDATE, CAUTHO 
      PARAMETER (CFILE=  '$RCSfile: st_brun.F,v $')
      PARAMETER (CREVIS= '$Revision: 1.6 $')
      PARAMETER (CSTATE= '$State: Exp $')
      PARAMETER (CDATE=  '$Date: 2004/05/25 20:37:10 $')
      PARAMETER (CAUTHO= '$Author: devita $')
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*132  CRMESS
      PARAMETER (CRNAME='ST_BRUN')
      PARAMETER (CRAUTH='Francois Roudot')
c
c  Local st defined variables:
c  ----------------------------
      logical ok
      integer irun,ievnt,irec
      integer nwrite
c_end_var
c
c  executable code for routine ST_BRUN:
c  ------------------------------------
c
      IF (NWRITE .LT. 1) THEN
        NWRITE = NWRITE + 1
        CRMESS='Reading ST calibration constants'
        CALL RECMES(CRNAME,'I',CRMESS)
      ENDIF


c     initialize geometry
      call stgeom_initialize(irun)

c---  get the run number
c
c      call RERNEV(irun,ievnt,irec)
c
c---  get default calibration constants 
c
c      call stcal_default(OK)
c
c---  get calibration constants valid for the run from map manager
c     they overwrite the default ones
c
      if(StType.eq.0) then
         call stcal_read_map(irun,StCal_values,StCal_uncertainties)
      else
         call stncal_read_map(irun,StnCal_values,StnCal_uncertainties)
      endif
         
c
      RETURN
      END
c
c------------------------------------------------------------------------------




