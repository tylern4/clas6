      SUBROUTINE ST_BRUN(irun)
c
c_begin_doc
c  RCS ID string
c  $Id: st_brun.F,v 1.10 2008/03/16 04:33:24 fklein Exp $
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
#include "bcs.inc"
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
      PARAMETER (CREVIS= '$Revision: 1.10 $')
      PARAMETER (CSTATE= '$State: Exp $')
      PARAMETER (CDATE=  '$Date: 2008/03/16 04:33:24 $')
      PARAMETER (CAUTHO= '$Author: fklein $')
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*132  CRMESS
      PARAMETER (CRNAME='ST_BRUN')
      PARAMETER (CRAUTH='Francois Roudot')
c
c  Local st defined variables:
c  ----------------------------
      logical ok
      integer ind,mlink,evtype
      integer irun,ievnt,irec
      integer nwrite, n2write
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

C     use C++ code, initialize global variable gCalib
      if (StType.eq.2) then
         if (n2write.lt.1) then
            n2write = n2write+1
            CRMESS='Using C++ code for new ST'
            CALL RECMES(CRNAME,'I',CRMESS)
         endif
         call stpp_initialize(irun)
         return
      endif

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
c--   use run #1 for simulation calibrations (if event type < 0 )
      ind = mlink(iw,'HEAD',0)
      evtype = iw(ind+5)
      if(evtype.lt.0) irun=1
c
      write(crmess,'(A,I2,A,I8)')'Read calibration constants for type',
     &                 stType,' and run',irun
      call recmes(crname,'W',crmess)
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




