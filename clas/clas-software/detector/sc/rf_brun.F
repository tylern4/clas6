      SUBROUTINE RF_BRUN(IRUN)
c
c_begin_doc
c  RCS ID string
c  $Id: rf_brun.F,v 1.1 1998/04/17 21:03:15 anciant Exp $
c
c  Documentation for subroutine RF_BRUN
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
c  Author:   Eric Anciant	march, 8th 1998
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
#include "rftcl.CMN"
c_end_inc
c
c_begin_var
c  input/output variables:
c  -----------------------

       integer IRUN ! Run number
c
c  Local pre-defined variables:
c  ---------------------------
c  RCS information: 
      CHARACTER*(*)  CFILE, CREVIS, CSTATE, CDATE, CAUTHO 
      PARAMETER (CFILE=  '$RCSfile: rf_brun.F,v $')
      PARAMETER (CREVIS= '$Revision: 1.1 $')
      PARAMETER (CSTATE= '$State: Exp $')
      PARAMETER (CDATE=  '$Date: 1998/04/17 21:03:15 $')
      PARAMETER (CAUTHO= '$Author: anciant $')
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='RF_BRUN')
      PARAMETER (CRAUTH='Eric Anciant')
c
c  Local tag defined variables:
c  ----------------------------
      INTEGER NWRITE
      logical OK
c_end_var
c
c  executable code for routine TAG_BRUN:
c  -------------------------------------
c
      IF (NWRITE .LT. 1) THEN
        NWRITE = NWRITE + 1
        CRMESS='RF begin run routine'
        CALL RECMES(CRNAME,'I',CRMESS)
      ENDIF 

      OK = .true.

c
c  If the tcl variable rf_get_cal is different from 0 (default is = 0)
c  we look for the calibration constants first in the local directory
c  in the file rfcal.dat
c

      if (rf_get_cal.ne.0) call rfcal_read_file(OK)

c
c  Normal case: we take the parameters from the Map_manager
c
     
      
      if (rf_get_cal.eq.0.or.(.not.ok)) call rfcal_read_map(OK,IRUN)
      
      RETURN
      END
c
c------------------------------------------------------------------------------




