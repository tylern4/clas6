      SUBROUTINE USER_BRUN(irun)
c
c_begin_doc
c  RCS ID string
c  $Id: user_brun.F,v 1.4 2008/03/16 04:33:28 fklein Exp $
c
c  Documentation for subroutine USER_BRUN
c
c  Purpose: USER supplied routined called at the BEGINNING of EVERY run
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
c  Author:   Arne Freyberger      Created:  Wed Mar 15 14:01:12 EST 1995
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
#include "histtcl.inc"
#include "user_control.inc"
c_end_inc
c
c_begin_var
c  input/output variables:
c  -----------------------
c
c  Local pre-defined variables:
c  ---------------------------
c  RCS information: 
      CHARACTER*(*)  CFILE, CREVIS, CSTATE, CDATE, CAUTHO 
      PARAMETER (CFILE=  '$RCSfile: user_brun.F,v $')
      PARAMETER (CREVIS= '$Revision: 1.4 $')
      PARAMETER (CSTATE= '$State: Exp $')
      PARAMETER (CDATE=  '$Date: 2008/03/16 04:33:28 $')
      PARAMETER (CAUTHO= '$Author: fklein $')
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='USER_BRUN')
      PARAMETER (CRAUTH='Arne Freyberger')
c
c  Local User defined variables:
c  -----------------------------
      INTEGER NWRITE, irun, flag
      CHARACTER*99  call_map
c_end_var
c
c  executable code for routine USER_BRUN:
c  -------------------------------------
c
      IF (NWRITE .LT. 1) THEN
        NWRITE = NWRITE + 1
        CRMESS='This is a hist_43676.hbookDUMMY routine, this message written once'
        CALL RECMES(CRNAME,'I',CRMESS)
      ENDIF
 
c  Perform TAGGER fill histograms
      IF (LTAGGER_H_DO) CALL TAG_FILL_BRUN(.true.,.false.,.false.,.false.)
       call revinm('CLAS_PARMS','Maps/RUN_CONTROL.map',call_map)
       call map_get_float(call_map,'beam','energy',1,ebeam, irun, flag)
       ebeam=ebeam/1000.
       print *,'Beam Energy ',ebeam
      RETURN
      END
c
c------------------------------------------------------------------------------




