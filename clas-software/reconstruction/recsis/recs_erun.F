      SUBROUTINE RECS_ERUN
c
c_begin_doc
c  RCS ID string
c  $Id: recs_erun.F,v 1.3 1999/07/02 01:43:35 gilfoyle Exp $
c
c  Documentation for subroutine RECS_ERUN
c
c  Purpose: Performs the END RUN RECSIS processing
c  --------
c
c  Calling Sequence:
c  ----------------
c
c  Input Parameters:  (Name - Type - Meaning)
c  ----------------
c
c  Output Parameters:  (Name - Type - Meaning)
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
c  Author:   Arne Freyberger      Created:  Tue Apr 11 14:01:47 EDT 1995
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
#include "re_units.inc"
#include "run_control.inc"
c_end_inc
c
c_begin_var
c  input/output variables:
c  -----------------------
c
c  Local pre-defined variables:
c  ---------------------------
c  RCS information: 
      CHARACTER*132  CFILE, CREVIS, CSTATE, CDATE, CAUTHO, CRCSID 
      PARAMETER ( CFILE = '$RCSfile: recs_erun.F,v $')
      PARAMETER ( CREVIS = '$Revision: 1.3 $')
      PARAMETER ( CSTATE = '$State: Exp $')
      PARAMETER ( CDATE = '$Date: 1999/07/02 01:43:35 $')
      PARAMETER ( CAUTHO = '$Author: gilfoyle $')
      DATA CRCSID/
     1'
     2$Id: recs_erun.F,v 1.3 1999/07/02 01:43:35 gilfoyle Exp $
     3'/
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
c      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='RECS_ERUN')
      PARAMETER (CRAUTH='Arne Freyberger')
c
c  Local User defined variables:
c  -----------------------------
c_end_var
c
c  executable code for routine RECS_ERUN:
c  -------------------------------------
c
c  ALWAYS WRITE END RUN RECORD!!!!
c
c      CALL FSEQW('CLASOUTPUT')
c
c for the online version close the histogram file here upon a
c detected change in run number. avoid this the first time through
c by checking that the unit has not been allocated (lhst=0).
c                                         gilfoyle 5/15/98

      if (lhst.ne.0 .and. read_et) then
         call hbook_close
      end if
      RETURN
      END
c
c------------------------------------------------------------------------------




