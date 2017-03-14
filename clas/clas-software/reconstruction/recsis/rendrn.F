      SUBROUTINE RENDRN
c
c_begin_doc
c  RCS ID string
c  $Id: rendrn.F,v 1.6 2006/01/24 23:05:03 bonus Exp $
c
c  Documentation for subroutine RENDRN
c
c  Purpose: END RUN processor
c  --------
c
c  Calling Sequence:
c  ----------------
c
c  Input Parameters:  None
c  ----------------
c
c  Output Parameters:  None
c  -----------------
c
c  Called from:  RECSIS
c  ------------
c
c  Other routines:
c  ---------------
c
c  Notes:
c  ------
c
c  Author:   Arne Freyberger      Created:  Tue Mar 14 16:26:06 EST 1995
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
#include "clasmdl.inc"
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
      PARAMETER ( CFILE = '$RCSfile: rendrn.F,v $')
      PARAMETER ( CREVIS = '$Revision: 1.6 $')
      PARAMETER ( CSTATE = '$State: Exp $')
      PARAMETER ( CDATE = '$Date: 2006/01/24 23:05:03 $')
      PARAMETER ( CAUTHO = '$Author: bonus $')
      DATA CRCSID/
     1'$Id: rendrn.F,v 1.6 2006/01/24 23:05:03 bonus Exp $
     3'/
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='RENDRN')
      PARAMETER (CRAUTH='Arne Freyberger')
c
c  Local User defined variables:
c  -----------------------------
      INTEGER NOENDRN, LENDRN, CKRUN
c_end_var
c
c  executable code for routine RENDRN:
c  -------------------------------------
c
c  First make sure this is really an end run.
c  To flag this see if the run number has changed, if not
c  return.
c
c febuary/march run run number changes in the head bank !!!
      if (nccrun.eq.0) return
c
c  keep track of last run seen by THIS routine
c
      LENDRN = CKRUN
c
c  do end run tagger stuff
c
      IF (LTAGGER_DO) CALL TAG_ERUN
c
c  do end of run tracking stuff
c
      if (ltrk_do) call trk_erun
c
c  do end of run cerenkov stuff
c
      if (lcc_do) call cc_erun
c
c do GEM end run stuff
c
      if (lgem_do) call gem_erun
c
c  Ok done bookkeeping move on to the real work  call end run processors
c
      if (lusr0_do .or. lusr1_do) CALL USER_ERUN
c
c Call the RECSIS end run processor
c
      CALL RECS_ERUN
c
c
c
      RETURN
      END
c
c------------------------------------------------------------------------------




