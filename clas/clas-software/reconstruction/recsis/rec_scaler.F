      SUBROUTINE REC_SCALER
c
c_begin_doc
c  RCS ID string
c  $Id: rec_scaler.F,v 1.3 1998/02/19 11:15:32 anciant Exp $
c
c  Documentation for subroutine REC_SCALER
c
c  Purpose:   This routine gets called on EVENTTYPE=0 events which are
c  --------   trigger/scaler reads
c
c  Input Parameters:  NONE
c  ----------------
c
c  Output Parameters:  NONE
c  -----------------
c
c  Other routines:
c  ---------------
c
c  Notes:
c  ------
c
c  Author:   Arne Freyberger      Created:  Fri Oct  3 11:33:06 EDT 1997
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
      PARAMETER (CFILE = '$RCSfile: rec_scaler.F,v $')
      PARAMETER (CREVIS = '$Revision: 1.3 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 1998/02/19 11:15:32 $')
      PARAMETER (CAUTHO = '$Author: anciant $')
      DATA CRCSID/   
     1'$Id: rec_scaler.F,v 1.3 1998/02/19 11:15:32 anciant Exp $'   
     2/   
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='REC_SCALER')
      PARAMETER (CRAUTH='Arne Freyberger')
c
c  Local User defined variables:
c  -----------------------------
c_end_var
c
c  executable code for routine REC_SCALER:
c----6----------------------------------------------------------------72
c


       call bm_read_trig
       
      IF (LTAGGER_DO) CALL TAG_SCALER

c
c   ----- call users scaler routine last.
c
       call user_scaler

      RETURN
      END
c
c------------------------------------------------------------------------------




