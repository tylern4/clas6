      SUBROUTINE REC_EPICS
c
c_begin_doc
c  RCS ID string
c  $Id: rec_epics.F,v 1.3 2001/01/09 20:25:22 freyberg Exp $
c
c  Documentation for subroutine REC_EPICS
c
c  Purpose:   This routine gets called on EVENTTYPE>=100 events which are
c  --------   epics/database events
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
      PARAMETER (CFILE = '$RCSfile: rec_epics.F,v $')
      PARAMETER (CREVIS = '$Revision: 1.3 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 2001/01/09 20:25:22 $')
      PARAMETER (CAUTHO = '$Author: freyberg $')
      DATA CRCSID/   
     1'$Id: rec_epics.F,v 1.3 2001/01/09 20:25:22 freyberg Exp $'   
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



c
c   ----- call users scaler routine last.
c
      call rec_get_epics
      call epics_beam_params

      call user_epics

      RETURN
      END
c
c------------------------------------------------------------------------------




