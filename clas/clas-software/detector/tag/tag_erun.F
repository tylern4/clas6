      SUBROUTINE TAG_ERUN
c
c_begin_doc
c  RCS ID string
c  $Id: tag_erun.F,v 1.5 1998/04/22 00:37:04 anciant Exp $
c
c  Documentation for subroutine TAG_ERUN
c
c  Purpose: TAGGER supplied routine called AT the END of EVERY RUN
c  --------
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
c  Author:   Francois Roudot      Created:  Thu Jun 19 12:00:00 EST 1997
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
      PARAMETER (CFILE=  '$RCSfile: tag_erun.F,v $')
      PARAMETER (CREVIS= '$Revision: 1.5 $')
      PARAMETER (CSTATE= '$State: Exp $')
      PARAMETER (CDATE=  '$Date: 1998/04/22 00:37:04 $')
      PARAMETER (CAUTHO= '$Author: anciant $')
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='TAG_ERUN')
      PARAMETER (CRAUTH='Francois Roudot')
c
c  Local User defined variables:
c  -----------------------------
      INTEGER NWRITE
      logical ok
c_end_var
c
c  executable code for routine TAG_ERUN:
c  -------------------------------------
c
      IF (NWRITE .LT. 1) THEN
        NWRITE = NWRITE + 1
        CRMESS='This is the tagger endrun routine'
        CALL RECMES(CRNAME,'I',CRMESS)
      ENDIF

c
c	Display warnings statistic for that run:
c       

      call tag_erun_warnings

      RETURN
      END
c

c
c------------------------------------------------------------------------------














