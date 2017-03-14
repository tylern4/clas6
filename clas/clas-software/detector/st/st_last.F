      SUBROUTINE ST_LAST
c
c_begin_doc
c  RCS ID string
c  $Id: st_last.F,v 1.3 1997/10/08 14:51:36 auger Exp $
c
c  Documentation for subroutine ST_LAST
c
c  Purpose:  ST supplied routined called ONCE at the end of job.
c  --------  
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
      PARAMETER (CFILE=  '$RCSfile: st_last.F,v $')
      PARAMETER (CREVIS= '$Revision: 1.3 $')
      PARAMETER (CSTATE= '$State: Exp $')
      PARAMETER (CDATE=  '$Date: 1997/10/08 14:51:36 $')
      PARAMETER (CAUTHO= '$Author: auger $')
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='ST_LAST')
      PARAMETER (CRAUTH='Francois Roudot')
c
c  Local St defined variables:
c  -----------------------------
      INTEGER NWRITE
c_end_var
c
c  executable code for routine ST_LAST:
c  -------------------------------------
c
      IF (NWRITE .LT. 1) THEN
        NWRITE = NWRITE + 1
        CRMESS='This is the DUMMY start counter last routine'
        CALL RECMES(CRNAME,'I',CRMESS)
      ENDIF

      RETURN
      END
c
c------------------------------------------------------------------------------




