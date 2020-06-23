      SUBROUTINE CHKFPK(LUN,IRET)
c
c_begin_doc
c  RCS ID string
c  $Id: chkfpk.F,v 1.1.1.1 1997/03/28 20:58:21 marki Exp $
c
c  Documentation for subroutine CHKFPK
c
c  Purpose: Looks at FPACK logical unit tables to see if FPACK is using LUN
c  --------
c
c  Calling Sequence:
c  ----------------
c
c  Input Parameters:  (LUN - I - Logical unit number)
c  ----------------
c
c  Output Parameters:  (IRET - I - -1 logical unit in use)
c  -----------------               +1                 FREE
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
c  Author:   Arne Freyberger      Created:  Wed Mar 15 11:28:15 EST 1995
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
#include "fcommon.inc"
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
      PARAMETER (CFILE=  '$RCSfile: chkfpk.F,v $')
      PARAMETER (CREVIS= '$Revision: 1.1.1.1 $')
      PARAMETER (CSTATE= '$State: Exp $')
      PARAMETER (CDATE=  '$Date: 1997/03/28 20:58:21 $')
      PARAMETER (CAUTHO= '$Author: marki $')
      DATA CRCSID/
     1'$Id: chkfpk.F,v 1.1.1.1 1997/03/28 20:58:21 marki Exp $'
     3/
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
c      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='CHKFPK')
      PARAMETER (CRAUTH='Arne Freyberger')
c
c  Local User defined variables:
c  -----------------------------
      INTEGER IRET, LUN
c_end_var
c
c  executable code for routine CHKFPK:
c  -------------------------------------
c
      IRET = 1
      IF (LUNIT(LUN) .GT. 0) IRET = -1
c
      RETURN
      END
c
c------------------------------------------------------------------------------




