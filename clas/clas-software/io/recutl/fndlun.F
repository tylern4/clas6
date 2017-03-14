      SUBROUTINE FNDLUN(RNAME,LUN)
c
c_begin_doc
c  RCS ID string
c  $Id: fndlun.F,v 1.1.1.1 1997/03/28 20:58:22 marki Exp $
c
c  Documentation for subroutine FNDLUN
c
c  Purpose:  Searches for an available logical unit number and reserves it
c  --------
c
c  Calling Sequence:
c  ----------------
c
c  Input Parameters:  (RNAME - C - Routine name)
c  ----------------
c
c  Output Parameters:  (LUN - I - Logical unit number)
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
c  Author:   Arne Freyberger      Created:  Tue Mar 14 16:29:42 EST 1995
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
      CHARACTER*132  CFILE, CREVIS, CSTATE, CDATE, CAUTHO, CRCSID 
      PARAMETER (CFILE=  '$RCSfile: fndlun.F,v $')
      PARAMETER (CREVIS= '$Revision: 1.1.1.1 $')
      PARAMETER (CSTATE= '$State: Exp $')
      PARAMETER (CDATE=  '$Date: 1997/03/28 20:58:22 $')
      PARAMETER (CAUTHO= '$Author: marki $')
      DATA CRCSID/
     1'$Id: fndlun.F,v 1.1.1.1 1997/03/28 20:58:22 marki Exp $
     3'/
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
c      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='FNDLUN')
      PARAMETER (CRAUTH='Arne Freyberger')
c
c  Local User defined variables:
c  -----------------------------
      CHARACTER*(*) RNAME
      INTEGER LUN, IRET
c_end_var
c
c  executable code for routine FNDLUN:
c  -------------------------------------
c
      DO 10 LUN= 20, 100
       CALL CHKLUN(LUN,IRET)
       IF (IRET .LT. 0) GOTO 10
C       CALL RESLUN(RNAME,LUN,IRET)
       RETURN
10    CONTINUE
c
      RETURN
      END
c
c------------------------------------------------------------------------------




