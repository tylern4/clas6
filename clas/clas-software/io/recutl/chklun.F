      SUBROUTINE CHKLUN(LUN,IRET)
c
c_begin_doc
c  RCS ID string
c  $Id: chklun.F,v 1.1.1.1 1997/03/28 20:58:22 marki Exp $
c
c  Documentation for subroutine CHKLUN
c
c  Purpose:  Checks to see if logical unit number LUN is already
c  --------  in use.
c
c  Calling Sequence:
c  ----------------
c
c  Input Parameters:  (LUN - I - Logical unit number)
c  ----------------
c
c  Output Parameters: (IRET - I - status of logical unit number)
c  -----------------              1 = not in use
c                                -1 = in use 
c                                -2 = in use by FPACK
c  Called from:
c  ------------
c
c  Other routines:
c  ---------------
c
c  Notes:
c  ------
c
c  Author:   Arne Freyberger      Created:  Wed Mar 15 09:56:54 EST 1995
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
      PARAMETER (CFILE=  '$RCSfile: chklun.F,v $')
      PARAMETER (CREVIS= '$Revision: 1.1.1.1 $')
      PARAMETER (CSTATE= '$State: Exp $')
      PARAMETER (CDATE=  '$Date: 1997/03/28 20:58:22 $')
      PARAMETER (CAUTHO= '$Author: marki $')
      DATA CRCSID/
     1'$Id: chklun.F,v 1.1.1.1 1997/03/28 20:58:22 marki Exp $'
     3/
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
c      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='CHKLUN')
      PARAMETER (CRAUTH='Arne Freyberger')
c
c  Local User defined variables:
c  -----------------------------
      INTEGER IRET, LUN, ITST
      LOGICAL LOPN
c_end_var
c
c  executable code for routine CHKLUN:
c  -------------------------------------
c
      IF (LUN .LE. 0) RETURN
      IRET = -1
      LOPN = .FALSE.
      INQUIRE(UNIT=LUN,OPENED=LOPN,ERR=999)
      IF (.NOT. LOPN) IRET = 1
999   CONTINUE
c
c   But somtimes INQUIRE misses files associated with FPACK,
c   this is probably due to the fact that FPACK uses "c" routines
c   to handle I/O
c
      CALL CHKFPK(LUN,ITST)
      IF (ITST .LT. 0) IRET = -2
c
      RETURN
      END
c
c------------------------------------------------------------------------------




