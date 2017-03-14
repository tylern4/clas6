      SUBROUTINE USER_XTRA
c
c_begin_doc
c  RCS ID string
c  $Id: user_xtra.F,v 1.1 2001/08/30 16:51:50 feuerbac Exp $
c
c  Documentation for subroutine USER_XTRA
c
c  Purpose:
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
c  Author:   Arne Freyberger      Created:  Wed Mar 15 14:05:36 EST 1995
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
      PARAMETER ( CFILE = '$RCSfile: user_xtra.F,v $')
      PARAMETER ( CREVIS = '$Revision: 1.1 $')
      PARAMETER ( CSTATE = '$State: Exp $')
      PARAMETER ( CDATE = '$Date: 2001/08/30 16:51:50 $')
      PARAMETER ( CAUTHO = '$Author: feuerbac $')
      DATA CRCSID/
     1'$Id: user_xtra.F,v 1.1 2001/08/30 16:51:50 feuerbac Exp $
     3'/
c  Module information:
      CHARACTER*132  CRNAME, CRAUTH
      CHARACTER*132  CRMESS
      PARAMETER (CRNAME='USER_XTRA')
      PARAMETER (CRAUTH='Arne Freyberger')
c
c  Local User defined variables:
c  -----------------------------
      INTEGER NWRITE
c_end_var
c
c  executable code for routine USER_XTRA:
c  -------------------------------------
c
      IF (NWRITE .LT. 1) THEN
        NWRITE = NWRITE + 1
        CRMESS='This is a DUMMY routine, this message written once'
        CALL RECMES(CRNAME,'I',CRMESS)
      ENDIF
      RETURN
      END
c
c------------------------------------------------------------------------------




