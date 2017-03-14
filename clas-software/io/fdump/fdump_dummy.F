      SUBROUTINE FDUMP_DUMMY
c
c_begin_doc
c  RCS ID string
c  $Id: fdump_dummy.F,v 1.1 1997/07/02 21:36:53 claslib Exp $
c
c  Documentation for subroutine FDUMP_DUMMY
c
c  Purpose:
c  --------
c
c  Input Parameters:  (Name - Type - Meaning)
c  ----------------
c
c  Output Parameters:  (Name - Type - Meaning)
c  -----------------
c
c  Other routines:
c  ---------------
c
c  Notes:
c  ------
c
c  Author:   Luc Murphy      Created:  Wed Jul  2 17:34:55 EDT 1997
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
      PARAMETER (CFILE = '$RCSfile: fdump_dummy.F,v $')
      PARAMETER (CREVIS = '$Revision: 1.1 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 1997/07/02 21:36:53 $')
      PARAMETER (CAUTHO = '$Author: claslib $')
      DATA CRCSID/   
     1'$Id: fdump_dummy.F,v 1.1 1997/07/02 21:36:53 claslib Exp $'   
     2/   
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='FDUMP_DUMMY')
      PARAMETER (CRAUTH='Luc Murphy')
c
c  Local User defined variables:
c  -----------------------------
      INTEGER NWRITE
c_end_var
c
c  executable code for routine FDUMP_DUMMY:
c----6----------------------------------------------------------------72
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




