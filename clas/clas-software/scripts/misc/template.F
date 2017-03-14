      subroutine template
c
c_begin_doc
c  RCS ID string
c  $Id: template.F,v 1.1.1.1 1997/04/07 15:27:44 freyberg Exp $
c
c  Documentation for subroutine template
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
c  Author:   John Doe      Created:  day-mon-xx
c  -------
c
c  Major revisions:
c  ----------------
c     
c
c_end_doc
c
      implicit none
      SAVE
c
c_begin_inc
c  include files :
c  ---------------------
c BOS common block  uncomment the next line for BOS include file
nocapc#include "bcs.inc"
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
nocap      PARAMETER (CFILE = '$RCSfile: template.F,v $')
nocap      PARAMETER (CREVIS = '$Revision: 1.1.1.1 $')
nocap      PARAMETER (CSTATE = '$State: Exp $')
nocap      PARAMETER (CDATE = '$Date: 1997/04/07 15:27:44 $')
nocap      PARAMETER (CAUTHO = '$Author: freyberg $')
nocap      DATA CRCSID/   
nocap     1'$Id: template.F,v 1.1.1.1 1997/04/07 15:27:44 freyberg Exp $'   
nocap     2/   
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='template')
nocap      PARAMETER (CRAUTH='John Doe')
c
c  Local User defined variables:
c  -----------------------------
      INTEGER NWRITE
c_end_var
c
c  executable code for routine template:
c----6----------------------------------------------------------------72
c
      IF (NWRITE .LT. 1) THEN
        NWRITE = NWRITE + 1
nocap        CRMESS='This is a DUMMY routine, this message written once'
        CALL RECMES(CRNAME,'I',CRMESS)
      ENDIF
      RETURN
      END
c
c------------------------------------------------------------------------------




