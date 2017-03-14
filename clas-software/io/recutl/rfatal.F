      SUBROUTINE RFATAL
c
c_begin_doc
c  RCS ID string
c  $Id: rfatal.F,v 1.1.1.1 1997/03/28 20:58:34 marki Exp $
c
c  Documentation for subroutine RFATAL
c
c  Purpose:  This routine is called if a FATAL flag is sent to RECMES,
c  --------  processing will STOP immediately when called.  this is meant
c            to be a dummy routine, the user should supply a replacement
c            RFATAL to execute the appropriate response to FATAL messages.
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
c  Author:   Arne Freyberger      Created:  Thu May  4 13:34:53 EDT 1995
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
      CHARACTER*(132)  CFILE, CREVIS, CSTATE, CDATE, CAUTHO, CRCSID
      PARAMETER (CFILE = '$RCSfile: rfatal.F,v $')
      PARAMETER (CREVIS = '$Revision: 1.1.1.1 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 1997/03/28 20:58:34 $')
      PARAMETER (CAUTHO = '$Author: marki $')
      DATA CRCSID/   
     1'$Id: rfatal.F,v 1.1.1.1 1997/03/28 20:58:34 marki Exp $'   
     2/   
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='RFATAL')
      PARAMETER (CRAUTH='Arne Freyberger')
c
c  Local User defined variables:
c  -----------------------------
      INTEGER NWRITE
c_end_var
c
c  executable code for routine RFATAL:
c----6----------------------------------------------------------------72
c
      IF (NWRITE .LT. 1) THEN
        NWRITE = NWRITE + 1
        CRMESS='This is a DUMMY routine, this message written once'
        CALL RECLOG(CRNAME,'I',CRMESS)
      ENDIF
c
c  OKAY now STOP!!!!
c
      STOP
c
c
c
      END
c
c------------------------------------------------------------------------------




