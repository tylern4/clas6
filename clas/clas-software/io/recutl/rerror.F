      SUBROUTINE RERROR
c
c_begin_doc
c  RCS ID string
c  $Id: rerror.F,v 1.1.1.1 1997/03/28 20:58:32 marki Exp $
c
c  Documentation for subroutine RERROR
c
c  Purpose:  This routine is called if an ERROR message is sent to RECMES.
c  --------  ALL processing will STOP upon receipt of an error message.
c            The user should supply a replacement RERROR to perform the
c            appropriate actions for ERROR messages
c
c  Input Parameters:  none
c  ----------------
c
c  Output Parameters: none
c  -----------------
c
c  Other routines:
c  ---------------
c
c  Notes:
c  ------
c
c  Author:   Arne Freyberger      Created:  Thu May  4 13:34:55 EDT 1995
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
      PARAMETER (CFILE = '$RCSfile: rerror.F,v $')
      PARAMETER (CREVIS = '$Revision: 1.1.1.1 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 1997/03/28 20:58:32 $')
      PARAMETER (CAUTHO = '$Author: marki $')
      DATA CRCSID/   
     1'$Id: rerror.F,v 1.1.1.1 1997/03/28 20:58:32 marki Exp $'   
     2/   
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='RERROR')
      PARAMETER (CRAUTH='Arne Freyberger')
c
c  Local User defined variables:
c  -----------------------------
      INTEGER NWRITE
c_end_var
c
c  executable code for routine RERROR:
c----6----------------------------------------------------------------72
c
      IF (NWRITE .LT. 1) THEN
        NWRITE = NWRITE + 1
        CRMESS='This is a DUMMY routine, this message written once'
        CALL RECLOG(CRNAME,'I',CRMESS)
      ENDIF
c
c   STOP NOW!!!
c
      STOP
c
      END
c
c------------------------------------------------------------------------------




