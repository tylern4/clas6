      INTEGER FUNCTION GETADDR(csignal)
c
c_begin_doc
c  RCS ID string
c  $Id: getaddr.F,v 1.4 1998/07/30 20:15:21 jyun Exp $
c
c  Documentation for subroutine GETADDR
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
c  Author:         Created:  Tue Jul 14 14:50:23 EDT 1998
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
c  input/output variables:csignal      
c  -----------------------
c
c  Local pre-defined variables:
c  ---------------------------
c  RCS information: 
      CHARACTER*132  CFILE, CREVIS, CSTATE, CDATE, CAUTHO, CRCSID
      PARAMETER (CFILE = '$RCSfile: getaddr.F,v $')
      PARAMETER (CREVIS = '$Revision: 1.4 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 1998/07/30 20:15:21 $')
      PARAMETER (CAUTHO = '$Author: jyun $')
      DATA CRCSID/   
     1'$Id: getaddr.F,v 1.4 1998/07/30 20:15:21 jyun Exp $'   
     2/   
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='GETADDR')
      PARAMETER (CRAUTH='whodidthis')
c
c  Local User defined variables:
c  -----------------------------
      INTEGER NWRITE, CSIGNAL
c_end_var
c
c  executable code for routine GETADDR:
c----6----------------------------------------------------------------72
c
      IF (NWRITE .LT. 1) THEN
        NWRITE = NWRITE + 1
        CRMESS='You have called a dummy IPC routine. It appears you are'
        CALL RECMES(CRNAME,'I',CRMESS)
        CRMESS='trying to use the online DD ring without compiling'
        CALL RECMES(CRNAME,'I',CRMESS)
        CRMESS='RECSIS with the "ONLINE=yes" option. If you do not want'
        CALL RECMES(CRNAME,'I',CRMESS)
        CRMESS='to use the DD ring then set the tcl variable'
        CALL RECMES(CRNAME,'I',CRMESS)
        CRMESS='READ_DD_RING to zero in your inititalization file.'
        CALL RECMES(CRNAME,'I',CRMESS)
      ENDIF
      GETADDR = 0
      RETURN
      END
c
c------------------------------------------------------------------------------




