      INTEGER FUNCTION IPC_msg_destroy(string1)
c
c_begin_doc
c  RCS ID string
c  $Id: ipc_msg_destroy.F,v 1.1 2002/11/08 21:33:06 gilfoyle Exp $
c
c  Documentation for subroutine IPC_INIT
c
c  Purpose: This is a dummy routine to take the place of a
c  -------- proprietary routine that is not part of the
c           standard distribution of RECSIS. The proprietary
c           routines are used for inter-process communication
c           (ipc) that is part of the online version of
c           RECSIS.
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
c  Author:   GILFOYLE   Created:  Mon Jun 30 10:22:49 EDT 1997
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
      PARAMETER (CFILE = '$RCSfile: ipc_msg_destroy.F,v $')
      PARAMETER (CREVIS = '$Revision: 1.1 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 2002/11/08 21:33:06 $')
      PARAMETER (CAUTHO = '$Author: gilfoyle $')
      DATA CRCSID/   
     1'$Id: ipc_msg_destroy.F,v 1.1 2002/11/08 21:33:06 gilfoyle Exp $'   
     2/   
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='IPC_INIT')
      PARAMETER (CRAUTH='GILFOYLE')
c
c  Local User defined variables:
c  -----------------------------
      INTEGER NWRITE
      character*60 string1, string2
c_end_var
c
c  executable code for routine IPC_INIT:
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
      IPC_msg_destroy = -1
      RETURN
      END
c
c------------------------------------------------------------------------------




