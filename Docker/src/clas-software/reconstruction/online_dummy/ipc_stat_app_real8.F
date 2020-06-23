      SUBROUTINE IPC_STATUS_APPEND_REAL8(string1,string2)
c
c_begin_doc
c  RCS ID string
c  $Id: ipc_stat_app_real8.F,v 1.1 1998/07/21 13:57:27 gilfoyle Exp $
c
c  Documentation for subroutine IPC_STATUS_APPEND_REAL8
c
c  Purpose:  Dummy routine to replace an IPC routine used for
c  --------  RECSIS ONLINE.
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
c  Author: gilfoyle   Created:  Wed Jul 16 09:52:49 EDT 1997
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
      PARAMETER (CFILE = '$RCSfile: ipc_stat_app_real8.F,v $')
      PARAMETER (CREVIS = '$Revision: 1.1 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 1998/07/21 13:57:27 $')
      PARAMETER (CAUTHO = '$Author: gilfoyle $')
      DATA CRCSID/   
     1'$Id: ipc_stat_app_real8.F,v 1.1 1998/07/21 13:57:27 gilfoyle Exp $'   
     2/   
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='IPC_STATUS_APPEND_REAL8')
      PARAMETER (CRAUTH='this is jerry gilfoyle')
c
c  Local User defined variables:
c  -----------------------------
      INTEGER NWRITE
      character*60 string1, string2
c_end_var
c
c  executable code for routine IPC_STATUS_APPEND_REAL8:
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
      RETURN
      END
c
c------------------------------------------------------------------------------




