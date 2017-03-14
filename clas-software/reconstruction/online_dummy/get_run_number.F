      SUBROUTINE GET_RUN_NUMBER
c
c_begin_doc
c  RCS ID string
c  $Id: get_run_number.F,v 1.1 1998/03/04 19:15:21 gilfoyle Exp $
c
c  Documentation for subroutine GET_RUN_NUMBER
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
c  Author:   GerardGilfoyle      Created:  Wed Mar  4 12:01:21 EST 1998
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
      PARAMETER (CFILE = '$RCSfile: get_run_number.F,v $')
      PARAMETER (CREVIS = '$Revision: 1.1 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 1998/03/04 19:15:21 $')
      PARAMETER (CAUTHO = '$Author: gilfoyle $')
      DATA CRCSID/   
     1'$Id: get_run_number.F,v 1.1 1998/03/04 19:15:21 gilfoyle Exp $'   
     2/   
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='GET_RUN_NUMBER')
      PARAMETER (CRAUTH='GerardGilfoyle')
c
c  Local User defined variables:
c  -----------------------------
      INTEGER NWRITE
c_end_var
c
c  executable code for routine GET_RUN_NUMBER:
c----6----------------------------------------------------------------72
c
      IF (NWRITE .LT. 1) THEN
        NWRITE = NWRITE + 1
        CRMESS='You have called a dummy GET_RUN_NUMBER routine. It '
        CALL RECMES(CRNAME,'I',CRMESS)
        CRMESS='appears you are trying to use the online DD ring without '
        CALL RECMES(CRNAME,'I',CRMESS)
        CRMESS='compiling RECSIS with the "ONLINE=yes" option. If you really'
        CALL RECMES(CRNAME,'I',CRMESS)
        CRMESS='do not want to use the DD ring, then set the tcl variable'
        CALL RECMES(CRNAME,'I',CRMESS)
        CRMESS='READ_DD_RING to zero in your inititalization file.'
        CALL RECMES(CRNAME,'I',CRMESS)
      ENDIF
      RETURN
      END
c
c------------------------------------------------------------------------------




