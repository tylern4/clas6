      SUBROUTINE KEEP_LOG(LOG_FILE,ITOGGLE)
c
c_begin_doc
c  RCS ID string
c  $Id: keep_log.F,v 1.1.1.1 1997/03/28 20:58:26 marki Exp $
c
c  Documentation for subroutine KEEP_LOG
c
c  Purpose:
c  --------
c
c  Input Parameters:  CIN - C* - 'ON' enable logging
c  ----------------              'OFF' disable logging
c                     ITOGGLE - I -  ITOGGLE < 0 set CIN_SAVE
c                                            > 0 return CIN_SAVE
c
c  Output Parameters:  CIN - C* - 'ON' enable logging
c  ----------------              'OFF' disable logging
c
c  Other routines:
c  ---------------
c
c  Notes:
c  ------
c
c  Author:   Arne Freyberger      Created:  Fri May  5 14:47:57 EDT 1995
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
      PARAMETER (CFILE = '$RCSfile: keep_log.F,v $')
      PARAMETER (CREVIS = '$Revision: 1.1.1.1 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 1997/03/28 20:58:26 $')
      PARAMETER (CAUTHO = '$Author: marki $')
      DATA CRCSID/   
     1'$Id: keep_log.F,v 1.1.1.1 1997/03/28 20:58:26 marki Exp $'   
     2/   
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
c      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='KEEP_LOG')
      PARAMETER (CRAUTH='Arne Freyberger')
c
c  Local User defined variables:
c  -----------------------------
      LOGICAL L_SAVE, LOG_FILE
      INTEGER ITOGGLE 
      DATA L_SAVE/.FALSE./
c_end_var
c
c  executable code for routine KEEP_LOG:
c----6----------------------------------------------------------------72
c
      IF (ITOGGLE .LT. 0) THEN
       L_SAVE = LOG_FILE
      ELSE
       LOG_FILE = L_SAVE
      ENDIF
      RETURN
      END
c
c------------------------------------------------------------------------------




