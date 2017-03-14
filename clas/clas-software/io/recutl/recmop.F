      SUBROUTINE RECMOP
c
c  Purpose:  Opens LOG file
c  --------
c
c  Author:   Arne Freyberger      Created:  Tue Mar 14 16:28:14 EST 1995
c  -------
c
      IMPLICIT NONE
      SAVE
c  include files :
c  ---------------------
#include "run_control.inc"
#include "re_units.inc"
c
c  Local pre-defined variables:
c  ---------------------------
c  RCS information: 
      CHARACTER*132 CRCSID 
      DATA CRCSID/
     1'$Id: recmop.F,v 1.5 1998/05/05 19:21:42 marki Exp $'
     3/
c
c  Local User defined variables:
c  -----------------------------
      INTEGER IJBNUM, IRET
      CHARACTER*132 CLOGNM
      LOGICAL LLOG_DEF_NAME
      INTEGER ICEND, LENOCC
      DATA CLOGNM/' '/
c_end_var
c
c  executable code for routine RECMOP:
c  -------------------------------------
c
c get the log file name
c
      IF (LOG_FILE_NAME(1:11) .EQ. 'use_default') THEN
         LLOG_DEF_NAME = .TRUE.
      ELSE
         LLOG_DEF_NAME = .FALSE.
      ENDIF
c
c     if default is to be used
c
      IF (LLOG_DEF_NAME) THEN
         CALL GET_SEQ(IJBNUM)
         WRITE (CLOGNM,10) IJBNUM
 10      FORMAT('re',I4.4,'.log')
c
c     else use name requested by user
c
      ELSE
         CLOGNM = LOG_FILE_NAME
      END IF
c 
      LLOG = 0
      CALL RESLUN('RECMOP', LLOG, IRET)
      IF (IRET .NE. 1)
     &     STOP 'RECMOP: Could not get unit number for log file'
      OPEN (UNIT = LLOG, FILE = CLOGNM, STATUS = 'UNKNOWN')
c
      RETURN
      END
c
c------------------------------------------------------------------------------




