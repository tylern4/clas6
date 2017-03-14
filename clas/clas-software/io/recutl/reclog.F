      SUBROUTINE RECLOG(RNAME,RLEVEL,RMESS)
c
c_begin_doc
c  RCS ID string
c  $Id: reclog.F,v 1.12 2000/06/29 17:55:52 marki Exp $
c
c  Documentation for subroutine RECLOG
c
c  Purpose:  Writes out a message to both terminal and LOG file
c  --------  RLEVEL is a descriptor stating the level of warning
c                        I = information
c                        W = warning things aren't quite right but not fatal
c                        E = error something wrong, output not to be trusted
c                               CALL FSEQE to stop processing at end of loop
c                        F = FATAL stop processing IMMEDIATELY
c                        N = No routine name prefix
c                        D = Debug messages
c                        L = FORCED logging of possibly suppressed I/W messages
c                        S = FORCED spooling of stored messages to log file
c
***************************************************************************
*  JOB CONTROL is performed in an upper level routine RECMES.             *
*                                                                         *
*  !!!!! Do not call RECLOG directly, call RECMES which will route the    *
*        message to RECLOG and take care of job control!!!!!!             *
*                                                                         *
***************************************************************************
c
c  Input Parameters: RNAME - C - The routine name
c  ----------------  RLEVEL- C - The level
c                    RMESS - C - The message
c
c  Output Parameters: NONE
c  -----------------
c
c  Other routines:  CALLED from RECMES which handles job control of ERROR
c  ---------------  and FATAL messages
c
c  Notes:
c  ------
c
c  Author:   Arne Freyberger      Created:  Thu Apr 13 09:39:08 EDT 1995
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
#include "re_units.inc"
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
      PARAMETER (CFILE=  '$RCSfile: reclog.F,v $')
      PARAMETER (CREVIS= '$Revision: 1.12 $')
      PARAMETER (CSTATE= '$State: Exp $')
      PARAMETER (CDATE=  '$Date: 2000/06/29 17:55:52 $')
      PARAMETER (CAUTHO= '$Author: marki $')
      DATA CRCSID/
     1'$Id: reclog.F,v 1.12 2000/06/29 17:55:52 marki Exp $'
     3/
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
c      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='RECLOG')
      PARAMETER (CRAUTH='Arne Freyberger')
c
c  Local User defined variables:
c  -----------------------------
      INTEGER LENOCC, LOCHAR, ICFNBL, indexa
      LOGICAL INTRAC
      EXTERNAL LENOCC, LOCHAR, ICFNBL, INTRAC, indexa
c
      INTEGER MAXSTORE, INITSTR, NORMSTR, MAXSTR, NMESS
      PARAMETER ( MAXSTORE = 250 )
      DATA INITSTR,NORMSTR/249,249/
c
      CHARACTER*(*) RMESS, RLEVEL, RNAME
      CHARACTER*256 TSTORE, MSTORE(MAXSTORE), CSTORE, PROMPT
      CHARACTER*50 CBLANK
      CHARACTER*2  CLEVEL
      CHARACTER*11 SNAME
c      INTEGER LIN, LOUT, LLOG, LHST, LTMP
      INTEGER IRET, NSTORED, ICBEG, ICEND, I, IADD
      INTEGER IEND_NAME, IMBEG, IMEND
      LOGICAL LNOMESS,LDUMP, LOG_FILE, LOPENED, LINIT, LINTER
      DATA LOPENED/.FALSE./
      DATA IEND_NAME/11/
      DATA CBLANK/'                                                  '/
      DATA LINIT/.TRUE./
c
c
c_end_var
c
c  executable code for routine RECLOG:
c  -------------------------------------
c
c  Get logical unit numbers of terminal LOUT and log file LLOG
c
c      CALL RECLUN(LIN,LOUT,LLOG,LHST,LTMP)
      CALL GET_LOG(LOG_FILE)
      CALL GET_PROMPT(PROMPT)
c
c   count messages in the beginning
c

      IF (LINIT) THEN
       LINIT = .FALSE.
       LINTER=.FALSE.
       INITSTR = MIN(INITSTR,MAXSTORE)
       NORMSTR = MIN(NORMSTR,MAXSTORE)
       LINTER = .TRUE.
C       IF (INTRAC()) THEN
C          LINTER = .TRUE.
C       ENDIF
      ENDIF
      IF (NMESS .LT. INITSTR) THEN
        NMESS = NMESS+1
        MAXSTR = INITSTR
      ELSE
        MAXSTR = NORMSTR
      ENDIF
c
c     check dump flag  also dump on ERRORs and FATALS, first capitalize the
c     level indicator
c
      CLEVEL = RLEVEL(1:lenocc(rlevel))
      CALL CLTOU(CLEVEL)
c
      LDUMP =.FALSE.
      IF (CLEVEL .EQ. 'S' .OR.
     1    CLEVEL .EQ. 'E' .OR.
     2    CLEVEL .EQ. 'F' .OR.
     3    CLEVEL .EQ. 'Q' .OR.
     4    NSTORED .GE. MAXSTR-1) LDUMP = .TRUE.

C
C  check the name length
C
       sname = rname(1:max(1,min(lenocc(rname),iend_name)))
c
c  Check message length
c
      IMEND = LENOCC(RMESS)
      LNOMESS = .FALSE.
      IF (IMEND .EQ. 0) LNOMESS = .TRUE.
c
c  Construction of message
c
      IF (LNOMESS .AND. .NOT. LDUMP) RETURN
c
c  find first non-blank
c
      IF (.NOT. LNOMESS) THEN
        IMBEG = ICFNBL(RMESS(1:IMEND),1,IMEND)
c
       If (CLEVEL(1:1) .EQ. 'N' .OR. CLEVEL(2:2) .EQ. 'N') THEN
         TSTORE=RMESS(IMBEG:IMEND)
         ICEND = IMEND
         ICBEG = IMBEG
       ELSE
         ICEND = LENOCC(SNAME)
         if (icend .gt. 0) TSTORE(1:ICEND) = SNAME(1:ICEND)
         if (iend_name .gt. icend) then
          TSTORE(ICEND+1:IEND_NAME) = 
     1             CBLANK(1:IEND_NAME-ICEND+1)
         endif
          ICBEG = IEND_NAME+1
          ICEND = ICBEG+2
          WRITE(TSTORE(ICBEG:ICEND),10)CLEVEL
10        FORMAT(A1,': ')
          ICBEG = ICEND+1
          IADD = IMEND - IMBEG + 1
          ICEND = ICBEG + IADD
          TSTORE(ICBEG:MIN(256,ICEND)) = RMESS(IMBEG:IMEND)
       ENDIF
c
c  First  if interactive write to LOUT=tty
c
        if (linter .and. icend .ne. 0) then
         IF (CLEVEL .NE. 'Q')  THEN
           WRITE(LOUT,12)TSTORE(1:min(256,ICEND))
c           if (clevel .ne. 'N') 
c     1      CALL TMPRO(PROMPT(1:LENOCC(PROMPT)+1))
c           CALL TMPRO(PROMPT(1:LENOCC(PROMPT)+1))
         ENDIF
         IF (CLEVEL .EQ. 'Q')  WRITE(LOUT,11)TSTORE(1:ICEND)
11       FORMAT(1X,A,$)
12       FORMAT(t1,A)
        endif
c
c  send it to the GUI
c
capf        call retcl_mes(tstore(1:min(256,icend)), MIN(256,ICEND))
c
      ENDIF
c
c    IF LOG_FILE is set false then user has turned off logging return
c
      IF (.NOT. LOG_FILE) RETURN
c
c
c  First check to see if LOG file is open
c
      CALL CHKLUN(LLOG,IRET)
c
c  if IRET > 0 file is not opened so store the message in MSTORE
c
c
      IF (IRET .GT. 0 .OR. .NOT.LDUMP) THEN
       IF (.NOT. LNOMESS) THEN
        NSTORED = NSTORED + 1
        MSTORE(MIN(MAXSTORE,NSTORED)) = TSTORE(1:MIN(256,ICEND))
       ENDIF
      ELSE
c
c  write to log file but first check to see if file is opened
c  if it isn't open it then dump stored messages 
c  to make it into this code IRET < 0 (file open) and LDUMP=.TRUE.
c 
       IF (NSTORED .GT. 0) THEN
        DO 20 I=1,min(NSTORED,maxstore)
          CSTORE = MSTORE(I)
          IADD = LENOCC(CSTORE)
          WRITE(LLOG,12)CSTORE(1:IADD)
19        FORMAT(A)
20      CONTINUE
        NSTORED = 0
       ENDIF
c
c  OK NOW dump the message into the log file
c
       IF (.NOT. LNOMESS) THEN
        WRITE(LLOG,12)TSTORE(1:min(256,ICEND))
       ENDIF
      ENDIF
c
      RETURN
      END
c
c------------------------------------------------------------------------------




