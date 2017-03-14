      SUBROUTINE RECMES(RNAME,CLEVEL,RMESS)
c
c  Documentation for subroutine RECMES
c
c  Purpose:  Calls RECLOG to route message to log file and TTY, also performs
c            job control based on the RLEVEL passed to this routine.
c            RLEVEL is a descriptor stating the level of warning
c                           I = information
c                           W = warning things aren't quite right but not fatal
c                           E = error something wrong, output not to be trusted
c                               CALL FSEQE to stop processing at end of loop
c                           F = FATAL stop processing IMMEDIATELY
c                           N = No routine name prefix
c                           S = Force messages to be Spooled to log file
c                           L = information that cannot be suppressed
c                           LN = information that cannot be suppressed
c                                with no routine prefix
c                           D = debug message
c
c   The I and W messages can be suppressed by calling
c
c              LINFO = .FALSE.
c              LWARN = .FALSE.
c              CALL SET_LEVEL(LINFO,LWARN,LDEBUG,LERROR,LFATAL)
c
c  Input Parameters:   RNAME  - C* - Routine name calling RECMES
c  ----------------    RLEVEL - C* - message level (see above)
c                      RMESS  - C* - message to be logged
c
c  Author:   Arne Freyberger      Created:  Tue Mar 14 16:28:18 EST 1995
c  -------
c
      IMPLICIT NONE
      SAVE
c
c  include files :
c  ---------------------
c BOS common block  uncomment the next line for BOS include file
#include "clasmdl.inc"
#include "re_units.inc"
c
c  Local pre-defined variables:
c  ---------------------------
c  RCS information: 
      CHARACTER*132 CRCSID 
      DATA CRCSID/
     1'$Id: recmes.F,v 1.7 1999/03/03 20:47:15 freyberg Exp $'
     3/
c  Module information:
      CHARACTER*100  CRMESS
c
c  Local User defined variables:
c  -----------------------------
c
      CHARACTER*(*)  CRNAME
      PARAMETER (CRNAME='RECMES')
      CHARACTER*(*) CLEVEL, RNAME, RMESS
      CHARACTER*128 CHEAD
      CHARACTER*50  RLEVEL
      LOGICAL LINFO, LWARN, LDEBUG, LERROR, LFATAL
      LOGICAL LOPENED, LOG_FILE, LINIT
      INTEGER IRUN, IEVENT, IOLDRUN, IOLDEVENT, NMESS, NREC
      INTEGER IRET
      INTEGER INITSTR
      DATA INITSTR/50/
c
      DATA CHEAD(1:11),CHEAD(18:32),CHEAD(49:69)
     1    /'Run Number-',' Event Number-',' Total records seen-'/
      DATA LOPENED/.FALSE./
      DATA LINIT/.TRUE./
c
c_end_var
c
c  executable code for routine RECMES:
c  -------------------------------------
c
      RLEVEL = CLEVEL
      CALL GET_LEVEL(LINFO,LWARN,LDEBUG,LERROR,LFATAL)
c
      IF ((RLEVEL(1:1) .EQ. 'I' .OR. RLEVEL(1:1) .EQ. 'i') .AND.
     1     .NOT. LINFO) RETURN
      IF ((RLEVEL(1:1) .EQ. 'N' .OR. RLEVEL(1:1) .EQ. 'n') .AND.
     1     .NOT. LINFO) RETURN
      IF ((RLEVEL(1:1) .EQ. 'W' .OR. RLEVEL(1:1) .EQ. 'w') .AND.
     1     .NOT. LWARN) RETURN
      IF ((RLEVEL(1:1) .EQ. 'D' .OR. RLEVEL(1:1) .EQ. 'd') .AND.
     1     .NOT. LDEBUG) RETURN
c
      NMESS = NMESS + 1

c
c  Construct RUN and EVENT number stamp, 
c  set LHEAD true run/event number changes
c
      IOLDRUN = IRUN
      IOLDEVENT = IEVENT
      CALL RERNEV(IRUN,IEVENT,NREC)
      IF (IEVENT .NE. IOLDEVENT) THEN
       WRITE(CHEAD(12:17),90)IRUN
       WRITE(CHEAD(33:48),91)IEVENT
       WRITE(CHEAD(70:75),90)NREC
90     FORMAT(I6)
 91    FORMAT(I14)
       CALL RECLOG(CRNAME,'N',CHEAD)
       NMESS = NMESS + 1
      ENDIF
c
c
c  Now call RECLOG to route the message to LOUT and LLOG
c
       CALL RECLOG(RNAME,RLEVEL,RMESS)
c
c  Check for FATAL flag
c
      IF (RLEVEL(1:1) .EQ. 'F' .OR.
     1    RLEVEL(1:1) .EQ. 'f')  THEN
        WRITE(CRMESS,*)'FATAL ERROR received processing will stop NOW!'
        CALL RECLOG(CRNAME,'F',CRMESS)
        CALL RFATAL
      ENDIF
c
c  Check error flag, error flag call RERROR to perform ERROR handle
c
      IF (RLEVEL(1:1) .EQ. 'e' .OR.
     1    RLEVEL(1:1) .EQ. 'E')  THEN
        WRITE(CRMESS,*)'Calling RERROR', 
     1         ' to execute ERROR sequence'
        CALL RECLOG(CRNAME,'E',CRMESS)
        CALL RERROR
      ENDIF

      RLEVEL(1:2) = '  '
      RETURN
      END
c
c------------------------------------------------------------------------------








