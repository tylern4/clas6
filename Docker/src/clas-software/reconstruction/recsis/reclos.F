      SUBROUTINE RECLOS
c
c  Documentation for subroutine RECLOS
c
c  Purpose:  Does an orderly closing of files associated with RECSIS package
c  --------
c
c  Author:   Arne Freyberger      Created:  Thu Mar 16 15:51:02 EST 1995
c  -------
c
      IMPLICIT NONE
      SAVE
c
c  include files :
c  ---------------------
#include "scat_control.inc"
#include "run_control.inc"
#include "re_units.inc"
c
c  Local variables:
c  ---------------------------
c  RCS information: 
      CHARACTER*132  CRCSID 
      DATA CRCSID/
     1'$Id: reclos.F,v 1.7 1998/11/20 14:05:25 fklein Exp $
     3'/
c  Module information:
      CHARACTER*(*)  CRNAME
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='RECLOS')
c
      integer lenocc
      external lenocc
c
      INTEGER IRET, ITEST, I, ILUN, IRUN, IEV, IREC, ISEQ
      LOGICAL LINIT, first_pass
      character*256 scat_string
      character*24  log_name
      DATA LINIT/.TRUE./,first_pass/.true./
c
c  executable code for routine RECLOS:
c  -------------------------------------
c
c   Get file name so you can write out the message
c   first check to see if it is still opened,
c   also write out last run and event processd

      if (log_file_name .eq. 'use_default') then
         call get_seq(iseq)
         write(log_name,199)iseq
 199     format('re',I4.4,'.log')
      else
         log_name = log_file_name
      endif
c
      CALL CHKLUN(LLOG,IRET)
      IF (IRET .LT. 0) THEN
       CALL RERNEV(IRUN, IEV, IREC)
       WRITE(CRMESS,10)IRUN, IEV
10     FORMAT('Last event processed was run# ',I6,'  event# ',I6)
       CALL RECMES(CRNAME,'I',CRMESS)
       call get_seq(itest)
       crmess(1:) = " "
       CRMESS(1:21) = 'Closing log file ---'
       CRMESS(22:) = log_name
       CALL RECMES(CRNAME,'I',CRMESS(1:42))
c
c   close the logical units and free up the logical unit number
c
       CALL RESLUN(CRNAME,-LLOG,IRET)
      ENDIF
c
c   Tell the user which files are still opened and who owns them
c
      DO 100 I=1,99
       IF (I .EQ. 5 .OR. I .EQ. 6 .OR. 
     1     I .EQ. 7 .OR. I .EQ. LLOG) GOTO 100
       CALL CHKLUN(I,IRET)
       IF (IRET .LT. 0) THEN
         IF (LINIT) THEN
           LINIT = .FALSE.
           CALL RECMES(CRNAME,'W',
     1    'The following logical units are still reserved')
           CRMESS = 
     1'the files are still open and will be closed upon exiting RECSIS'
           CALL RECMES(CRNAME,'W',CRMESS)
          ENDIF
           ILUN = I
           CALL RESLUN(CRNAME,ILUN,ITEST)
       ENDIF
100   CONTINUE
c
c   Now close the LOG file, first remeber to empty the log file spool
c

      CALL RECMES(CRNAME,'S','Ciao....')
      CALL CHKLUN(LLOG,IRET)
      IF (IRET .LT. 0) THEN
         CALL RESLUN(CRNAME,-LLOG,IRET)
         CLOSE(LLOG)
         LLOG = 0
      END IF
c
c   close SCAT stuff
c
      if (lscat) then
       scat_string='die scum'
       CALL  Scat_SendMsgNr (Scat_Handle(1), 
     1                      scat_string(1:lenocc(scat_string)))
       CALL  Scat_UnRegister ()
      endif
c
      RETURN
      END
c
c------------------------------------------------------------------------------
