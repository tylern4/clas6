      SUBROUTINE RESTAT
c
c_begin_doc
c  RCS ID string
c  $Id: restat.F 3.1 1996/12/23 17:13:02 freyberg Exp c
c  Documentation for subroutine RESTAT
c
c  Purpose:  Prints out JOB statistics
c  --------
c
c  Calling Sequence:
c  ----------------
c
c  Input Parameters:  (Name - Type - Meaning)
c  ----------------
c
c  Output Parameters:  (Name - Type - Meaning)
c  -----------------
c
c  Called from:
c  ------------
c
c  Other routines:
c  ---------------
c
c  Notes:
c  ------
c
c  Author:   Arne Freyberger      Created:  Sun Mar 19 09:43:13 EST 1995
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
#include "clasmdl.inc"
#include "fcommon.inc"
#include "run_control.inc"
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
      PARAMETER ( CFILE = '$RCSfile: restat.F,v $')
      PARAMETER ( CREVIS = '$Revision: 1.14 $')
      PARAMETER ( CSTATE = '$State: Exp $')
      PARAMETER ( CDATE = '$Date: 1999/07/02 01:43:59 $')
      PARAMETER ( CAUTHO = '$Author: gilfoyle $')
      DATA CRCSID/
     1'$Id: restat.F,v 1.14 1999/07/02 01:43:59 gilfoyle Exp $
     3'/
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS,FNAME, crstat
      CHARACTER*9 FSTATC
      PARAMETER (CRNAME='RESTAT')
      PARAMETER (CRAUTH='Arne Freyberger')
c
c  Local User defined variables:
c  -----------------------------
      INTEGER LENOCC
      EXTERNAL LENOCC
      INTEGER I, IZERO, IFILE, IBEG, IEND, NEVT
      REAL TTIME, ATIME
      CHARACTER*20  CTALLY(NTRIG+1),CTEMP
      CHARACTER*1 BLANK
      DATA BLANK /' '/
      DATA CTALLY/15*'Physics Event',
     1               'Sync Event',
     2               'Begin Run',
     3               'Go Event',
     4               'Pause Event',
     5               'End Run',
     6               'Unknown Trigger','Job Init'/
c_end_var
c
c  executable code for routine RESTAT:
c  -------------------------------------
c

      call recmes(BLANK,'S','  ')
      crstat= '|     RECSIS    status      executing'
      if (lpause) crstat= '|     RECSIS    status      paused'
      if (lquit) crstat= '|     RECSIS    status      quitting'
      if (lexit)  crstat= '|     RECSIS    status      exiting'
c
      WRITE(CRMESS,801) 
      CALL RECMES(BLANK,'L',CRMESS)
c
c  tell the user which files are opened by FPACK
c
      WRITE(CRMESS,802)
c      CALL RECMES(BLANK,'N',CRMESS)
c
      call recmes(BLANK,'L',crstat)
c
      WRITE(CRMESS,802)
c      CALL RECMES(BLANK,'N',CRMESS)
c
      WRITE(CRMESS,801)
      CALL RECMES(BLANK,'L',CRMESS)
c
      WRITE(CRMESS,802)
c      CALL RECMES(BLANK,'N',CRMESS)
c
      WRITE(CRMESS,9)
9     FORMAT('|    Files currently known to FPACK',
     1       20x,'status',8x,'# recs  |')
      CALL RECMES(BLANK,'L',CRMESS)
      WRITE(CRMESS,802)
c      CALL RECMES(BLANK,'N',CRMESS)
      DO 35 IFILE = 1,LW+LR
       IF (INFU(ISGEN+1,IFILE) .EQ. 0) GOTO 35
       IF (INFU(ISGEN+1,IFILE) .EQ. 1) THEN
          FSTATC = 'Read'
          NEVT = INFU(IRSTA+3,IFILE)
       ELSEIF (INFU(ISGEN+1,IFILE) .EQ. 2) THEN
          FSTATC = 'Read EOF'
          NEVT = INFU(IRSTA+3,IFILE)
       ELSEIF (INFU(ISGEN+1,IFILE) .EQ. 3) THEN
          FSTATC = 'Write'
          NEVT = INFU(IWSTA+3,IFILE)
       ELSEIF (INFU(ISGEN+1,IFILE) .EQ. 4) THEN
          FSTATC = 'Wrote EOF'
          NEVT = INFU(IWSTA+3,IFILE)
       ELSEIF (INFU(ISGEN+1,IFILE) .EQ. 5) THEN
          FSTATC = 'Rewind'
          NEVT = INFU(IRSTA+3,IFILE)
       ENDIF
       FNAME = DTFLNM(IFILE)
       IEND = LENOCC(FNAME)
       IBEG = 1
       IF (IEND .GE. 44) THEN
         IBEG = IEND - 43
       ENDIF
       WRITE(CRMESS,700)FNAME(IBEG:MAX(IEND+1,45)), FSTATC, NEVT
700    FORMAT('|     ',A45,4X,A9,3X,I8,'  |')
       CALL RECMES(BLANK,'L',CRMESS)
35    CONTINUE
c
      WRITE(CRMESS,802)
c      CALL RECMES(BLANK,'N',CRMESS)
      WRITE(CRMESS,801)
      CALL RECMES(BLANK,'L',CRMESS)
c
c telluser that you're reading DD ring data.
c
      if (read_et) then
         WRITE(CRMESS,802)
         CALL RECMES(BLANK,'L',CRMESS)
         WRITE(CRMESS,801)
         CALL RECMES(BLANK,'N',CRMESS)
         WRITE(CRMESS,802)
         CALL RECMES(BLANK,'N',CRMESS)
         write(crmess,720)
 720     format('|     Reading data from ET chain.',45x,'|')
         call recmes(BLANK,'L',crmess)
         WRITE(CRMESS,802)
         CALL RECMES(BLANK,'N',CRMESS)
         WRITE(CRMESS,801)
         CALL RECMES(BLANK,'N',CRMESS)
         WRITE(CRMESS,802)
         CALL RECMES(BLANK,'L',CRMESS)
      end if
c
c  tell the user which event types are being processed
c
      WRITE(CRMESS,900)
      CALL RECMES(BLANK,'L',CRMESS)
      WRITE(CRMESS,905)
      CALL RECMES(BLANK,'L',CRMESS)
      I = NTRIG+1
      CTEMP = CTALLY(I)
      TTIME = CPUSUM(I)
      ATIME = 0.0
      IZERO = 0
      WRITE(CRMESS,910)CTEMP(1:16),I,IZERO,
     1                  IZERO,TTIME,ATIME
      CALL RECMES(BLANK,'L',CRMESS)
      DO 10 I=1,21
       IF (ITALLY(I)+ITALLY_MISSED(I) .EQ. 0) GOTO 10
       CTEMP = CTALLY(I)
       TTIME = CPUSUM(I)
       ATIME = 1000.*CPUSUM(I)/(ITALLY(I)+ITALLY_MISSED(I))
       WRITE(CRMESS,910)CTEMP(1:16),I,ITALLY(I),
     1                  ITALLY_MISSED(I)+ITALLY_SKIPED(I),TTIME,ATIME
       CALL RECMES(BLANK,'L',CRMESS)
10    CONTINUE
      WRITE(CRMESS,801)
      CALL RECMES(BLANK,'L',CRMESS)
800   FORMAT(63('-'),'|')
801   FORMAT('|',76('-'),'|')
802   FORMAT('|',76(' '),'|')
900   FORMAT('|  Event Class',2x,'Index',2x,'Number of',2X,
     1       'Number of   ',
     1       2x,'  Total    ',1x,' Average',9x,'|')
905   FORMAT('|             ',2x,'     ',2x,'records  ',2X,
     1       'skipped recs',
     1       2x,'  time(sec)',1x,' time (msec)',5X,'|')
910   FORMAT('|  ',A16,1X,I2,2X,I7,4X,I7,4X,F9.3,
     1       4X,F10.3,8X,'|')
      RETURN
      END
c
c------------------------------------------------------------------------------










