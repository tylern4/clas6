      SUBROUTINE RESLUN(RNAME,LUN,IRET)
c
c_begin_doc
c  RCS ID string
c  $Id: reslun.F,v 1.4 1998/05/05 19:21:43 marki Exp $
c
c  Documentation for subroutine RESLUN
c
c  Purpose: Used to allocate FORTRAN logical unit numbers for I/O.  No code 
c  -------- should hardwire in Logical Unit Numbers, instead they are assigned
c           dynamically via a call to RESLUN.  Here's a simple example:
C           MYLUN = 0
c           CALL RESLUN(CRNAME,MYLUN, IRET)
c           open(init=MYLUN, name=MYFILE....)
c
c  Calling Sequence:
c  ----------------
c
c  Input Parameters:  (RNAME - C - routine name)
c  ----------------      LUN - I - LUN > 0 logical unit number to be reserved
c                                  LUN = 0 returns free logical unit number
c                                  LUN < 0 logical unit number is freed
c                       
c  Output Parameters:  IRET - I - return flag   1 OK
c                                                -1 something wrong
c                       LUN - I - logical unit number, set to zero on error
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
c  Author:   Arne Freyberger      Created:  Wed Mar 15 09:42:28 EST 1995
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
      PARAMETER (CFILE=  '$RCSfile: reslun.F,v $')
      PARAMETER (CREVIS= '$Revision: 1.4 $')
      PARAMETER (CSTATE= '$State: Exp $')
      PARAMETER (CDATE=  '$Date: 1998/05/05 19:21:43 $')
      PARAMETER (CAUTHO= '$Author: marki $')
      DATA CRCSID/
     1'$Id: reslun.F,v 1.4 1998/05/05 19:21:43 marki Exp $'
     3/
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='RESLUN')
      PARAMETER (CRAUTH='Arne Freyberger')
#include "reslun.inc"
c
c  Local User defined variables:
c  -----------------------------
      INTEGER LENOCC
      EXTERNAL LENOCC
c
      CHARACTER*(*) RNAME
      CHARACTER*72  RTABLE(MLUN), RESERVE
      INTEGER LUN, IRET, ISTAT, ICEND, ICEND2
      INTEGER IBEG, IEND, I, L
      LOGICAL LUSED(MLUN), LFPACK, LFIRST
      DATA LUSED/MLUN*.FALSE./
      DATA RTABLE/MLUN*'Unknown'/
      DATA LFIRST/.TRUE./
c_end_var
c
c  executable code for routine RESLUN:
c  -------------------------------------
c
      IF (LFIRST) THEN
        LFIRST = .FALSE.
        DO I = 1, 20
          LUSED(I) = .TRUE.
        ENDDO
      ENDIF

      IF (LUN .EQ. 0) THEN
       IBEG = 20
       IEND = MLUN
      ELSE
       IBEG = ABS(LUN)
       IEND = ABS(LUN)
       CALL CHKLUN(ABS(LUN),ISTAT)
       IF (ISTAT .LT. 0) THEN
         LUSED(ABS(LUN)) = .TRUE.
         IRET = -1
         IF (ISTAT .EQ. -2) THEN
          RTABLE(ABS(LUN)) = 'FPACK'
         ELSE
          IF (.NOT. LUSED(ABS(LUN))) RTABLE(ABS(LUN)) = 'UNKNOWN'
         ENDIF
       ENDIF
c
c  First test the logical table
c  and return if unit number is already reserved
c
       IF (LUN .GT. 0) THEN
        IF (LUSED(LUN)) THEN
         IRET = -1
c  Write out a message
         GOTO 110
        ENDIF
       ELSE
c
c  LUN is negative so free up logical unit number BUT watch out for FPACK
c
        LFPACK = .FALSE.
        IF (RTABLE(ABS(LUN)) .EQ. 'FPACK') LFPACK = .TRUE.
        RESERVE=RTABLE(ABS(LUN))
        ICEND = LENOCC(RESERVE)
c
        IF (LUSED(ABS(LUN)) .AND. .NOT. LFPACK) THEN
         LUSED(ABS(LUN)) = .FALSE.
         RTABLE(ABS(LUN)) = 'Unknown '
         IRET = 1
        ELSEIF (.NOT. LUSED(ABS(LUN)) .AND. .NOT. LFPACK) THEN
         IRET = 1
        ELSE
         IRET = -1
        ENDIF
        GOTO 110
       ENDIF
      ENDIF
c
c  Start the LOOP,  if LUN is greater than 0
c
      IF ( LUN .GE. 0 ) THEN
      DO 100 L = IBEG,IEND
c
c  Now check IF the file has been opened without RESLUN call
c
       IF (LUSED(L)) GOTO 100
c
c  ISTAT < 0  implies logical unit number is opened 
c
       CALL CHKLUN(L,ISTAT)
       IF (ISTAT .LT. 0) THEN
         LUSED(L) = .TRUE.
         IRET = -1
         IF (ISTAT .EQ. -2) THEN
          RTABLE(L) = 'FPACK'
         ELSE
          RTABLE(L) = 'UNKNOWN'
         ENDIF
       ELSE
         LUSED(L) = .TRUE.
         IRET = 1
         RTABLE(L) = RNAME
         GOTO 110
       ENDIF
100   CONTINUE
      ENDIF
110   CONTINUE
      IF (LUN .EQ. 0) LUN = L
c
10    FORMAT('Logical unit number: ',I3,' already reserved by ',A12)
15    FORMAT('Logical unit number: ',I3,' illegally opened by ',A12)
20    FORMAT('Reserving logical unit number: ',I3,' for routine ',A12)
30    FORMAT('Freeing unit: ',I3,
     1       ' reserved by ',A,' closed by ',A)
40    FORMAT('Logical unit number: ',I3,' already free! ',A12)
55    FORMAT('Logical unit number: ',I3,' in use by: ',A12)
56    FORMAT('Logical unit number: ',I3,' NOT freed!')
      RETURN
      END
c
c------------------------------------------------------------------------------




