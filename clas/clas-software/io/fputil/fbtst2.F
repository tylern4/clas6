CDECK  ID>, FBTST2.
      SUBROUTINE FBTST2
c
c_begin_doc
c  RCS ID string
c  $Id: fbtst2.F,v 1.1.1.1 1997/03/28 20:55:42 marki Exp $
c
c  Purpose:
c  --------
c
c  Author:
c  -------
c     Volker Blobel          xxx Apr 15 15:00:00 EDT 1994
c
c  Major revisions:
c  ----------------
c     Serguei Boiarinov      Thu Oct 12 11:52:54 EDT 1995
c
c_end_doc
c
      IMPLICIT NONE
      SAVE
c
c_begin_inc
c  include files :
c  ---------------------
#include "bcscom.inc"
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
      PARAMETER (CFILE = '$rcsRCSfile$')
      PARAMETER (CREVIS = '$rcsRevision$')
      PARAMETER (CSTATE = '$rcsState$')
      PARAMETER (CDATE = '$rcsDate$')
      PARAMETER (CAUTHO = '$rcsAuthor$')
      DATA CRCSID/
     1'$Id: fbtst2.F,v 1.1.1.1 1997/03/28 20:55:42 marki Exp $'
     2/
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
c      CHARACTER*100  CRMESS
      PARAMETER (CRAUTH='Serguei Boiarinov')
      PARAMETER (CRNAME='FBTST2.F')
c
c  Local User defined variables:
c  -----------------------------
      INTEGER STATUS,BOSOPEN,BOSCLOSE,BOSREAD,BOSWRITE,HANDLE1,HANDLE2
c_end_var
c----6----------------------------------------------------------------72
C-----------------------------------------------------------------------
C-- open FPACK files for reading and writing
      STATUS = bosOpen('FPACK1.DAT','r',HANDLE1)
      IF(status.NE.0) print *,' STATUS=',status,'    handle1=',handle1
      STATUS = bosOpen('FPACK2.DAT','w',HANDLE2)
      IF(status.NE.0) print *,' STATUS=',status,'    handle2=',handle2
C-- read & write loop
  10  CONTINUE
      status = bosRead(handle1,'E')
      print *,' STATUS bosRead(handle1,E) =',status
      IF(status.EQ.-1) THEN
        print *,' End of file in test 2'
        GOTO 100
      ENDIF
      IF(status.GT.0) THEN
        print *,' Error in test 2'
        GOTO 100
      ENDIF
C-- write banks to file
      status = bosWrite(handle2,'E')
      print *,' STATUS bosWrite(handle2,E) =',status
C-- print all banks named in the list 'E'
      CALL BPRNT(IW,'E')
C-- drop all banks specified in the list E
      CALL BDROP(IW,'E')
      CALL BGARB(IW)
      GOTO 10                ! end event loop
C-- write FPACK buffers
 100  CONTINUE
C-- write banks to file
      status = bosWrite(handle2,'0')
      print *,' STATUS bosWrite(handle2,0) =',status
      WRITE(6,*) ' Test 2: exit'
C-- print all banks named in the list 'E'
      CALL BPRNT(IW,'E')
C-- close F-pack file
      status = bosClose(handle1)
      print *,' STATUS bosClose(handle1) =',status
      status = bosClose(handle2)
      print *,' STATUS bosClose(handle2) =',status
      print *,'=================== end of tst2 ====================='
      RETURN
      END
