CDECK  ID>, FBTST5.
      SUBROUTINE FBTST5
c
c_begin_doc
c  RCS ID string
c  $Id: fbtst5.F,v 1.1.1.1 1997/03/28 20:55:43 marki Exp $
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
#include "bossys.inc"
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
     1'$Id: fbtst5.F,v 1.1.1.1 1997/03/28 20:55:43 marki Exp $'
     2/
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
c      CHARACTER*100  CRMESS
      PARAMETER (CRAUTH='Serguei Boiarinov')
      PARAMETER (CRNAME='FBTST5.F')
c
c  Local User defined variables:
c  -----------------------------
      INTEGER NN,IEV,IKY,IHEAD,NCOL,NROW,IRAWD,I,IERR
      INTEGER MDROP,NBANK
      REAL RNDM,DUMMY
c_end_var
c----6----------------------------------------------------------------72
C-- open FPACK file for writing
      CALL FPARM('OPEN UNIT=11 FILE="FPACK4.DAT" WRITE
     & RECL=32760 STATUS=NEW')
C-- bank formats
cboy      CALL BKFMT('HEAD','10I')
      CALL BKFMT('HEAD','B16')
      CALL BKFMT('RAWD','I')
C
      NN = 2         ! the number of events
      DO 40 IEV=1,NN ! event loop
* drop key bank for E list ( to have event number increment in segment
* header - see s/r fwbos )
        IKY = MDROP(IW,'+KYE',0)
***************************************************
*     CREATE AND FILL BANK (HEAD,0)
*     I             FMT   BANK(HEAD,0)
*     ---------------------------------
*     1              I    EXPERIMENT NUMBER
*     2              I    RUN NUMBER
*     3              I    EVENT NUMBER
*     4              I    DATA TYPE 0=DATA >0=MONTE CARLO
*     5              I    FURTHER DATA IDENTIFIER
*     6              I    ...
*     7              I    ...
*     8              I    ...
*     9              I    DATE YYMMDD
*     10             I    TIME   HHMM
***************************************************
        IHEAD=NBANK('HEAD',0,10,5)
      print *,' ihead=',ihead
        IF(IHEAD.EQ.0) GOTO 40
        IW(IHEAD+2)=4711
        IW(IHEAD+3)= IEV
        CALL DATIME(IW(IHEAD+9),IW(IHEAD+10))
***************************************************
*     CREATE AND FILL BANK (RAWD,0)
*     I             FMT   BANK(RAWD,0)
*     --------------------------------
*     1              F    ENERGY
*     ...            F    ) REPEATED
*     NCOL*NROW      F    ) REPEATED
***************************************************
        NCOL=10.0+10.0*RNDM(DUMMY)
        NROW=10.0+10.0*RNDM(DUMMY)
      print *,' ncol,nrow=',ncol,nrow
        IRAWD=NBANK('RAWD',0,NCOL,NROW)
      print *,' irawd=',irawd
        IF(IRAWD.EQ.0) GOTO 100
        DO I=1,NCOL*NROW
ccc          RW(IRAWD+I)=RNDM(I)
          IW(IRAWD+I)=10.0*RNDM(DUMMY)
        ENDDO
***************************************************
*     CREATE AND FILL BANKS (CHEN,J)
*     I             FMT   BANK(CHEN,J)
*     --------------------------------
*     1              I    CHANNEL NUMBER
*     2              F    ENERGY
*     3              I    ) REPEATED (CHANNEL NUMBER)
*     4              F    ) REPEATED (ENERGY)
*     ...
*     NCOL*NROW
***************************************************
c        NR=0
c        NL=5.0+5.0*RNDM(DUMMY)
c        DO L=1,NL
c          NR=NR+1+RNDM(L)*2
c          ICHEN=NBANK('CHEN',NR,6)
cccc          ICHEN=NBANK('CHEN',NR,6,1)
c          IF(ICHEN.EQ.0) THEN
c            PRINT *,' ALARM EXIT !!!'
c            GOTO 100
c          ENDIF
c          ICHAN=256*RNDM(LDUMMY)
c          IW(ICHEN+1)=ICHAN
c          RW(ICHEN+2)=RNDM(L)
c          IW(ICHEN+3)=ICHAN+1
c          RW(ICHEN+4)=RNDM(L+1)+RNDM(L-1)
c          IW(ICHEN+5)=ICHAN+2
c          RW(ICHEN+6)=RNDM(L+2)
c        ENDDO
C-- define event-list
c        CALL BLIST(IW,'E=','HEADRAWDCHEN')
        CALL BLIST(IW,'E=','HEADRAWD')
C-- compress definition
      print *,' before FMTDEF'
        CALL FMTDEF('HEAD',10,'I')
        CALL FMTDEF('RAWD',NCOL,'I')
      print *,' after FMTDEF'
C-- write all banks specified in the list E
        CALL FWBOS(IW,11,'E',IERR)
        PRINT *,' IERR(after FWBOS-(E))=',IERR
*=======================================================================
C-- print all banks named in the list 'E'
        WRITE(6,*) ' Test 5:'
        CALL BPRNT(IW,'E')
C-- drop all banks specified in the list E
        CALL BDROP(IW,'E')
        CALL BGARB(IW)
   40 CONTINUE                ! end event loop
C-- write FPACK buffers
      CALL FWBOS(IW,11,'0',IERR)
      PRINT *,' IERR(after FWBOS-(0))=',IERR
*=======================================================================
C-- print all banks named in the list 'E'
      WRITE(6,*) ' Test 5:'
      CALL BPRNT(IW,'E')
C-- rewind FPACK files
***        CALL FURWND(11)
 100  CONTINUE
C-- close F-pack file
      CALL FPARM('CLOSE FILE11')
      print *,'=================== end of tst5 ====================='
      RETURN
      END
