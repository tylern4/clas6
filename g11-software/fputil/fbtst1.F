CDECK  ID>, FBTST1.
      SUBROUTINE FBTST1
c
c_begin_doc
c  RCS ID string
c  $Id: fbtst1.F,v 1.1.1.1 1997/03/28 20:55:42 marki Exp $
c
c  Purpose:
c  --------
c     *   EXAMPLE,                                                     *
c     *      GENERATE BANKS FOR NN EVENTS AND WRITE TO DATA SET 1      *
c     *      (USES RANDOM GENERATOR RNDM FROM CERNLIB)                 *
c     *                                                                *
c     *   CREATE BANK (HEAD,0) WITH INFORMATION ON RUN AND EVENT       *
c     *   NUMBER ETC                                                   *
c     *                                                                *
c     *   CREATE BANK (RAWD,0) WITH RANDOM FLOATING POINT NUMBERS      *
c     *                                                                *
c     *   CREATE BANKS (CHEN,J) , EACH WITH SOME CHANNEL NUMBERS       *
c     *   AND ENERGY                                                   *
c
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
#include "bcs.inc"
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
     1'$Id: fbtst1.F,v 1.1.1.1 1997/03/28 20:55:42 marki Exp $'
     2/
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
c      CHARACTER*100  CRMESS
      PARAMETER (CRAUTH='Serguei Boiarinov')
      PARAMETER (CRNAME='FBTST1.F')
c
c  Local User defined variables:
c  -----------------------------
      INTEGER NN,IEV,IKY,IHEAD,ND,IRAWD,I,NR,NL,L,ICHEN,ICHAN,IERR
      INTEGER MDROP,NBANK,LDUMMY
      REAL RNDM,DUMMY
c_end_var
c----6----------------------------------------------------------------72
C-- open FPACK file for writing
      CALL FPARM('OPEN UNIT=11 FILE="FPACK1.DAT" WRITE
     & RECL=32760 STATUS=NEW')
C
C-- bank formats
cboy        CALL BKFMT('HEAD','B16')
        CALL BKFMT('HEAD','10I')
        CALL BKFMT('RAWD','F')
        CALL BKFMT('CHEN','(I,F)')
C
      NN = 2         ! the number of events
      DO 40 IEV=1,NN ! event loop
* drop key bank for E list ( to have event number increment in segment
* header - see s/r fwbos )
        IKY = MDROP(IW,'+KYE',0)
***************************************************
*     CREATE AND FILL BANK (HEAD,0)
*     I   FMT   BANK(HEAD,0)
*     ----------------------
*     1    I    EXPERIMENT NUMBER
*     2    I    RUN NUMBER
*     3    I    EVENT NUMBER
*     4    I    DATA TYPE 0=DATA >0=MONTE CARLO
*     5    I    FURTHER DATA IDENTIFIER
*     6    I    ...
*     7    I    ...
*     8    I    ...
*     9    I    DATE YYMMDD
*     10   I    TIME   HHMM
***************************************************
        IHEAD=NBANK('HEAD',0,10,2)
        IF(IHEAD.EQ.0) GOTO 40
        IW(IHEAD+2)=4711
        IW(IHEAD+3)= IEV
        IW(IHEAD+4)=   4
        IW(IHEAD+5)=   5
        IW(IHEAD+6)=   6
        IW(IHEAD+7)=   7
        IW(IHEAD+8)=   8
        IW(IHEAD+9)=   9
        IW(IHEAD+10)=  10
        IW(IHEAD+11)=  11
        IW(IHEAD+12)=  12
        IW(IHEAD+13)=  13
        IW(IHEAD+14)=  14
        IW(IHEAD+15)=  15
cboy        CALL DATIME(IW(IHEAD+9),IW(IHEAD+10))
***************************************************
*     CREATE AND FILL BANK (RAWD,0)
*     I   FMT   BANK(RAWD,0)
*     ----------------------
*     1    F    ENERGY
*     ...  F    ) REPEATED
*     ND   F    ) REPEATED
***************************************************
        ND=10.0+10.0*RNDM(DUMMY)
        IRAWD=NBANK('RAWD',0,ND,1)
        IF(IRAWD.EQ.0) GOTO 100
        DO I=1,ND
          RW(IRAWD+I)=RNDM(I)
        ENDDO
***************************************************
*     CREATE AND FILL BANKS (CHEN,J)
*     I   FMT   BANK(CHEN,J)
*     ----------------------
*     1    I    CHANNEL NUMBER
*     2    F    ENERGY
*     3    I    ) REPEATED (CHANNEL NUMBER)
*     4    F    ) REPEATED (ENERGY)
*     ...
*     ND
***************************************************
        NR=0
        NL=5.0+5.0*RNDM(DUMMY)
        DO L=1,NL
          NR=NR+1+RNDM(L)*2
          ICHEN=NBANK('CHEN',NR,6,1)
          IF(ICHEN.EQ.0) THEN
            PRINT *,' ALARM EXIT !!!'
            GOTO 100
          ENDIF
          ICHAN=256*RNDM(LDUMMY)
          IW(ICHEN+1)=ICHAN
          RW(ICHEN+2)=RNDM(L)
          IW(ICHEN+3)=ICHAN+1
          RW(ICHEN+4)=RNDM(L+1)+RNDM(L-1)
          IW(ICHEN+5)=ICHAN+2
          RW(ICHEN+6)=RNDM(L+2)
        ENDDO
C-- define event-list
        CALL BLIST(IW,'E=','HEADRAWDCHEN')
C-- write all banks specified in the list E
        CALL FWBOS(IW,11,'E',IERR)
        PRINT *,' IERR(after FWBOS-(E))=',IERR
*=======================================================================
C-- print all banks named in the list 'E'
        WRITE(6,*) ' Test 1:'
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
      WRITE(6,*) ' Test 1:'
      CALL BPRNT(IW,'E')
C-- rewind FPACK files
***        CALL FURWND(11)
 100  CONTINUE
C-- close F-pack file
      print *,' FMT test'
      CALL WPRNT(IW,IDFMT)
      print *,' end of FMT test'
      CALL FPARM('CLOSE FILE11')
      print *,'=================== end of tst1 ====================='
      RETURN
      END
