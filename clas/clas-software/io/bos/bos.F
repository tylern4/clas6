CDECK  ID>, BOS.
      SUBROUTINE BOS(JW,NDIM)
c
c_begin_doc
c  RCS ID string
c  $Id: bos.F,v 1.1.1.1 1997/03/28 20:49:31 marki Exp $
c
c  Purpose:
c  --------
c     INITIALIZATION OF THE BOS - SYSTEM
c
c     NAR    = NUMBER OF INITIALIZED ARRAYS ( 0 MEANS FIRST CALL)
c
c
c     NAMESD = NUMBER OF NAMES
c     NRESD  = NUMBER OF RESERVED WORDS (CHANGED BY CALL BNRES)
c     NUW    = NUMBER OF RETURN CODE MESSAGES TO BE PRINTED
c     NUB    = NUMBER OF BANKS TO BE PRINTED BY USER CALLS
c     NLP    = NUMBER OF BANKS OF THE SAME NAME FOR TREE SEARCH
c              (IF THERE ARE MORE THAN NLP BANKS OF THE SAME NAME, A)
c              (WORK BANK OF INDICES/NUMBERS WILL BE CREATED, TO ALLOW)
c              (BINARY SEARCH IN NLINK/MLINK            )
c
c     A WORK BANK WITH NR/INDEX PAIRS IS CREATED, IF MORE THAN NLP
c     NAMED BANKS OF THE SAME NUMBER EXIST, TO SPEED UP NLINK/ML
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
#include "param.inc"
c_end_inc
c
c_begin_var
c  input/output variables:
c  -----------------------
      INTEGER    JW(*),NDIM
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
     1'$Id: bos.F,v 1.1.1.1 1997/03/28 20:49:31 marki Exp $'
     2/
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
c      CHARACTER*100  CRMESS
      PARAMETER (CRAUTH='Serguei Boiarinov')
      PARAMETER (CRNAME='BOS.F')
c
c  Local User defined variables:
c  -----------------------------
      INTEGER KW,LGDUM,LCL,NAR,NAMESD,NRESD,NUW,NUB,NLP,LOCIW,LOCJW,MDIM
      INTEGER LOCF,INTCHA,NAMIND,M,NSQRT,I,MCLEAR,J,NAMAX
      INTEGER NSYSTR(322)
      EQUIVALENCE (NSYST,NSYSTR(1))
      COMMON/BOSMDL/LGDUM(6),LCL(10)
      COMMON/HCS/KW(100)
      CHARACTER*8 ARNAME
      INTEGER IFMTA(9),LARR(10)
      DATA NAR/0/,IFMTA/0,4,0,10,13,10,1000002,0,0/
      DATA NAMESD/100/,NRESD/100/,NUW/100/,NUB/100/,NLP/40/
c_end_var
c----6----------------------------------------------------------------72
C-----------------------------------------------------------------------
      NARR=0
      LOCIW=LOCF(IW(1))
      LOCJW=LOCF(JW(1))
      MDIM=IABS(NDIM)
      IF(NAR.EQ.0) THEN
C FIRST CALL, INITIALIZE BASIC ARRAY IW IN COMMON/BCS/
        IF(LOCIW.NE.LOCJW.OR.NDIM.LT.0) THEN
          WRITE(NPR,1001)
          CALL BABEND('BOS')
        END IF
C CLEAR AND DEFINE SYSTEM COMMON/SYSCOM/
        MDIM=MAX0(MDIM,1000)
        DO M=1,322
          NSYSTR(M)=0
        ENDDO
        NSYST=NSS
        NAMES=NAMESD
        NRESR=NRESD
        NLPLM=NLP
        DO M=1,10
          LCL(M)=0
        ENDDO
        CALL TIMEL(TLEFT)
C DETERMINE PRIME NUMBER FOR HASH FUNCTION
        NPRIM=NAMES+MOD(NAMES,2)+1
   10   NPRIM=NPRIM-2
        NSQRT=SQRT(FLOAT(NPRIM))
        DO I=3,NSQRT,2
          IF(I*(NPRIM/I).EQ.NPRIM) GOTO 10
        ENDDO
      ELSE IF(NDIM.GE.0) THEN
C CHECK, WETHER THE ARRAY IS ALREADY INITIALIZED
        DO I=1,NAR
          IF(LOCJW.EQ.IARR(I)) THEN
C ARRAY ALREADY USED
            IF(I.EQ.1) GOTO 100
            IF(MDIM.EQ.0) MDIM=LARR(I)
            GOTO 40
          END IF
        ENDDO
      ENDIF
C STORE LOCATION OF NEW ARRAY, IF NDIM NOT NEGATIVE
      IF(NDIM.GE.0) THEN
         IF(NAR.GE.10) THEN
            WRITE(NPR,1002)
            CALL BABEND('BOS')
         END IF
         NAR=NAR+1
         IARR(NAR)=LOCJW
         LARR(NAR)=MDIM
         NARR=NAR
      END IF
C
C     NAME FOR STANDARD ARRAYS
C
   40 IF(LOCIW.EQ.LOCJW) THEN
         ARNAME='/BCS/IW '
      ELSE IF(LOCF(KW(1)).EQ.LOCJW) THEN
         ARNAME='/HCS/KW '
      ELSE
         ARNAME='        '
      ENDIF
C
C     CLEAR ARRAY
C
      IF(NDIM.GE.0) THEN
         MCLEAR=MDIM
      ELSE
         MCLEAR=NSYST+NAMES
      END IF
      DO M=1,MCLEAR
        JW(M)=0
      ENDDO
C
C     INITIALIZE ARRAY
C
      JW(  4)=1
      JW(LUC)=NCA
      JW(LUP)=NPR
      JW(LUW)=NUW
      JW(LUB)=NUB
      JW(IAR+1)=INTCHA(ARNAME(1:4))
      JW(IAR+2)=INTCHA(ARNAME(5:8))
      JW(ICK)=12345
C
      JW(NJA)=NARR
      JW(NJW)=MDIM
      JW(LJW)=LOCJW
      JW(INM)=NAMES+NSYST+1
C named banks on a page boundary
      I = LOCF(JW(JW(INM)))
      J = ((I+127)/128)*128
      JW(INM)=JW(INM)+(J-I)
      JW(IGP)=JW(INM)
      JW(IWK)=MDIM+1
      JW(IGN)=MDIM
      JW(ICK)=12345
      JW(LCH)=1
C CREATE SYSTEM WORK BANK FOR LINK INDICES
      JW(IDL)=0
      CALL WBANK(JW,JW(IDL),NAMES,1,*90)
      JW(JW(IDL)-3)=INTCHA('+LNK')
C FOLLOWING WORK BANKS ONLY FOR BASIC ARRAY
      IF(LOCIW.NE.LOCJW) GOTO 100
C CREATE SYSTEM WORK BANKS
C   WORK BANK FOR NAMES
      CALL WBANK(IW,IDNAM,NAMES,1,*90)
      IW(IDNAM)=0
      IW(IDNAM-3)=INTCHA('+NAM')
C   WORK BANK FOR POINTER
      CALL WBANK(IW,IDPTR,NAMES+NPRIM,1,*90)
      IW(IDPTR-3)=INTCHA('+PTR')
C   WORK BANK FOR BANK FORMAT INDICES
      CALL WBANK(IW,IDFMT,NAMES,1,*90)
      IW(IDFMT-3)=INTCHA('+IDF')
C
      NAMI=NAMIND('+FMT')
      CALL WBANK(IW,IW(IDFMT+1),9,1,*90)
      IW(IW(IDFMT+1)-3)=INTCHA('+FMT')
      IFMTA(1)=INTCHA('+FMT')
      CALL UCOPY(IFMTA,IW(IW(IDFMT+1)+1),9)
      GOTO 100
C
   90 WRITE(NPR,1003)
      CALL BABEND('BOS')
C CHANGE DEFAULT MAX NR OF NAMES
      ENTRY BNAMES(NAMAX)
      NAMESD=MAX(10,NAMAX)
      GOTO 100
C ENTRY TO INCREASE NR OF RESERVED WORDS
      ENTRY BNRES
      NRESD=NRESD+100
C
  100 RETURN
 1001 FORMAT('0BOS++++++++ INITIALIZATION ERROR - STOP'/
     1       14X,'BASIC ARRAY NOT FIRST ARRAY'/)
 1002 FORMAT('0BOS++++++++ INITIALIZATION ERROR - STOP'/
     1       14X,'MORE THAN 10 ARRAYS'/)
 1003 FORMAT('0BOS++++++++ INITIALIZATION ERROR - STOP'/
     1       14X,'ARRAY LENGTH TO SMALL'/)
      END
