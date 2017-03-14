*CMZ :  1.00/00 16/12/94  17.00.00  by  Pavel Binko
*-- Author :    Volker Blobel   16/12/94
      SUBROUTINE STMTLL
*
************************************************************************
*
*     Recognize logical language elements
*     and standardize certain language elements
*
************************************************************************
*
*KEEP,CTEXTIR.
*     one statement up to 5280 = 66 * 80 characters
      PARAMETER (LSTMT=5280)
      CHARACTER STMT*(LSTMT),ERRMES*72,NORMES*72
      PARAMETER (NDIMIT=640)
      REAL RTX(4,NDIMIT)
      COMMON/CTXTIR/NST,MST,KST,INU,NTX,ITX(4,NDIMIT)
      COMMON/CTXTST/STMT,ERRMES,NORMES
      EQUIVALENCE (ITX(1,1),RTX(1,1))
      SAVE/CTXTIR/,/CTXTST/
*KEND.
      CHARACTER    VARIAB*5, CH*2
*     CHARACTER    VARNAM*8, VBRNAM*8
      CHARACTER*6  PRT(3,4)
      CHARACTER*5  LOGOP(13)*5
*     logical language elements
      DATA LOGOP/'GT','GE','LT','LE','EQ','NE','NOT','AND','OR','EQV',
     +           'NEQV','TRUE','FALSE'/
*
************************************************************************
*
*     Logical keywords have ITX(3,I) = 7 and ITX(4,I) accordingt to:
*
*      1   .GT.   >          6   .NE.  <>           11   .NEQV.
*      2   .GE.   >=         7   .NOT.              12   .TRUE.
*      3   .LT.   <          8   .AND.              13   .FALSE.
*      4   .LE.   <=         9   .OR.
*      5   .EQ.   ==        10   .EQV.
*
************************************************************************
*
*     first text / second text / combined text
      DATA PRT/'GO','TO','GOTO','END','IF','ENDIF',
     +         'ELSE','IF','ELSEIF','END','DO','ENDDO'/
*
      INU=1
*     identify logical operators
      IA=0
*     text (class 3)  is searched for in (IA+1,NTX)
   10 I=JSCAN(IA+1,NTX,3)
      IA=I
      IF(I.EQ.0) GOTO 20
      VARIAB=STMT(ITX(1,I):ITX(2,I))
      DO 11 IS=1,13
      IF(VARIAB.EQ.LOGOP(IS)) GOTO 12
   11 CONTINUE
      GOTO 10
*     find next elements to the left and to the right
   12 IL=IA
   13 IL=IL-1
      IF(IL.LE.0) GOTO 10
      IF(ITX(3,IL).EQ.0) GOTO 13
      IF(ITX(3,IL).NE.1.OR.ITX(4,IL).NE.10) GOTO 10
      IR=IA
   14 IR=IR+1
      IF(IR.GT.NTX) GOTO 10
      IF(ITX(3,IR).EQ.0) GOTO 14
      IF(ITX(3,IR).NE.1.OR.ITX(4,IR).NE.10) GOTO 10
*     both must be special characters and ...
C     IF(ITX(3,IL).NE.1.OR.ITX(3,IR).NE.1) GOTO 10
*     ... both must be dots
C     IF(ITX(4,IL).NE.10.OR.ITX(4,IR).NE.10) GOTO 10
*     logical operator found, modify table
      ITX(3,IL)=0
      ITX(3,IR)=0
      ITX(3,I )=7
      ITX(4,I )=IS
      GOTO 10
*     further logical operators (special characters)
   20 IA=0
*     special char (class = 1)  is searched for
   30 I=JSCAN(IA+1,NTX,1)
      IA=I
      IF(I.EQ.0) GOTO 40
      VARIAB=' '
      CH=STMT(ITX(1,I):ITX(2,I))
      IF(CH.EQ.'=') THEN
         J=JSCAN(IA,NTX,0)
         IF(J.NE.0) THEN
            IF(ITX(3,J).EQ.1) THEN
               IF(STMT(ITX(1,J):ITX(2,J)).EQ.'=') VARIAB='EQ'
            END IF
         END IF
      ELSE IF(CH.EQ.'/') THEN
         J=JSCAN(IA,NTX,0)
         IF(J.NE.0) THEN
            IF(ITX(3,J).EQ.1) THEN
               IF(STMT(ITX(1,J):ITX(2,J)).EQ.'=') VARIAB='NE'
            END IF
         END IF
      ELSE IF(CH.EQ.'<'.OR.CH.EQ.'>') THEN
         IF(CH.EQ.'<') CH='LT'
         IF(CH.EQ.'>') CH='GT'
         VARIAB=CH
         J=JSCAN(IA,NTX,0)
         IF(J.NE.0) THEN
            IF(ITX(3,J).EQ.1) THEN
               IF(STMT(ITX(1,J):ITX(2,J)).EQ.'=') THEN
                  VARIAB =CH(1:1)//'E'
               ELSE IF(CH.EQ.'LT'.AND.STMT(ITX(1,J):ITX(2,J)).EQ.'>')
     +            THEN
                  VARIAB='NE'
               END IF
            END IF
         END IF
      END IF
      IF(VARIAB.EQ.' ') GOTO 30
      DO 35 IS=1,13
      IF(VARIAB.EQ.LOGOP(IS)) GOTO 36
   35 CONTINUE
      GOTO 30
   36 ITX(3,I)=7
      ITX(4,I)=IS
      IF(IS.EQ.1.OR.IS.EQ.3) GOTO 30
      ITX(2,I)=ITX(2,J)
      ITX(3,J)=0
      GOTO 30
   40 CONTINUE
*********************************************************************
*   This part has been commented out because it contains
*   unaccessible code ( compiler warning ) and it is actually
*   not used in applications.             26.08.93
*********************************************************************
*     combine  text, for example END IF to ENDIF
*     IA=0
*     NOTE=0
*     text is searched for
*  50 I=JSCAN(IA,NTX,0)
*     IA=I
*     IF(I.EQ.0) GOTO 80
*     IF(ITX(3,I).NE.2.OR.ITX(4,I).EQ.0) THEN
*        NOTE=0
*     ELSE
*        there is text
*        IF(NOTE.EQ.0) THEN
*           VARNAM=STMT(ITX(1,I):ITX(2,I))
*           NOTE=1
*           J=I
*        ELSE IF(NOTE.EQ.1) THEN
*           VBRNAM=STMT(ITX(1,I):ITX(2,I))
*           DO 60 K=1,4
*           IF(VARNAM.EQ.PRT(1,K).AND.VBRNAM.EQ.PRT(2,K)) THEN
*              ITX(2,J)=ITX(2,I)
*              ITX(3,I)=0
*              STMT(ITX(1,J):ITX(2,J))=PRT(3,K)
*              NOTE=0
*              GOTO 50
*           END IF
*  60       CONTINUE
*           VARNAM=VBRNAM
*           J=I
*        END IF
*     END IF
*     finally compress
   80 J=0
      DO 90 I=1,NTX
      IF(ITX(3,I).EQ.0) GOTO 90
      J=J+1
      ITX(1,J)=ITX(1,I)
      ITX(2,J)=ITX(2,I)
      ITX(3,J)=ITX(3,I)
      ITX(4,J)=ITX(4,I)
   90 CONTINUE
      NTX=J
*
  100 RETURN
      END
