*CMZ :  1.00/00 16/12/94  17.00.00  by  Pavel Binko
*-- Author :    Volker Blobel   16/12/94
      SUBROUTINE FPARM(TEXT)
*
      CHARACTER*(*) TEXT
*
************************************************************************
*
*     Execute statements immediately
*
*     for: OPEN
*          SELECT
*          RSELECT
*          CLOSE
*          CLOSE ALL
*          FILECOPY
*          REWIND
*          TIMESTOP
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
      CHARACTER TEXT2*(LSTMT)
      CHARACTER KWTEXT*16,COPMES*72,NORCOP*72,BTEXT*7
      LOGICAL INIT, LIFLAG, PURCOM
      DATA INIT/.TRUE./
*
      IF(INIT) THEN
         INIT=.FALSE.
*        interactive job
         CALL FQCSET('PRINT',0)
      END IF
*     update print flag
      LIFLAG=.FALSE.
      CALL FQCOND('PRINT',IPFLAG)
      IF(IPFLAG.NE.0) LIFLAG=.TRUE.
*
*     determine <tab> characters, ASCII code is 9
    2 I = INDEX(TEXT,CHAR(9))
      IF(I.NE.0) THEN
         TEXT(I:I) = ' '
         GOTO 2
      END IF
*
*     determine pure comment lines
      PURCOM=.FALSE.
      IF(TEXT.EQ.' '.OR.TEXT(1:1).EQ.'*') THEN
         PURCOM=.TRUE.
      ELSE
         DO 7 I=1,LENB(TEXT)
         IF(TEXT(I:I).NE.' ') GOTO 8
    7    continue
         I=LENB(TEXT)
    8    IF(TEXT(I:I).EQ.'!') PURCOM=.TRUE.
      END IF
*
      BTEXT=' STMT: '
      IF(PURCOM) BTEXT=' *   : '
*
*     print eventually
      IF(LIFLAG) THEN
*
*        hide the userid and password
         TEXT2 = TEXT
         LB=LENB(TEXT2)
         CALL FPHIDE(TEXT2,LB)
*
*        print text input
         IF(LB.LE.72) THEN
            WRITE(*,102) BTEXT,TEXT2
         ELSE
*           split long text into pieces
            IB=0
    1       IA=IB+1
            DO 5 I=MIN0(LB,IA+71),IA,-1
            IF(TEXT2(I:I).EQ.' ') GOTO 6
    5       CONTINUE
            I=MIN0(LB,IA+71)
    6       IB=I
            IF(LB-IA.LT.71) IB=LB
            COPMES=TEXT2(IA:IB)
            IF(IA.EQ.1) WRITE(*,102) BTEXT,COPMES
            IF(IA.NE.1) WRITE(*,102) '       ',COPMES
            IF(IB.LT.LB) GOTO 1
         END IF
      END IF
*
      CALL FINITL
      IF(PURCOM) GOTO 100
      CALL LINEIN(TEXT)
   10 CALL STMTST(NTX,KWTEXT)
      IF(NTX.EQ.0) GOTO 100
      COPMES=ERRMES
      ERRMES=' '
      NORCOP=NORMES
      NORMES=' '
      IF(LFEXEC(KWTEXT).NE.0) THEN
         IF(LIFLAG.AND.ERRMES.NE.' ') THEN
            WRITE(*,101) ' ERR-> ',ERRMES
         END IF
         IF(ERRMES.EQ.' ') ERRMES=COPMES
         IF(LIFLAG.AND.NORMES.NE.' ') THEN
            WRITE(*,101) ' EXEC> ',NORMES
         END IF
         IF(NORMES.EQ.' ') NORMES=NORCOP
         GOTO 10
      END IF
      ERRMES='      What? '
      IF(LIFLAG) WRITE(*,101) ' ERR-> ',ERRMES
      GOTO 10
*
  100 RETURN
  101 FORMAT(1X,A7,A72,/)
  102 FORMAT(1X,A7,A72)
      END
