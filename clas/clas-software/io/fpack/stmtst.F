*CMZ :  1.00/00 16/12/94  17.00.00  by  Pavel Binko
*-- Author :    Volker Blobel   16/12/94
      SUBROUTINE STMTST(NTEL,FTEXT)
*
      CHARACTER*(*) FTEXT
*
************************************************************************
*
*     First scan through text with recognition of:
*        special characters (incl. **)
*        numbers (unsigned integers)
*        text within ' or "
*        other text
*     Return NTEL = number of text elements
*            FTEXT= first text or blank, if text not first item
*
************************************************************************
*
      CHARACTER CH*1
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
*
      INU=0
      IF(NST.GT.0) THEN
*        immediately after input of a line
         NN =NST
         NST=-NST
      ELSE IF(NST.LT.0) THEN
*        another text?
         IF(MST.NE.0) THEN
*           remaining text exists - shift back
            DO 01 I=-NST+2,MST
   01       STMT(I+NST-1:I+NST-1)=STMT(I:I)
            NST=MST+NST-1
            MST=0
*           get next start (argument is dummy)
            CALL LINENT(' ')
            NN=NST
            NST=-NST
         ELSE
            NN=0
            NST=0
         END IF
      ELSE
         NN=0
      END IF
*     now NN is the number of characters in string STMT
      NTX=0
      IF(NN.EQ.0) GOTO 99
      ICHAR0=ICHAR('0')
      IST=1
*     search for next non-blank character
   20 DO 30 I=IST,NN
      IF(STMT(I:I).NE.' ') GOTO 40
   30 CONTINUE
      GOTO 99
*     CH or STMT(I:I) is next start character (nonblank)
   40 CH=STMT(I:I)
      IH=INDEX('+-*/*=(),.:;<>[]?%$&',CH)
      NTX=NTX+1
      IST=I+1
*     ITX(1,.) = index of start-character
*     ITX(2,.) = index of end-character
*     ITX(3,.) = class
*
      ITX(1,NTX)=I
      ITX(2,NTX)=I
      ITX(3,NTX)=1
      ITX(4,NTX)=IH
      IF(IH.EQ.3) THEN
*        for a star (*)
         IF(NTX.GT.1) THEN
*           test the next character
            IF(ITX(3,NTX-1).EQ.1.AND.ITX(4,NTX-1).EQ.3.AND.
     +         ITX(2,NTX-1).EQ.ITX(1,NTX)-1) THEN
*              this is a double star
               NTX=NTX-1
               ITX(2,NTX)=ITX(2,NTX)+1
               ITX(4,NTX)=5
            END IF
         END IF
      END IF
      IF(IH.NE.0) GOTO 20
      IF(CH.EQ.''''.OR.CH.EQ.'"') THEN
*        text string in apostrophs or quotation marks
         I=IST
   50    IF(I.LE.NN) THEN
            IF(STMT(I:I).EQ.CH) THEN
*              test double character
               IF(I+1.LE.NN) THEN
                  IF(STMT(I+1:I+1).EQ.CH) THEN
*                   remove double character
                    DO 55 J=I,ITX(1,NTX)+1,-1
   55               STMT(J+1:J+1)=STMT(J:J)
                    ITX(1,NTX)=ITX(1,NTX)+1
                    I=I+2
                    GOTO 50
                 END IF
               END IF
               GOTO 60
            END IF
            I=I+1
            GOTO 50
         END IF
         I=NN+1
   60    ITX(1,NTX)=ITX(1,NTX)+1
         ITX(2,NTX)=I-1
         ITX(3,NTX)=2
         ITX(4,NTX)=0
         IST=I+1
      ELSE IF(CH.EQ.'!') THEN
*        only comment follows
         ITX(2,NTX)=NN
         ITX(3,NTX)=0
         ITX(4,NTX)=0
         GOTO 99
      ELSE
         INUM=ICHAR(CH)-ICHAR0
         IF(INUM.GE.0.AND.INUM.LE.9) THEN
*           this is a number - continue with number search
            DO 70 I=IST,NN
            IN=ICHAR(STMT(I:I))-ICHAR0
            IF(IN.LT.0.OR.IN.GT.9) GOTO 80
            INUM=INUM*10+IN
   70       CONTINUE
            I=NN+1
   80       ITX(2,NTX)=I-1
            ITX(3,NTX)=4
            ITX(4,NTX)=INUM
            IST=I
         ELSE
*           not a number - continue with search for separator
            DO 90 I=IST,NN
            IF(STMT(I:I).EQ.' ') GOTO 95
            IH=INDEX('+-*/*=(),.:;<>[]?%$"&''!',STMT(I:I))
            IF(IH.NE.0) GOTO 95
   90       CONTINUE
            I=NN+1
   95       ITX(2,NTX)=I-1
            CALL UCASE(STMT(ITX(1,NTX):ITX(2,NTX)))
            ITX(3,NTX)=3
            ITX(4,NTX)=0
            IST=I
         END IF
      END IF
      GOTO 20
   99 FTEXT=' '
      NTEL=NTX
      IF(NTX.LE.0) GOTO 100
      IF(ITX(3,1).NE.2.AND.ITX(3,1).NE.3) GOTO 100
      FTEXT=STMT(ITX(1,1):ITX(2,1))
*
  100 RETURN
      END
