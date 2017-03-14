*CMZ :  1.00/00 16/12/94  17.00.00  by  Pavel Binko
*-- Author :    Volker Blobel   16/12/94
      SUBROUTINE LINEIN(LINE)
*
      CHARACTER*(*) LINE
*
************************************************************************
*
*     One line of "L" code or "TEXT"  data
*     special characters significant in this subroutine are  ; ! ' "
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
      INTEGER IJ(4)
      CHARACTER CHSP(4)*1
      LOGICAL START
*     special characters are  ; ! ' "
      DATA START/.TRUE./,CHSP/';','!','''','"'/
*
      IF(START) THEN
         START=.FALSE.
         NST=0
         KST=0
      END IF
*
*     ignore pure comment lines and ...
      IF(LINE(1:1).EQ.'*') GOTO 100
*     ... blank lines
      IF(LINE.EQ.' ') GOTO 100
*     --------------------------------------
*     determine non-blank text range IA...IB
      NB=LEN(LINE)
      DO 02 IB=NB,1,-1
      IF(LINE(IB:IB).NE.' ') GOTO 04
   02 CONTINUE
   04 DO 06 IA=1,IB
      IF(LINE(IA:IA).NE.' ') GOTO 08
   06 CONTINUE
   08 MST=0
*     --------------------------------------
      IF(KST.NE.0) THEN
*        this is a continuation line
         IF(LINE(IA:IA).EQ.'&') THEN
*           starts with continuation character
            NST=KST+IB-IA
            IF(NST.GT.LSTMT) THEN
               WRITE(*,*) ' Input string too long, overflow occurs in:'
               WRITE(*,*) LINE
            ELSE
               STMT(KST+1:NST)=LINE(IA+1:IB)
            END IF
         ELSE
            NST=KST+IB
            IF(NST.GT.LSTMT) THEN
               WRITE(*,*) ' Input string too long, overflow occurs in:'
               WRITE(*,*) LINE
            ELSE
               STMT(KST+1:NST)=LINE(1:IB)
            END IF
         END IF
         KST=0
      ELSE
*        this ist a new statement
         NST =IB
         IF(NST.GT.LSTMT) THEN
            WRITE(*,*) ' Input string too long, overflow occurs in:'
            WRITE(*,*) LINE
         ELSE
            STMT=LINE
         END IF
      END IF
*
************************************************************************
*
      ENTRY LINENT(LINE)
*
************************************************************************
*
*     L = text-string flag
*     K = last checked character index
*
************************************************************************
*
   10 L=0
      K=0
*     reset index positions
      DO 20 J=1,4
   20 IJ(J)=-1
*     update position values
   30 M=0
      DO 50 J=1,4
      IF(IJ(J).EQ.0) GOTO 50
      IF(IJ(J).GT.K) GOTO 40
      IF(K.LT.NST) THEN
         IJ(J)=INDEX(STMT(K+1:NST),CHSP(J))
      ELSE
         IJ(J)=0
      END IF
      IF(IJ(J).EQ.0) GOTO 50
      IJ(J)=IJ(J)+K
*     find next special character (type is M, position will be K)
   40 IF(M.EQ.0) M=J
      IF(IJ(J).LT.IJ(M)) M=J
   50 CONTINUE
*     finished, if no special character
      IF(M.EQ.0) GOTO 60
      K=IJ(M)
*
      IF(L.EQ.0.AND.M.EQ.1) THEN
*        semicolon, not within text string
         MST=NST
         NST=IJ(M)-1
         GOTO 90
      END IF
      IF(L.EQ.0) THEN
*        not within text string
         IF(M.EQ.2) THEN
*           a ! means comment
            NST=IJ(M)-1
            GOTO 60
         ELSE
*           start of text string with ' or "
            L=M
         END IF
      ELSE IF(M.EQ.L) THEN
*        within text string only text string character is significant
         IF(K.LT.NST) THEN
            IF(STMT(K+1:K+1).EQ.CHSP(M)) THEN
*              double char means internal character (not ended)
               K=K+1
               GOTO 30
            END IF
         END IF
*        text string ended
         L=0
      END IF
      GOTO 30
*
*     find last non-blank character
   60 DO 70 I=NST,1,-1
      IF(STMT(I:I).NE.' ') GOTO 80
   70 CONTINUE
      I=0
   80 NST=I
      IF(NST.EQ.0) GOTO 100
      IF(STMT(NST:NST).EQ.'&') THEN
*        continuation
         KST=NST-1
         NST=0
         GOTO 100
      END IF
*     processing of one statement
   90 CONTINUE
*
  100 RETURN
      END
