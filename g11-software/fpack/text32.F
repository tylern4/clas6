*CMZ :  1.00/00 16/12/94  17.00.00  by  Pavel Binko
*-- Author :    Volker Blobel   16/12/94
      SUBROUTINE TEXT32
*
************************************************************************
*
*     Replace type 3 text by type 2 text
*
************************************************************************
*
      CHARACTER*1 CH
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
      I=0
   10 IF(I.GE.NTX) GOTO 100
      I=I+1
      IF(ITX(3,I).NE.3) GOTO 10
*     type 3 text, allow . : () contigous
      J=I
   20 IF(J.EQ.NTX) GOTO 40
      IF(ITX(1,J+1)-1.NE.ITX(2,J)) GOTO 40
      IF(ITX(3,J+1).NE.1) GOTO 10
      CH=STMT(ITX(1,J+1):ITX(2,J+1))
      IF(CH.EQ.',') GOTO 40
      J=J+1
      IF(INDEX('.:()_[]',CH).EQ.0) GOTO 10
      K=J
   30 IF(K.EQ.NTX) GOTO 10
      K=K+1
      IF(ITX(3,K).NE.3) GOTO 10
      IF(ITX(1,K)-1.NE.ITX(2,K-1)) GOTO 10
      J=K
      GOTO 20
*     connect everything from I to J
   40 ITX(2,I)=ITX(2,J)
   50 IF(J.EQ.I) THEN
         I=J
         GOTO 10
      END IF
      ITX(3,J)=0
      J=J-1
      GOTO 50
*
  100 RETURN
      END
