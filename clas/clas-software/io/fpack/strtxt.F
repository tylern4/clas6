*CMZ :  1.00/00 16/12/94  17.00.00  by  Pavel Binko
*-- Author :    Volker Blobel   16/12/94
      SUBROUTINE STRTXT(I,STXT,N)
*
      CHARACTER*8 STXT(*)
*
************************************************************************
*
*     Collect stream of TEXT strings (class 2),
*     separated optionally by commas
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
*
      N=0
C     CALL STMTPR
      I=I-1
   10 IF(I.EQ.NTX) GOTO 100
      IF(ITX(3,I+1).NE.2) GOTO 100
      I=I+1
*     store text string
      N=N+1
      STXT(N)=STMT(ITX(1,I):ITX(2,I))
      IF(I.EQ.NTX.OR.N.GT.500) GOTO 100
      IF(ITX(3,I+1).EQ.1) THEN
         IF(STMT(ITX(1,I+1):ITX(2,I+1)).EQ.',') THEN
            I=I+1
         END IF
      END IF
      GOTO 10
*
  100 RETURN
      END
