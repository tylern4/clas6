*CMZ :  1.00/00 16/12/94  17.00.00  by  Pavel Binko
*-- Author :    Volker Blobel   16/12/94
      FUNCTION JSCAN(IA,IE,NT)
*
************************************************************************
*
*     Search forward or backward from IA to IE for type NT
*     NT ne 0 means: next element with type = NT (including IA)
*     NT =  0 means: next active element (not including IA)
*     return with zero if not found
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
      JSCAN=0
      IF(IA.LT.0.OR.IA.GT.NTX) GOTO 100
      IF(IE.LT.0.OR.IE.GT.NTX) GOTO 100
      IF(IA.LE.IE) THEN
         NI=+1
      ELSE
         NI=-1
      END IF
      I=IA
      IF(NT.EQ.0) THEN
*        next active element
         IF(IA.EQ.IE) GOTO 100
   10    I=I+NI
         IF(ITX(3,I).EQ.0) THEN
*           inactive element
            IF(I.EQ.IE) GOTO 100
            GOTO 10
         END IF
      ELSE
*        look for element with type NT
   20    IF(ITX(3,I).NE.NT) THEN
            IF(I.EQ.IE) GOTO 100
            I=I+NI
            GOTO 20
         END IF
      END IF
      JSCAN=I
*
  100 RETURN
      END
