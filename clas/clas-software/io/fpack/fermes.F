*CMZ :  1.00/00 16/12/94  17.00.00  by  Pavel Binko
*-- Author :    Volker Blobel   16/12/94
      SUBROUTINE FERMES(TEXT,NE)
*
      CHARACTER*(*) TEXT
*
************************************************************************
*
*     NE = 0   get normal message
*     NE = 1   get error message
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
      IF(NE.EQ.0) THEN
         TEXT=NORMES
         NORMES=' '
      ELSE
         TEXT=ERRMES
         ERRMES=' '
      END IF
*
  100 RETURN
      END
