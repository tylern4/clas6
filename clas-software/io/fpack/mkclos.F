*CMZ :  1.00/00 16/12/94  17.00.00  by  Pavel Binko
*-- Author :    Volker Blobel   16/12/94
      SUBROUTINE MKCLOS(LUN,DANAME)
*
      CHARACTER*(*) DANAME
*
************************************************************************
*
*     Make CLOSE
*
************************************************************************
*
      CHARACTER STR*12
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
      IF(DANAME.EQ.'ALL') THEN
*        close all units
         CALL FCLOS
      ELSE
*        close specific unit
         IF(LUN.EQ.0) THEN
            LUN=IFRST(DANAME,0)
            IF(LUN.EQ.0) THEN
*              error - nothing to close
               GOTO 100
            END IF
         END IF
         CALL FUCLOS(LUN)
         IF(IOS.EQ.0) THEN
            NORMES='File closed'
         ELSE
            CALL PNVI(IOS,STR,JT)
            ERRMES='File CLOSE with error code IOS= '//STR(1:JT)
         END IF
      END IF
*
  100 RETURN
      END
