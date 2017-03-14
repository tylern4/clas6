*CMZ :  1.00/00 16/12/94  17.00.00  by  Pavel Binko
*-- Author :    Volker Blobel   16/12/94
      SUBROUTINE PNVE(FLT,STR,JS)
*
      CHARACTER*(*) STR
*
************************************************************************
*
*     Make exponent only: STR='E+1' or 'E-7'
*
************************************************************************
*
      DATA ICHAR0/0/
      IF(ICHAR0.EQ.0) ICHAR0=ICHAR('0')
      STR=' '
      IE=IFIX(ALOG10(ABS(FLT))+100.01)-100
      STR(1:1)='E'
      JS=1
      IF(IE.LT.0) THEN
         JS=JS+1
         STR(JS:JS)='-'
      END IF
      JE=MIN0(99,IABS(IE))
      IF(JE.GE.10) THEN
         JS=JS+1
         STR(JS:JS)=CHAR(ICHAR0+JE/10)
         JE=MOD(JE,10)
      END IF
      JS=JS+1
      STR(JS:JS)=CHAR(ICHAR0+JE)
*
  100 RETURN
      END
