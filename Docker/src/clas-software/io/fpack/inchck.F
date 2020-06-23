*CMZ :  1.00/00 16/12/94  17.00.00  by  Pavel Binko
*-- Author :    Volker Blobel   16/12/94
      FUNCTION INCHCK(ITEM,LIST,N)
*
      INTEGER LIST(*)
*
************************************************************************
*
*     Compare ITEM with list of limits ...
*
*     INCHCK = 0   ITEM inside
*     INCHCK = 1   ITEM outside
*     INCHCK = 2   ITEM outside high of all limits
*
*     0 X     means   X        equality
*     1 X     means   X :      lower limit
*     2 Y     means     : Y    upper limit
*     3 X Y   means   X : Y    interval
*
************************************************************************
*
      LOGICAL OUTHI
*
      OUTHI=.TRUE.
      IN=0
      I =0
   10 IF(I.GE.N) GOTO 90
      IF(LIST(I+1).EQ.0) THEN
*        check for equality
         IF(ITEM.EQ.LIST(I+2)) GOTO 100
         IF(ITEM.LT.LIST(I+2)) OUTHI=.FALSE.
      ELSE IF(LIST(I+1).EQ.1) THEN
*        check for lower limit
         OUTHI=.FALSE.
         IF(ITEM.GE.LIST(I+2)) GOTO 100
      ELSE IF(LIST(I+1).EQ.2) THEN
*        check for upper limit
         IF(ITEM.LE.LIST(I+2)) GOTO 100
      ELSE
*        interval
         IF(ITEM.GE.LIST(I+2).AND.ITEM.LE.LIST(I+3)) GOTO 100
         IF(ITEM.LT.LIST(I+2)) OUTHI=.FALSE.
         I=I+1
      END IF
      I=I+2
      GOTO 10
   90 IN=1
      IF(OUTHI) IN=2
  100 INCHCK=IN
*
      RETURN
      END
