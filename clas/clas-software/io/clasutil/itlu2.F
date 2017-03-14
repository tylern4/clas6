CDECK  ID>, ITLU2.
*CMZ :  2.00/00 10/02/91  09.24.57  by  Stephan Egli
*CMZ :  1.04/00 29/04/90  15.24.30  by  Stephan Egli
*-- Author :
      FUNCTION ITLU2(ITEM1,ITEM2,TAB,AUX)
      INTEGER TAB(2,*),AUX(*)
*     search loop in five statements
      KTLU=AUX(1)+MOD(IABS(ITEM1+ITEM2),AUX(2))+1
 2222 KTLU=AUX(KTLU+5)
      IF(KTLU.NE.0) THEN
         IF(ITEM1.NE.TAB(1,KTLU)) GOTO 2222
         IF(ITEM2.NE.TAB(2,KTLU)) GOTO 2222
      END IF
*     end of search loop - result in KTLU
      ITLU2=KTLU
      RETURN
      END
