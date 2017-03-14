CDECK  ID>, ITLU.
*CMZ :  2.00/00 10/02/91  09.24.57  by  Stephan Egli
*CMZ :  1.04/00 29/04/90  15.24.30  by  Stephan Egli
*-- Author :
      FUNCTION ITLU(ITEM,TAB,AUX)
      INTEGER TAB(*),AUX(*)
*     search loop in five statements
      KTLU=AUX(1)+MOD(IABS(ITEM),AUX(2))+1
 1111 KTLU=AUX(KTLU+5)
      IF(KTLU.NE.0) THEN
         IF(ITEM.NE.TAB(KTLU)) GOTO 1111
      END IF
*     end of search loop - result in KTLU
      ITLU=KTLU
      RETURN
      END
