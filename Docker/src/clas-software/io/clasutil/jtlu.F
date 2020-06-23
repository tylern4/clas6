CDECK  ID>, JTLU.
*CMZ :  2.00/00 10/02/91  09.24.57  by  Stephan Egli
*CMZ :  1.04/00 29/04/90  15.24.30  by  Stephan Egli
*-- Author :
      FUNCTION JTLU(ITEM,TAB,AUX)
      INTEGER TAB(*),AUX(*)
      J=AUX(1)+MOD(IABS(ITEM),AUX(2))+1
   10 I=J
      J=AUX(I+5)
      IF(J.NE.0) THEN
*        repeat if not equal
         IF(ITEM.NE.TAB(J)) GOTO 10
      ELSE
*        not in list - add new antry in table TAB
         IF(AUX(1).EQ.AUX(3)) GOTO 100
         J=AUX(3)+1
         AUX(3)=J
         TAB(J)=ITEM
*        insert pointer
         AUX(I+5)=J
      END IF
  100 JTLU=J
      RETURN
      END
