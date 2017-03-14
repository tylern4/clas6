CDECK  ID>, JTLU2.
*CMZ :  2.00/00 10/02/91  09.24.58  by  Stephan Egli
*CMZ :  1.04/00 29/04/90  15.24.30  by  Stephan Egli
*-- Author :
      FUNCTION JTLU2(ITEM1,ITEM2,TAB,AUX)
      INTEGER TAB(2,*),AUX(*)
      J=AUX(1)+MOD(IABS(ITEM1+ITEM2),AUX(2))+1
   10 I=J
      J=AUX(I+5)
      IF(J.NE.0) THEN
*        repeat if not equal
         IF(ITEM1.NE.TAB(1,J)) GOTO 10
         IF(ITEM2.NE.TAB(2,J)) GOTO 10
      ELSE
*        not in list - add new antry in table TAB
         IF(AUX(1).EQ.AUX(3)) GOTO 100
         J=AUX(3)+1
         AUX(3)=J
         TAB(1,J)=ITEM1
         TAB(2,J)=ITEM2
*        insert pointer
         AUX(I+5)=J
      END IF
  100 JTLU2=J
      RETURN
      END
