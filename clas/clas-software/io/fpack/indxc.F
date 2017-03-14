*CMZ :  1.00/00 16/12/94  17.00.00  by  Pavel Binko
*-- Author :    Volker Blobel   16/12/94
      FUNCTION INDXC(IAR,IND)
*
      INTEGER IAR(*)
*
************************************************************************
*
*     Return index of next higher ident (w.r.t. index IND)
*        if IND = 0 or negative, index of first bank is returned
*
************************************************************************
*
      INDXC=0
      IF(IND.LE.0) THEN
*        last index 0 or negative - return first index
         IF(IAR(8).EQ.0) GOTO 100
         IAR(4)=8
      ELSE
*        IND is positive
         IDENT=IAR(IND-2)
         IF(IAR(IAR(4)).NE.IDENT) THEN
*           call INDXB to restore IAR(4)
            JND=INDXB(IAR,IDENT)
            IF(JND.EQ.0) GOTO 100
         END IF
      END IF
*
      IF(IAR(4)+2.LE.8+IAR(8)) THEN
         IAR(4)=IAR(4)+2
         INDXC=IAR(IAR(4)-1)
      END IF
*
  100 RETURN
      END
