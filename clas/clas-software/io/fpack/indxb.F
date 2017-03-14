*CMZ :  1.00/00 16/12/94  17.00.00  by  Pavel Binko
*-- Author :    Volker Blobel   16/12/94
      FUNCTION INDXB(IAR,IDENT)
*
      INTEGER IAR(*)
*
************************************************************************
*
*     Return index of bank for identifier IDENT
*     If IDENT is not found, returned value is zero.
*     Internal index is stored in IAR(4); negative if not found.
*
************************************************************************
*
*     reset first eight words after initialization
      IF(IAR(2).EQ.0) THEN
         IF(IAR(1).LE.200) STOP 'INDXA array short'
         IAR(2)=200
         IAR(3)=0
         IAR(4)=0
*        create system bank with 2*96 words
         IAR(5)=192
         IAR(6)=0
         IAR(7)=0
*        ... with zero data words
         IAR(8)=0
*        system bank will contain pairs: IDENT/INDEX
      END IF
*     find IDENT in table of IND/IDENT pairs
      IF(IAR(8).EQ.0) THEN
*        no entry in table yet
         I=8
      ELSE IF(IDENT.LE.IAR(10)) THEN
         IF(IDENT.EQ.IAR(10)) THEN
            I=10
            INDXB=IAR(I-1)
            GOTO 100
         END IF
*        below first element
         I=8
      ELSE IF(IDENT.GE.IAR(8+IAR(8))) THEN
         I=8+IAR(8)
         IF(IDENT.EQ.IAR(8+IAR(8))) THEN
*           last element found
            INDXB=IAR(I-1)
            GOTO 100
         END IF
*        above last element
      ELSE
*        binary search
         IA=10
         IB=8+IAR(8)
         I =((IA+IB)/4)*2
*        test index I
   10    IF(IDENT.EQ.IAR(I)) THEN
*           found, convert to bank index
            INDXB=IAR(I-1)
            GOTO 100
         END IF
         IF(IDENT.GT.IAR(I)) THEN
            IA=I
         ELSE
            IB=I
         END IF
*        new center value ...
         I=((IA+IB)/4)*2
*        ... finished?
         IF(I.NE.IA) GOTO 10
      END IF
      I=-I-2
      INDXB=0
  100 IAR(4)=I
*
      RETURN
      END
