*CMZ :  1.00/00 16/12/94  17.00.00  by  Pavel Binko
*-- Author :    Volker Blobel   16/12/94
      FUNCTION NUMTAG(NUMBER,IAR,NAR)
*
      INTEGER IAR(*)
*
************************************************************************
*
*     Check wether NUMBER is in (special) list IAR(NAR)
*     function =  0   not tagged
*              =  1   tagged (inside)
*              =  2   not tagged low
*              =  3   not tagged high
*              = -1   formal error
*
************************************************************************
*
      INOUT=0
*
*     check overall limits
      IF(IAR(1).EQ.0) THEN
      ELSE IF(IAR(1).EQ.1) THEN
*        lower limit
         IF(NUMBER.LT.IAR(2)) GOTO 60
      ELSE IF(IAR(1).EQ.2) THEN
*        upper limit
         IF(NUMBER.GT.IAR(3)) GOTO 70
      ELSE IF(IAR(1).EQ.3) THEN
*        lower and upper limit
         IF(NUMBER.LT.IAR(2)) GOTO 60
         IF(NUMBER.GT.IAR(3)) GOTO 70
      ELSE
         GOTO 90
      END IF
*
      I=3
*
   10 IF(I.GE.NAR) GOTO 100
*     at least two items
      IF(I+2.GT.NAR) GOTO 90
      IF(IAR(I+1).EQ.0) THEN
*        single number
         IF(NUMBER.EQ.IAR(I+2)) GOTO 80
      ELSE IF(IAR(I+1).EQ.1) THEN
*        lower limit given
         IF(NUMBER.GE.IAR(I+2)) GOTO 80
      ELSE IF(IAR(I+1).EQ.2) THEN
*        upper limit given
         IF(NUMBER.LE.IAR(I+2)) GOTO 80
      ELSE IF(IAR(I+1).EQ.3) THEN
*        upper limit given
         IF(I+3.GT.NAR) GOTO 90
         IF(NUMBER.GE.IAR(I+2).AND.NUMBER.LE.IAR(I+3)) GOTO 80
         I=I+1
      ELSE
         GOTO 90
      END IF
*
      I=I+2
      GOTO 10
*
*     not tagged low
   60 INOUT=2
      GOTO 100
*
*     not tagged high
   70 INOUT=3
      GOTO 100
*
*     number found
   80 INOUT=1
      GOTO 100
*
*     formal error
   90 INOUT=-1
*
  100 NUMTAG=INOUT
*
      RETURN
      END
