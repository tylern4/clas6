*CMZ :  1.00/00 16/12/94  17.00.00  by  Pavel Binko
*-- Author :    Pavel Binko   16/12/94
      FUNCTION LENB(TEXT)
*
      CHARACTER*(*) TEXT
*
************************************************************************
*
*     LENB(TEXT) = length of non-blank text
*
************************************************************************
*
      IF(TEXT.EQ.' ') THEN
         LENB=0
      ELSE
         LE=LEN(TEXT)
   10    IF(LE.GT.4) THEN
            IF(TEXT(LE/2:LE).EQ.' ') THEN
               LE=LE/2-1
               GOTO 10
            END IF
         END IF
         DO 20 I=LE,1,-1
         IF(TEXT(I:I).NE.' ') GOTO 30
   20    CONTINUE
   30    LENB=I
      END IF
*
  100 RETURN
      END
