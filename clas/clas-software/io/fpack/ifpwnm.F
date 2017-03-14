#ifndef Linux
*CMZU:  1.00/13 03/05/95  15.33.42  by  Unknown
#endif /* not Linux */
*CMZ :  1.00/00 16/12/94  17.00.00  by  Pavel Binko
*-- Author :    Ruten Gurin   16/12/94
      FUNCTION IFPWNM(FROM)
*
      CHARACTER*(*) FROM
*
      CHARACTER  ALPHA*26, DIGITS*10
      DATA ALPHA, DIGITS / 'ABCDEFGHIJKLMNOPQRSTUVWXYZ', '0123456789' /
      SAVE ALPHA, DIGITS
*
      IFPWNM = -1
      LL = LENB(FROM)
      IF(LL.LE.0) RETURN
*
      IF( LL.EQ.8 ) THEN
         IF(FROM(1:1).EQ.'C' .AND.
     +      INDEX(DIGITS,FROM(2:2)).NE.0 .AND.
     +      INDEX(DIGITS,FROM(3:3)).NE.0 .AND.
     +      INDEX(DIGITS,FROM(4:4)).NE.0 .AND.
     +      INDEX(DIGITS,FROM(5:5)).NE.0 .AND.
     +      INDEX(DIGITS,FROM(6:6)).NE.0 .AND.
     +      INDEX(DIGITS,FROM(7:7)).NE.0 .AND.
     +      INDEX(DIGITS,FROM(8:8)).NE.0)
     +         IFPWNM = 10000000 +
     +                   1000000*(INDEX(DIGITS,FROM(2:2)) - 1) +
     +                    100000*(INDEX(DIGITS,FROM(3:3)) - 1) +
     +                     10000*(INDEX(DIGITS,FROM(4:4)) - 1) +
     +                      1000*(INDEX(DIGITS,FROM(5:5)) - 1) +
     +                       100*(INDEX(DIGITS,FROM(6:6)) - 1) +
     +                        10*(INDEX(DIGITS,FROM(7:7)) - 1) +
     +                            INDEX(DIGITS,FROM(8:8)) - 1
      ELSE IF( LL.EQ.3 ) THEN
         IF(INDEX(ALPHA, FROM(1:1)).NE.0 .AND.
     +      INDEX(DIGITS,FROM(2:2)).NE.0 .AND.
     +      INDEX(DIGITS,FROM(3:3)).NE.0)
     +         IFPWNM = 100*(INDEX(ALPHA, FROM(1:1)) - 1) +
     +                   10*(INDEX(DIGITS,FROM(2:2)) - 1) +
     +                       INDEX(DIGITS,FROM(3:3)) - 1
      ELSE
         IFPWNM = -1
      ENDIF
*
      RETURN
      END
