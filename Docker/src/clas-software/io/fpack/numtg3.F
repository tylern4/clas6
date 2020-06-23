*CMZ :  1.00/00 16/12/94  17.00.00  by  Pavel Binko
*-- Author :    Ruten Gurin   16/12/94
      FUNCTION NUMTG3(IBUF,J,NUM)
*
      INTEGER IBUF(*),J,NUM
*
      LOGICAL STOPFLAG
*
      STOPFLAG = .TRUE.
      NUMTG3 = 1
      I = J+1
      LIM = J+IBUF(J)
1     CONTINUE
         IF( I.GT.LIM ) THEN
            IF( STOPFLAG ) NUMTG3 = 2
            RETURN
         ENDIF
         IF( IBUF(I).EQ.0 ) THEN
            IF( NUM.EQ.IBUF(I+1) ) GOTO 100
            IF( NUM.LT.IBUF(I+1) ) STOPFLAG = .FALSE.
            I = I+2
            GOTO 1
         ELSE IF( IBUF(I).EQ.1 ) THEN
            IF( NUM.GE.IBUF(I+1) ) GOTO 100
            STOPFLAG = .FALSE.
            I = I+2
            GOTO 1
         ELSE IF( IBUF(I).EQ.2 ) THEN
            IF( NUM.LE.IBUF(I+1) ) GOTO 100
            I = I+2
            GOTO 1
         ELSE IF( IBUF(I).EQ.3 ) THEN
            IF( NUM.GE.IBUF(I+1).AND.NUM.LE.IBUF(I+2) ) GOTO 100
            IF( NUM.LE.IBUF(I+2) ) STOPFLAG = .FALSE.
            I = I+3
            GOTO 1
         ELSE
            CALL LOGERROR('Invalid SELECT intervals')
         ENDIF
100   CONTINUE
      NUMTG3 = 0
*
      RETURN
      END
