*CMZ :  1.00/00 16/12/94  17.00.00  by  Pavel Binko
*-- Author :    Zbigniew Szkutnik   16/12/94
      SUBROUTINE KBACKF(CLIENT,STREAM,IOS)
*
      INTEGER CLIENT, STREAM
*
************************************************************************
*
*     An interface to C-routine kbackc which makes a backspace
*     in a remote keyed access file, on return IOS=0 means
*     success, nonzero value indicates an error
*
************************************************************************
*
      INTEGER BCOUNT
      COMMON/KEYSTAT/BCOUNT
      IF(CLIENT.LT.0) THEN
*       network channel is dead, try to recover
        CALL FNRES(CLIENT,STREAM,1,IRET)
        IF(IRET.NE.0) THEN
*         reconnect failed
          IOS=1
          RETURN
        END IF
      END IF
  1   CALL KBACKC(CLIENT,STREAM,IOS)
      IF(IOS.EQ.3) THEN
*       network channel is dead, try to recover
        CALL FNRES(CLIENT,STREAM,1,IRET)
        IF(IRET.EQ.0) THEN
*         reconnect successful
          GO TO 1
        END IF
        IOS=1
      END IF
      BCOUNT = BCOUNT+1
*
      RETURN
      END
