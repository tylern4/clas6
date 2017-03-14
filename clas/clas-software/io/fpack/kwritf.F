*CMZ :  1.00/00 16/12/94  17.00.00  by  Pavel Binko
*-- Author :    Zbigniew Szkutnik   16/12/94
      SUBROUTINE KWRITF(CLIENT,STREAM,NBYTES,BUFFER,BUFSIZE,IOS)
*
      INTEGER CLIENT,STREAM,NBYTES,BUFSIZE,BUFFER(BUFSIZE)
*
************************************************************************
*
*     An interface to C-routine kwritf which writes a record into
*     a remote keyed access file
*
************************************************************************
*
      INTEGER BCOUNT
      COMMON/KEYSTAT/BCOUNT
      IF(CLIENT.LT.0) THEN
*       network channel is dead, try to recover
        CALL FNRES(CLIENT,STREAM,0,IRET)
        IF(IRET.NE.0) THEN
*         reconnect failed
          IOS=1
          RETURN
        END IF
      END IF
   2  CALL KWRITC(CLIENT,STREAM,NBYTES,BUFFER,IOS)
      IF(IOS.EQ.3) THEN
*       network channel is dead, try to recover
        CALL FNRES(CLIENT,STREAM,0,IRET)
        IF(IRET.EQ.0) THEN
*         reconnect successful
          GO TO 2
        END IF
        IOS=1
      END IF
      BCOUNT = 0
*
      RETURN
      END
