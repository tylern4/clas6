*CMZ :  1.00/00 16/12/94  17.00.00  by  Pavel Binko
*-- Author :    Zbigniew Szkutnik   16/12/94
      SUBROUTINE KREWF(CLIENT,STREAM,NBYTES,BUFFER,BUFSIZE,IOS)
*
      INTEGER CLIENT,STREAM,NBYTES,BUFSIZE,BUFFER(BUFSIZE)
*
************************************************************************
*
*     An interface to C-routine krewc which rewrites the last read
*     record in a remote keyed access file
*
************************************************************************
*
      IF(CLIENT.LT.0) THEN
*       network channel is dead, try to recover
        CALL FNRES(CLIENT,STREAM,1,IRET)
        IF(IRET.NE.0) THEN
*         reconnect failed
          IOS=1
          RETURN
        END IF
      END IF
   2  CALL KREWC(CLIENT,STREAM,NBYTES,BUFFER,IOS)
      IF(IOS.EQ.3) THEN
*       network channel is dead, try to recover
        CALL FNRES(CLIENT,STREAM,1,IRET)
        IF(IRET.EQ.0) THEN
*         reconnect successful
          GO TO 2
        END IF
        IOS=1
      END IF
*
      RETURN
      END
