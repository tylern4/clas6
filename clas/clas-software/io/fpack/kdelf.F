*CMZ :  1.00/00 16/12/94  17.00.00  by  Pavel Binko
*-- Author :    Zbigniew Szkutnik   16/12/94
      SUBROUTINE KDELF(CLIENT,STREAM,IOS)
*
      INTEGER CLIENT, STREAM
*
************************************************************************
*
*     An interface to C-routine kdelc which deletes a last read record
*
************************************************************************
*
* from a remote keyed access file
      IF(CLIENT.LT.0) THEN
*       network channel is dead, try to recover
        CALL FNRES(CLIENT,STREAM,1,IRET)
        IF(IRET.NE.0) THEN
*         reconnect failed
          IOS=1
          RETURN
        END IF
      END IF
   1  CALL KDELC(CLIENT,STREAM,IOS)
      IF(IOS.EQ.2) THEN
*       network channel is dead, try to recover
        CALL FNRES(CLIENT,STREAM,1,IRET)
        IF(IRET.EQ.0) THEN
*         reconnect successful
          GO TO 1
        END IF
        IOS=1
      END IF
*
      RETURN
      END
