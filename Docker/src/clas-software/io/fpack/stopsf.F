*CMZ :  1.00/00 16/12/94  17.00.00  by  Pavel Binko
*-- Author :    Zbigniew Szkutnik   16/12/94
      SUBROUTINE STOPSF(CLIENT,RET)
*     An interface to C-routine stopsc which stops a remote server.
      INTEGER CLIENT,RET
      IF(CLIENT.LT.0) THEN
*       network channel is dead
        RET=-1
        RETURN
      END IF
      CALL STOPSC(CLIENT,RET)
*
      RETURN
      END
