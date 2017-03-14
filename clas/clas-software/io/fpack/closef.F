*CMZ :  1.00/00 16/12/94  17.00.00  by  Pavel Binko
*-- Author :    Zbigniew Szkutnik   16/12/94
      SUBROUTINE CLOSEF(CLIENT,STREAM,RET)
*
      INTEGER CLIENT, STREAM, RET
*
************************************************************************
*
*     An interface to C-routine closec which closes a remote file.
*
************************************************************************
*
      IF(CLIENT.LT.0) THEN
*       network channel is dead
        RET=-1
        RETURN
      END IF
      CALL CLOSEC(CLIENT,STREAM,RET)
      RETURN
      END
