*CMZ :  1.00/00 16/12/94  17.00.00  by  Pavel Binko
*-- Author :    Zbigniew Szkutnik   16/12/94
      SUBROUTINE REWINF(CLIENT,STREAM,RET)
*
      INTEGER CLIENT, STREAM, RET
*
************************************************************************
*
*     An interface to C-routine rewinc which rewinds a remote file.
*
************************************************************************
*
      INTEGER BCOUNT
      COMMON/KEYSTAT/BCOUNT
      IF(CLIENT.LT.0) THEN
*       network channel is dead, try to recover
        CALL FNRES(CLIENT,STREAM,0,RET)
        IF(RET.NE.0) THEN
*         reconnect failed
          RETURN
        END IF
        BCOUNT = 0
        RETURN
      END IF
      CALL REWINC(CLIENT,STREAM,RET)
      IF(RET.EQ.-2) CALL FNRES(CLIENT,STREAM,0,RET)
      IF(RET.EQ.-1) RETURN
      BCOUNT = 0
*
      RETURN
      END
