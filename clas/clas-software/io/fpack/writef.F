*CMZ :  1.00/00 16/12/94  17.00.00  by  Pavel Binko
*-- Author :    Zbigniew Szkutnik   16/12/94
      SUBROUTINE WRITEF(CLIENT,STREAM,IBYT,NBYTES,BUFFER,BUFSIZE,RET)
*
      INTEGER CLIENT,STREAM,NBYTES,BUFSIZE,BUFFER(BUFSIZE),RET
*
************************************************************************
*
*     An interface to C-routine writec which writes to a remote file
*
************************************************************************
*
      IF(CLIENT.LT.0) THEN
*       network channel is dead, try to recover
        CALL FNRES(CLIENT,STREAM,1,IRET)
        IF(IRET.NE.0) THEN
*         reconnect failed
          RET=-1
          CALL FPWMSG(1,'WRITEF',
     +              'Network channel is dead and reconnect failed')
          RETURN
        END IF
      END IF
  1   CALL WRITEC(CLIENT,STREAM,IBYT,NBYTES,BUFFER,RET)
      IF(RET.EQ.-2) THEN
*        network channel is dead, try to recover
         CALL FNRES(CLIENT,STREAM,1,IRET)
         IF(IRET.EQ.0) THEN
*          reconnect successful
           GO TO 1
         END IF
         RET=-1
         CALL FPWMSG(2,'WRITEF',
     +              'Network channel is dead and reconnect failed')
      END IF
*
      RETURN
      END
