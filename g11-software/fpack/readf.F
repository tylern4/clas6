*CMZ :  1.00/00 16/12/94  17.00.00  by  Pavel Binko
*-- Author :    Zbigniew Szkutnik   16/12/94
      SUBROUTINE READF(CLIENT,STREAM,NSKIP,NBYTES,BUFFER,BUFSIZE,NREAD)
*
      INTEGER CLIENT,STREAM,NSKIP,NBYTES,NREAD,BUFSIZE,BUFFER(BUFSIZE)
*
************************************************************************
*
*     An interface to C-routine readc which reads from remote files.
*
************************************************************************
*
      IF(CLIENT.LT.0) THEN
*       Network channel is dead, try to recover
        CALL FNRES(CLIENT,STREAM,1,IRET)
        IF(IRET.NE.0) THEN
*         Reconnect failed
          NREAD=-1
          CALL FPWMSG(1,'READF',
     +              'Network channel is dead and reconnect failed')
          RETURN
        END IF
      END IF
  1   CALL READC(CLIENT,STREAM,NSKIP,NBYTES,BUFFER,NREAD)
      IF(NREAD.EQ.-1) THEN
*        Network channel is dead, try to recover
         CALL FNRES(CLIENT,STREAM,1,IRET)
         IF(IRET.EQ.0) THEN
*          Reconnect successful
           GO TO 1
         END IF
         CALL FPWMSG(2,'READF',
     +               'Network channel is dead and reconnect failed')
      END IF
*
      RETURN
      END
