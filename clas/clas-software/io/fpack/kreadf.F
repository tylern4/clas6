*CMZ :  1.00/00 16/12/94  17.00.00  by  Pavel Binko
*-- Author :    Zbigniew Szkutnik   16/12/94
      SUBROUTINE KREADF(CLIENT,STREAM,NBYTES,BUFFER,BUFSIZE,KEYCODE,
     +                  KEY,IRET)
*
      CHARACTER KEY*(*)
      INTEGER CLIENT,STREAM,NBYTES,BUFSIZE,BUFFER(BUFSIZE)
*
************************************************************************
*
*      An interface to C-routine kreadc which reads a record from
*      a remote keyed access file. KEYCODE can be:
*      0 for sequential retrivial
*      1 corresponding to Fortran KEY
*      2 corresponding to Fortran KEYGE
*
*      IRET is a return value with the following meaning:
*      0 success
*      1 EOF found (for sequential retrivial) or error for direct
*      2 key not found (for direct) or error for sequential
*      3 network problem
*
************************************************************************
*
      INTEGER BCOUNT
      COMMON/KEYSTAT/BCOUNT
      INTEGER*2 STRLEN
      IF(CLIENT.LT.0) THEN
*       network channel is dead, try to recover
        IF(KEYCODE.GT.0) THEN
           CALL FNRES(CLIENT,STREAM,0,IRET)
        ELSE
           CALL FNRES(CLIENT,STREAM,1,IRET)
        END IF
        IF(IRET.NE.0) THEN
*         reconnect failed
          IRET=3
          RETURN
        END IF
      END IF
      STRLEN=LEN(KEY)
  1   CALL KREADC(CLIENT,STREAM,NBYTES,BUFFER,KEYCODE,KEY,IRET,STRLEN)
      IF(IRET.EQ.3) THEN
*       network channel is dead, try to recover
        IF(KEYCODE.GT.0) THEN
           CALL FNRES(CLIENT,STREAM,0,IRET)
        ELSE
           CALL FNRES(CLIENT,STREAM,1,IRET)
        END IF
        IF(IRET.EQ.0) THEN
*         reconnect successful
          GO TO 1
        END IF
        IRET=3
      END IF
      BCOUNT = 0
*
      RETURN
      END
