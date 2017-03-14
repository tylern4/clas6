*CMZ :  1.00/00 16/12/94  17.00.00  by  Pavel Binko
*-- Author :    Zbigniew Szkutnik   16/12/94
      SUBROUTINE OPENF(CLIENT,STREAM,HOST,NAME,ACTION,RECL,SPUN,NR1,NR2)
*
      INTEGER CLIENT, STREAM, RECL
      CHARACTER*(*) NAME, HOST, ACTION, SPUN
*
************************************************************************
*
*     An interface to C-routine openc which opens remote files
*
************************************************************************
*
      CHARACTER LENREC*12, OPTIONS*127
      COMMON/LOCKSTAT/LOCKFLAG
      INTEGER*2 STRLEN1,STRLEN2,STRLEN3
*
      LOCKFLAG=0
      ISPC=1
      IF(SPUN.EQ.'CYL') ISPC=2
      IF(SPUN.EQ.'REC') ISPC=RECL
      CALL PNVI(RECL,LENREC,JS)
      IF(ACTION.EQ.'READ') THEN
        OPTIONS='rb'
      ELSE
        IF(ACTION.EQ.'WRITE') THEN
          OPTIONS='wb,recfm=f,lrecl='//LENREC(1:JS)
        ELSE
          IF(ACTION.EQ.'READWRITE') THEN
            OPTIONS='a+b,recfm=f,lrecl='//LENREC(1:JS)
          ELSE
            IF(ACTION.EQ.'MODIFY') THEN
              OPTIONS='r+b'
            ELSE
              WRITE(6,101)
 101          FORMAT(5X,33H Improper specification of ACTION)
              RETURN
            END IF
          END IF
        END IF
      END IF
      STRLEN1=LENB(NAME)
      STRLEN2=LENB(HOST)
      STRLEN3=LENB(OPTIONS)
      CALL OPENC(CLIENT,STREAM,NAME,HOST,OPTIONS,ISPC,NR1,NR2,STRLEN1,
     +           STRLEN2,STRLEN3)
*
      RETURN
      END
