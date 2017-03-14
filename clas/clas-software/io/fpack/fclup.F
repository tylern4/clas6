*CMZ :  1.00/00 16/12/94  17.00.00  by  Pavel Binko
*-- Author :    Volker Blobel   16/12/94
      SUBROUTINE FCLUP(CLASSW,NCLASS)
*
      INTEGER NCLASS(30),CLASSW
*
************************************************************************
*
*     Unpack classword in to array
*
************************************************************************
*
      IF(CLASSW.EQ.0) GOTO 20
      ICLASW=CLASSW
      DO 10 I=1,30
#if !defined(SunOS) && !defined(IRIX)
      IF(IAND(ICLASW,1).EQ.0) THEN
#else /* SunOS */
      IF(AND(ICLASW,1).EQ.0) THEN
#endif /* SunOS */
         NCLASS(I)=0
      ELSE
         NCLASS(I)=1
      END IF
#if !defined(SunOS) && !defined(IRIX)
   10 ICLASW=ISHFT(ICLASW,-1)
#else /* SunOS */
   10 ICLASW=RSHIFT(ICLASW,1)
#endif /* SunOS */
      GOTO 100
   20 DO 30 I=1,30
   30 NCLASS(I)=0
  100 RETURN
      END
