*CMZ :  1.00/00 16/12/94  17.00.00  by  Pavel Binko
*-- Author :    Volker Blobel   16/12/94
      SUBROUTINE FCLPR(CLASSW)
*
      INTEGER CLASSW
*
************************************************************************
*
*     Print classword
*
************************************************************************
*
      INTEGER MCLASS(30)
*     ...
      ICLASW=CLASSW
      MCL=0
      DO 10 I=1,30
#if !defined(SunOS) && !defined(IRIX)
      IF(IAND(ICLASW,1).NE.0) THEN
#else /* SunOS */
      IF(AND(ICLASW,1).NE.0) THEN
#endif /* SunOS */
         MCL=MCL+1
         MCLASS(MCL)=I
      END IF
#if !defined(SunOS) && !defined(IRIX)
   10 ICLASW=ISHFT(ICLASW,-1)
#else /* SunOS */
   10 ICLASW=RSHIFT(ICLASW,1)
#endif /* SunOS */
*     print
      IF(MCL.EQ.0) THEN
         WRITE(6,101)
      ELSE
         WRITE(*,102)  (MCLASS(I),I=1,MCL)
      END IF
*
  100 RETURN
  101 FORMAT(' classes:  NONE')
  102 FORMAT(' classes: ',30I3)
      END
