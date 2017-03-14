*CMZ :  1.00/00 16/12/94  17.00.00  by  Pavel Binko
*-- Author :    Volker Blobel   16/12/94
      SUBROUTINE FCLPA(CLASSW,NCLASS)
*
      INTEGER NCLASS(30),CLASSW
*
************************************************************************
*
*     Pack classword from array
*
************************************************************************
*
      ICLASW=0
      DO 10 I=30,1,-1
      IF(NCLASS(I).EQ.0) THEN
#if !defined(SunOS) && !defined(IRIX)
         ICLASW=ISHFT(ICLASW,1)
#else /* SunOS */
         ICLASW=LSHIFT(ICLASW,1)
#endif /* SunOS */
      ELSE
#if !defined(SunOS) && !defined(IRIX)
         ICLASW=IOR(ISHFT(ICLASW,1),1)
#else /* SunOS */
         ICLASW=OR(LSHIFT(ICLASW,1),1)
#endif /* SunOS */
      END IF
   10 CONTINUE
      CLASSW=ICLASW
*
  100 RETURN
      END
