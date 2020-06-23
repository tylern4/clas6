*CMZ :  1.00/00 16/12/94  17.00.00  by  Pavel Binko
*-- Author :    Ruten Gurin   16/12/94
      SUBROUTINE FPKENC(ISRC,IDST)
*
      INTEGER ISRC(2),IDST(2)
*
************************************************************************
*
*     ISRC keeps the string in local code with the leftmost char
*     placed in leftmost byte of the word.
*     We convert it to ASCII string with the leftmost char
*     placed in most significant byte of the word.
*
************************************************************************
*
      INTEGER C1
*
#ifdef Linux
      C1(II,N) = ISHFT(IAND(ISHFT(II,-N),255),24-N)
#endif /* Linux */
      DO 1 I=1,2
         ICH = ISRC(I)
#ifdef Linux
         ICH=IOR(C1(ICH,24),IOR(C1(ICH,16),IOR(C1(ICH,8),C1(ICH,0))))
#endif /* Linux */
         IDST(I) = ICH
 1    CONTINUE
      END
