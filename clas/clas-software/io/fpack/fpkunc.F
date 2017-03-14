#ifdef HPUX
*CMZ :  1.00/00 16/12/94  17.00.00  by  Pavel Binko
*-- Author :    Ruten Gurin   16/12/94
      SUBROUTINE FPKUNC(ISRC,IDST,CDST)
*
      INTEGER ISRC(2),IDST(2)
      CHARACTER*(*) CDST
*
************************************************************************
*
*     ISRC keeps ASCII string with the leftmost char
*     placed in most significant byte of the word.
*     We convert it to char CDST and integer IDST,
*     putting leftmost char to leftmost byte of the word.
*
************************************************************************
*
      CHARACTER CHA*4, CWRK*8, C1*1
*
      C1(II,N) = CHAR(IAND(ISHFT(II,-N),255))
      DO 1 I=1,2
         ICH = ISRC(I)
         CHA=C1(ICH,24)//C1(ICH,16)//C1(ICH,8)//C1(ICH,0)
         IDST(I) = ICH
         CWRK(I*4-3:I*4) = CHA
 1    CONTINUE
      CDST = CWRK
      END
#elif defined Linux
*CMZ :  1.00/00 16/12/94  17.00.00  by  Pavel Binko
*-- Author :    Ruten Gurin   16/12/94
      SUBROUTINE FPKUNC(ISRC,IDST,CDST)
*
      INTEGER ISRC(2),IDST(2)
      CHARACTER*(*) CDST
*
************************************************************************
*
*     ISRC keeps ASCII string with the leftmost char
*     placed in most significant byte of the word.
*     We convert it to char CDST and integer IDST,
*     putting leftmost char to leftmost byte of the word.
*
************************************************************************
*
      CHARACTER CHA*4, CWRK*8, C1*1
*
      C1(II,N) = CHAR(IAND(ISHFT(II,-N),255))
      DO 1 I=1,2
         ICH = ISRC(I)
         CHA=C1(ICH,24)//C1(ICH,16)//C1(ICH,8)//C1(ICH,0)
*KEEP,ICHCHA.
*     get hollerith 4H from char*4
      ICH=IOR(ISHFT(ICHAR(CHA(4:4)),24),IOR(ISHFT(ICHAR(CHA(3:3)),16),
     +    IOR(ISHFT(ICHAR(CHA(2:2)), 8),ICHAR(          CHA(1:1)))))
*KEND.
         IDST(I) = ICH
         CWRK(I*4-3:I*4) = CHA
 1    CONTINUE
      CDST = CWRK
      END
#elif defined SunOS
*CMZ :  1.00/00 16/12/94  17.00.00  by  Pavel Binko
*-- Author :    Ruten Gurin   16/12/94
      SUBROUTINE FPKUNC(ISRC,IDST,CDST)
*
      INTEGER ISRC(2),IDST(2)
      CHARACTER*(*) CDST
*
************************************************************************
*
*     ISRC keeps ASCII string with the leftmost char
*     placed in most significant byte of the word.
*     We convert it to char CDST and integer IDST,
*     putting leftmost char to leftmost byte of the word.
*
************************************************************************
*
      CHARACTER CHA*4, CWRK*8, C1*1
*
      C1(II,N) = CHAR(AND(RSHIFT(II,N),255))
      DO 1 I=1,2
         ICH = ISRC(I)
         CHA=C1(ICH,24)//C1(ICH,16)//C1(ICH,8)//C1(ICH,0)
         IDST(I) = ICH
         CWRK(I*4-3:I*4) = CHA
 1    CONTINUE
      CDST = CWRK
      END
#else
*CMZ :  1.00/00 16/12/94  17.00.00  by  Pavel Binko
*-- Author :    Ruten Gurin   16/12/94
      SUBROUTINE FPKUNC(ISRC,IDST,CDST)
*
      INTEGER ISRC(2),IDST(2)
      CHARACTER*(*) CDST
*
************************************************************************
*
*     ISRC keeps ASCII string with the leftmost char
*     placed in most significant byte of the word.
*     We convert it to char CDST and integer IDST,
*     putting leftmost char to leftmost byte of the word.
*
************************************************************************
*
      CHARACTER CHA*4, CWRK*8, C1*1
*
      C1(II,N) = CHAR(IAND(ISHFT(II,-N),255))
      DO 1 I=1,2
         ICH = ISRC(I)
         CHA=C1(ICH,24)//C1(ICH,16)//C1(ICH,8)//C1(ICH,0)
         IDST(I) = ICH
         CWRK(I*4-3:I*4) = CHA
 1    CONTINUE
      CDST = CWRK
      END
#endif
