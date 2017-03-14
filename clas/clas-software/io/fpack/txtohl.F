*CMZ :  1.00/00 16/12/94  17.00.00  by  Pavel Binko
*-- Author :    Volker Blobel   16/12/94
      SUBROUTINE TXTOHL(FMT,IHL,NHL)
*
      CHARACTER*(*) FMT
      INTEGER IHL(*)
*
************************************************************************
*
*     Copy character text to hollerith text (integer, 4 char per word)
*
************************************************************************
*
      CHARACTER CHA*4
*
      L=LEN(FMT)
      NHL=(L+3)/4
      DO 10 I=1,NHL
      CHA=FMT(4*I-3:MIN0(L,4*I))
*KEEP,ICHCHA.
*     get hollerith 4H from char*4
#ifndef Linux
      ICH=IOR(ISHFT(ICHAR(CHA(1:1)),24),IOR(ISHFT(ICHAR(CHA(2:2)),16),
     +    IOR(ISHFT(ICHAR(CHA(3:3)), 8),ICHAR(          CHA(4:4)))))
#else /* Linux */
      ICH=IOR(ISHFT(ICHAR(CHA(4:4)),24),IOR(ISHFT(ICHAR(CHA(3:3)),16),
     +    IOR(ISHFT(ICHAR(CHA(2:2)), 8),ICHAR(          CHA(1:1)))))
#endif /* Linux */
*KEND.
   10 IHL(I)=ICH
*
  100 RETURN
      END
