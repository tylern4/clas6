*CMZ :  1.00/00 16/12/94  17.00.00  by  Pavel Binko
*-- Author :    Volker Blobel   16/12/94
      SUBROUTINE HLTOTX(IHL,NHL,FMT,NCH)
*
      CHARACTER*(*) FMT
      INTEGER IHL(*)
*
************************************************************************
*
*     Copy hollerith text (integer, 4 char per word) to character text
*
************************************************************************
*
      CHARACTER CHA*4
*
      L=LEN(FMT)
      IF(NHL.LE.0) THEN
         FMT(1:1)=' '
         NCH=1
         GOTO 100
      END IF
      NCH=MIN0(L,4*NHL)
      DO 10 I=1,NHL
      ICH=IHL(I)
*KEEP,CHAICH.
*     get char*4 from hollerith 4H
#ifndef Linux
      CHA=CHAR(IAND(ISHFT(ICH,-24),255))//CHAR(IAND(ISHFT(ICH,-16),255))
     +  //CHAR(IAND(ISHFT(ICH,- 8),255))//CHAR(IAND      (ICH     ,255))
#else /* Linux */
      CHA=CHAR(IAND      (ICH     ,255))//CHAR(IAND(ISHFT(ICH,- 8),255))
     +  //CHAR(IAND(ISHFT(ICH,-16),255))//CHAR(IAND(ISHFT(ICH,-24),255))
#endif /* Linux */
*KEND.
      IF(4*I-3.GT.L) GOTO 100
   10 FMT(4*I-3:MIN0(L,4*I))=CHA
      IF(FMT(NCH:NCH).EQ.' ') THEN
         NCH=NCH-1
         IF(FMT(NCH:NCH).EQ.' ') THEN
            NCH=NCH-1
            IF(FMT(NCH:NCH).EQ.' ') THEN
               NCH=NCH-1
            END IF
         END IF
      END IF
*
  100 RETURN
      END
