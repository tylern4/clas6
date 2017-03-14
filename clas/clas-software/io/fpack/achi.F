*CMZ :  1.00/00 16/12/94  17.00.00  by  Pavel Binko
*-- Author :    Volker Blobel   16/12/94
      CHARACTER*4 FUNCTION ACHI(ICH)
*
************************************************************************
*
*     Function returns CHAracter from Integer
*
************************************************************************
*
      CHARACTER*4 CHA
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
      ACHI=CHA
*
      RETURN
      END
