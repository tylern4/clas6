*CMZ :  1.00/00 16/12/94  17.00.00  by  Pavel Binko
*-- Author :    Volker Blobel   16/12/94
      INTEGER FUNCTION ICHA(TEXT)
*
      CHARACTER*(*) TEXT
*
************************************************************************
*
*     Function returns Integer from CHAracter
*
************************************************************************
*
      CHARACTER CHA*4
*
      CHA=TEXT
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
      ICHA=ICH
*
      RETURN
      END
