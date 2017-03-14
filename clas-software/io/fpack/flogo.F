*CMZ :  1.00/00 16/12/94  17.00.00  by  Pavel Binko
*-- Author :    Volker Blobel   16/12/94
      SUBROUTINE FLOGO
*
************************************************************************
*
*     Print LOGO
*
************************************************************************
*
      CHARACTER*8 VERSQQ
      LOGICAL START
      DATA START/.TRUE./
*
      IF(START) THEN
         START=.FALSE.
*KEEP,VERSQQ.
#ifndef Linux
      VERSQQ = ' 1.00/16'
      IVERSQ =  10016
#else /* Linux */
      VERSQQ = ' 1.00/08'
      IVERSQ =  10008
#endif /* Linux */
*KEND.
         WRITE(*,101) VERSQQ
      END IF
*
  101 FORMAT(//
     +' OOOOOOOOOOO          OOOOOOOOO       OOOOO    ',
     +'    OOOOO    OOOO   OOOO'/
     +' OOOOOOOOOOO          OOOOOOOOOOO    OOOOOOO   ',
     +'  OOOOOOOOO  OOOO   OOOO'/
     +'  OOO                  OOO    OOO   OOO   OOO  ',
     +' OOOO    OOO  OOO  OOO  '/
     +'  OOO                  OOO    OOO  OOO     OOO ',
     +' OOO          OOO OOO   '/
     +'  OOOOOOOO    OOOOO    OOOOOOOOOO  OOOOOOOOOOO ',
     +' OOO          OOOOOO    '/
     +'  OOOOOOOO    OOOOO    OOOOOOOO    OOOOOOOOOOO ',
     +' OOO          OOOOOO    '/
     +'  OOO                  OOO         OOO     OOO ',
     +' OOO          OOO OOO   '/
     +'  OOO                  OOO         OOO     OOO ',
     +' OOOO    OOO  OOO  OOO  '/
     +' OOOOO                OOOOO       OOOOO   OOOOO',
     +'  OOOOOOOOO  OOOO   OOOO'/
     +' OOOOO                OOOOO       OOOOO   OOOOO',
     +'    OOOOO    OOOO   OOOO'///
     +'    Version ',A8,'  operating ...'/)
*
  100 RETURN
      END
