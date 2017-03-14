#ifndef Linux
*CMZU:  1.00/14 30/11/95  13.27.08  by  Peter Faulkner
#endif /* not Linux */
*CMZ :  1.00/00 16/12/94  17.00.00  by  Pavel Binko
*-- Author :    Volker Blobel   16/12/94
      SUBROUTINE FWSPEC(LUN,NTOT,BUFFER,IEND)
*
#ifdef Linux
      INTEGER BUFFER(NTOT)
*
#endif /* Linux */
************************************************************************
*
*     Special write for one record - dummy version in FPACK library
*
************************************************************************
*
#ifndef Linux
      implicit none
      integer LUN,NTOT,BUFFER,IEND
      dimension BUFFER(NTOT)
 
 
  999 return
      end
#else /* Linux */
  100 RETURN
      END
#endif /* Linux */
