#ifndef Linux
*CMZU:  1.00/14 30/11/95  13.25.49  by  Peter Faulkner
#endif /* not Linux */
*CMZ :  1.00/00 16/12/94  17.00.00  by  Pavel Binko
*-- Author :    Volker Blobel   16/12/94
      SUBROUTINE FRSPEC(LUN,NSKIP,IRC,NTOT,BUFFER,IEND)
*
#ifdef Linux
      INTEGER BUFFER(NTOT)
*
#endif /* Linux */
************************************************************************
*
*     Special read for one record - dummy version in F-PACK library
*     for sequential: skip NSKIP records before
*     for direct:     read record number IRC
*
************************************************************************
*
#ifndef Linux
      implicit none
      integer LUN,NSKIP,IRC,NTOT,BUFFER,IEND
      dimension BUFFER(NTOT)
 
 
  999 return
      end
#else /* Linux */
  100 RETURN
      END
#endif /* Linux */
