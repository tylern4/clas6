#ifndef Linux
      subroutine dummy_keyswap
      implicit none
      return
      end
#else /* Linux */
      SUBROUTINE KEYSWAP(KEY1,KEY2)
*
      INTEGER KEY1(5), KEY2(5)
*
************************************************************************
*
*     Copy the record key KEY1 to KEY2 with swapping the
*     integer part. (Routine needed for network access from
*     VAX-type computers to remote keyed access files.)
*
************************************************************************
*
      INTEGER IARR(3)
*
      DO 1 I=1,2
  1   KEY2(I)=KEY1(I)
      DO 2 I=1,3
      IARR(I)=KEY1(2+I)
*KEEP,B08SWP.
         JARR=IARR(I)
         IARR(I)=IOR(IOR(IOR(ISHFT(JARR,24),ISHFT(JARR,-24)),
     +                   ISHFT(IAND(JARR,65280),8)),
     +               ISHFT(IAND(JARR,16711680),-8))
*KEND.
  2   KEY2(2+I)=IARR(I)
*
      RETURN
      END
#endif /* Linux */
