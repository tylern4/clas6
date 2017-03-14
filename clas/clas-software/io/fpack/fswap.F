#ifndef Linux
*CMZ :  1.00/12 11/02/96  18.06.43  by  Wiesiek Dlugosz
#endif /* not Linux */
*CMZ :  1.00/00 16/12/94  17.00.00  by  Pavel Binko
*-- Author :    Volker Blobel   16/12/94
      SUBROUTINE FSWAP(IREC,NC)
*
      INTEGER IREC(*)
*
************************************************************************
*
*     byte swapping for array IREC(1)...IREC(NC), if NC > 0
*     if NC = 0, taken from word 2 (after conversion)
*     IREC(3) is the byte swapping controlword
*
************************************************************************
*
      PARAMETER (NB1234= 1+256*(2+256*(3+256*4)) )
      PARAMETER (NB3412= 3+256*(4+256*(1+256*2)) )
      PARAMETER (NB4321= 4+256*(3+256*(2+256*1)) )
*     byte masks
      PARAMETER (MASK2 = 255*256)
      PARAMETER (MASK3 = 255*256*256)
*
      IF(IREC(3).EQ.NB1234) THEN
*        no conversion necessary
      ELSE IF(IREC(3).EQ.NB3412) THEN
*        convert first two words
         IREC(1)=IOR(ISHFT(IREC(1),16),ISHFT(IREC(1),-16))
         IREC(2)=IOR(ISHFT(IREC(2),16),ISHFT(IREC(2),-16))
         IF(NC.EQ.0) THEN
            NUSEDW =IREC(2)
         ELSE
            NUSEDW =NC
         END IF
*        convert remaining words
         DO 10 I=3,NUSEDW
   10    IREC(I)=IOR(ISHFT(IREC(I),16),ISHFT(IREC(I),-16))
      ELSE IF(IREC(3).EQ.NB4321) THEN
*        convert first two words
         IREC(1)=
     +    IOR(  IOR(ISHFT(IREC(1),24),IAND(ISHFT(IREC(1),08),MASK3)),
     +          IOR(IAND(ISHFT(IREC(1),-8),MASK2),ISHFT(IREC(1),-24)))
         IREC(2)=
     +    IOR(  IOR(ISHFT(IREC(2),24),IAND(ISHFT(IREC(2),08),MASK3)),
     +          IOR(IAND(ISHFT(IREC(2),-8),MASK2),ISHFT(IREC(2),-24)))
         IF(NC.EQ.0) THEN
            NUSEDW =IREC(2)
         ELSE
            NUSEDW =NC
         END IF
*        convert remaining words
         DO 20 I=3,NUSEDW
   20    IREC(I)=
     +    IOR(  IOR(ISHFT(IREC(I),24),IAND(ISHFT(IREC(I),08),MASK3)),
     +          IOR(IAND(ISHFT(IREC(I),-8),MASK2),ISHFT(IREC(I),-24)))
      END IF
*
  100 RETURN
      END
