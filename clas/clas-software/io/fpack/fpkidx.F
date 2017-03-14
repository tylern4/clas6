*CMZ :  1.00/00 16/12/94  17.00.00  by  Pavel Binko
*-- Author :    Pavel Binko   16/12/94
      SUBROUTINE FPKIDX(IARR)
*
      INTEGER IARR(*)
*
      CHARACTER CK*8
      INTEGER IA(2)
*
      CALL FPKUNC(IARR(1),IA,CK)
      WRITE(*,*) '      ',CK,IARR(3),IARR(4)
*
      RETURN
      END
