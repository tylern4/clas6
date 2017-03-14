*CMZ :  1.00/00 16/12/94  17.00.00  by  Pavel Binko
*-- Author :    Volker Blobel   16/12/94
      SUBROUTINE PNVZ(NUM,STR,JS)
*
      CHARACTER*(*) STR
*
************************************************************************
*
*     Convert integer NUM into character string STR, JS non-blank
*     characters,. JS = 8) in hexadecimal representation
*     Examples:
*     CALL PNVZ( 15,STR,JS)   -> STR='0000000F' ; JS = 8
*     CALL PNVZ(255,STR,JS)   -> STR='000000FF' ; JS = 8
*
************************************************************************
*
      CHARACTER*12 DIG(0:15)*1
      DATA DIG/'0','1','2','3','4','5','6','7','8','9',
     +         'A','B','C','D','E','F'/
      JS=8
      KHEX=NUM
      DO 10 I=0,7
*KEEP,GETHEX.
*     extract rightmost hex number JHEX from KHEX and shift
      JHEX= IAND(KHEX,15)
      KHEX=ISHFT(KHEX,-4)
*KEND.
   10 STR(JS-I:JS-I)=DIG(JHEX)
*
  100 RETURN
      END
