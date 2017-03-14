*CMZ :  1.00/00 16/12/94  17.00.00  by  Pavel Binko
*-- Author :    Volker Blobel   16/12/94
      SUBROUTINE UCASE(TEXT)
*
      CHARACTER*(*) TEXT
*
************************************************************************
*
*     convert text to upper case characters
*
************************************************************************
*
      LOGICAL CURR
      CHARACTER*26 CHU
      CHARACTER*26 CHL
      INTEGER NJ(0:255)
      DATA  CHU/'ABCDEFGHIJKLMNOPQRSTUVWXYZ'/
      DATA  CHL/'abcdefghijklmnopqrstuvwxyz'/
      DATA  CURR/.FALSE./
      DATA    NJ/256*0/
*
      IF(CURR) GOTO 20
      CURR=.TRUE.
      DO 10 I=1,26
      J=ICHAR(CHL(I:I))
   10 NJ(J)=I
   20 DO 30 I=1,LEN(TEXT)
      IF(TEXT(I:I).EQ.' ') GOTO 30
      J=ICHAR(TEXT(I:I))
      IF(NJ(J).NE.0) TEXT(I:I)=CHU(NJ(J):NJ(J))
   30 CONTINUE
*
  100 RETURN
      END
