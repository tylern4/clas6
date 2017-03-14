*CMZ :  1.00/00 16/12/94  17.00.00  by  Pavel Binko
*-- Author :    Volker Blobel   16/12/94
      SUBROUTINE PNVG(FLT,STR,JS)
*
      CHARACTER*(*) STR
*
************************************************************************
*
*     The same as PNVF, but for graphics.
*
************************************************************************
*
      CHARACTER*12 TTR
      IF(FLT.EQ.0.0) THEN
         STR='0'
         JS=1
         GOTO 100
      END IF
      CALL PNVF(FLT,STR,JS)
      IE=INDEX(STR(1:JS),'E')
      IF(IE.EQ.0) THEN
         IF(STR(JS:JS).EQ.'.') THEN
            STR(JS:JS)=' '
            JS=JS-1
         END IF
      ELSE
         IF(STR(IE-1:IE-1).EQ.'.') THEN
            NE=1+JS-IE
            TTR(1:NE)=STR(IE:JS)
            STR(IE-1:IE-2+NE)=TTR(1:NE)
            JS=JS-1
         END IF
      END IF
*
  100 RETURN
      END
