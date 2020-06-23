*CMZ :  1.00/00 16/12/94  17.00.00  by  Pavel Binko
*-- Author :    Volker Blobel   16/12/94
      SUBROUTINE TEXTD(STR)
*
      CHARACTER STR*(*)
*
************************************************************************
*
*     Text output in text form
*     STR = ' ' means writeout and clear buffer
*
************************************************************************
*
*     character buffer
      CHARACTER STRING*255
*KEEP,CHASTR.
      COMMON/CHASTR/STRING
      SAVE/CHASTR/
*KEEP,NUMSTR.
      COMMON/NUMSTR/LUNST,LSTT,LST,NST,IAST
*     LUNST = unit; LST = length of string; NST = actual string length
*     IAST  = index of start character;     LSTT=length of string write
      SAVE/NUMSTR/
*KEND.
*
      IF(STR.EQ.' ') THEN
         IF(NST.GT.IAST) THEN
            WRITE(LUNST,101) STRING(1:LSTT)
         END IF
         STRING=' '
         NST=IAST-1
      ELSE
         JS=LEN(STR)
         IF(NST.LE.0) THEN
            STRING=' '
            NST=IAST-1
         ELSE IF(NST+JS+3.GT.LST) THEN
            WRITE(LUNST,101) STRING(1:LSTT)
            STRING=' '
            NST=IAST-1
         END IF
         STRING(NST+1:NST+JS)=STR(1:JS)
         NST=NST+JS
      END IF
      GOTO 100
*     entry to add optional comments to the string
      ENTRY TEXTC(STR)
      JS=LEN(STR)
      IF(LST-JS+1.LE.NST) GOTO 100
      STRING(LST-JS+1:LST)=STR
      NST=LST
*
  100 RETURN
  101 FORMAT(A)
      END
