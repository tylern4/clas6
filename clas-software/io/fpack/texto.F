*CMZ :  1.00/00 16/12/94  17.00.00  by  Pavel Binko
*-- Author :    Volker Blobel   16/12/94
      SUBROUTINE TEXTO(IARR,NWRD,IFMT,NFMT,IRAST)
*
      INTEGER IARR(*), IFMT(*)
*
************************************************************************
*
*     Data output in text form of
*          IARR(1)...IARR(NWRD)
*                 using format description IFMT(1)...IFMT(NFMT)
*
************************************************************************
*
      INTEGER LV(3,10)
*
*     character buffer
      CHARACTER*255 STRING, STR*12, CHA*4
*KEEP,CHASTR.
      COMMON/CHASTR/STRING
      SAVE/CHASTR/
*KEEP,NUMSTR.
      COMMON/NUMSTR/LUNST,LSTT,LST,NST,IAST
*     LUNST = unit; LST = length of string; NST = actual string length
*     IAST  = index of start character;     LSTT=length of string write
      SAVE/NUMSTR/
*KEND.
      IF(NWRD.LE.0) GOTO 100
*     initialize format use
      IMT=0
      IB =0
*     skipping not used
      NSKIP=0
      MSKIP=NSKIP
      IF(NFMT.EQ.1) THEN
*        only one item
         NCNF=999999999
         KCNF=MOD(IFMT(1),10)
      ELSE
*        more than one item
         NCNF =0
         LEV  =0
         ITERM=0
      END IF
      GOTO 15
*     get next format code
    5 IMT=IMT+1
      IF(IMT.GT.NFMT) THEN
*        end of format stmt reached, back to last parenthesis
         IMT=ITERM
         GOTO 5
      END IF
      IF(IFMT(IMT).EQ.0) THEN
*        right parenthesis
         LV(3,LEV)=LV(3,LEV)+1
         IF(LV(3,LEV).GE.LV(2,LEV)) THEN
            ITERM=LV(1,LEV)-1
            LEV=LEV-1
         ELSE
            IMT=LV(1,LEV)
         END IF
      ELSE
         NCNF=IFMT(IMT)/10
         KCNF=IFMT(IMT)-10*NCNF
         IF(KCNF.EQ.0) THEN
*           left parenthesis
            LEV=LEV+1
            LV(1,LEV)=IMT
            LV(2,LEV)=NCNF
            LV(3,LEV)=0
         ELSE
*           format F or I or A or
            IF(IMT.NE.NFMT-1.OR.LEV.EQ.0) GOTO 15
            IF(IMT.NE.LV(1,LEV)+1)        GOTO 15
*           speed up case of repeated format at the end
            NCNF=999999999
            GOTO 15
         END IF
      END IF
      GOTO 5
*
   15 IF(NCNF.EQ.0) GOTO 5
      IF(MSKIP.GT.0) THEN
*        still some words to skip
         IF(MSKIP.GE.NCNF) THEN
*           end of skipping not yet reached
            MSKIP=MSKIP-NCNF
*           NCNF=0
            GOTO 5
         ELSE
*           end of skipping reached
            NCNF=NCNF-MSKIP
            MSKIP=0
         END IF
      END IF
*     define limits for next conversion and update counter
      IA=IB+1
      IB=MIN0(NWRD,IB+NCNF)
      NCNF=NCNF-IB+IA-1
*     words IA...IB according to type KCNF
*     ==================================================================
      IF(KCNF.EQ.3) THEN
*        character (A) data, store within '
         DO 10 I=IA,IB
         ICH=IARR(I)
*KEEP,CHAICH.
*     get char*4 from hollerith 4H
#ifndef Linux
      CHA=CHAR(IAND(ISHFT(ICH,-24),255))//CHAR(IAND(ISHFT(ICH,-16),255))
     +  //CHAR(IAND(ISHFT(ICH,- 8),255))//CHAR(IAND      (ICH     ,255))
#else /* Linux */
      CHA=CHAR(IAND      (ICH     ,255))//CHAR(IAND(ISHFT(ICH,- 8),255))
     +  //CHAR(IAND(ISHFT(ICH,-16),255))//CHAR(IAND(ISHFT(ICH,-24),255))
#endif /* Linux */
*KEND.
         STR=CHA
         JS=4
         IF(NST.LE.0) THEN
            STRING=' '
            NST=IAST-1
         ELSE IF(NST.LT.IAST) THEN
            NST=IAST-1
         ELSE IF(NST+JS+3.LE.LST) THEN
*           sufficient space
         ELSE
*           insufficient space
            IF(I.NE.IA) THEN
*              add final '
               NST=NST+1
               STRING(NST:NST)=''''
            END IF
            WRITE(LUNST,'(A)') STRING(1:LSTT)
            STRING=' '
            NST=IAST-1
            IF(I.NE.IB) THEN
*              add first '
               NST=NST+1
               STRING(NST:NST)=''''
            END IF
         END IF
         IF(I.EQ.IA) THEN
            NST=NST+1
            STRING(NST:NST)=''''
         END IF
         STRING(NST+1:NST+JS)=STR(1:JS)
         NST=NST+JS
   10    CONTINUE
         NST=NST+1
         STRING(NST:NST)=''''
      ELSE IF(KCNF.EQ.7) THEN
*        character (C) data (not within ')
         DO 20 I=IA,IB
         ICH=IARR(I)
*KEEP,CHAICH.
*     get char*4 from hollerith 4H
#ifndef Linux
      CHA=CHAR(IAND(ISHFT(ICH,-24),255))//CHAR(IAND(ISHFT(ICH,-16),255))
     +  //CHAR(IAND(ISHFT(ICH,- 8),255))//CHAR(IAND      (ICH     ,255))
#else /* Linux */
      CHA=CHAR(IAND      (ICH     ,255))//CHAR(IAND(ISHFT(ICH,- 8),255))
     +  //CHAR(IAND(ISHFT(ICH,-16),255))//CHAR(IAND(ISHFT(ICH,-24),255))
#endif /* Linux */
*KEND.
         STR=CHA
         JS=4
         IF(NST.LE.0) THEN
            STRING=' '
            NST=IAST-1
         ELSE IF(NST.LT.IAST) THEN
         ELSE IF(NST+JS+3.LE.LST) THEN
C           NST=NST+1
         ELSE
            WRITE(LUNST,'(A)') STRING(1:LSTT)
            STRING=' '
            NST=IAST-1
         END IF
         STRING(NST+1:NST+JS)=STR(1:JS)
         NST=NST+JS
   20    CONTINUE
      ELSE
*        floating pt, integer, hex
         DO 30 I=IA,IB
         IF(KCNF.EQ.1) THEN
            CALL PNVF(IARR(I),STR,JS)
         ELSE IF(KCNF.EQ.2) THEN
            CALL PNVI(IARR(I),STR,JS)
         ELSE
            CALL PNVZ(IARR(I),STR,JS)
         END IF
 
         IF(NST.LE.0) THEN
            STRING=' '
            NST=IAST-1
         ELSE IF(NST.LT.IAST) THEN
 
         ELSE IF(NST+JS+3.LE.LST) THEN
            NST=NST+1
         ELSE
            WRITE(LUNST,'(A)') STRING(1:LSTT)
            STRING=' '
            NST=IAST-1
         END IF
         IF(KCNF.GT.3) THEN
            STRING(NST+1:NST+1)='Z'
            NST=NST+1
            IF(STR(1:4).EQ.'0000') THEN
               STRING(NST+1:NST+JS-4)=STR(5:JS)
               NST=NST+JS-4
            ELSE
               STRING(NST+1:NST+JS)=STR(1:JS)
               NST=NST+JS
            END IF
         ELSE
*           floating point and integer
            IF(IRAST.GT.1) THEN
*              improve position of text according to raster IRAST
   25          NST=((NST+JS+IRAST-IAST+1)/IRAST)*IRAST+IAST-1-JS
               IF(NST+JS.GT.LST) THEN
                  WRITE(LUNST,'(A)') STRING(1:LSTT)
                  STRING=' '
                  NST=IAST-1
                  GOTO 25
               END IF
            END IF
            STRING(NST+1:NST+JS)=STR(1:JS)
            NST=NST+JS
         END IF
   30    CONTINUE
      END IF
*
      IF(IB.LT.NWRD) GOTO 15
*
  100 RETURN
      END
