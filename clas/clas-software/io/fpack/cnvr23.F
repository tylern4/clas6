*CMZ :  1.00/00 16/12/94  17.00.00  by  Pavel Binko
*-- Author :    Pavel Binko   16/12/94
      SUBROUTINE CNVR23(IARR,NWRD,NSKIP,IFMT,NFMT)
*
      INTEGER IARR(*), IFMT(*)
*
************************************************************************
*
*     Convert 2 (IBM) to 3 (VAX)
*
*        Convert the data of array (IARR(I), I=1...NWRD)
*        using the format code      IFMT(J), J=1...NFMT)
*     If NSKIP not zero, then NSKIP words have been converted with the
*     given format before and format use has to start with word NSKIP+1.
*
************************************************************************
*
      INTEGER LV(3,10)
*KEEP,ARETOA.
      INTEGER ETOA(0:255)
*KEEP,BITMSK.
      INTEGER MVITOX(0:15)
      INTEGER MVVTOI(0:3)
      INTEGER MVATOI(0:3)
      PARAMETER
     + ( M31 =-2147483647-1, M23   =    8388608, M15   =   32768,
     +   M3024 = 2130706432, M3023 = 2139095040, M2200 = 8388607,
     +   M1407 =      32640, M0600 =        127,
     +   M0300 =         15, M0100 =          3,
     +   IBGAPO= 2147483647, ISMAPO=    8388608,
     +   IBGVAX=     -32769, ISMVAX=        128,
     +   IDATOI=  557842432, IDITOA=        130,
     +   IDITOV=        128, IDVTOI=  549453824,
     +   IDATOV=        256, IDVTOA=   16777216,
     +   MXATOV= 2122317824, MNVTOA=        384 )
      DATA (MVITOX(I),I=0,15) / 3,3,2,2,1,1,1,1,0,0,0,0,0,0,0,0 /
      DATA (MVVTOI(I),I=0,3) /  0,-3,-2,-1 /
      DATA (MVATOI(I),I=0,3) / -2,-1,-0,-3 /
*KEEP,DTETOA.
      DATA (ETOA(I),I=0,255) /
     +   0, 32, 32, 32, 32, 32, 32, 32, 32,  5, 32, 32, 32, 13, 32, 32,
     +  32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32,
     +  32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32,
     +  32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32,
     +  32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 46, 60, 40, 43,124,
     +  38, 32, 32, 32, 32, 32, 32, 32, 32, 32, 33, 36, 42, 41, 59, 94,
     +  45, 47, 32, 32, 32, 32, 32, 32, 32, 32, 32, 44, 37, 95, 62, 63,
     +  32, 32, 32, 32, 32, 32, 32, 32, 32, 96, 58, 35, 64, 39, 61, 34,
     +  32, 97, 98, 99,100,101,102,103,104,105, 32, 32, 32, 32, 32, 32,
     +  32,106,107,108,109,110,111,112,113,114, 32, 32, 32, 32, 32, 32,
     +  32,126,115,116,117,118,119,120,121,122, 32, 32, 32, 91, 32, 32,
     +  32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 93, 32, 32,
     + 123, 65, 66, 67, 68, 69, 70, 71, 72, 73, 32, 32, 32, 32, 32, 32,
     + 125, 74, 75, 76, 77, 78, 79, 80, 81, 82, 32, 32, 32, 32, 32, 32,
     +  92, 32, 83, 84, 85, 86, 87, 88, 89, 90, 32, 32, 32, 32, 32, 32,
     +  48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 32, 32, 32, 32, 32, 32 /
*KEND.
*
*KEEP,CNVPRP.
      IF(NWRD.LE.0) GOTO 100
*     initialize format use
      IMT=0
      IB =0
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
*KEND.
*
* Convert words IA...IB according to type KCNF
*
      IF(KCNF.EQ.1) THEN
*        Floating point 32 bits format
         DO 10 I=IA,IB
*KEEP,R4ITOV.
      JARR = IARR(I)
      IF(JARR.NE.0) THEN
#if !defined(SunOS) && !defined(IRIX)
        LESHFT = MVITOX(IAND(ISHFT(JARR,-20),M0300))
        IEXPO = ISHFT(IAND(JARR,M3024),-22) - IDITOV - LESHFT
#else /* SunOS */
        LESHFT = MVITOX( AND(RSHIFT(JARR, 20),M0300))
        IEXPO = RSHIFT( AND(JARR,M3024), 22) - IDITOV - LESHFT
#endif /* SunOS */
        IF(IEXPO.LE.0) THEN
#if !defined(SunOS) && !defined(IRIX)
          IARR(I) = IOR(ISHFT(IAND(JARR,M31),-16),ISMVAX)
#else /* SunOS */
          IARR(I) =  OR(RSHIFT( AND(JARR,M31), 16),ISMVAX)
#endif /* SunOS */
        ELSE IF(IEXPO.GT.255) THEN
#if !defined(SunOS) && !defined(IRIX)
            IARR(I) = IOR(ISHFT(IAND(JARR,M31),-16),IBGVAX)
#else /* SunOS */
            IARR(I) =  OR(RSHIFT( AND(JARR,M31), 16),IBGVAX)
#endif /* SunOS */
          ELSE
#if !defined(SunOS) && !defined(IRIX)
            MANT = IAND(ISHFT(JARR,LESHFT),M2200)
            IARR(I) = IOR(IOR(IOR(ISHFT(MANT,-16),ISHFT(MANT,16)),
     +                ISHFT(IEXPO,7)),ISHFT(IAND(JARR,M31),-16))
#else /* SunOS */
            MANT =  AND(LSHIFT(JARR,LESHFT),M2200)
            IARR(I) = OR(OR(OR(RSHIFT(MANT, 16),LSHIFT(MANT,16)),
     +                LSHIFT(IEXPO,7)),RSHIFT(AND(JARR,M31),16))
#endif /* SunOS */
        END IF
      END IF
*KEND.
   10    CONTINUE
*
      ELSE IF(KCNF.EQ.2) THEN
*        Integer word format
*        DO 20 I=IA,IB
*  20    CONTINUE
*
      ELSE IF(KCNF.EQ.3) THEN
*        Hollerith
         DO 30 I=IA,IB
*KEEP,H4ITOV.
         JARR=IARR(I)
#if !defined(SunOS) && !defined(IRIX)
         IARR(I)=IOR(IOR      (ETOA( IAND(ISHFT(JARR,-24),255) ),
     +                   ISHFT(ETOA( IAND(ISHFT(JARR,-16),255) ), 8)),
     +               IOR(ISHFT(ETOA( IAND(ISHFT(JARR,- 8),255) ),16),
     +                   ISHFT(ETOA( IAND(      JARR     ,255 )),24)))
#else /* SunOS */
         IARR(I)=OR(OR      (ETOA(  AND(RSHIFT(JARR, 24),255) ),
     +                LSHIFT(ETOA(  AND(RSHIFT(JARR, 16),255) ), 8)),
     +             OR(LSHIFT(ETOA(  AND(RSHIFT(JARR,  8),255) ),16),
     +                LSHIFT(ETOA(  AND(      JARR     ,255 )),24)))
#endif /* SunOS */
*KEND.
   30    CONTINUE
*
      ELSE IF(KCNF.EQ.4) THEN
*        Integer 16 bits
         DO 40 I=IA,IB
*KEEP,B16SWP.
#if !defined(SunOS) && !defined(IRIX)
         IARR(I)=IOR(ISHFT(IARR(I),16),ISHFT(IARR(I),-16))
#else /* SunOS */
         IARR(I)= OR(LSHIFT(IARR(I),16),RSHIFT(IARR(I), 16))
#endif /* SunOS */
*KEND.
   40    CONTINUE
*
      ELSE IF(KCNF.EQ.5) THEN
*        Integer 8 bits
         DO 50 I=IA,IB
*KEEP,B08SWP.
         JARR=IARR(I)
#if !defined(SunOS) && !defined(IRIX)
         IARR(I)=IOR(IOR(IOR(ISHFT(JARR,24),ISHFT(JARR,-24)),
     +                   ISHFT(IAND(JARR,65280),8)),
     +               ISHFT(IAND(JARR,16711680),-8))
#else /* SunOS */
         IARR(I)= OR( OR( OR(LSHIFT(JARR,24),RSHIFT(JARR, 24)),
     +                   LSHIFT( AND(JARR,65280),8)),
     +               RSHIFT( AND(JARR,16711680), 8))
#endif /* SunOS */
*KEND.
   50    CONTINUE
*
      ELSE IF(KCNF.EQ.6) THEN
*        Integer 32 bits
*        DO 60 I=IA,IB
*  60    CONTINUE
      END IF
*
   90 IF(IB.LT.NWRD) GOTO 15
*
  100 RETURN
      END
