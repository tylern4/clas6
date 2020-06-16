#ifndef Linux
*CMZU:  1.00/16 29/02/96  17.02.14  by  Unknown
#endif /* not Linux */
*CMZU:  1.00/01 23/12/94  00.35.38  by  Unknown
*CMZ :  1.00/00 16/12/94  17.00.00  by  Pavel Binko
*-- Author :    Ruten Gurin   16/12/94
      SUBROUTINE FPSRCH(IMODE,IFOUND)
*
************************************************************************
*
*     Search for the key position in index block
*
************************************************************************
*
*KEEP,FCOMMON.
#ifndef Linux
*     IEEE format
      PARAMETER (LOCAL=1)
#else /* Linux */
*     DEC format (little-endian IEEE)
      PARAMETER (LOCAL=4)
#endif /* Linux */
*     Number of units
      PARAMETER (NUNITS=30)
*     The first possible free logical unit number
      PARAMETER (NFREEU=11)
*     Length of the integer vector IOPEN (for open and close)
      PARAMETER (IOPMAX=30)
*     Length of the integer vector LUNPAR (for statistics and inquire)
      PARAMETER (ILPMAX=30)
*     Length of the integer vector LISTI (file attributes)
      PARAMETER (ILIMAX=5)
*     length of different unit-information
      PARAMETER (NOPEN=50,NLGEN=15,NLSTU=25,NLSTA=10,NLKEY=11,NLHDR=70)
*     offset of different unit-information
      PARAMETER (ISGEN=NOPEN)
      PARAMETER (IRSTU=NOPEN+NLGEN,
     +           IRSTA=NOPEN+NLGEN+NLSTU,
     +           IRKEY=NOPEN+NLGEN+NLSTU+NLSTA,
     +           IRHDR=NOPEN+NLGEN+NLSTU+NLSTA+NLKEY,
     +           IWSTU=NOPEN+NLGEN+NLSTU+NLSTA+NLKEY+NLHDR)
      PARAMETER (IWSTA=IWSTU+NLSTU,
     +           IWKEY=IWSTU+NLSTU+NLSTA,
     +           IWHDR=IWSTU+NLSTU+NLSTA+NLKEY,
     +           NLDIM=IWSTU+NLSTU+NLSTA+NLKEY+NLHDR)
      COMMON/FUCOM/LUNW,LUNR,LW,LR,INDW,INDR,LUND,IVL(NUNITS),
     +             LUNIT(99),LUNBK(NUNITS),INFU(NLDIM,NUNITS),
     +             INFMT(200),IFMT(200),NFMT,
     +             NAMEB1,NAMEB2,NFMTB,IFMTB(200),NFMTH,IFMTH(64)
*     NAMEB1=  first part of BOSname
*     NAMEB2= second part of BOSname
*     IFMTB(1) ... IFMTB(NFMTB) = integer format information
*     IFMTH(1) ... IFMTH(NFMTH) = hollerith format information
      CHARACTER  FILNAM*256,HOSNAM*127,DATNAM*16,GENNAM*256,VOLNAM*6
      COMMON/DBCHOM/FILNAM(NUNITS),HOSNAM(NUNITS),DATNAM(NUNITS)
     +,             GENNAM(NUNITS),VOLNAM(NUNITS)
*     name of the data file
      CHARACTER*256 DTFLNM(NUNITS),RLFLNM(NUNITS)
*     host name for the data file
      CHARACTER*127 DTFLHO(NUNITS),RLFLHO(NUNITS)
*     MeDia type for the DaTa FiLe
      CHARACTER*16  DTFLMD(NUNITS),RLFLMD(NUNITS)
*     CuRrent data NaMe in DaTa file (it's used by index files)
      INTEGER     DTCRNM(2,NUNITS)
*     CuRrent Run Number in DaTa file (it's used by index files)
      INTEGER     DTCRRN(NUNITS)
*     volume name of cartridge for the data file
      CHARACTER*127 DTFLVS(NUNITS)
*     key block in data file
      INTEGER DTFLKB(NUNITS)
*     key position in data file block
      INTEGER DTFLPS(NUNITS)
*     index type
      LOGICAL IDXNEW
      COMMON/DBCHC1/DTFLNM,DTFLHO,DTFLMD,DTFLVS
     +,             RLFLNM,RLFLHO,RLFLMD
      COMMON/DBCHC2/DTFLKB,DTFLPS,DTCRNM,DTCRRN
      COMMON/DBCHC3/IDXNEW
      SAVE/FUCOM/,/DBCHOM/,/DBCHC1/,/DBCHC2/,/DBCHC3/
*KEEP,FBUFFER.
*     buffer common
      PARAMETER (NBUF=100000)
      COMMON/FBUF/IBUF(NBUF)
      SAVE/FBUF/
*KEND.
*
*     let's find the index block
      LEVEL = INFU(25,LR)
      INDX = INDXB(IBUF,1000*LUNR+9+LEVEL)
      IF( LEVEL.LE.0 .OR. INDX.LE.0 ) CALL LOGERROR('FPSRCH')
*
*     number of data words
      NW = IBUF(INDX+22)
*
*     suppose the key is not found
      IFOUND = 0
      IF( NW.LT.7 ) THEN
*        empty index block
         NPOS = 0
*        requested key is greater than (unexisting) key
         GOTO 100
      ENDIF
*
*     data segment header position in index block
      JP = 13
*     data position
      KP = JP + IBUF(INDX+JP+1)
*
      NPOS = -7
 10   CONTINUE
         NPOS = NPOS+7
         IF( NPOS.EQ.NW ) THEN
*           requested key is greater than the last key
            IF( IBUF(INDX+17).GT.1 ) THEN
*              we keep NPOS inside the block
               NPOS = NPOS-7
            ENDIF
            GOTO 100
         ENDIF
*
*        compare the keys
*        suppose that requested key is less than current
         DO 20 I=1,4
            N2 = INFU(IRSTU+14-1+I,LR)
*           zero is the smallest possible key
            N1 = IBUF(INDX+KP+NPOS+I)
*           the order is: 0 ... maxint minint ... -1
            IF( N1.NE.N2 ) THEN
               IF( N2.EQ.0 ) GOTO 100
               IF( N2.GT.0 .AND. N1.LT.0 ) GOTO 100
               IF( N1.GT.0 .AND. N2.LT.0 ) GOTO 10
               IF( N2.LT.N1 ) GOTO 100
               GOTO 10
            ENDIF
 20      CONTINUE
*
*        we have found the key
         IFOUND = 1
 100  CONTINUE
*
      IF( IBUF(INDX+17).GT.1 ) GOTO 200
      IF( IMODE.EQ.0 ) GOTO 200
*
      IF( IFOUND.EQ.1 ) THEN
*        check, whether the key was presented already
#ifndef Linux
         IF(INFU(IRSTU+12,LR).LE.-1.OR.INFU(IRSTU+12,LR).GE.1) THEN
#else /* Linux */
         IF( INFU(IRSTU+12,LR).EQ.-1 .OR. INFU(IRSTU+12,LR).EQ.1 ) THEN
#endif /* Linux */
*           yes, it was
            GOTO 200
         ENDIF
      ENDIF
*
#ifndef Linux
*     if MAXIMKEY ...
#else /* Linux */
*     if (MAXIMKEY | MINIMKEY) & descending order then
#endif /* Linux */
*     bypass moving the position
#ifndef Linux
      IF(INFU(IRSTU+14,LR).EQ.-1 .AND.
#else /* Linux */
      IF( INFU(IRSTU+12,LR).LT.0  .AND.
     +   ( ( INFU(IRSTU+14,LR).EQ.-1 .AND.
#endif /* Linux */
     +     INFU(IRSTU+15,LR).EQ.-1 .AND.
     +       INFU(IRSTU+16,LR).EQ.-1 .AND.
#ifndef Linux
     +         INFU(IRSTU+17,LR).EQ.-1 ) THEN
         IF(INFU(IRSTU+12,LR).EQ.-2) THEN
            NPOS = NW-7
         ELSEIF(INFU(IRSTU+12,LR).EQ.-1) THEN
*           FRKEY will decrement this position
            NPOS = NW
         ENDIF
         GOTO 200
      ENDIF
*     If MINIMKEY ...
      IF(INFU(IRSTU+14,LR).EQ.0  .AND.
#else /* Linux */
     +       INFU(IRSTU+17,LR).EQ.-1 ) .OR.
     +     ( INFU(IRSTU+14,LR).EQ.0  .AND.
#endif /* Linux */
     +     INFU(IRSTU+15,LR).EQ.0  .AND.
     +       INFU(IRSTU+16,LR).EQ.0  .AND.
#ifndef Linux
     +         INFU(IRSTU+17,LR).EQ.0  ) THEN
         IF(INFU(IRSTU+12,LR).EQ.2) THEN
*           requested position, ascending order
            NPOS = 0
         ELSEIF(INFU(IRSTU+12,LR).EQ.1) THEN
*           FRKEY will increment this position
            NPOS = -7
         ENDIF
         GOTO 200
      ENDIF
#else /* Linux */
     +       INFU(IRSTU+17,LR).EQ.0  ) ) )  GOTO 200
#endif /* Linux */
*
#ifndef Linux
      IF(IFOUND.EQ.0) THEN
         IF(INFU(IRSTU+12,LR).EQ.2) GOTO 200
         IF(INFU(IRSTU+12,LR).EQ.-2) THEN
            NPOS = NPOS - 7
            GOTO 200
         ENDIF
      ENDIF
#else /* Linux */
      IF(IFOUND.EQ.0 .AND. INFU(IRSTU+12,LR).EQ.-2) GOTO 200
#endif /* Linux */
*
      IF( INFU(IRSTU+12,LR).GE.0 ) THEN
*        if we scan in ascending order - move the position backward
         NPOS = NPOS-7
      ELSE
*        move the position forward
#ifndef Linux
         IF(NPOS.LT.NW) THEN
#endif /* not Linux */
            NPOS = NPOS+7
#ifndef Linux
         ELSE
            NPOS = NW
         ENDIF
#endif /* not Linux */
      ENDIF
*
#ifndef Linux
 200  CONTINUE
      IBUF(INDX+15) = NPOS
#else /* Linux */
 200  IBUF(INDX+15) = NPOS
#endif /* Linux */
*
      RETURN
      END
