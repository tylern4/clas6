#ifndef Linux
*CMZU:  1.00/16 08/03/96  15.36.39  by  Wiesiek Dlugosz
#endif /* not Linux */
*CMZ :  1.00/00 16/12/94  17.00.00  by  Pavel Binko
*-- Author :    Ruten Gurin   16/12/94
      SUBROUTINE FPCKTP(IRES)
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
*     check, if top level index block was changed
*
*     suppose that it was not
      IRES = 0
*
*     let's find the buffer
      INDR = INDXB(IBUF,1000*LUNR+9+1)
*     old counter
      NUM1X = IBUF(INDR+8)
      NUM2X = IBUF(INDR+9)
*     current position in top index block
      NPOSX = IBUF(INDR+15)
#ifndef Linux
*
      NTRY = 0
      INFU(27,LR) = 1
#endif /* not Linux */
   10 CONTINUE
*     reread the top index
#ifdef Linux
      INFU(27,LR) = 1
#endif /* Linux */
      CALL FRRECX(LUNR,IBUF(INDR),IBUF(INDR+1),IER)
      IF( IER.NE.0 ) CALL LOGERROR('FPCKTP. Corrupted ordered file')
      NUM1Y = IBUF(INDR+8)
      NUM2Y = IBUF(INDR+9)
#ifndef Linux
      NUM3Y = IBUF(INDR+IBUF(INDR+1))
      IF( NUM1Y.EQ.1 .AND. NUM2Y.EQ.1 .OR. NUM3Y.EQ.1 ) THEN
#else /* Linux */
      IF( NUM1Y.EQ.1 .AND. NUM2Y.EQ.1 ) THEN
#endif /* Linux */
*        "unfinished", try the alternative
#ifndef Linux
         NTRY = NTRY + 1
         IF(INFU(27,LR).NE.2) INFU(27,LR) = 1
         IF(NTRY.GT.100000) GOTO 333
#else /* Linux */
         IF(INFU(27,LR).NE.2) CALL LOGERROR('FPCKTP. FRKEY alt')
         INFU(27,LR) = 2
#endif /* Linux */
         GOTO 10
      ENDIF
#ifndef Linux
      IF( NUM1Y.NE.NUM1X.OR.NUM2Y.NE.NUM2X.OR.NUM2Y.NE.NUM3Y) THEN
#else /* Linux */
      IF( NUM1Y.NE.NUM1X.OR.NUM2Y.NE.NUM2X ) THEN
#endif /* Linux */
*        file was changed
         IRES = 1
*        we have top level only
         INFU(25,LR) = 1
*        new last position
         INFU(30,LR) = IBUF(INDR+18)
         INFU(26,LR) = IBUF(INDR+18) + 1
      ELSE
*        restore the old position
         IBUF(INDR+15) = NPOSX
      ENDIF
#ifndef Linux
      RETURN
333   CONTINUE
      PRINT *,'FPCKTP. The ordered file is locked for reading process'
      PRINT *,'It''s likely the REPAIR procedure is running now or '
      PRINT *,'has been run before and failed!'
      PRINT *,'I need to stop Your job with STOP 333 statement.'
      PRINT *,'Try later again.'
      PRINT *,'        Sorry for inconvenience,    FPACK.'
      STOP 333
#endif /* not Linux */
      END
#ifndef Linux
 
#endif /* not Linux */
