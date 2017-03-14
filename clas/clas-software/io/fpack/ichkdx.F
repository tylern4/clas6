#ifndef Linux
*CMZU:  1.00/16 05/03/96  23.29.41  by  Unknown
*-- Author :    Unknown   05/03/96
      INTEGER FUNCTION ICHKDX(BLK)
      INTEGER BLK
*KEEP,FCOMMON.
*     IEEE format
      PARAMETER (LOCAL=1)
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
      IERR   = 1
      IBLK   = -1
      IF(LR.EQ.0.OR.LW.EQ.0.OR.LUNR.EQ.0.OR.LUNW.EQ.0) GOTO 300
*
      ISVLR  = LR
      ISVLNR = LUNR
      LR     = LW
      LUNR   = LUNW
      ISV27  = INFU(27,LR)
      IND    = INDXB(IBUF,1000*LUNW+3)
      IF(IND.EQ.0) IND = INDXA(IBUF,1000*LUNW+3,INFU(1,LW)/4)
      IF(IND.EQ.0) CALL LOGERROR('ICHKDX.No space for working buffer')
*
      NERR  = 0
      IBLK  = 3
      INFU(27,LR) = IBLK
10    CALL FRRECX(LUNR,IBUF(IND),IBUF(IND+1),IERR)
      IF(IERR.GT.0) THEN
         NERR = NERR+1
         IF(NERR.GT.100) CALL LOGERROR('ICHKDX.Too many read errors')
         GOTO 10
      ENDIF
      IF(IERR.LT.0) GOTO 100
      LAST = IBLK
*     Convert to LOCAL format
      CALL FSWAP(IBUF(IND+1),0)
      CALL FRCONV(IBUF(IND+1),LOCAL,ICER)
*
*      PRINT '(1X,2A4,1X,''BLOCK# '',I9)',IBUF(IND+6),IBUF(IND+7),IBLK
      IBLK = IBLK+1
      GOTO 10
100   IBLK = MAX(LAST,BLK)
*
      INFU(27,LR) = ISV27
      LR   = ISVLR
      LUNR = ISVLNR
*      PRINT *,'ICHKDX = ',IBLK
300   ICHKDX = IBLK
      RETURN
      END
#else /* Linux */
      subroutine dummy_ichkdx
      implicit none
      return
      end
#endif /* Linux */
