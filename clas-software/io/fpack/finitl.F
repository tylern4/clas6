*CMZ :  1.00/00 16/12/94  17.00.00  by  Pavel Binko
*-- Author :    Volker Blobel   16/12/94
      SUBROUTINE FINITL
*
************************************************************************
*
*     Initialize F-package and system\L
*
************************************************************************
*
      LOGICAL START
      DATA START/.FALSE./
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
*KEEP,EDITCOM.
      COMMON/CEDIT/LUNLOG,LOGFLG,LLMODE,LPMODE,LREXEC
*     LUNSCR = unit for scratch textfile
*     LOGFLG = 0 nologging of commands = 1 logging of commands
*     LLMODE = 1   batch
*            = 2   interactive
*     LPMODE = 1   print = WRITE(*,... for batch
*            = 2   print = WRITE(*,... for alpha terminal
*            = 3   print GKS           for graphics terminal
*     LREXEC = 1   execute statements immediately
*            = 2   no execution (diagnostic only)
      SAVE/CEDIT/
*KEEP,CTEXTIR.
*     one statement up to 5280 = 66 * 80 characters
      PARAMETER (LSTMT=5280)
      CHARACTER STMT*(LSTMT),ERRMES*72,NORMES*72
      PARAMETER (NDIMIT=640)
      REAL RTX(4,NDIMIT)
      COMMON/CTXTIR/NST,MST,KST,INU,NTX,ITX(4,NDIMIT)
      COMMON/CTXTST/STMT,ERRMES,NORMES
      EQUIVALENCE (ITX(1,1),RTX(1,1))
      SAVE/CTXTIR/,/CTXTST/
*KEND.
      INTEGER LISTI(ILIMAX)
*
      IF(START) GOTO 100
      START=.TRUE.
      LUND=0
      DO 10 I=1,99
   10 LUNIT(I)=0
      DO 20 J=1,NUNITS
      LUNBK(J)=0
      FILNAM(J) = ' '
      HOSNAM(J) = ' '
   20 DATNAM(J) = ' '
*     init common ! ...
      IBUF(1)=NBUF
      IBUF(2)=0
*     ... and clear for test only
      DO 30 I=3,NBUF
   30 IBUF(I)=0
*     init  EDIT\L
      LUNSCR=0
      LOGFLG=0
*     assign SYSIN, 80 bytes, seq text read on unit 5
      LISTI(1)=80
      LISTI(2)=1
      LISTI(3)=2
      LISTI(4)=1
      LISTI(5)=0
      CALL FIPARM(5,'SYSIN',LISTI)
      FILNAM(LUNIT(5)) = ' '
      HOSNAM(LUNIT(5)) = ' '
      DATNAM(LUNIT(5)) = 'SYSIN'
*     assign SYSOUT, 80 bytes, seq text write on unit 6
      LISTI(1)=80
      LISTI(2)=1
      LISTI(3)=2
      LISTI(4)=2
      LISTI(5)=0
      CALL FIPARM(6,'SYSOUT',LISTI)
      FILNAM(LUNIT(6)) = ' '
      HOSNAM(LUNIT(6)) = ' '
      DATNAM(LUNIT(6)) = 'SYSOUT'
*     reserved units for GKS
      DO 40 I=71,99
   40 CALL FIPARM(-I,' ',LISTI)
*     interactive or batch?
      LLMODE=2
      LPMODE=2
*     further initialization
      ERRMES=' '
      NORMES=' '
      LREXEC=1
*     Initialize the ERROR TRACE report, i.e.
*     "forget" all past errors (silent mode)
      CALL FPEMSG(0)
*
  100 RETURN
      END
