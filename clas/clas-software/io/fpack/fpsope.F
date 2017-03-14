*CMZ :  1.00/00 16/12/94  17.00.00  by  Pavel Binko
*-- Author :    Zbigniew Szkutnik   16/12/94
      SUBROUTINE FPSOPE(LUN,FINAME,ACTION,IOS)
*
      CHARACTER*(*) FINAME, ACTION
*
************************************************************************
*
*     Open a file from C on UNIX machines
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
*KEND.
      INTEGER*2 NAMLEN, MODLEN
      CHARACTER*4 MODE
      COMMON/CIO/IOBPTR(NUNITS)
      SAVE CIO
*
      NAMLEN=LENB(FINAME)
      IF(NAMLEN.LT.0) NAMLEN=LEN(FINAME)
*
      IF(ACTION.EQ.'READ') THEN
         MODE='r'
         MODLEN=1
      ELSE IF(ACTION.EQ.'READWRITE') THEN
         MODE='a+'
         MODLEN=2
      ELSE IF(ACTION.EQ.'WRITE') THEN
         MODE='w'
         MODLEN=1
      ELSE IF(ACTION.EQ.'UPDATE') THEN
         MODE='r+'
         MODLEN=2
      ELSE
         IOS=1
         RETURN
      END IF
*
      LX = LUNIT(LUN)
*
      IF(INFU(12,LX).EQ.5) THEN
*       tape unit
        MEDIUM=1
      ELSE
*       disk unit
        MEDIUM=0
      END IF
      IBLKS=INFU(1,LX)
      IF(INFU(17,LX).NE.0) THEN
        MBYTES = (INFU(17,LX) * INFU(1,LX))/10**6 + 1
      ELSE
        MBYTES = 0
      END IF
      CALL COPEN(LUNPTR,FINAME,MODE,MEDIUM,IBLKS,MBYTES,
     +              IOBPTR(LX),IOS,NAMLEN,MODLEN)
      INFU(8,LX)=LUNPTR
*
*     Fill in the statistics in case of successfull open,
*     action READWRITE and the first access to the unit
      IF(IOS.EQ.0 .AND. ACTION.EQ.'READWRITE' .AND.
     +                  INFU(IWSTA+2,LX).EQ.0) THEN
         CALL CSTAT(FINAME,NAMLEN,IFILSZ,IRET)
         INFU(IWSTA+2,LX) = IFILSZ/INFU(1,LX)
         INFU(IWSTA+4,LX) = IFILSZ/1000
         INFU(IWSTA+7,LX) = MOD(IFILSZ,1000)
      END IF
*
      RETURN
      END
