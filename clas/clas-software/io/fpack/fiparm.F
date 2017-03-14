*CMZ :  1.00/00 16/12/94  17.00.00  by  Pavel Binko
*-- Author :    Volker Blobel   16/12/94
      SUBROUTINE FIPARM(LUN,DANAME,LISTI)
*
      CHARACTER*(*) DANAME
*
************************************************************************
*
*     Preliminary version to define unit parameters for F-package
*
************************************************************************
*
      CHARACTER DANAM2*16
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
      INTEGER LISTI(ILIMAX)
*
      IF(LUN.EQ.0) THEN
*        get free unit number
         DO 22 I=NFREEU,99
         IF(LUNIT(I).EQ.0) GOTO 24
   22    CONTINUE
         I=100
   24    LUN=I
      END IF
*     define data name, evtl by default
      DANAM2=DANAME
      IF(DANAM2.EQ.' ') THEN
         DANAM2='FILE00'
         DANAM2(6:6)=CHAR(ICHAR('0')+MOD(LUN,10))
         DANAM2(5:5)=CHAR(ICHAR('0')+LUN/10)
      END IF
      IF(IABS(LUN).LT.1.OR.IABS(LUN).GT.99) GOTO 100
      IF(LUN.LT.0) THEN
*        negative LUN means unit is reserved and should not be used
         LUNIT(-LUN)=-1
         GOTO 100
      END IF
*     check existing entries and close unit with same number or name
      IF(LUNIT(LUN).NE.0) THEN
         IF(LUNIT(LUN).LT.0) THEN
            WRITE(*,*) ' FUPARM: Attempt to use a reserved unit'
            STOP
         END IF
*        allow new list
         LINX=LUNIT(LUN)
         GOTO 28
      END IF
      LAN=IFRST(DANAM2,0)
      IF(LAN.NE.0) THEN
         CALL FUCLOS(LAN)
      END IF
*     find free slot ...
      DO 26 I=1,LUND
      IF(LUNBK(I).EQ.0) THEN
*        ... old one
         LINX=I
         GOTO 28
      END IF
   26 CONTINUE
      IF(LUND.EQ.NUNITS)  THEN
         WRITE(*,*) ' FUPARM: Too many F-units used - STOP'
         STOP
      END IF
*     ... new one
      LUND=LUND+1
      LINX=LUND
*     pointer from unit number to array index ...
   28 LUNIT(LUN)=LINX
*     ... and pointer back from array index to unit number
      LUNBK(LINX)=LUN
*     reset array
      DO 30 I=1,NLDIM
   30 INFU(I,LINX)=0
*     copy from argument
      DO 40 I=1,ILIMAX
   40 INFU(I,LINX)=LISTI(I)
*     make entry of dataname
      IDUM =IFRST(DANAM2,LUN)
      DATNAM(LUNIT(LUN))=DANAM2
      IF(DANAME.EQ.' ') DANAME=DANAM2
*     initialize FILNAM and HOSNAM
      FILNAM(LUNIT(LUN))=' '
      HOSNAM(LUNIT(LUN))=' '
*
  100 RETURN
      END
