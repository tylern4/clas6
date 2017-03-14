*CMZ :  1.00/00 16/12/94  17.00.00  by  Pavel Binko
*-- Author :    Volker Blobel   16/12/94
      SUBROUTINE FPRFLS
*
************************************************************************
*
*     Print overview over all files (one line for each file)
*
************************************************************************
*
      CHARACTER DANAME*16, FINAME*256, HONAME*127, EXPL*72
*
      CHARACTER TACCES(5)*7, TFORMT(2)*6, TACTIO(4)*9
      DATA TACCES/'    seq','    dir','  keyed','special','ordered'/
      DATA TFORMT/'binary','  text'/
      DATA TACTIO/'     read','    write','readwrite','   modify'/
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
      INTEGER LUNPAR(ILPMAX)
*
      NFILES=0
      DO 10 LUN=1,99
      IBLA=IFRST(DANAME,-LUN)
      IF(IBLA.EQ.0) GOTO 10
      HONAME=' '
      CALL FQFILE(DANAME,FINAME,HONAME,LUNPAR,IER)
      IF(IER.NE.0) GOTO 10
      IF(NFILES.EQ.0) THEN
         WRITE(*,101)
      END IF
      NFILES=NFILES+1
      EXPL=' '
      NEXP =0
*
      IF(HONAME.NE.' ') THEN
         N=LENB(HONAME)
         EXPL(1:6)='(host='
         EXPL(7:8+N)=HONAME(1:N)//') '
         NEXP=8+N
      END IF
*
      IF(FINAME.NE.' ') THEN
         N=LENB(FINAME)
         IF(N+NEXP.GT.72) N=72-NEXP
         EXPL(NEXP+1:NEXP+N)=FINAME(1:N)
      END IF
*
      WRITE(*,102) LUN,DANAME,TACCES(LUNPAR(2)),TFORMT(LUNPAR(3)),
     +             TACTIO(LUNPAR(4)),LUNPAR(5),EXPL
   10 CONTINUE
*
      IF(NFILES.NE.0) GOTO 100
      WRITE(*,103)
*
  100 RETURN
  101 FORMAT(1X/' F-Package file overview        (by CALL FPRFLS)'/
     +          ' unit  dataname........  access format    action ',
     +       'rl/bytes  file...'/
     +          ' ----  ----------------  ------ ------ --------- ',
     +       '--------  -------')
  102 FORMAT(1X,I3,3X,A16,1X,A7,1X,A6,1X,A9,I9,2X,A)
  103 FORMAT(1X/' No F-Package files open')
      END
