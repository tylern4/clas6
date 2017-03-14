*CMZ :  1.00/00 16/12/94  17.00.00  by  Pavel Binko
*-- Author :    Pavel Binko   16/12/94
      SUBROUTINE FQFILE(DANAME,FINAME,HONAME,LUNPAR,IERR)
*
      CHARACTER*(*) DANAME, FINAME, HONAME
*
************************************************************************
*
*     File inquire
*     ------------
*
*     Input parameter :     DANAME = Symbolic data name
*     Output parameters :   FINAME = File name
*                           HONAME = Host name
*                           LUNPAR = Array of file parameters
*     LUNPAR
*
*     (1) = Unit number
*     (2) = Access  ( 1=seq    2=dir    3=keyed   4=special  5=ordered )
*     (3) = Form    ( 1=binary 2=text )
*     (4) = Action  ( 1=read   2=write  3=readwrite  4=modify )
*     (5) = Physical record length in bytes
*     (6) = Number of physical records read
*     (7) = Number of physical records written
*     (8) = Number of logical records read
*     (9) = Number of logical records written
*    (10) = Number of Kbytes read
*
*    (11) = Number of Kbytes written
*    (12) = not used (Number of bytes read (with information))
*    (13) = not used (Number of bytes written (with information))
*    (14) = Average length of logical records on input
*    (15) = Average length of logical records written
*    (16) = Number of data blocks read
*    (17) = Number of data blocks written
*    (18) = Number of data blocks written without format ( B32 added )
*
*    (19 - 30) = not used yet
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
      INTEGER LUNPAR(ILPMAX)
*
      IERR=0
*
      FINAME=' '
      HONAME=' '
*
      DO 10 I=1,ILPMAX
         LUNPAR(I)=0
   10 CONTINUE
*
      LUNINQ = IFRST(DANAME,0)
      IF(LUNINQ.EQ.0) THEN
         IERR=-1
         GOTO 100
      END IF
      LUNFP = LUNIT(LUNINQ)
*
*     host name
      HONAME = HOSNAM(LUNFP)
*
*     file name
      FINAME = FILNAM(LUNFP)
*
*     unit number
      LUNPAR(1) = LUNINQ
*     access
      LUNPAR(2) = INFU(2,LUNFP)
*     form
      LUNPAR(3) = INFU(3,LUNFP)
*     action
      LUNPAR(4) = INFU(4,LUNFP)
*     record length
      LUNPAR(5) = INFU(1,LUNFP)
*     number of physical records read
      LUNPAR(6) = INFU(IRSTA+2,LUNFP)
*     number of physical records written
      LUNPAR(7) = INFU(IWSTA+2,LUNFP)
*     number of logical records read
      LUNPAR(8) = INFU(IRSTA+3,LUNFP)
*     number of logical records written
      LUNPAR(9) = INFU(IWSTA+3,LUNFP)
*     number of Kbytes read
      LUNPAR(10) = INFU(IRSTA+4,LUNFP)
*     number of Kbytes written
      LUNPAR(11) = INFU(IWSTA+4,LUNFP)
*     average length of logical record on input
      IF(INFU(IRSTA+6,LUNFP).NE.0) LUNPAR(14) =
     +  LUNPAR(10)*1000.0 / INFU(IRSTA+6,LUNFP)
*     average length of logical records written
      IF(LUNPAR(9).NE.0) LUNPAR(15) = LUNPAR(11)*1000.0 / LUNPAR(9)
*     number of data blocks read
      LUNPAR(16) = INFU(IRSTA+5,LUNFP)
*     number of data blocks written
      LUNPAR(17) = INFU(IWSTA+5,LUNFP)
*     number of data blocks written without format ( B32 added )
      LUNPAR(18) = INFU(IWSTA+6,LUNFP)
*
  100 RETURN
      END
