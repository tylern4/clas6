*CMZ :  1.00/00 16/12/94  17.00.00  by  Pavel Binko
*-- Author :    Pavel Binko   16/12/94
      SUBROUTINE FPIOPEN(IOPEN,LX)
*
************************************************************************
*
*     Restore IOPEN from INFU
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
      INTEGER IOPEN(IOPMAX)
*
      DO 10 I=1,IOPMAX
   10 IOPEN(I)=0
*
*     logical unit number
      IOPEN( 1)=LUNBK(LX)
*
*     physical record length in bytes
      IOPEN( 2)=INFU( 1,LX)
*
*     primary space
      IOPEN( 3)=INFU(17,LX)
*
*     secondary space
      IOPEN( 4)=INFU(18,LX)
*
*     blocking factor
      IOPEN( 5)=INFU(19,LX)
*
*     number of buffers
      IOPEN( 6)=INFU(20,LX)
*
*     SPLITMB
      IOPEN( 8)=INFU(21,LX)
*
*     action (read, write, readwrite, modify)
      IOPEN( 9)=INFU( 4,LX)
*
*     status (old, new, scratch)
      IOPEN(10)=INFU(16,LX)
*
*     access (seq, dir, keyed, special, ordered)
      IOPEN(11)=INFU( 2,LX)
*
*     form (binary, text)
      IOPEN(12)=INFU( 3,LX)
*
*     word format (local, IEEE, IBM, VAX, DEC)
      IOPEN(13)=INFU( 5,LX)
*
*     logical record separation
      IOPEN(14)=INFU(11,LX)
*
*     NOOPEN or DEFER or STAGE or STAGEKEEP or UNIXTAPE
      IOPEN(15)=INFU(12,LX)
*
*     compress
      IOPEN(16)=INFU(13,LX)
*
*     catalogue (SHIFT)
      IF(INFU(15,LX).EQ.2) IOPEN(17)=1
*
*     SKIPCOREV - skip corrupted event at the end of file
      IOPEN(18)=INFU(32,LX)
*
*     DUNIT=lun - log unit for data file accessed from index file
*     (specifically for DSYIBM cartridge access)
      IOPEN(19)=INFU(35,LX)
*
*     parameter GENER="list_of_generic_names" given
      IOPEN(20)=INFU(33,LX)
*
*     set flag "DSTAGE parameter has been set"
      IOPEN(21)=INFU(37,LX)
*
*     set flag "DDEFER parameter has been set"
      IOPEN(22)=INFU(38,LX)
*
*     SPLITEV
      IOPEN(23)=INFU(39,LX)
*
*     tape server ( 0=default IBM tape server
*                   1=AMPEX tape server
*                   2=ACS access using OSM service commands )
      IOPEN(24)=INFU(41,LX)
*
*     not yet used
C     IOPEN( 7)=0
C     IOPEN(25)=0
C     IOPEN(26)=0
C     IOPEN(27)=0
C     IOPEN(28)=0
C     IOPEN(29)=0
C     IOPEN(30)=0
*
      RETURN
      END
