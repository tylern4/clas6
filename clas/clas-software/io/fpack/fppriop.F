*CMZ :  1.00/00 16/12/94  17.00.00  by  Pavel Binko
*-- Author :    Pavel Binko   16/12/94
      SUBROUTINE FPPRIOP(IOPEN,CHOUT)
*
      CHARACTER*(*) CHOUT
*
************************************************************************
*
*     Print the vector IOPEN
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
      WRITE(*,*) '*****************************************************'
      WRITE(*,*) CHOUT
      WRITE(*,*) '-----------------------------------------------------'
      WRITE(*,1) ' 1) Logical unit number          = ',IOPEN(1)
      WRITE(*,1) ' 2) Physical record length       = ',IOPEN(2)
*     WRITE(*,1) ' 3) Primary space                = ',IOPEN(3)
*     WRITE(*,1) ' 4) Secondary space              = ',IOPEN(4)
*     WRITE(*,1) ' 5) Blocking factor              = ',IOPEN(5)
*     WRITE(*,1) ' 6) Number of buffers            = ',IOPEN(6)
*     WRITE(*,1) ' 7) Not yet used                 = ',IOPEN(7)
*     WRITE(*,1) ' 8) Number of MB per file        = ',IOPEN(8)
      WRITE(*,1) ' 9) Action (read,write,rw,mod)   = ',IOPEN(9)
      WRITE(*,1) '10) Status (old,new,scratch)     = ',IOPEN(10)
      WRITE(*,1) '11) Access (seq,dir,key,sp,ord)  = ',IOPEN(11)
      WRITE(*,1) '12) Form (binary,text)           = ',IOPEN(12)
*     WRITE(*,1) '13) Word format (local,IEEE,..)  = ',IOPEN(13)
*     WRITE(*,1) '14) Record separation            = ',IOPEN(14)
      WRITE(*,1) '15) NOOPEN,DEFER,STAGE,KEEP,UNIX = ',IOPEN(15)
*     WRITE(*,1) '16) Compact                      = ',IOPEN(16)
      WRITE(*,1) '17) Shift                        = ',IOPEN(17)
*     WRITE(*,1) '18) Skip corrupted events        = ',IOPEN(18)
*     WRITE(*,1) '19) DUNIT                        = ',IOPEN(19)
      WRITE(*,1) '20) FATMEN flag                  = ',IOPEN(20)
*     WRITE(*,1) '21) DSTAGE                       = ',IOPEN(21)
*     WRITE(*,1) '22) DDEFER                       = ',IOPEN(22)
*     WRITE(*,1) '23) Number of EV per file        = ',IOPEN(23)
      WRITE(*,1) '24) Remote tape server           = ',IOPEN(24)
      WRITE(*,*) '-----------------------------------------------------'
*
  100 RETURN
*
    1 FORMAT(A35,I6)
*
      END
