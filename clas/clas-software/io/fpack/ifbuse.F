#ifndef Linux
*CMZ :  1.00/16 11/03/96  16.48.17  by  Sergey Esenov
*-- Author :    Sergey Esenov   11/03/96
      INTEGER FUNCTION IFBUSE(IFLAG)
*
*     returns the number of physical records used in ordered file
*
*     If IFLAG = 0   - this number is taken from top index block
*        IFLAG <> 0  - this number is counted by real reading of a file.
*
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
      IFBUSE = 0
*
      IF(LR.EQ.0.OR.LUNR.EQ.0) GOTO 100
      IF(INFU(25,LR).EQ.0)     GOTO 100
 
*
      IF(IFLAG.NE.0) GOTO 10
*
      INX = INDXB(IBUF,1000*LUNR+9+1)
      IF(INX.LE.0) CALL LOGERROR('IFBUSE. Top index buffer not found')
      IFBUSE = IBUF(INX+18)
      GOTO 100
*
10    ISV27 = INFU(27,LR)
      INR = INDXB(IBUF,1000*LUNR+888)
      IF(INR.LE.0) THEN
         INR = INDXA(IBUF,1000*LUNR+888,INFU(1,LR)/4)
         IF(INR.EQ.0)
     +     CALL LOGERROR('IFBUSE. No space available for buffer')
      ENDIF
*
20    INFU(27,LR)=IFBUSE+1
      CALL FRRECX(LUNR,IBUF(INR),IBUF(INR+1),IEND)
      IF(IEND.LT.0) THEN
*        EOF encountered ...
         INFU(27,LR)=ISV27
         GOTO 100
      ENDIF
      IFBUSE = IFBUSE+1
      GOTO 20
*
100   RETURN
      END
#else /* Linux */
      subroutine dummy_ifbuse
      implicit none
      return
      end
#endif /* Linux */
