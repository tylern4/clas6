#ifndef Linux
*CMZU:  1.00/14 30/11/95  13.34.00  by  Peter Faulkner
*-- Author :    Stephan Egli
      SUBROUTINE FCOPY1(DAIN,DAOUT,IEND)
*
      CHARACTER*(*) DAIN,DAOUT
*
************************************************************************
*
*     Copy 1 logical records from DAIN into DAOUT.
*     IEND=-1 for EOF
*
*     Version for Centipede input and output tasks.
*
************************************************************************
*
      PARAMETER (NDIM=4000)
      INTEGER DATA(NDIM)
*
      CHARACTER NAMKEY*8, NAMDAT*8, FORMAT*180
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
      IEND=0
*
*     save read and write units ...
      LRSAVE=LR
      LWSAVE=LW
*
*     ... and define new units
      CALL FRNAME(DAIN)
      CALL FWNAME(DAOUT)
 
*     start of copy loop -----------------------------------
*
*     key
   10 CALL FRKEY2(NAMKEY,NUM1,NUM2,NDATWD,IRER)
      IF(IRER.LT.0)THEN
        CALL FWEOD
        IEND=-1
        GOTO 1000
      ENDIF
      IF(IRER.GT.0)THEN
        CALL FWEOD
        WRITE(6,*) ' ERROR in reading, FCOPY1 stopped. Ret. code =',IEND
        IEND=IRER
        GOTO 1000
      ENDIF
      CALL FWKEY(NAMKEY,NUM1,NUM2,NDATWD)
*
*     data header
   20 CALL FRHDR(NAMDAT,NR,NCOL,NROW,FORMAT,NF,IRER)
      IF(IRER.LT.0) GOTO 40
      IF(IRER.GT.0) GOTO 10
      CALL FWHDR(NAMDAT,NR,NCOL,NROW,FORMAT(1:NF))
*
*     data
   30 CALL FRDAT(NDATA,DATA,NDIM)
      CALL FWDAT(NDATA,DATA)
      IF(NDATA.EQ.NDIM) GOTO 30
      GOTO 20
   40 CALL FWEND(IWER)
*
1000  LR=LRSAVE
      LW=LWSAVE
*
      RETURN
      END
#else /* Linux */
      subroutine dummy_fcopy1
      implicit none
      return
      end
#endif /* Linux */
