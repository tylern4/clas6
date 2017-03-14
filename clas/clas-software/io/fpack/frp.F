#ifndef Linux
*CMZ :  1.00/14 02/03/96  15.13.22  by  Wiesiek Dlugosz
#endif /* not Linux */
*CMZ :  1.00/00 16/12/94  17.00.00  by  Pavel Binko
*-- Author :    Volker Blobel   16/12/94
      SUBROUTINE FRP
*
************************************************************************
*
*     reading test print program
*
************************************************************************
*
      PARAMETER (NDIMA=1000)
      REAL A(NDIMA)
#ifndef Linux
      CHARACTER*8 NAME, CLASSES, FORMAT*100
#else /* Linux */
      CHARACTER*8 NAME, FORMAT*100
#endif /* Linux */
      DATA NPRI/3/
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
*
   10 NAME='_'
      CALL FRKEY(NAME,NR1,NR2,NDATWD,IERR)
      WRITE(6,109)
      IF(IERR.NE.0) THEN
         WRITE(6,105) 'FRKEY',IERR
         IF(IERR.GT.0) GOTO 10
         GOTO 100
      END IF
#ifndef Linux
      CALL PNVZ(NDATWD, CLASSES, 8)
      WRITE(6,101) NAME,NR1,NR2,CLASSES
#else /* Linux */
      WRITE(6,101) NAME,NR1,NR2,NDATWD
#endif /* Linux */
   20 NAME='_'
      CALL FRHDR(NAME,NR,NCOL,NROW,FORMAT,NCH,IERR)
      IF(IERR+1.EQ.0) GOTO 10
      IF(IERR.NE.0) THEN
         WRITE(6,105) 'FRHDR',IERR
         GOTO 10
      END IF
      WRITE(6,102) NAME,NR,NCOL,NROW,FORMAT(1:NCH)
   30 CALL FRDAT(NR,A,NDIMA)
      IF(NR.EQ.0) GOTO 20
      IF(NPRI.GT.0) THEN
         WRITE(6,104) NR
         NPRI=NPRI-1
C        CALL DUMPAR(A,1,10)
C        CALL DUMPAR(A,1,NR)
      END IF
      IF(NR.EQ.NDIMA) GOTO 30
      GOTO 20
*     GOTO 10
*
  100 RETURN
#ifndef Linux
  101 FORMAT(' Record-key = ',A8,2I8,4X,'Dataword =',A8)
#else /* Linux */
  101 FORMAT(' Record-key = ',A8,2I8,4X,'Dataword =',Z12)
#endif /* Linux */
  102 FORMAT('       Bank = ',A8,I8,12X,'NcolNrow =',2I6,' ',A)
C 103 FORMAT('              ',A)
  104 FORMAT('              ',I8,' data words read')
  105 FORMAT('          =-> ',A5,' returned with error',I3)
  109 FORMAT(1X)
      END
