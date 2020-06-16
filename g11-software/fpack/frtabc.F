*CMZ :  1.00/00 16/12/94  17.00.00  by  Pavel Binko
*-- Author :    Volker Blobel   16/12/94
      SUBROUTINE FRTABC(DANAME)
*
      CHARACTER*(*) DANAME
*
************************************************************************
*
*     Print table of content
*
************************************************************************
*
      CHARACTER NAMKEY*8, TITLE*8
      INTEGER ITABLE(2,10)
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
      WRITE(*,101) DANAME
*     save current R value LUNR, LR, restore later
      LRS  =LR
      LUNRS=LUNR
      CALL FRNAME(DANAME)
*     check undefined unit
      IF(LR.EQ.0.OR.LUNR.EQ.0) THEN
         WRITE(*,102) 'file not existing'
         GOTO 90
      END IF
      N=0
   10 CALL FRKEY(NAMKEY,NR1,NR2,NDATWD,IERR)
      IF(N.EQ.0) THEN
*        store name
         TITLE=NAMKEY
         WRITE(*,103) NAMKEY
      ELSE
         IF(IERR.NE.0.OR.N.EQ.10.OR.TITLE.NE.NAMKEY) THEN
*           print previous values
            IF(N.NE.0) WRITE(*,104) ((ITABLE(I,J),I=1,2),J=1,N)
            N=0
         END IF
      END IF
      IF(IERR.NE.0) GOTO 80
      IF(TITLE.NE.NAMKEY) THEN
         WRITE(*,103) NAMKEY
         TITLE=NAMKEY
      END IF
      N=N+1
      ITABLE(1,N)=NR1
      ITABLE(2,N)=NR2
      GOTO 10
*     restore
   80 WRITE(*,102) '... end of printout.'
   90 LR  =LRS
      LUNR=LUNRS
*
  100 RETURN
  101 FORMAT('0FRTABC called with symbolic filename ',A)
  102 FORMAT(12X,A/)
  103 FORMAT('0 ',A8,2X,5(6X,'NUM1',6X,'NUM2',4X)/
     +       '  ========  ',5(6X,'--------------',4X))
  104 FORMAT((12X,5(2I10,4X)))
      END
