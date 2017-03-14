*CMZ :  1.00/00 16/12/94  17.00.00  by  Pavel Binko
*-- Author :    Volker Blobel   16/12/94
      SUBROUTINE FPCPIX(DAIN,DAOUT,IERR)
*
      CHARACTER*(*) DAIN,DAOUT
*
************************************************************************
*
*     copy keyes from DAIN into index DAOUT
*     returned flag IERR
*
************************************************************************
*
      CHARACTER*8 RECNAME
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
*KEEP,FBUFFER.
*     buffer common
      PARAMETER (NBUF=100000)
      COMMON/FBUF/IBUF(NBUF)
      SAVE/FBUF/
*KEND.
*
      IERR = 1
      NN=0
      CALL FQCOND('PRINT',IPFLAG)
      IF(IPFLAG.GE.2) THEN
         WRITE(*,101) DAIN,DAOUT
  101    FORMAT('0CALL IDXCOPY(',A16,20X,'! input  file '/
     +                       12X,A16,20X,'! output file '/
     +                  22X,'IFLAG)',20X,'! return flag')
      END IF
*
*     save read and write units ...
      LRSAVE=LR
      LWSAVE=LW
      LUNRSV=LUNR
      LUNWSV=LUNW
*
*     ... and define new units
      CALL FRNAME(DAIN)
      CALL FWNAME(DAOUT)
      IF( LR.EQ.0 .OR. LW.EQ.0 ) THEN
         IF(IPFLAG.GE.0) THEN
            WRITE(*,*) ' *******************************************'
            WRITE(*,*) ' '
            IF(LR.EQ.0)
     +         WRITE(*,*) ' IDXCOPY. Error: Input file not opened'
            IF(LW.EQ.0)
     +         WRITE(*,*) ' IDXCOPY. Error: Output file not opened'
            WRITE(*,*) ' '
            WRITE(*,*) ' *******************************************'
         END IF
         GOTO 80
      END IF
*
*     start of copy loop -----------------------------------
*
70    CALL FRKEY(RECNAME,NR1,NR2,NDATW,IERR)
      IF( IERR.NE.0 ) THEN
         IF(IERR.LT.0)IERR = 0
         GOTO 80
      ENDIF
      CALL FPWRIX(DAIN,DAOUT,RECNAME,NR1,NR2,NDATW,IERR)
      IF( IERR.NE.0 ) THEN
         WRITE(*,*) ' IDXCOPY. Error during writing the index file'
         IERR = 1
         GOTO 80
      ENDIF
      GOTO 70
*
*     end of copy loop -------------------------------------
*
   80 IF(IPFLAG.GE.2) THEN
            WRITE(*,103) NN,'Logical records copied'
  103    FORMAT('0',I16,1X,A)
         CALL FPRFLS
      END IF
*
      LR=LRSAVE
      LW=LWSAVE
      LUNR=LUNRSV
      LUNW=LUNWSV
*
  100 RETURN
      END
