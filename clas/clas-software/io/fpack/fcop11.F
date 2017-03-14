*CMZ :  1.00/00 16/12/94  17.00.00  by  Pavel Binko
*-- Author :    Volker Blobel   16/12/94
      SUBROUTINE FCOP11(NREC,IFLG)
*
************************************************************************
*
*     1-to-1 copy routine
*
************************************************************************
*
      INTEGER JBUF(12)
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
      CALL FQCOND('PRINT',IPFLAG)
      IF(IPFLAG.GE.2) THEN
         WRITE(*,*) ' -------------------------------------------'
         WRITE(*,*) ' '
         WRITE(*,*) ' FCOP11. Using fast 1-to-1 copy program'
         WRITE(*,*) ' It copies ONE input file into ONE output file'
         WRITE(*,*) ' Change of RECL and SELECTions are NOT possible'
         WRITE(*,*) ' '
         WRITE(*,*) ' -------------------------------------------'
      END IF
*
      NREC=0
      IFLG=0
      IF(LR.EQ.0.OR.LW.EQ.0) GOTO 90
*
      NTOT = INFU(1,LR)/4
*     get input buffer bank
      INDR=INDXB(IBUF,1000*LUNR+1)
      IF(INDR.EQ.0) THEN
         IP=0
         CALL FRREC(LUNR,12,JBUF,IEND)
         CALL FSWAP(JBUF,12)
         INFU(IRSTU+2,LR)=IEND
         INFU(IRSTU+3,LR)=0
         IF(IEND.EQ.0) GOTO 20
         IF(IEND.LT.0) THEN
*           general status is: end-of-file at reading
            INFU(ISGEN+1,LR)=2
            GOTO 90
         END IF
*        read error
         GOTO 90
   20    CALL FURWND(LUNR)
         INFU(ISGEN+1,LR)=1
         NTOT=JBUF(1)
*        create bank with correct bank length
         INDR=INDXA(IBUF,1000*LUNR+1,NTOT)
      END IF
*
*     no output buffer !
      INFU(1,LR)=NTOT*4
      INFU(1,LW)=NTOT*4
*
*     read next physical record (evtl skip before)
   80 CALL FRREC(LUNR,IBUF(INDR),IBUF(INDR+1),IEND)
      INFU(IRSTU+2,LR)=IEND
      INFU(IRSTU+3,LR)=0
      IF(IEND.LT.0) GOTO 100
      IF(IEND.GT.0) GOTO 90
*
*     write record
      CALL FWREC(LUNW,IBUF(INDR+1),IBUF(INDR+1),IEND)
      NREC=NREC+1
      IF(MOD(NREC,3).EQ.0) THEN
         CALL FQCOND('TIME',ITFLAG)
         IF(ITFLAG.NE.0) THEN
            IF(IPFLAG.GE.0) THEN
               WRITE(*,*) ' -------------------------------------------'
               WRITE(*,*) ' '
               WRITE(*,*) ' FCOP11. Stop - time limit reached'
               WRITE(*,*) ' '
               WRITE(*,*) ' -------------------------------------------'
            END IF
            GOTO 90
         END IF
      END IF
      GOTO 80
*
   90 IFLG=1
*
  100 RETURN
      END
