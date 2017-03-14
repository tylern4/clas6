*CMZ :  1.00/00 16/12/94  17.00.00  by  Pavel Binko
*-- Author :    Volker Blobel   16/12/94
      SUBROUTINE FWREC(LUN,NTOT,BUFFER,IEND)
*
      INTEGER BUFFER(*)
*
************************************************************************
*
*     General write
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
*KEEP,FBUFFER.
*     buffer common
      PARAMETER (NBUF=100000)
      COMMON/FBUF/IBUF(NBUF)
      SAVE/FBUF/
*KEND.
      CHARACTER*72 ERRMSG
*
      IEND=0
      IF(LW.EQ.0.OR.LUNW.NE.LUN) GOTO 100
      IF(INFU(2,LW).EQ.1) THEN
* ------ sequential write ----------------------------------
         INFU(IRSTU+3,LW)=INFU(IRSTU+3,LW) + 1
         IF(INFU(9,LW).NE.0) THEN
*           remote file
            CALL FWNET(LUN,0,NTOT,BUFFER,IEND)
         ELSE
*           local file
            CALL FWSEQ(LUN,NTOT,BUFFER,IEND)
         END IF
      ELSE IF(INFU(2,LW).EQ.2) THEN
* ------ direct read - determine record number -------------
         IRCN=INFU(IWSTU+3,LW)
         IF(INFU(9,LW).NE.0) THEN
*           remote file
            CALL FWNET(LUN,IRCN,NTOT,BUFFER,IEND)
         ELSE
*           local file
            CALL FWDIR(LUN,IRCN,NTOT,BUFFER,IEND)
         END IF
      ELSE IF(INFU(2,LW).EQ.3) THEN
* ------ keyed write ---------------------------------------
         CALL FWKEC(LUN,BUFFER(2),BUFFER,IEND)
      ELSE IF(INFU(2,LW).EQ.4) THEN
* ------ special write -------------------------------------
         CALL FWSPEC(LUN,NTOT,BUFFER,IEND)
      ELSE IF(INFU(2,LW).EQ.5) THEN
* ------ ordered write--------------------------------------
         IRCN = INFU(26,LW)
         IF( IRCN.GT.INFU(30,LW) ) INFU(30,LW) = IRCN
         INFU(26,LW) = IRCN+1
         IF(INFU(9,LW).NE.0) THEN
*           remote file
            CALL FWNET(LUN,IRCN,NTOT,BUFFER,IEND)
         ELSE
*           local file
            CALL FWDIR(LUN,IRCN,NTOT,BUFFER,IEND)
         END IF
      END IF
*
*     set flag "something written"
      INFU(IRSTU+11,LW)=1
*
*     write statistics
      IF(IEND.NE.0) THEN
         IF(IEND.GT.0) THEN
            WRITE(ERRMSG,'(A,I10)')
     +         'Error writing of physical record#',INFU(IWSTA+2,LW)+1
            CALL FPWMSG(IEND,'FWREC',ERRMSG)
         ENDIF
         GOTO 999
      ENDIF
*
*     number of physical records written
      INFU(IWSTA+2,LW)=INFU(IWSTA+2,LW)+1
*     number of bytes written (with information)
      ISZ = 4*BUFFER(2)
      ISZ = ISZ+INFU(IWSTA+7,LW)
      INFU(IWSTA+4,LW)=INFU(IWSTA+4,LW)+ISZ/1000
      INFU(IWSTA+7,LW)=MOD(ISZ,1000)
      GOTO 100
*
  999 WRITE(*,*) '**************************************************'
      WRITE(*,*) '*                                                *'
      WRITE(*,*) '*               Program ABORT.                   *'
      WRITE(*,*) '*        Fatal error detected in FWREC.          *'
      WRITE(*,*) '*                                                *'
      WRITE(*,*) '*            Writting not possible.              *'
      WRITE(*,*) '*                                                *'
      WRITE(*,*) '**************************************************'
      WRITE(*,*) ' Error code from the last WRITE command is :',IEND
      STOP
*
  100 RETURN
      END
