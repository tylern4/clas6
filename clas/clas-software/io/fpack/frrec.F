*CMZ :  1.00/00 16/12/94  17.00.00  by  Pavel Binko
*-- Author :    Volker Blobel   16/12/94
      SUBROUTINE FRREC(LUN,NTOT,BUFFER,IEND)
*
      INTEGER BUFFER(*)
*
************************************************************************
*
*     General subroutine for reading of one physical record
*
*     IEND = -1   end-of-file
*          =  0   no error
*          =  1   read error
*
*-----------------------------------------------------------------------
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
*
      INTEGER BUFCPY(4)
      CHARACTER*72 ERRMSG
*
      IEND=1
      IF(LR.EQ.0.OR.LUNR.NE.LUN) GOTO 100
*
      IF(INFU(2,LR).EQ.1) THEN
* ------ sequential read -----------------------------------
         IF(INFU(10,LR).GT.0) THEN
*           read data file via index file
            IRCN=INFU(27,LR)
            CALL FRSEQX(LUN,IRCN,NTOT,BUFFER,IEND)
         ELSE
*           read data file
            IRCN=INFU(IRSTU+5,LR)+1
            CALL FRSEQ(LUN,IRCN,NTOT,BUFFER,IEND)
         ENDIF
      ELSE IF(INFU(2,LR).EQ.2) THEN
* ------ direct read ---------------------------------------
         IF(INFU(10,LR).GT.0) THEN
*           read data file via index file
            IRCN=INFU(27,LR)
         ELSE
*           read data file
            IRCN=INFU(IRSTU+5,LR)+1
         ENDIF
         CALL FRDIR(LUN,IRCN,NTOT,BUFFER,IEND)
      ELSE IF(INFU(2,LR).EQ.3) THEN
* ------ keyed read ----------------------------------------
         CALL FRKEC(LUN,NTOT,BUFFER,IEND)
      ELSE IF(INFU(2,LR).EQ.4) THEN
* ------ special read --------------------------------------
         NRSKIP = 0
         IRCN=INFU(IRSTU+5,LR)+1
         CALL FRSPEC(LUN,NRSKIP,IRCN,NTOT,BUFFER,IEND)
      ELSE IF(INFU(2,LR).EQ.5) THEN
* ------ ordered read --------------------------------------
         IRCN = INFU(27,LR)
         INFU(27,LR) = IRCN+1
         CALL FRDIR(LUN,IRCN,NTOT,BUFFER,IEND)
      END IF
*
      IF(IEND.NE.0) THEN
         IF(IEND.GT.0) THEN
            WRITE(ERRMSG,'(A,I10)')
     +              'Read error of physical record#',INFU(IRSTA+2,LR)+1
            CALL FPWMSG(IEND,'FRREC',ERRMSG)
         ENDIF
         GOTO 100
      ENDIF
*
*     Store actual record number
      INFU(IRSTU+5,LR)=IRCN
*
*     Swap first four words in a special buffer
      DO 90 I=1,4
   90 BUFCPY(I)=BUFFER(I)
      CALL FSWAP(BUFCPY,4)
*     Store actual format code / origin code
      INFU(IRSTU+20,LR)=BUFCPY(4)
*     Store RECL in bytes
      INFU(1,LR)=4*BUFCPY(1)
*
      IF(NTOT.EQ.12) GOTO 100
*
*     Number of physical records read
      INFU(IRSTA+2,LR)=INFU(IRSTA+2,LR)+1
      INFU(IRSTU+7,LR)=INFU(IRSTU+7,LR)+1
*
*     Statistics in case of data file
      ISZ = 4*BUFCPY(2)
      ISZ = ISZ+INFU(IRSTA+7,LR)
      INFU(IRSTA+4,LR)=INFU(IRSTA+4,LR)+ISZ/1000
      INFU(IRSTA+7,LR)=MOD(ISZ,1000)
*
*     Statistics in case of index file
      LX = INFU(10,LR)
      IF(LX.GT.0) THEN
         INFU(IRSTA+2,LX)=INFU(IRSTA+2,LX)+1
         ISZ = 4*BUFCPY(2)
         ISZ = ISZ+INFU(IRSTA+7,LX)
         INFU(IRSTA+4,LX)=INFU(IRSTA+4,LX)+ISZ/1000
         INFU(IRSTA+7,LX)=MOD(ISZ,1000)
      ENDIF
*
  100 RETURN
      END
