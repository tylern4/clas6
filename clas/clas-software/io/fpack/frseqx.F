*CMZ :  1.00/01 16/01/95  16.04.51  by  Unknown
*CMZ :  1.00/00 16/12/94  17.00.00  by  Pavel Binko
*-- Author :    Volker Blobel   16/12/94
      SUBROUTINE FRSEQX(LUN,IRCN,NTOT,BUFFER,IEND)
*
      INTEGER BUFFER(NTOT)
*
************************************************************************
*
*     Sequential read of one physical record in a sequential file
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
*KEND.
*
      CHARACTER*72 ERRMSG
*
      IEND = 0
      LX = LUNIT(LUN)
*
      NRSKIP = IRCN - INFU(14,LX) - 1
      IF(NRSKIP.LT.0) THEN
         CALL FRWSEQ(LUN)
         NRSKIP = IRCN - 1
      ENDIF
*
      IF(INFU(9,LX).NE.0) THEN
*        Remote file
         CALL FRNET(LUN,NRSKIP,NTOT,BUFFER,IEND)
      ELSE
*        Local file
         IF(INFU(8,LX).NE.-1) THEN
*           Local file opened from C
            DO 3 I=1,NRSKIP
               CALL CGET(INFU(8,LX),0,INFU(15,LX),NTOT,BUFFER,ISTAT)
               IF(ISTAT.EQ.-1) GOTO 20
               IF(ISTAT.NE.0) GOTO 10
    3       CONTINUE
         ELSE
*           Local file opened from FORTRAN
            IF(NRSKIP.GT.0) THEN
               DO 5 I=1,NRSKIP
    5          READ(UNIT=LUN,ERR=10,END=20,IOSTAT=ISTAT)
            ENDIF
            READ(UNIT=LUN,ERR=10,END=20,IOSTAT=ISTAT) BUFFER
         END IF
      END IF
*
*     Successful read operation
*     INFU(14,LX) = physical record number (from input parameter)
      INFU(14,LX) = IRCN
      GOTO 100
*
*     Error exit
   10 IEND = 1
      WRITE(ERRMSG,'(A,I5)') 'Read error. IOSTAT=',ISTAT
      CALL FPWMSG(IEND,'FRSEQX',ERRMSG)
      GOTO 100
*
*     End-of-file exit
   20 IEND = -1
*
  100 RETURN
      END
