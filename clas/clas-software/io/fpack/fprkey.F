#ifdef HPUX
*CMZ :  1.00/00 16/12/94  17.00.00  by  Pavel Binko
*-- Author :    Volker Blobel   16/12/94
      SUBROUTINE FPRKEY
*
************************************************************************
*
*     Print current (read) record key
*
************************************************************************
*
      CHARACTER NAMKEY*8, DANAME*16, STRG*12, LINE*130
      INTEGER MCLASS(30)
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
*KEND.
*
      CALL FRQKEY(NAMKEY,NUM1,NUM2,ICLASW,IORIG,IRECNC,INEXT)
      IF(LUNR.LE.0.OR.INEXT.LT.0) GOTO 100
      DANAME=' '
      IBLA=IFRST(DANAME,-LUNR)
*     unpack classes
      MCL=0
      DO 10 I=1,30
      IF(IAND(ICLASW,1).NE.0) THEN
         MCL=MCL+1
         MCLASS(MCL)=I
      END IF
   10 ICLASW=ISHFT(ICLASW,-1)
      LINE=DANAME
      DO 15 I=1,16
      IF(DANAME(I:I).EQ.' ') GOTO 16
   15 CONTINUE
      I=17
   16 IF(I.EQ.1) THEN
         NST=3
      ELSE
         LINE(I:I)=':'
         NST=I+2
      END IF
      LINE(NST+1:NST+8)=NAMKEY
      NST=NST+9
*
      CALL PNVI(NUM1,STRG,IST)
      JST=(IST/3+1)*3
      KST=1+JST-IST
      LINE(NST+KST:NST+JST)=STRG(1:IST)
      NST=NST+JST
*
      CALL PNVI(NUM2,STRG,IST)
      JST=(IST/3+1)*3
      KST=1+JST-IST
      LINE(NST+KST:NST+JST)=STRG(1:IST)
      NST=NST+JST
*
      IF(IORIG.EQ.1) THEN
         LINE(NST+1:NST+7)=' (BOS) '
         NST=NST+7
      ELSE IF(IORIG.EQ.2) THEN
         LINE(NST+1:NST+87)=' (LOOK) '
         NST=NST+8
      END IF
*
      IF(MCL.EQ.0) THEN
         LINE(NST+1:NST+15)=' classes = NONE'
         NST=NST+15
      ELSE
         NCL=NST
         LINE(NST+1:NST+11)=' classes = '
         NST=NST+11
         DO 20 I=1,MCL
         CALL PNVI(MCLASS(I),STRG,IST)
         JST=(IST/3+1)*3
         IF(NST+JST.GT.130) THEN
            WRITE(*,101) LINE
            LINE(1:NCL)=' '
            NST=NCL+11
         END IF
         KST=1+JST-IST
         LINE(NST+KST:NST+JST)=STRG(1:IST)
         NST=NST+JST
   20    CONTINUE
      END IF
      WRITE(*,101) LINE
*
  100 RETURN
  101 FORMAT(1X,A130)
      END
#elif defined Linux
*CMZ :  1.00/00 16/12/94  17.00.00  by  Pavel Binko
*-- Author :    Volker Blobel   16/12/94
      SUBROUTINE FPRKEY
*
************************************************************************
*
*     Print current (read) record key
*
************************************************************************
*
      CHARACTER NAMKEY*8, DANAME*16, STRG*12, LINE*130
      INTEGER MCLASS(30)
*KEEP,FCOMMON.
*     DEC format (little-endian IEEE)
      PARAMETER (LOCAL=4)
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
      CALL FRQKEY(NAMKEY,NUM1,NUM2,ICLASW,IORIG,IRECNC,INEXT)
      IF(LUNR.LE.0.OR.INEXT.LT.0) GOTO 100
      DANAME=' '
      IBLA=IFRST(DANAME,-LUNR)
*     unpack classes
      MCL=0
      DO 10 I=1,30
      IF(IAND(ICLASW,1).NE.0) THEN
         MCL=MCL+1
         MCLASS(MCL)=I
      END IF
   10 ICLASW=ISHFT(ICLASW,-1)
      LINE=DANAME
      DO 15 I=1,16
      IF(DANAME(I:I).EQ.' ') GOTO 16
   15 CONTINUE
      I=17
   16 IF(I.EQ.1) THEN
         NST=3
      ELSE
         LINE(I:I)=':'
         NST=I+2
      END IF
      LINE(NST+1:NST+8)=NAMKEY
      NST=NST+9
*
      CALL PNVI(NUM1,STRG,IST)
      JST=(IST/3+1)*3
      KST=1+JST-IST
      LINE(NST+KST:NST+JST)=STRG(1:IST)
      NST=NST+JST
*
      CALL PNVI(NUM2,STRG,IST)
      JST=(IST/3+1)*3
      KST=1+JST-IST
      LINE(NST+KST:NST+JST)=STRG(1:IST)
      NST=NST+JST
*
      IF(IORIG.EQ.1) THEN
         LINE(NST+1:NST+7)=' (BOS) '
         NST=NST+7
      ELSE IF(IORIG.EQ.2) THEN
         LINE(NST+1:NST+87)=' (LOOK) '
         NST=NST+8
      END IF
*
      IF(MCL.EQ.0) THEN
         LINE(NST+1:NST+15)=' classes = NONE'
         NST=NST+15
      ELSE
         NCL=NST
         LINE(NST+1:NST+11)=' classes = '
         NST=NST+11
         DO 20 I=1,MCL
         CALL PNVI(MCLASS(I),STRG,IST)
         JST=(IST/3+1)*3
         IF(NST+JST.GT.130) THEN
            WRITE(*,101) LINE
            LINE(1:NCL)=' '
            NST=NCL+11
         END IF
         KST=1+JST-IST
         LINE(NST+KST:NST+JST)=STRG(1:IST)
         NST=NST+JST
   20    CONTINUE
      END IF
      WRITE(*,101) LINE
*
  100 RETURN
  101 FORMAT(1X,A130)
      END
#elif defined SunOS
*CMZ :  1.00/00 16/12/94  17.00.00  by  Pavel Binko
*-- Author :    Volker Blobel   16/12/94
      SUBROUTINE FPRKEY
*
************************************************************************
*
*     Print current (read) record key
*
************************************************************************
*
      CHARACTER NAMKEY*8, DANAME*16, STRG*12, LINE*130
      INTEGER MCLASS(30)
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
*KEND.
*
      CALL FRQKEY(NAMKEY,NUM1,NUM2,ICLASW,IORIG,IRECNC,INEXT)
      IF(LUNR.LE.0.OR.INEXT.LT.0) GOTO 100
      DANAME=' '
      IBLA=IFRST(DANAME,-LUNR)
*     unpack classes
      MCL=0
      DO 10 I=1,30
      IF(AND(ICLASW,1).NE.0) THEN
         MCL=MCL+1
         MCLASS(MCL)=I
      END IF
   10 ICLASW=RSHIFT(ICLASW,1)
      LINE=DANAME
      DO 15 I=1,16
      IF(DANAME(I:I).EQ.' ') GOTO 16
   15 CONTINUE
      I=17
   16 IF(I.EQ.1) THEN
         NST=3
      ELSE
         LINE(I:I)=':'
         NST=I+2
      END IF
      LINE(NST+1:NST+8)=NAMKEY
      NST=NST+9
*
      CALL PNVI(NUM1,STRG,IST)
      JST=(IST/3+1)*3
      KST=1+JST-IST
      LINE(NST+KST:NST+JST)=STRG(1:IST)
      NST=NST+JST
*
      CALL PNVI(NUM2,STRG,IST)
      JST=(IST/3+1)*3
      KST=1+JST-IST
      LINE(NST+KST:NST+JST)=STRG(1:IST)
      NST=NST+JST
*
      IF(IORIG.EQ.1) THEN
         LINE(NST+1:NST+7)=' (BOS) '
         NST=NST+7
      ELSE IF(IORIG.EQ.2) THEN
         LINE(NST+1:NST+87)=' (LOOK) '
         NST=NST+8
      END IF
*
      IF(MCL.EQ.0) THEN
         LINE(NST+1:NST+15)=' classes = NONE'
         NST=NST+15
      ELSE
         NCL=NST
         LINE(NST+1:NST+11)=' classes = '
         NST=NST+11
         DO 20 I=1,MCL
         CALL PNVI(MCLASS(I),STRG,IST)
         JST=(IST/3+1)*3
         IF(NST+JST.GT.130) THEN
            WRITE(*,101) LINE
            LINE(1:NCL)=' '
            NST=NCL+11
         END IF
         KST=1+JST-IST
         LINE(NST+KST:NST+JST)=STRG(1:IST)
         NST=NST+JST
   20    CONTINUE
      END IF
      WRITE(*,101) LINE
*
  100 RETURN
  101 FORMAT(1X,A130)
      END
#else
*CMZ :  1.00/00 16/12/94  17.00.00  by  Pavel Binko
*-- Author :    Volker Blobel   16/12/94
      SUBROUTINE FPRKEY
*
************************************************************************
*
*     Print current (read) record key
*
************************************************************************
*
      CHARACTER NAMKEY*8, DANAME*16, STRG*12, LINE*130
      INTEGER MCLASS(30)
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
*KEND.
*
      CALL FRQKEY(NAMKEY,NUM1,NUM2,ICLASW,IORIG,IRECNC,INEXT)
      IF(LUNR.LE.0.OR.INEXT.LT.0) GOTO 100
      DANAME=' '
      IBLA=IFRST(DANAME,-LUNR)
*     unpack classes
      MCL=0
      DO 10 I=1,30
      IF(IAND(ICLASW,1).NE.0) THEN
         MCL=MCL+1
         MCLASS(MCL)=I
      END IF
   10 ICLASW=ISHFT(ICLASW,-1)
      LINE=DANAME
      DO 15 I=1,16
      IF(DANAME(I:I).EQ.' ') GOTO 16
   15 CONTINUE
      I=17
   16 IF(I.EQ.1) THEN
         NST=3
      ELSE
         LINE(I:I)=':'
         NST=I+2
      END IF
      LINE(NST+1:NST+8)=NAMKEY
      NST=NST+9
*
      CALL PNVI(NUM1,STRG,IST)
      JST=(IST/3+1)*3
      KST=1+JST-IST
      LINE(NST+KST:NST+JST)=STRG(1:IST)
      NST=NST+JST
*
      CALL PNVI(NUM2,STRG,IST)
      JST=(IST/3+1)*3
      KST=1+JST-IST
      LINE(NST+KST:NST+JST)=STRG(1:IST)
      NST=NST+JST
*
      IF(IORIG.EQ.1) THEN
         LINE(NST+1:NST+7)=' (BOS) '
         NST=NST+7
      ELSE IF(IORIG.EQ.2) THEN
         LINE(NST+1:NST+87)=' (LOOK) '
         NST=NST+8
      END IF
*
      IF(MCL.EQ.0) THEN
         LINE(NST+1:NST+15)=' classes = NONE'
         NST=NST+15
      ELSE
         NCL=NST
         LINE(NST+1:NST+11)=' classes = '
         NST=NST+11
         DO 20 I=1,MCL
         CALL PNVI(MCLASS(I),STRG,IST)
         JST=(IST/3+1)*3
         IF(NST+JST.GT.130) THEN
            WRITE(*,101) LINE
            LINE(1:NCL)=' '
            NST=NCL+11
         END IF
         KST=1+JST-IST
         LINE(NST+KST:NST+JST)=STRG(1:IST)
         NST=NST+JST
   20    CONTINUE
      END IF
      WRITE(*,101) LINE
*
  100 RETURN
  101 FORMAT(1X,A130)
      END
#endif
