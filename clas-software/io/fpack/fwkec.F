#ifdef HPUX
*CMZ :  1.00/00 16/12/94  17.00.00  by  Pavel Binko
*-- Author :    Volker Blobel   16/12/94
      SUBROUTINE FWKEC(LUN,NTOT,BUFFER,IEND)
*
      INTEGER BUFFER(NTOT)
*
************************************************************************
*
*     Keyed write, overwriting records with duplicate key
*
************************************************************************
*
      INTEGER DUMMY(12)
      CHARACTER KEY*20, TITLE*8
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
      IEND=0
      INF8 = INFU(8,LUNIT(LUN))
      INF9 = INFU(9,LUNIT(LUN))
*
      IF(INF9.EQ.0) THEN
*        local file
*        local keyed access not yet implemented
         GOTO 90
      ELSE
*        remote file
         CALL KWRITF(INF9,INF8,4*NTOT,BUFFER,NTOT,IOS)
         IF(IOS.EQ.2) GOTO 20
      END IF
      IF(IOS.NE.0) IEND=1
      GOTO 100
*     overwrite record with duplicate key ...
   20 CALL TRAKEY('HU',KEY,BUFFER(6),TITLE,NR1,NR2,NRC)
      IF(NRC.NE.0) GOTO 90
      CALL TRAKEY('UC',KEY,DUMMY,TITLE,NR1,NR2,NRC)
      IF(INF9.EQ.0) THEN
*        local file
*        local keyed access not yet implemented
         GOTO 90
      ELSE
*        remote file
         CALL KREADF(INF9,INF8,48,DUMMY,12,1,KEY,IRET)
         IF(IRET.EQ.2) GOTO 90
         CALL KREWF(INF9,INF8,4*NTOT,BUFFER,NTOT,IOS)
         IF(IOS.EQ.2) GOTO 90
      END IF
*     now delete existing records with same key and different order flag
   30 NRC=NRC+1
      CALL TRAKEY('UC',KEY,DUMMY,TITLE,NR1,NR2,NRC)
      IF(INF9.EQ.0) THEN
*        local file
*        local keyed access not yet implemented
         GOTO 90
      ELSE
*        remote file
         CALL KREADF(INF9,INF8,48,DUMMY,12,1,KEY,IRET)
         IF(IRET.EQ.2) GOTO 100
         CALL KDELF(INF9,INF8,IOS)
      END IF
      GOTO 30
*
   90 IEND=1
*
  100 RETURN
      END
#elif defined Linux
*CMZ :  1.00/00 16/12/94  17.00.00  by  Pavel Binko
*-- Author :    Volker Blobel   16/12/94
      SUBROUTINE FWKEC(LUN,NTOT,BUFFER,IEND)
*
      INTEGER BUFFER(NTOT)
*
************************************************************************
*
*     Keyed write, overwriting records with duplicate key
*
************************************************************************
*
      INTEGER DUMMY(12)
      CHARACTER KEY*20, TITLE*8
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
      IEND=0
      INF8 = INFU(8,LUNIT(LUN))
      INF9 = INFU(9,LUNIT(LUN))
*
      IF(INF9.EQ.0) THEN
*        local file
*        local keyed access impossible
         GOTO 90
      ELSE
*        remote file
         CALL KWRITF(INF9,INF8,4*NTOT,BUFFER,NTOT,IOS)
         IF(IOS.EQ.2) GOTO 20
      END IF
      IF(IOS.NE.0) IEND=1
      GOTO 100
*     overwrite record with duplicate key ...
   20 CALL TRAKEY('HU',KEY,BUFFER(6),TITLE,NR1,NR2,NRC)
      IF(INF9.NE.0) THEN
*     swap character part of the key (for remote writing, the remote
*     word format must be used i.e. WFIBM in the current implementation
*     and TRAKEY HU gives swapped key in this case! )
*     KEY is used here as temporary storage only.
        KEY(1:8)=TITLE
        TITLE(1:1)=KEY(4:4)
        TITLE(2:2)=KEY(3:3)
        TITLE(3:3)=KEY(2:2)
        TITLE(4:4)=KEY(1:1)
        TITLE(5:5)=KEY(8:8)
        TITLE(6:6)=KEY(7:7)
        TITLE(7:7)=KEY(6:6)
        TITLE(8:8)=KEY(5:5)
      ENDIF
      IF(NRC.NE.0) GOTO 90
      CALL TRAKEY('UC',KEY,DUMMY,TITLE,NR1,NR2,NRC)
      IF(INF9.EQ.0) THEN
*        local file
*        local keyed access impossible
         GOTO 90
      ELSE
*        remote file
         CALL KREADF(INF9,INF8,48,DUMMY,12,1,KEY,IRET)
         IF(IRET.EQ.2) GOTO 90
         CALL KREWF(INF9,INF8,4*NTOT,BUFFER,NTOT,IOS)
         IF(IOS.EQ.2) GOTO 90
      END IF
*     now delete existing records with same key and different order flag
   30 NRC=NRC+1
      CALL TRAKEY('UC',KEY,DUMMY,TITLE,NR1,NR2,NRC)
      IF(INF9.EQ.0) THEN
*        local file
*        local keyed access impossible
         GOTO 90
      ELSE
*        remote file
         CALL KREADF(INF9,INF8,48,DUMMY,12,1,KEY,IRET)
         IF(IRET.EQ.2) GOTO 100
         CALL KDELF(INF9,INF8,IOS)
      END IF
      GOTO 30
*
   90 IEND=1
*
  100 RETURN
      END
#elif defined SunOS
*CMZ :  1.00/00 16/12/94  17.00.00  by  Pavel Binko
*-- Author :    Volker Blobel   16/12/94
      SUBROUTINE FWKEC(LUN,NTOT,BUFFER,IEND)
*
      INTEGER BUFFER(NTOT)
*
************************************************************************
*
*     Keyed write, overwriting records with duplicate key
*
************************************************************************
*
      INTEGER DUMMY(12)
      CHARACTER KEY*20, TITLE*8
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
      IEND=0
      INF8 = INFU(8,LUNIT(LUN))
      INF9 = INFU(9,LUNIT(LUN))
*
      IF(INF9.EQ.0) THEN
*        local file
*        local keyed access impossible
         GOTO 90
      ELSE
*        remote file
         CALL KWRITF(INF9,INF8,4*NTOT,BUFFER,NTOT,IOS)
         IF(IOS.EQ.2) GOTO 20
      END IF
      IF(IOS.NE.0) IEND=1
      GOTO 100
*     overwrite record with duplicate key ...
   20 CALL TRAKEY('HU',KEY,BUFFER(6),TITLE,NR1,NR2,NRC)
      IF(NRC.NE.0) GOTO 90
      CALL TRAKEY('UC',KEY,DUMMY,TITLE,NR1,NR2,NRC)
      IF(INF9.EQ.0) THEN
*        local file
*        local keyed access impossible
         GOTO 90
      ELSE
*        remote file
         CALL KREADF(INF9,INF8,48,DUMMY,12,1,KEY,IRET)
         IF(IRET.EQ.2) GOTO 90
         CALL KREWF(INF9,INF8,4*NTOT,BUFFER,NTOT,IOS)
         IF(IOS.EQ.2) GOTO 90
      END IF
*     now delete existing records with same key and different order flag
   30 NRC=NRC+1
      CALL TRAKEY('UC',KEY,DUMMY,TITLE,NR1,NR2,NRC)
      IF(INF9.EQ.0) THEN
*        local file
*        local keyed access impossible
         GOTO 90
      ELSE
*        remote file
         CALL KREADF(INF9,INF8,48,DUMMY,12,1,KEY,IRET)
         IF(IRET.EQ.2) GOTO 100
         CALL KDELF(INF9,INF8,IOS)
      END IF
      GOTO 30
*
   90 IEND=1
*
  100 RETURN
      END
#else
*CMZ :  1.00/00 16/12/94  17.00.00  by  Pavel Binko
*-- Author :    Volker Blobel   16/12/94
      SUBROUTINE FWKEC(LUN,NTOT,BUFFER,IEND)
*
      INTEGER BUFFER(NTOT)
*
************************************************************************
*
*     Keyed write, overwriting records with duplicate key
*
************************************************************************
*
      INTEGER DUMMY(12)
      CHARACTER KEY*20, TITLE*8
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
      IEND=0
      INF8 = INFU(8,LUNIT(LUN))
      INF9 = INFU(9,LUNIT(LUN))
*
      IF(INF9.EQ.0) THEN
*        local file
*        local keyed access not yet implemented
         GOTO 90
      ELSE
*        remote file
         CALL KWRITF(INF9,INF8,4*NTOT,BUFFER,NTOT,IOS)
         IF(IOS.EQ.2) GOTO 20
      END IF
      IF(IOS.NE.0) IEND=1
      GOTO 100
*     overwrite record with duplicate key ...
   20 CALL TRAKEY('HU',KEY,BUFFER(6),TITLE,NR1,NR2,NRC)
      IF(NRC.NE.0) GOTO 90
      CALL TRAKEY('UC',KEY,DUMMY,TITLE,NR1,NR2,NRC)
      IF(INF9.EQ.0) THEN
*        local file
*        local keyed access not yet implemented
         GOTO 90
      ELSE
*        remote file
         CALL KREADF(INF9,INF8,48,DUMMY,12,1,KEY,IRET)
         IF(IRET.EQ.2) GOTO 90
         CALL KREWF(INF9,INF8,4*NTOT,BUFFER,NTOT,IOS)
         IF(IOS.EQ.2) GOTO 90
      END IF
*     now delete existing records with same key and different order flag
   30 NRC=NRC+1
      CALL TRAKEY('UC',KEY,DUMMY,TITLE,NR1,NR2,NRC)
      IF(INF9.EQ.0) THEN
*        local file
*        local keyed access not yet implemented
         GOTO 90
      ELSE
*        remote file
         CALL KREADF(INF9,INF8,48,DUMMY,12,1,KEY,IRET)
         IF(IRET.EQ.2) GOTO 100
         CALL KDELF(INF9,INF8,IOS)
      END IF
      GOTO 30
*
   90 IEND=1
*
  100 RETURN
      END
#endif
