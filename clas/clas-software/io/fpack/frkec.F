#ifdef HPUX
*CMZ :  1.00/00 16/12/94  17.00.00  by  Pavel Binko
*-- Author :    Volker Blobel   16/12/94
      SUBROUTINE FRKEC(LUN,NTOT,BUFFER,IEND)
*
      INTEGER BUFFER(NTOT)
*
************************************************************************
*
*     Read keyed access
*
************************************************************************
*
      INTEGER IDUM(12)
      CHARACTER KEY*20, NAMKEY*8
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
      IEND = 1
      INF8 = INFU(8,LR)
      INF9 = INFU(9,LR)
*
*     NTOT=12 is special case (first read access to the file)
      IF(NTOT.EQ.12) THEN
         IF(INF9.EQ.0) THEN
*           local file
            WRITE(*,*) ' local keyed access not yet implemented'
            GOTO 90
         ELSE
*           remote file
            CALL REWINF(INF9,INF8,IRET)
            CALL KREADF(INF9,INF8,4*NTOT,BUFFER,NTOT,0,' ',IOS)
            IF(IOS.EQ.3) RETURN
            IF(IOS.EQ.1) GOTO 90
         END IF
         IEND=0
         GOTO 100
      END IF
*
      IF(INFU(IRSTU+11,LR).NE.0) THEN
         INFU(IRSTU+11,LR)=0
*
*        reposition record key after some disturbing operation
         IF(INFU(IRSTU+12,LR).EQ.2) THEN
            INFU(IRSTU+13,LR)=0
*           new position/direction requested by call
            CALL TRAKEY('HC',KEY,INFU(IRSTU+14,LR),NAMKEY,NUM1,NUM2,NRD)
            IF(INF9.EQ.0) THEN
*              local file
               WRITE(*,*) ' local keyed access not yet implemented'
               GOTO 90
            ELSE
*              remote file
               CALL KREADF(INF9,INF8,4*NTOT,BUFFER,NTOT,2,KEY,IOS)
               IF(IOS.EQ.3) RETURN
               IF(IOS.EQ.2) GOTO 90
            END IF
            INFU(IRSTU+12,LR)=1
            GOTO 69
*
         ELSE IF(INFU(IRSTU+12,LR).EQ.-2) THEN
            INFU(IRSTU+13,LR)=0
*           new position/direction requested by call
            CALL TRAKEY('HC',KEY,INFU(IRSTU+14,LR),NAMKEY,NUM1,NUM2,NRD)
            IF(INF9.EQ.0) THEN
*              local file
               WRITE(*,*) ' local keyed access not yet implemented'
               GOTO 90
            ELSE
*              remote file
               CALL KREADF(INF9,INF8,4*NTOT,BUFFER,NTOT,2,KEY,IOS)
               IF(IOS.EQ.3) RETURN
               IF(IOS.EQ.2) GOTO 30
            END IF
            NEQ=0
            DO 13 I=1,5
            IF(INFU(IRSTU+13+I,LR).NE.BUFFER(5+I)) NEQ=1
*           NEQ = 1 for notequal keys
   13       INFU(IRSTU+13+I,LR)=BUFFER(5+I)
            INFU(IRSTU+12,LR)=-1
            IF(NEQ.EQ.0) GOTO 69
            GOTO 50
*           for KEY not found while direction backward
   30       INFU(IRSTU+12,LR) = -1
            INFU(IRSTU+13,LR) =  0
            INFU(IRSTU+14,LR) =  0
            INFU(IRSTU+15,LR) =  0
            INFU(IRSTU+16,LR) =  0
            INFU(IRSTU+17,LR) =  0
            INFU(IRSTU+18,LR) = -1
            INFU(IRSTU+19,LR) =  0
            GOTO 50
*
         ELSE IF(INFU(IRSTU+13,LR).EQ.1) THEN
*           rewind done before - reset
            DO 10 I=1,10
   10       INFU(IRSTU+10+I,LR)=0
*
         ELSE
*           some IO since last read - reposition
            CALL TRAKEY('HC',KEY,INFU(IRSTU+14,LR),NAMKEY,NUM1,NUM2,NRD)
            IF(INF9.EQ.0) THEN
*              local file
               WRITE(*,*) ' local keyed access not yet implemented'
               GOTO 90
            ELSE
*              remote file
               CALL KREADF(INF9,INF8,48,IDUM,12,2,KEY,IOS)
               IF(IOS.EQ.3) RETURN
               IF(IOS.EQ.2) GOTO 90
            END IF
            DO 11 I=1,5
   11       INFU(IRSTU+13+I,LR)=IDUM(5+I)
         END IF
*
      END IF
*
*     now adjust position in case of backward
   50 IF(INFU(IRSTU+19,LR).EQ.0.AND.INFU(IRSTU+12,LR).LT.0) THEN
*        backward reading from last accessed key
         NBACK=2+INFU(IRSTU+18,LR)
*        ... space back before first record from last access
   40    IF(NBACK.GT.0) THEN
            IF(INF9.EQ.0) THEN
*              local file
               WRITE(*,*) ' local keyed access not yet implemented'
               GOTO 90
            ELSE
*              remote file
               CALL KBACKF(INF9,INF8,IOS)
               IF(IOS.NE.0) RETURN
            END IF
            NBACK=NBACK-1
            GOTO 40
         END IF
*        read a record ...
         IF(INF9.EQ.0) THEN
*           local file
            WRITE(*,*) ' local keyed access not yet implemented'
            GOTO 90
         ELSE
*           remote file
            CALL KREADF(INF9,INF8,48,IDUM,12,0,' ',IOS)
            IF(IOS.EQ.3) RETURN
            IF(IOS.EQ.1) GOTO 90
         END IF
*        compare key just read with previous key - end reached?
         NEQ=0
         DO 45 I=1,4
         IF(INFU(IRSTU+13+I,LR).NE.IDUM(5+I)) NEQ=1
   45    CONTINUE
*        store the key just read
         DO 47 I=1,5
   47    INFU(IRSTU+13+I,LR)=IDUM(5+I)
         IF(NEQ.EQ.0) GOTO 90
         CALL TRAKEY('HU',KEY,IDUM(6),NAMKEY,NUM1,NUM2,NRD)
*        ... and space back before first part
         NBACK=1+NRD
   60    IF(NBACK.GT.0) THEN
            IF(INF9.EQ.0) THEN
*              local file
               WRITE(*,*) ' local keyed access not yet implemented'
               GOTO 90
            ELSE
*              remote file
               CALL KBACKF(INF9,INF8,IOS)
               IF(IOS.NE.0) RETURN
            END IF
            NBACK=NBACK-1
            GOTO 60
         END IF
      END IF
*
*     sequential retrieval ...
      IF(INF9.EQ.0) THEN
*        local file
         WRITE(*,*) ' local keyed access not yet implemented'
         GOTO 90
      ELSE
*        remote file
         CALL KREADF(INF9,INF8,4*NTOT,BUFFER,NTOT,0,' ',IOS)
         IF(IOS.EQ.3) RETURN
         IF(IOS.EQ.1) GOTO 90
      END IF
*
*     ... and store record key just read
   69 DO 70 I=1,5
   70 INFU(IRSTU+13+I,LR)=BUFFER(5+I)
*
*     set "notfirst" flag
      INFU(IRSTU+19,LR)=1
      IEND=0
      GOTO 100
*
*     end-of-data
   90 CONTINUE
      IEND=-1
*
  100 RETURN
      END
#elif defined Linux
*CMZ :  1.00/00 16/12/94  17.00.00  by  Pavel Binko
*-- Author :    Volker Blobel   16/12/94
      SUBROUTINE FRKEC(LUN,NTOT,BUFFER,IEND)
*
      INTEGER BUFFER(NTOT)
*
************************************************************************
*
*     Read keyed access
*
************************************************************************
*
      INTEGER IDUM(12)
      CHARACTER KEY*20, NAMKEY*8
      INTEGER NEWKEY(5)
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
      IEND = 1
      INF8 = INFU(8,LR)
      INF9 = INFU(9,LR)
*
*     NTOT=12 is special case (first read access to the file)
      IF(NTOT.EQ.12) THEN
         IF(INF9.EQ.0) THEN
*           local file
            WRITE(*,*) ' local keyed access impossible'
            GOTO 90
         ELSE
*           remote file
            CALL REWINF(INF9,INF8,IRET)
            CALL KREADF(INF9,INF8,4*NTOT,BUFFER,NTOT,0,' ',IOS)
            IF(IOS.EQ.3) RETURN
            IF(IOS.EQ.1) GOTO 90
         END IF
         IEND=0
         GOTO 100
      END IF
*
      IF(INFU(IRSTU+11,LR).NE.0) THEN
         INFU(IRSTU+11,LR)=0
*
*        reposition record key after some disturbing operation
         IF(INFU(IRSTU+12,LR).EQ.2) THEN
            INFU(IRSTU+13,LR)=0
*           new position/direction requested by call
            CALL TRAKEY('HC',KEY,INFU(IRSTU+14,LR),NAMKEY,NUM1,NUM2,NRD)
            IF(INF9.EQ.0) THEN
*              local file
               WRITE(*,*) ' local keyed access impossible'
               GOTO 90
            ELSE
*              remote file
               CALL KREADF(INF9,INF8,4*NTOT,BUFFER,NTOT,2,KEY,IOS)
               IF(IOS.EQ.3) RETURN
               IF(IOS.EQ.2) GOTO 90
            END IF
            INFU(IRSTU+12,LR)=1
            IF(INF9.EQ.0) THEN
               DO 91 I=1,5
   91          NEWKEY(I)=BUFFER(5+I)
            ELSE
               CALL KEYSWAP(BUFFER(6),NEWKEY)
            END IF
            GOTO 69
*
         ELSE IF(INFU(IRSTU+12,LR).EQ.-2) THEN
            INFU(IRSTU+13,LR)=0
*           new position/direction requested by call
            CALL TRAKEY('HC',KEY,INFU(IRSTU+14,LR),NAMKEY,NUM1,NUM2,NRD)
            IF(INF9.EQ.0) THEN
*              local file
               WRITE(*,*) ' local keyed access impossible'
               GOTO 90
            ELSE
*              remote file
               CALL KREADF(INF9,INF8,4*NTOT,BUFFER,NTOT,2,KEY,IOS)
               IF(IOS.EQ.3) RETURN
               IF(IOS.EQ.2) GOTO 30
            END IF
            NEQ=0
            IF(INF9.EQ.0) THEN
               DO 12 I=1,5
   12          NEWKEY(I)=BUFFER(5+I)
            ELSE
               CALL KEYSWAP(BUFFER(6),NEWKEY)
            END IF
            DO 13 I=1,5
            IF(INFU(IRSTU+13+I,LR).NE.NEWKEY(I)) NEQ=1
*           NEQ = 1 for notequal keys
   13       INFU(IRSTU+13+I,LR)=NEWKEY(I)
            INFU(IRSTU+12,LR)=-1
            IF(NEQ.EQ.0) GOTO 69
            GOTO 50
*           for KEY not found while direction backward
   30       INFU(IRSTU+12,LR) = -1
            INFU(IRSTU+13,LR) =  0
            INFU(IRSTU+14,LR) =  0
            INFU(IRSTU+15,LR) =  0
            INFU(IRSTU+16,LR) =  0
            INFU(IRSTU+17,LR) =  0
            INFU(IRSTU+18,LR) = -1
            INFU(IRSTU+19,LR) =  0
            GOTO 50
*
         ELSE IF(INFU(IRSTU+13,LR).EQ.1) THEN
*           rewind done before - reset
            DO 10 I=1,10
   10       INFU(IRSTU+10+I,LR)=0
*
         ELSE
*           some IO since last read - reposition
            CALL TRAKEY('HC',KEY,INFU(IRSTU+14,LR),NAMKEY,NUM1,NUM2,NRD)
            IF(INF9.EQ.0) THEN
*              local file
               WRITE(*,*) ' local keyed access impossible'
               GOTO 90
            ELSE
*              remote file
               CALL KREADF(INF9,INF8,48,IDUM,12,2,KEY,IOS)
               IF(IOS.EQ.3) RETURN
               IF(IOS.EQ.2) GOTO 90
            END IF
            IF(INF9.EQ.0) THEN
               DO 16 I=1,5
   16          INFU(IRSTU+13+I,LR)=IDUM(5+I)
            ELSE
               CALL KEYSWAP(IDUM(6),NEWKEY)
               DO 11 I=1,5
   11          INFU(IRSTU+13+I,LR)=NEWKEY(I)
            END IF
         END IF
*
      END IF
*
*     now adjust position in case of backward
   50 IF(INFU(IRSTU+19,LR).EQ.0.AND.INFU(IRSTU+12,LR).LT.0) THEN
*        backward reading from last accessed key
         NBACK=2+INFU(IRSTU+18,LR)
*        ... space back before first record from last access
   40    IF(NBACK.GT.0) THEN
            IF(INF9.EQ.0) THEN
*              local file
               WRITE(*,*) ' local keyed access impossible'
               GOTO 90
            ELSE
*              remote file
               CALL KBACKF(INF9,INF8,IOS)
               IF(IOS.NE.0) RETURN
            END IF
            NBACK=NBACK-1
            GOTO 40
         END IF
*        read a record ...
         IF(INF9.EQ.0) THEN
*           local file
            WRITE(*,*) ' local keyed access impossible'
            GOTO 90
         ELSE
*           remote file
            CALL KREADF(INF9,INF8,48,IDUM,12,0,' ',IOS)
            IF(IOS.EQ.3) RETURN
            IF(IOS.EQ.1) GOTO 90
         END IF
*        compare key just read with previous key - end reached?
         NEQ=0
         IF(INF9.NE.0) THEN
            CALL KEYSWAP(IDUM(6),NEWKEY)
         ELSE
            DO 46 I=1,5
   46       NEWKEY(I)=IDUM(5+I)
         END IF
         DO 45 I=1,4
         IF(INFU(IRSTU+13+I,LR).NE.NEWKEY(I)) NEQ=1
   45    CONTINUE
*        store the key just read
         DO 47 I=1,5
   47    INFU(IRSTU+13+I,LR)=NEWKEY(I)
         IF(NEQ.EQ.0) GOTO 90
         CALL TRAKEY('HU',KEY,NEWKEY,NAMKEY,NUM1,NUM2,NRD)
*        ... and space back before first part
         NBACK=1+NRD
   60    IF(NBACK.GT.0) THEN
            IF(INF9.EQ.0) THEN
*              local file
               WRITE(*,*) ' local keyed access impossible'
               GOTO 90
            ELSE
*              remote file
               CALL KBACKF(INF9,INF8,IOS)
               IF(IOS.NE.0) RETURN
            END IF
            NBACK=NBACK-1
            GOTO 60
         END IF
      END IF
*
*     sequential retrieval ...
      IF(INF9.EQ.0) THEN
*        local file
         WRITE(*,*) ' local keyed access impossible'
         GOTO 90
      ELSE
*        remote file
         CALL KREADF(INF9,INF8,4*NTOT,BUFFER,NTOT,0,' ',IOS)
         IF(IOS.EQ.3) RETURN
         IF(IOS.EQ.1) GOTO 90
      END IF
*
*     ... and store record key just read
      IF(INF9.EQ.0) THEN
         DO 71 I=1,5
   71    NEWKEY(I)=BUFFER(5+I)
      ELSE
         CALL KEYSWAP(BUFFER(6),NEWKEY)
      END IF
   69 DO 70 I=1,5
   70 INFU(IRSTU+13+I,LR)=NEWKEY(I)
*
*     set "notfirst" flag
      INFU(IRSTU+19,LR)=1
      IEND=0
      GOTO 100
*
*     end-of-data
   90 CONTINUE
      IEND=-1
*
  100 RETURN
      END
#elif defined SunOS
*CMZ :  1.00/00 16/12/94  17.00.00  by  Pavel Binko
*-- Author :    Volker Blobel   16/12/94
      SUBROUTINE FRKEC(LUN,NTOT,BUFFER,IEND)
*
      INTEGER BUFFER(NTOT)
*
************************************************************************
*
*     Read keyed access
*
************************************************************************
*
      INTEGER IDUM(12)
      CHARACTER KEY*20, NAMKEY*8
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
      IEND = 1
      INF8 = INFU(8,LR)
      INF9 = INFU(9,LR)
*
*     NTOT=12 is special case (first read access to the file)
      IF(NTOT.EQ.12) THEN
         IF(INF9.EQ.0) THEN
*           local file
            WRITE(*,*) ' local keyed access impossible'
            GOTO 90
         ELSE
*           remote file
            CALL REWINF(INF9,INF8,IRET)
            CALL KREADF(INF9,INF8,4*NTOT,BUFFER,NTOT,0,' ',IOS)
            IF(IOS.EQ.3) RETURN
            IF(IOS.EQ.1) GOTO 90
         END IF
         IEND=0
         GOTO 100
      END IF
*
      IF(INFU(IRSTU+11,LR).NE.0) THEN
         INFU(IRSTU+11,LR)=0
*
*        reposition record key after some disturbing operation
         IF(INFU(IRSTU+12,LR).EQ.2) THEN
            INFU(IRSTU+13,LR)=0
*           new position/direction requested by call
            CALL TRAKEY('HC',KEY,INFU(IRSTU+14,LR),NAMKEY,NUM1,NUM2,NRD)
            IF(INF9.EQ.0) THEN
*              local file
               WRITE(*,*) ' local keyed access impossible'
               GOTO 90
            ELSE
*              remote file
               CALL KREADF(INF9,INF8,4*NTOT,BUFFER,NTOT,2,KEY,IOS)
               IF(IOS.EQ.3) RETURN
               IF(IOS.EQ.2) GOTO 90
            END IF
            INFU(IRSTU+12,LR)=1
            GOTO 69
*
         ELSE IF(INFU(IRSTU+12,LR).EQ.-2) THEN
            INFU(IRSTU+13,LR)=0
*           new position/direction requested by call
            CALL TRAKEY('HC',KEY,INFU(IRSTU+14,LR),NAMKEY,NUM1,NUM2,NRD)
            IF(INF9.EQ.0) THEN
*              local file
               WRITE(*,*) ' local keyed access impossible'
               GOTO 90
            ELSE
*              remote file
               CALL KREADF(INF9,INF8,4*NTOT,BUFFER,NTOT,2,KEY,IOS)
               IF(IOS.EQ.3) RETURN
               IF(IOS.EQ.2) GOTO 30
            END IF
            NEQ=0
            DO 13 I=1,5
            IF(INFU(IRSTU+13+I,LR).NE.BUFFER(5+I)) NEQ=1
*           NEQ = 1 for notequal keys
   13       INFU(IRSTU+13+I,LR)=BUFFER(5+I)
            INFU(IRSTU+12,LR)=-1
            IF(NEQ.EQ.0) GOTO 69
            GOTO 50
*           for KEY not found while direction backward
   30       INFU(IRSTU+12,LR) = -1
            INFU(IRSTU+13,LR) =  0
            INFU(IRSTU+14,LR) =  0
            INFU(IRSTU+15,LR) =  0
            INFU(IRSTU+16,LR) =  0
            INFU(IRSTU+17,LR) =  0
            INFU(IRSTU+18,LR) = -1
            INFU(IRSTU+19,LR) =  0
            GOTO 50
*
         ELSE IF(INFU(IRSTU+13,LR).EQ.1) THEN
*           rewind done before - reset
            DO 10 I=1,10
   10       INFU(IRSTU+10+I,LR)=0
*
         ELSE
*           some IO since last read - reposition
            CALL TRAKEY('HC',KEY,INFU(IRSTU+14,LR),NAMKEY,NUM1,NUM2,NRD)
            IF(INF9.EQ.0) THEN
*              local file
               WRITE(*,*) ' local keyed access impossible'
               GOTO 90
            ELSE
*              remote file
               CALL KREADF(INF9,INF8,48,IDUM,12,2,KEY,IOS)
               IF(IOS.EQ.3) RETURN
               IF(IOS.EQ.2) GOTO 90
            END IF
            DO 11 I=1,5
   11       INFU(IRSTU+13+I,LR)=IDUM(5+I)
         END IF
*
      END IF
*
*     now adjust position in case of backward
   50 IF(INFU(IRSTU+19,LR).EQ.0.AND.INFU(IRSTU+12,LR).LT.0) THEN
*        backward reading from last accessed key
         NBACK=2+INFU(IRSTU+18,LR)
*        ... space back before first record from last access
   40    IF(NBACK.GT.0) THEN
            IF(INF9.EQ.0) THEN
*              local file
               WRITE(*,*) ' local keyed access impossible'
               GOTO 90
            ELSE
*              remote file
               CALL KBACKF(INF9,INF8,IOS)
               IF(IOS.NE.0) RETURN
            END IF
            NBACK=NBACK-1
            GOTO 40
         END IF
*        read a record ...
         IF(INF9.EQ.0) THEN
*           local file
            WRITE(*,*) ' local keyed access impossible'
            GOTO 90
         ELSE
*           remote file
            CALL KREADF(INF9,INF8,48,IDUM,12,0,' ',IOS)
            IF(IOS.EQ.3) RETURN
            IF(IOS.EQ.1) GOTO 90
         END IF
*        compare key just read with previous key - end reached?
         NEQ=0
         DO 45 I=1,4
         IF(INFU(IRSTU+13+I,LR).NE.IDUM(5+I)) NEQ=1
   45    CONTINUE
*        store the key just read
         DO 47 I=1,5
   47    INFU(IRSTU+13+I,LR)=IDUM(5+I)
         IF(NEQ.EQ.0) GOTO 90
         CALL TRAKEY('HU',KEY,IDUM(6),NAMKEY,NUM1,NUM2,NRD)
*        ... and space back before first part
         NBACK=1+NRD
   60    IF(NBACK.GT.0) THEN
            IF(INF9.EQ.0) THEN
*              local file
               WRITE(*,*) ' local keyed access impossible'
               GOTO 90
            ELSE
*              remote file
               CALL KBACKF(INF9,INF8,IOS)
               IF(IOS.NE.0) RETURN
            END IF
            NBACK=NBACK-1
            GOTO 60
         END IF
      END IF
*
*     sequential retrieval ...
      IF(INF9.EQ.0) THEN
*        local file
         WRITE(*,*) ' local keyed access impossible'
         GOTO 90
      ELSE
*        remote file
         CALL KREADF(INF9,INF8,4*NTOT,BUFFER,NTOT,0,' ',IOS)
         IF(IOS.EQ.3) RETURN
         IF(IOS.EQ.1) GOTO 90
      END IF
*
*     ... and store record key just read
   69 DO 70 I=1,5
   70 INFU(IRSTU+13+I,LR)=BUFFER(5+I)
*
*     set "notfirst" flag
      INFU(IRSTU+19,LR)=1
      IEND=0
      GOTO 100
*
*     end-of-data
   90 CONTINUE
      IEND=-1
*
  100 RETURN
      END
#else
*CMZ :  1.00/00 16/12/94  17.00.00  by  Pavel Binko
*-- Author :    Volker Blobel   16/12/94
      SUBROUTINE FRKEC(LUN,NTOT,BUFFER,IEND)
*
      INTEGER BUFFER(NTOT)
*
************************************************************************
*
*     Read keyed access
*
************************************************************************
*
      INTEGER IDUM(12)
      CHARACTER KEY*20, NAMKEY*8
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
      IEND = 1
      INF8 = INFU(8,LR)
      INF9 = INFU(9,LR)
*
*     NTOT=12 is special case (first read access to the file)
      IF(NTOT.EQ.12) THEN
         IF(INF9.EQ.0) THEN
*           local file
            WRITE(*,*) ' local keyed access not yet implemented'
            GOTO 90
         ELSE
*           remote file
            CALL REWINF(INF9,INF8,IRET)
            CALL KREADF(INF9,INF8,4*NTOT,BUFFER,NTOT,0,' ',IOS)
            IF(IOS.EQ.3) RETURN
            IF(IOS.EQ.1) GOTO 90
         END IF
         IEND=0
         GOTO 100
      END IF
*
      IF(INFU(IRSTU+11,LR).NE.0) THEN
         INFU(IRSTU+11,LR)=0
*
*        reposition record key after some disturbing operation
         IF(INFU(IRSTU+12,LR).EQ.2) THEN
            INFU(IRSTU+13,LR)=0
*           new position/direction requested by call
            CALL TRAKEY('HC',KEY,INFU(IRSTU+14,LR),NAMKEY,NUM1,NUM2,NRD)
            IF(INF9.EQ.0) THEN
*              local file
               WRITE(*,*) ' local keyed access not yet implemented'
               GOTO 90
            ELSE
*              remote file
               CALL KREADF(INF9,INF8,4*NTOT,BUFFER,NTOT,2,KEY,IOS)
               IF(IOS.EQ.3) RETURN
               IF(IOS.EQ.2) GOTO 90
            END IF
            INFU(IRSTU+12,LR)=1
            GOTO 69
*
         ELSE IF(INFU(IRSTU+12,LR).EQ.-2) THEN
            INFU(IRSTU+13,LR)=0
*           new position/direction requested by call
            CALL TRAKEY('HC',KEY,INFU(IRSTU+14,LR),NAMKEY,NUM1,NUM2,NRD)
            IF(INF9.EQ.0) THEN
*              local file
               WRITE(*,*) ' local keyed access not yet implemented'
               GOTO 90
            ELSE
*              remote file
               CALL KREADF(INF9,INF8,4*NTOT,BUFFER,NTOT,2,KEY,IOS)
               IF(IOS.EQ.3) RETURN
               IF(IOS.EQ.2) GOTO 30
            END IF
            NEQ=0
            DO 13 I=1,5
            IF(INFU(IRSTU+13+I,LR).NE.BUFFER(5+I)) NEQ=1
*           NEQ = 1 for notequal keys
   13       INFU(IRSTU+13+I,LR)=BUFFER(5+I)
            INFU(IRSTU+12,LR)=-1
            IF(NEQ.EQ.0) GOTO 69
            GOTO 50
*           for KEY not found while direction backward
   30       INFU(IRSTU+12,LR) = -1
            INFU(IRSTU+13,LR) =  0
            INFU(IRSTU+14,LR) =  0
            INFU(IRSTU+15,LR) =  0
            INFU(IRSTU+16,LR) =  0
            INFU(IRSTU+17,LR) =  0
            INFU(IRSTU+18,LR) = -1
            INFU(IRSTU+19,LR) =  0
            GOTO 50
*
         ELSE IF(INFU(IRSTU+13,LR).EQ.1) THEN
*           rewind done before - reset
            DO 10 I=1,10
   10       INFU(IRSTU+10+I,LR)=0
*
         ELSE
*           some IO since last read - reposition
            CALL TRAKEY('HC',KEY,INFU(IRSTU+14,LR),NAMKEY,NUM1,NUM2,NRD)
            IF(INF9.EQ.0) THEN
*              local file
               WRITE(*,*) ' local keyed access not yet implemented'
               GOTO 90
            ELSE
*              remote file
               CALL KREADF(INF9,INF8,48,IDUM,12,2,KEY,IOS)
               IF(IOS.EQ.3) RETURN
               IF(IOS.EQ.2) GOTO 90
            END IF
            DO 11 I=1,5
   11       INFU(IRSTU+13+I,LR)=IDUM(5+I)
         END IF
*
      END IF
*
*     now adjust position in case of backward
   50 IF(INFU(IRSTU+19,LR).EQ.0.AND.INFU(IRSTU+12,LR).LT.0) THEN
*        backward reading from last accessed key
         NBACK=2+INFU(IRSTU+18,LR)
*        ... space back before first record from last access
   40    IF(NBACK.GT.0) THEN
            IF(INF9.EQ.0) THEN
*              local file
               WRITE(*,*) ' local keyed access not yet implemented'
               GOTO 90
            ELSE
*              remote file
               CALL KBACKF(INF9,INF8,IOS)
               IF(IOS.NE.0) RETURN
            END IF
            NBACK=NBACK-1
            GOTO 40
         END IF
*        read a record ...
         IF(INF9.EQ.0) THEN
*           local file
            WRITE(*,*) ' local keyed access not yet implemented'
            GOTO 90
         ELSE
*           remote file
            CALL KREADF(INF9,INF8,48,IDUM,12,0,' ',IOS)
            IF(IOS.EQ.3) RETURN
            IF(IOS.EQ.1) GOTO 90
         END IF
*        compare key just read with previous key - end reached?
         NEQ=0
         DO 45 I=1,4
         IF(INFU(IRSTU+13+I,LR).NE.IDUM(5+I)) NEQ=1
   45    CONTINUE
*        store the key just read
         DO 47 I=1,5
   47    INFU(IRSTU+13+I,LR)=IDUM(5+I)
         IF(NEQ.EQ.0) GOTO 90
         CALL TRAKEY('HU',KEY,IDUM(6),NAMKEY,NUM1,NUM2,NRD)
*        ... and space back before first part
         NBACK=1+NRD
   60    IF(NBACK.GT.0) THEN
            IF(INF9.EQ.0) THEN
*              local file
               WRITE(*,*) ' local keyed access not yet implemented'
               GOTO 90
            ELSE
*              remote file
               CALL KBACKF(INF9,INF8,IOS)
               IF(IOS.NE.0) RETURN
            END IF
            NBACK=NBACK-1
            GOTO 60
         END IF
      END IF
*
*     sequential retrieval ...
      IF(INF9.EQ.0) THEN
*        local file
         WRITE(*,*) ' local keyed access not yet implemented'
         GOTO 90
      ELSE
*        remote file
         CALL KREADF(INF9,INF8,4*NTOT,BUFFER,NTOT,0,' ',IOS)
         IF(IOS.EQ.3) RETURN
         IF(IOS.EQ.1) GOTO 90
      END IF
*
*     ... and store record key just read
   69 DO 70 I=1,5
   70 INFU(IRSTU+13+I,LR)=BUFFER(5+I)
*
*     set "notfirst" flag
      INFU(IRSTU+19,LR)=1
      IEND=0
      GOTO 100
*
*     end-of-data
   90 CONTINUE
      IEND=-1
*
  100 RETURN
      END
#endif
