*CMZ :  1.00/00 16/12/94  17.00.00  by  Pavel Binko
*-- Author :    Pavel Binko   16/12/94
      SUBROUTINE MKIXOP(IRC)
*
***********************************************************************
*
*     Open data file through the index file
*
***********************************************************************
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
*KEEP,CTEXTIR.
*     one statement up to 5280 = 66 * 80 characters
      PARAMETER (LSTMT=5280)
      CHARACTER STMT*(LSTMT),ERRMES*72,NORMES*72
      PARAMETER (NDIMIT=640)
      REAL RTX(4,NDIMIT)
      COMMON/CTXTIR/NST,MST,KST,INU,NTX,ITX(4,NDIMIT)
      COMMON/CTXTST/STMT,ERRMES,NORMES
      EQUIVALENCE (ITX(1,1),RTX(1,1))
      SAVE/CTXTIR/,/CTXTST/
*KEND.
*
      INTEGER IOPEN(IOPMAX), LISTI(ILIMAX), LUNPAR(ILPMAX)
      PARAMETER(IWSZ=100)
      INTEGER W(IWSZ)
      CHARACTER DANAME*16, FINAME*256, HONAME*127, STRLUN*2
*
      LOGICAL START
      DATA START/.TRUE./
*
      CHARACTER TACCES(5)*7,TFORMT(2)*6,TACTIO(4)*9
      DATA TACCES/'    seq','    dir','  keyed','special','ordered'/
      DATA TFORMT/'binary','  text'/
      DATA TACTIO/'     read','    write','readwrite','   modify'/
*
      IF(START) THEN
         START=.FALSE.
         DO 10 I=1,ILIMAX
            LISTI(I)=0
   10    CONTINUE
      END IF
*
      IRC=0
      DO 20 I=1,IOPMAX
         IOPEN(I) = 0
 20   CONTINUE
*
      ITRY = 0
 30   CONTINUE
*
*     logical unit number
      IOPEN(1)  = LUNBK(LR)
*     bufno
      IOPEN(6)  = 2
*     read
      IOPEN(9)  = 1
*     old
      IOPEN(10) = 1
*     direct
      IOPEN(11) = 2
*     binary
      IOPEN(12) = 1
*     maybe SHIFT
      IF( INFU(28,LR).EQ.2 ) IOPEN(17) = 1
*
      IF( IDXNEW ) THEN
*        new index format
         IOPEN( 2) = INFU(34,LR)
         IF( ITRY.GT.0 ) THEN
            CALL FPCSTE(DTFLNM(LR),DTFLHO(LR),DTFLMD(LR),IOPEN,IRC)
         ELSE
            CALL FPCUST(DTFLNM(LR),DTFLHO(LR),DTFLMD(LR),IOPEN,IRC)
         ENDIF
         IF( IRC.NE.0 ) GOTO 60
         ITRY = ITRY+1
      ELSE
*        old index format
         IF( (DTFLVS(LR).EQ.'$.STAGED' .OR.
     +        (ITRY.EQ.0 .AND. INFU(37,LR).EQ.1))
     +        .AND. INFU(38,LR).EQ.0 ) THEN
*
*           If DDEFER parameter has not been set and ...
*           if index was done from staged cartridge data file or
*           from cartridge data file but user has set DSTAGE parameter
*           then we try ...
            ITRY = ITRY + 1
*           stage
            IOPEN(15) = 3
*           recl
            IOPEN(2) = INFU(34,LR)
         ELSEIF( DTFLVS(LR).NE.' ' ) THEN
*           sequential
            IOPEN(11) = 1
*           defer
            IOPEN(15) = 2
*           recl
            IOPEN(2) = INFU(34,LR)
         ENDIF
      ENDIF
*
      DANAME = DATNAM(LR)
      L = LENB(DANAME)
      IF(IOPEN(11).EQ.1) THEN
         IF(INFU(35,LR).EQ.0) THEN
            CALL FPWMSG(35,'MKIXOP','DUNIT parameter not defined')
            IRC=6
            GOTO 100
         ENDIF
         CALL PNVI(INFU(35,LR),STRLUN,LSTR)
         L = MIN(L,7)
         DANAME = DANAME(1:L)//' '//STRLUN(:LSTR)//' DCART'
         CALL FUPARM(INFU(35,LR),DANAME,LISTI)
         IOPEN(1) = INFU(35,LR)
      ELSEIF(IOPEN(11).EQ.2) THEN
         L= MIN(L,10)
         DANAME = DANAME(:L)//' DDISK'
*     check we have already captured disk datafile unit
         IF(INFU(36,LR).EQ.0)
     +        CALL FUPARM(INFU(36,LR),DANAME,LISTI)
         IOPEN(1) = INFU(36,LR)
      ELSE
         CALL FPWMSG(6,'MKIXOP','KEYED, SPECIAL and ORDERED'//
     + ' accesses are not supported from index')
         IRC=6
         GOTO 100
      ENDIF
      IF(.NOT.IDXNEW) HONAME = HOSNAM(LR)
      IRC = 0
      CALL MKOPEN(IOPEN,DTFLNM(LR),DANAME,DTFLHO(LR),' ',
     +     ' ',' ',IRC)
      IF( IRC.EQ.0 ) GOTO 80
      IF( IDXNEW )   GOTO 30
      IF( .NOT.IDXNEW .AND. INFU(35,LR).NE.0 .AND.
     +        (IOPEN(15).EQ.3 .OR. IOPEN(15).EQ.4) ) THEN
         IF(DTFLVS(LR).EQ.'$.STAGED') DTFLVS(LR)='-.STAGED'
         GOTO 30
      ENDIF
 60   INFU(7,LR) = 0
      DTFLNM(LR) = ' '
 70   CONTINUE
      CALL FRDAT(N,W,IWSZ)
      IF( N.NE.0 ) GOTO 70
*     endloop 70
      INFU(7,LR) = -1
      GOTO 99
 80   CONTINUE
      LUNRDAT = IFRST(DANAME,0)
      LRDAT = LUNIT(LUNRDAT)
*   set up cross references of dad (index file) and son (data file)
      INFU(7,LR) = LRDAT
      INFU(10,LRDAT) = LR
*   set data file record length from temporary storage if necessary
      IF(INFU(1,LRDAT).EQ.0) INFU(1,LRDAT)=INFU(34,LR)
*   set new current physical record and address into it
      INFU(27,LRDAT) = DTFLKB(LR)
      INFU(IRSTU+8,LRDAT) = DTFLPS(LR)
*   clear pointer to last physical record number
*     ( it's important for cartridge data file )
      INFU(14,LRDAT)      = 0
*   set general status of datafile to READing
      INFU(ISGEN+1,LRDAT) = 1
*   look if it is necessary to print active file overview
      CALL FQCOND('PRINT',IPFLAG)
      IF(IPFLAG.GT.1) THEN
         CALL FQFILE(DANAME,FINAME,HONAME,LUNPAR,IER15)
         IF(IER15.EQ.0) THEN
            WRITE(*,'(5X,''+'',65(''-''),''+'')')
            WRITE(*,4010)
            WRITE(*,4011) LUNRDAT,DANAME(1:16),TACCES(LUNPAR(2)),
     +           TFORMT(LUNPAR(3)),TACTIO(LUNPAR(4)),LUNPAR(5),
     +           DTFLVS(LR)(1:6)
            WRITE(*,'(5X,''+'',65(''-''),''+'',/)')
         ENDIF
      ENDIF
*
      GOTO 100
*
 99   IRC=-1
*
 100  RETURN
*
4010  FORMAT(5X,'| unit  dataname........  access format    action ',
     + 'rl/bytes volume |')
4011  FORMAT(5X,'|',1X,I3,3X,A16,1X,A7,1X,A6,1X,A9,I9,1X,A6,1X,'|')
      END
