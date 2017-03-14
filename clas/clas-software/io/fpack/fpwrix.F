*CMZ :  1.00/05 06/08/95  17.56.35  by  Wiesiek Dlugosz
*CMZU:  1.00/02 10/02/95  16.07.05  by  Wiesiek Dlugosz
*CMZ :  1.00/00 16/12/94  17.00.00  by  Pavel Binko
*-- Author :    Ruten Gurin   16/12/94
      SUBROUTINE FPWRIX(FROM,TO,NAMKEY,NUM1,NUM2,ICL,IERR)
*
      CHARACTER*(*) FROM,TO,NAMKEY
*
************************************************************************
*
*     Write one entry into index file
*
************************************************************************
*
      CHARACTER*8 NAMEC
      CHARACTER*8 RECNAME
      INTEGER IHL(100)
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
      CHARACTER*512 TMPBUF, HONAME*127
      CHARACTER*12 STR,STR2,MODA(0:4)*9
      DATA MODA/'Unknown','READ','WRITE','READWRITE','MODIFY'/
*
      IERR = 0
*
      LRSAV = LR
      LWSAV = LW
      LUNRSAV = LUNR
      LUNWSAV = LUNW
*
      LFR = LENB(FROM)
      LTO = LENB(TO)
*
      IF(FROM(:LFR).EQ.TO(:LTO)) GOTO 90
*
      LUNR = IFRST(FROM,0)
      IF( LUNR.LE.0 .OR. LUNR.GT.99 ) GOTO 91
      LR = LUNIT(LUNR)
      IF( LR.LE.0 ) GOTO 92
*     Access must be 'read' or 'readwrite'
      IF(INFU(4,LR).NE.1 .AND. INFU(4,LR).NE.3) GOTO 93
*
      LUNW = IFRST(TO,0)
      IF( LUNW.LE.0 .OR. LUNW.GT.99 ) GOTO 94
      LW = LUNIT(LUNW)
      IF( LW.LE.0 ) GOTO 95
*
*     Access must be 'write' or 'readwrite'
      IF(INFU(4,LW).NE.2 .AND. INFU(4,LW).NE.3) GOTO 96
*
      IF( DTFLNM(LR).EQ.' ') GOTO 97
      NAMEC = NAMKEY(1:MIN(8,LENB(NAMKEY)))
      IFLAG2 = 0
      IFLAG = 0
      NR1 = INFU(IRKEY+6,LR)
      NR2 = INFU(IRKEY+7,LR)
      CALL HLTOTX(INFU(IRKEY+4,LR),2,RECNAME,NCH)
75    IF( DTFLKB(LR).LE.0 .OR. DTFLPS(LR).LE.0 .OR.
     +    NUM1.NE.NR1 .OR. NUM2.NE.NR2 .OR. NAMEC.NE.RECNAME ) THEN
         CALL FRKEY(RECNAME,NR1,NR2,NDATW,IERR)
         IFLAG2 = 1
         IF( IERR.EQ.0 ) GOTO 75
         IF( IERR.GT.0 ) GOTO 98
         IF( IFLAG.EQ.1 ) GOTO 99
         IFLAG = 1
         CALL FURWND(LUNR)
         GOTO 75
      ENDIF
      IF( IFLAG2.NE.0 ) THEN
         IF( DTFLNM(LR).EQ.' ') GOTO 97
      ENDIF
      IF( INFU(7,LW).EQ.0 .OR. DTFLNM(LR).NE.DTFLNM(LW) .OR.
     +                         DTFLHO(LR).NE.DTFLHO(LW) .OR.
     +                         DTFLMD(LR).NE.DTFLMD(LW)) THEN
         DTFLNM(LW) = DTFLNM(LR)
         DTFLHO(LW) = DTFLHO(LR)
         DTFLMD(LW) = DTFLMD(LR)
         RLFLNM(LW) = RLFLNM(LR)
         RLFLHO(LW) = RLFLHO(LR)
         RLFLMD(LW) = RLFLMD(LR)
*
         IF ( IDXNEW ) THEN
            L = LENB(RLFLNM(LW))
         ELSE
            L = LENB(DTFLNM(LW))
         ENDIF
*
         ISV24 = INFU(24,LW)
         INFU(24,LW) = 1
         INFU(7,LW) = 0
*     set label of index file
         CALL FWKEY('FPIDXNEW',1,2,3)
*     write FPINDEX0 bank in the following format:
*        file_name;host_name;media_type;
*
         IF(INFU(34,LR).EQ.0) THEN
*     LR refers to data file so that get its record length
            ILRECL=INFU(1,LR)
         ELSE
*     LR refers to input index file keeping in INFU(34,LR) the
*     record length of data file
            ILRECL=INFU(34,LR)
         ENDIF
*                              SHIFT        RECL
*                                |           |
*                                v           v
         CALL FWHDR('FPINDEX0',INFU(28,LR),ILRECL,6,'A')
 
         IF( IDXNEW ) THEN
            TMPBUF = RLFLNM(LW)(1:L)//';'
         ELSE
            TMPBUF = DTFLNM(LW)(1:L)//';'
         ENDIF
 
         LBUF = L+1
*     pack host name
         HONAME = ' '
 
         IF( IDXNEW ) THEN
          L = LENB(RLFLHO(LW))
          IF(L.EQ.0 .OR. (L.NE.0 .AND. RLFLHO(LW)(1:L).EQ.'LOCAL'))THEN
            CALL FPGTH(HONAME,L)
          ELSE
            HONAME=RLFLHO(LW)(1:L)
          ENDIF
         ELSE
          L = LENB(DTFLHO(LW))
          IF(L.EQ.0 .OR. (L.NE.0 .AND. DTFLHO(LW)(1:L).EQ.'LOCAL'))THEN
            CALL FPGTH(HONAME,L)
          ELSE
            HONAME=DTFLHO(LW)(1:L)
          ENDIF
         ENDIF
 
         TMPBUF=TMPBUF(:LBUF)//HONAME(1:L)//';'
         LBUF=LBUF+L+1
*     pack media type
 
         IF( IDXNEW ) THEN
            L = LENB(RLFLMD(LW))
         ELSE
            L = LENB(DTFLMD(LW))
         ENDIF
 
         IF( L.EQ.0 )       GOTO 97
 
         IF( IDXNEW ) THEN
            TMPBUF=TMPBUF(1:LBUF)//RLFLMD(LW)(1:L)//';;'
         ELSE
            TMPBUF=TMPBUF(1:LBUF)//DTFLMD(LW)(1:L)//';;'
         ENDIF
 
         LBUF = LBUF+L+2
         CALL TXTOHL(TMPBUF(1:LBUF),IHL,NHL)
         CALL FWDAT(NHL,IHL)
*
         CALL FWHDR('FPINDEX1',7,8,9,'I')
         INFU(24,LW) = ISV24
*        initialize run name and run number to force FPACK
*        writing them out to index file
         CALL TXTOHL('        ',DTCRNM(1,LW),2)
         DTCRRN(LW)=-1
      ENDIF
      INFU(7,LW) = 0
*
*+++++++++++++++++ NOTE +++++++++++++++++++++++++++++++++++++++++++++++++
*     Before we always wrote out 7 words of index item per event
*     -------------------------
*     Run name            2 words    hollerith
*     Run number          1 word     integer
*     -------------------------
*     Event number        1 word     integer
*     Classword           1 word     integer
*     Address of event    2 words    integer
*
*  Because of the first 3 words are changed quite rarely, we are going
*  to keep last 4 words: event number, classword and address and the first
*  3 words in case they have been changed. A sequence of the fours of
*  words has a marker at the end, which denotes the following:
*      -1  run number has been changed
*      -2  run name and, therefore, a run number as well have been changed
*  We use negative numbers as markers because a marker is on place we
*  should wait for event number which is always positive number.
*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*
*  Use IHL array as tmp buffer for reducing the number of CALLs to FWDAT
      NHL=0
*     As soon as  run name is kept as the hollerits ...
      IF( DTCRNM(1,LW).NE.INFU(IRKEY+4,LR) .OR.
     +    DTCRNM(2,LW).NE.INFU(IRKEY+5,LR) ) THEN
*         ... copy markers of changing both run name and run number
 
          IHL(NHL+1)=-2
          NHL=NHL+1
*         ... copy 2 words of new run name to IHL-buffer
          DO 80 J=1,2
             DTCRNM(J,LW) = INFU(IRKEY+3+J,LR)
             IHL(NHL+J)=DTCRNM(J,LW)
80        CONTINUE
*         and copy 1 word of new run number
          DTCRRN(LW) = INFU(IRKEY+6,LR)
          IHL(NHL+3) = DTCRRN(LW)
          NHL=NHL+3
      ENDIF
      IF( DTCRRN(LW).NE.INFU(IRKEY+6,LR) ) THEN
*         ... set marker of changing a run number
          IHL(NHL+1) = -1
*         ... copy 1 word of new run number
          DTCRRN(LW) = INFU(IRKEY+6,LR)
          IHL(NHL+2) = DTCRRN(LW)
          NHL = NHL+2
      ENDIF
*     copy 1 word of event number
      IHL(NHL+1) = INFU(IRKEY+7,LR)
*     check classword given by user
      IF( ICL.EQ.-1 ) THEN
*        ... copy classword from datafile
         IHL(NHL+2) = INFU(IRKEY+9,LR)
      ELSE
*        ... copy classword of user
         IHL(NHL+2) = ICL
      ENDIF
*     ... copy address of event on data file
      IHL(NHL+3) = DTFLKB(LR)
      IHL(NHL+4) = DTFLPS(LR)
*     ... increment counter
      NHL=NHL+4
*     and now it's  a real WRITE
      CALL FWDAT(NHL,IHL)
      INFU(7,LW) = 1
      GOTO 110
*
90    ERRMES='Input "'//FROM(:LFR)//'" and output "'//
     +    TO(:LTO)//'" are the same'
      GOTO 100
91    CALL PNVI(LUNR,STR,J)
      ERRMES='Input unit '//STR(:J)//' out of range'
      GOTO 100
92    CALL PNVI(LR,STR,J)
      ERRMES='Input unit index '//STR(:J)//' out of range'
      GOTO 100
93    I=INFU(4,LR)
      CALL PNVI(I,STR,J)
      IF(I.LT.1 .OR. I.GT.4) I=0
      ERRMES='Invalid input mode = '//STR(:J)//' ('//
     +       MODA(I)(:LENB(MODA(I)))//')'
      GOTO 100
94    CALL PNVI(LUNW,STR,J)
      ERRMES='Output unit '//STR(:J)//' out of range'
      GOTO 100
95    CALL PNVI(LW,STR,J)
      ERRMES='Output unit index '//STR(:J)//' out of range'
      GOTO 100
96    I=INFU(4,LW)
      CALL PNVI(I,STR,J)
      IF(I.LT.0 .OR. I.GT.4) I=0
      ERRMES='Invalid output mode = '//STR(:J)//' ('//
     +       MODA(I)(:LENB(MODA(I)))//')'
      GOTO 100
97    ERRMES='Invalid input data file name'
      GOTO 100
98    ERRMES='Error reading on input '//FROM(1:LFR)
      GOTO 100
99    IERR=-1
      CALL PNVI(NUM1,STR,J)
      CALL PNVI(NUM2,STR2,J)
      ERRMES='Event '//NAMEC//' '//STR(:J)//' '//STR2(:J)//
     +      ' not found on '//FROM(1:LFR)
      GOTO 110
100   IERR=1
      CALL FPWMSG(IERR,'FPWRIX',ERRMES)
110   CONTINUE
*
      LR = LRSAV
      LW = LWSAV
      LUNR = LUNRSAV
      LUNW = LUNWSAV
*
      RETURN
      END
