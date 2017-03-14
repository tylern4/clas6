#ifndef Linux
*CMZU:  1.00/16 12/03/96  12.27.45  by  Dave Sankey at RAL
#endif /* not Linux */
*CMZ :  1.00/00 16/12/94  17.00.00  by  Pavel Binko
*-- Author :    Volker Blobel   16/12/94
      SUBROUTINE FRKEX(NAMKEY,NUM1,NUM2,NDATWD,IER)
*
      CHARACTER*(*) NAMKEY
*
************************************************************************
*
*     read and return next key block (segment code 0 or 1)
*
************************************************************************
*
      INTEGER JBUF(12)
      CHARACTER NAMEC*8, ORIGT*8
      CHARACTER CHA*4, STRING*256, FTEXT*16
      LOGICAL IS01,ILLG
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
*KEEP,DORIGL.
*     text for origin flag, used for text IO
      CHARACTER*8 ORIGL(15)
      DATA ORIGL/'BOS     ','LOOK    ','        ','        ','        ',
     +           '        ','        ','        ','        ','        ',
     +           '        ','        ','        ','        ','        '/
*KEEP,ARATOE.
      INTEGER ATOE(0:255)
*KEEP,DTATOE.
      DATA (ATOE(I),I=0,255) /
     +   0, 64, 64, 64, 64,  9, 64, 64, 64, 64, 64, 64, 64, 13, 64, 64,
     +  64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64,
     +  64, 90,127,123, 91,108, 80,125, 77, 93, 92, 78,107, 96, 75, 97,
     + 240,241,242,243,244,245,246,247,248,249,122, 94, 76,126,110,111,
     + 124,193,194,195,196,197,198,199,200,201,209,210,211,212,213,214,
     + 215,216,217,226,227,228,229,230,231,232,233,173,224,189, 95,109,
     + 121,129,130,131,132,133,134,135,136,137,145,146,147,148,149,150,
     + 151,152,153,162,163,164,165,166,167,168,169,192, 79,208,161, 64,
     +  64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64,
     +  64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64,
     +  64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64,
     +  64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64,
     +  64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64,
     +  64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64,
     +  64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64,
     +  64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64 /
*KEND.
*
*     statement functions
      NEXTIP(IP)=IP+IBUF(INDR+IP+11)+11
      NEXTJP(JP)=JP+IBUF(INDR+JP+ 1)+IBUF(INDR+JP+ 9)
      ILLG(I)   =I.LT.0.OR.I.GT.3
      IS01(I)   =I.EQ.0.OR.I.EQ.1
*
*     check of status of unit ------------------------------------------
*
*     error if no read unit defined
   01 IF(LR.EQ.0) GOTO 99
*
*     get record pointers for binary input
      CALL FIPCOP(IP,JP,KP,LR)
*
*     important status words
*        INFU(ISGEN+1,LR) = read write etc
*        INFU(IRSTU+2,LR) = error code of last read operation
*        INFU(IRSTU+3,LR) = error code of actual logical record
*
*     check status for sequential or special IO only
      IF(INFU(2,LR).NE.1.AND.INFU(2,LR).NE.4) GOTO 5
*
*     general status of unit
      IF( INFU(ISGEN+1,LR).EQ.0 .OR. INFU(ISGEN+1,LR).EQ.5 ) THEN
*        status is undef or rewind - switch to reading
         INFU(ISGEN+1,LR)=1
         INFU(IRSTU+2,LR)=0
         INFU(IRSTU+3,LR)=0
*        drop bank too
      ELSE IF(INFU(ISGEN+1,LR).EQ.2) THEN
*        status is end-of-file at reading
         GOTO 99
      ELSE IF(INFU(ISGEN+1,LR).EQ.4) THEN
*        status is end-file written - rewind and switch to reading
         CALL FURWND(LUNR)
         INFU(ISGEN+1,LR)=1
         INFU(IRSTU+2,LR)=0
         INFU(IRSTU+3,LR)=0
*        drop bank too
      ELSE IF(INFU(ISGEN+1,LR).NE.1) THEN
*        status is write
         GOTO 93
      END IF
*
*     branch for text type
    5 IF(INFU(3,LR).EQ.2) GOTO 60
*
*     binary (unformatted) input ---------------------------------------
*     check last read operation
      IF(INFU(IRSTU+3,LR).LT.0) INFU(IRSTU+3,LR)=0
      IF(INFU(IRSTU+2,LR).NE.0.OR.INFU(IRSTU+3,LR).NE.0) THEN
*      last read operation was in error then depending on SKIPCOREV...
         IF(INFU(32,LR).EQ.1) THEN
*           whether read new record
            IP=0
         ELSE
*           or stop following reading at all
            INFU(ISGEN+1,LR) = 2
            GOTO 99
         ENDIF
      END IF
*     set "first" flag for keyed access
      INFU(IRSTU+19,LR)=0
*     buffer bank( + 1 for input)
      LEVEL = INFU(25,LR)
      IF(LEVEL.EQ.0) THEN
         NUMBNK = 1000*LUNR+1
      ELSE
         NUMBNK = 1000*LUNR+9+LEVEL
      ENDIF
      INDR=INDXB(IBUF,NUMBNK)
      IF(INDR.EQ.0) THEN
         IF(LEVEL.EQ.0) THEN
            IP=0
*           read (try 10 times) and back space
            DO 10 ITRY=1,10
               INFU(27,LR) = 1
               CALL FRREC(LUNR,12,JBUF,IEND)
               IF(IEND.LT.0)GOTO 99
               CALL FURWND(LUNR)
               CALL FSWAP(JBUF,12)
               INFU(IRSTU+2,LR)=IEND
               INFU(IRSTU+3,LR)=0
               IF(IEND.EQ.0) GOTO 20
   10       CONTINUE
*           permanent read error
            GOTO 99
   20       CONTINUE
#ifdef Linux
            INFU(ISGEN+1,LR)=1
#endif /* Linux */
*           initialize format description of event name (2 word, 8 char)
*         eventually convert to local - in this form wrong, true format?
            INFU(1,LR) = JBUF(1)*4
#ifndef Linux
            INFU(ISGEN+1,LR)=1
#endif /* not Linux */
         END IF
*        create bank with correct bank length
         INDR=INDXA(IBUF,NUMBNK,INFU(1,LR)/4)
         IF(INDR.EQ.0) GOTO 95
         INFU(27,LR) = 1
      END IF
*
*     value of IP
*              IP > 1   pointing to current record header
*              IP = 1   record is read in, IP not yet defined
*              IP = 0   no record read in
*              IP < 0   ?
*
*     determine record position (if any)
   25 NSKRK=0
      IF(IP.NE.0) THEN
*        record pointer is existing
         IF(NEXTIP(IP).LT.IBUF(INDR+2)) THEN
*           not the last segment
            IP=NEXTIP(IP)
            GOTO 30
         END IF
      END IF
      IP=0
*     read next physical record (evtl skip before)
      CALL FRREC(LUNR,IBUF(INDR),IBUF(INDR+1),IEND)
      INFU(29,LR) = INFU(14,LR)
      IF(IEND.LT.0) GOTO 99
      INFU(IRSTU+2,LR)=IEND
      INFU(IRSTU+3,LR)=0
*     test read error
      IF(IEND.GT.0) GOTO 91
*     convert eventually to local format
      CALL  FSWAP(IBUF(INDR+1),0)
*     record read, not yet converted
      INFU(IRSTU+1,LR)=-1
*     set word format of input file (important for index files)
      INFU(5,LR)=IBUF(INDR+4)/16
*     store actual record number
*-BUG>INFU(IRSTU+5,LR)=IBUF(INDR+5)/100
*     successful read - reset pointer
   28 IP=2
   29 IF(NSKRK.GT.0) THEN
*        skip to next record key
         IP=NEXTIP(IP)
         IF(IP.GE.IBUF(INDR+2)) GOTO 92
         NSKRK=NSKRK-1
         GOTO 29
      END IF
*     reset and copy record segment
   30 CONTINUE
      CALL FPCPRK(LR,IBUF(INDR+IP+1))
*     check segment code of new record segment
      IPS=INFU(IRKEY+10,LR)
      IF(ILLG(IPS)) GOTO 94
      IF(.NOT.IS01(IPS)) GOTO 25
      JP=0
      GOTO 50
*
*     code for text input ----------------------------------------------
*
   60 CONTINUE
 
   70 NSTR=INFU(1,LR)
      IF(NSTR.LT.72.OR.NSTR.GT.256) NSTR=80
      INFU(IRSTU+2,LR)=0
      INFU(IRSTU+3,LR)=0
*     read or copy from buffer
      INDR=INDXB(IBUF,1000*LUNR+1)
      IF(INDR.EQ.0) THEN
         INDR = INDXA(IBUF,1000*LUNR+1,64)
         IF(INDR.EQ.0) GOTO 95
         IBUF(INDR) = 0
      END IF
      ILEN = IBUF(INDR)
      IF(ILEN.EQ.0) THEN
         READ(LUNR,'(A)',END=99) STRING(1:NSTR)
      ELSE
         STRING=' '
         CALL HLTOTX(IBUF(INDR+1),(NSTR+3)/4,STRING,NCH)
         IBUF(INDR) = 0
      END IF
*     fast search for record start: R... or bR...
      IF(STRING(1:1).NE.' ') THEN
         IF(STRING(1:1).NE.'R'.AND.STRING(1:1).NE.'r') GOTO 70
      ELSE
         IF(STRING(2:2).NE.'R'.AND.STRING(2:2).NE.'r') GOTO 70
      END IF
*     translate and interprete
      CALL LINEIN(STRING(1:NSTR))
      CALL STMTST(NTEL,FTEXT)
      CALL STMTNU
c     CALL STMTPR
*  first text should be "RECL" or "RECORD"
      IF(NTX.LT.1)      GOTO 70
      IF(ITX(3,1).NE.3) GOTO 70
      CALL UCASE(STMT(ITX(1,1):ITX(2,1)))
      IF(STMT(ITX(1,1):ITX(2,1)).EQ.'RECL') THEN
*        RECL = recl
         IF(NTX.LT.3)                          GOTO 70
         IF(STMT(ITX(1,2):ITX(2,2)).NE.'=')    GOTO 70
         IF(ITX(3,3).NE.4)                     GOTO 70
         IF(ITX(4,3).LT.72.OR.ITX(4,3).GT.256) GOTO 70
*        store record length
         INFU(1,LR)=ITX(4,3)
         GOTO 70
      ELSE IF(STMT(ITX(1,1):ITX(2,1)).EQ.'RECORD') THEN
*        RECORD str num num num, optionally followed by BOS or LOOK
         IF(NTX.LT.5)      GOTO 70
         IF(ITX(3,2).NE.3) GOTO 70
         NAMEC=STMT(ITX(1,2):ITX(2,2))
         IF(ITX(3,3).NE.4) GOTO 70
         NUM1=ITX(4,3)
         IF(ITX(3,4).NE.4) GOTO 70
         NUM2=ITX(4,4)
         IF(ITX(3,4).NE.4.AND.ITX(3,4).NE.5) GOTO 70
         NDATWD=ITX(4,5)
         IORIG=0
*        check origin code BOS or LOOK or ...
         IF(NTX.GE.6.AND.(ITX(3,6).EQ.3.OR.ITX(3,6).EQ.4)) THEN
            ORIGT=STMT(ITX(1,6):ITX(2,6))
            CALL UCASE(ORIGT)
            DO 73 ILG=1,15
            IF(ORIGL(ILG).EQ.' ') GOTO 80
            IF(ORIGL(ILG).EQ.ORIGT) THEN
               IORIG=ILG
               GOTO 80
            END IF
   73       CONTINUE
         END IF
      END IF
   80 IER=0
*     store record header
      INFU(IRKEY+1,LR)=67305985
      INFU(IRKEY+2,LR)=IORIG
      INFU(IRKEY+3,LR)=0
      CHA=NAMEC(1:4)
*KEEP,ICHCHA.
*     get hollerith 4H from char*4
#ifndef Linux
      ICH=IOR(ISHFT(ICHAR(CHA(1:1)),24),IOR(ISHFT(ICHAR(CHA(2:2)),16),
     +    IOR(ISHFT(ICHAR(CHA(3:3)), 8),ICHAR(          CHA(4:4)))))
#else /* Linux */
      ICH=IOR(ISHFT(ICHAR(CHA(4:4)),24),IOR(ISHFT(ICHAR(CHA(3:3)),16),
     +    IOR(ISHFT(ICHAR(CHA(2:2)), 8),ICHAR(          CHA(1:1)))))
#endif /* Linux */
*KEND.
      INFU(IRKEY+4,LR)=ICH
      CHA=NAMEC(5:8)
*KEEP,ICHCHA.
*     get hollerith 4H from char*4
#ifndef Linux
      ICH=IOR(ISHFT(ICHAR(CHA(1:1)),24),IOR(ISHFT(ICHAR(CHA(2:2)),16),
     +    IOR(ISHFT(ICHAR(CHA(3:3)), 8),ICHAR(          CHA(4:4)))))
#else /* Linux */
      ICH=IOR(ISHFT(ICHAR(CHA(4:4)),24),IOR(ISHFT(ICHAR(CHA(3:3)),16),
     +    IOR(ISHFT(ICHAR(CHA(2:2)), 8),ICHAR(          CHA(1:1)))))
#endif /* Linux */
*KEND.
      INFU(IRKEY+5,LR)=ICH
      INFU(IRKEY+6,LR)=NUM1
      INFU(IRKEY+7,LR)=NUM2
      INFU(IRKEY+8,LR)=0
      INFU(IRKEY+9,LR)=NDATWD
      INFU(IRKEY+10,LR)=0
      INFU(IRKEY+11,LR)=0
      IP=2
*
*     again common code for binary and formatted input -----------------
*
*     set status to "key read"
   50 INFU(IRSTU+4,LR)=1
*     reset class word flag for safety
      INFU(IWSTU+11,LR)=0
      IER=0
*     store code of actual logical record
      INFU(IRSTU+3,LR)=IER
*     restore record pointer
      CALL FIPCOP(IP,JP,KP,-LR)
*     convert key name into character string
      ICH=INFU(IRKEY+4,LR)
*KEEP,CHAICH.
*     get char*4 from hollerith 4H
#ifndef Linux
      CHA=CHAR(IAND(ISHFT(ICH,-24),255))//CHAR(IAND(ISHFT(ICH,-16),255))
     +  //CHAR(IAND(ISHFT(ICH,- 8),255))//CHAR(IAND      (ICH     ,255))
#else /* Linux */
      CHA=CHAR(IAND      (ICH     ,255))//CHAR(IAND(ISHFT(ICH,- 8),255))
     +  //CHAR(IAND(ISHFT(ICH,-16),255))//CHAR(IAND(ISHFT(ICH,-24),255))
#endif /* Linux */
*KEND.
      NAMEC(1:4)=CHA
      ICH=INFU(IRKEY+5,LR)
*KEEP,CHAICH.
*     get char*4 from hollerith 4H
#ifndef Linux
      CHA=CHAR(IAND(ISHFT(ICH,-24),255))//CHAR(IAND(ISHFT(ICH,-16),255))
     +  //CHAR(IAND(ISHFT(ICH,- 8),255))//CHAR(IAND      (ICH     ,255))
#else /* Linux */
      CHA=CHAR(IAND      (ICH     ,255))//CHAR(IAND(ISHFT(ICH,- 8),255))
     +  //CHAR(IAND(ISHFT(ICH,-16),255))//CHAR(IAND(ISHFT(ICH,-24),255))
#endif /* Linux */
*KEND.
      NAMEC(5:8)=CHA
*     move record key to arguments
      NAMKEY=NAMEC
      NUM1  =INFU(IRKEY+6,LR)
      NUM2  =INFU(IRKEY+7,LR)
      NDATWD=INFU(IRKEY+9,LR)
*
*     increase number of logical records read (before selection)
      INFU(IRSTA+6,LR)=INFU(IRSTA+6,LR)+1
*
      GOTO 100
*
*     all error conditions are set here
*               only argument IER is returned
*     pure read error
   91 IER = 1
      ERRMES='Read error of physical record'
      GOTO 87
*     unexpected end-of-data (record incomplete)
   92 IER = 2
      ERRMES='Unexpected end_of_data (record incomplete)'
      GOTO 87
*     error in calling order
   93 IER = 3
      ERRMES='Error in calling order. Data file corrupted'
      GOTO 87
*     illegal segment code
   94 IER = 4
      ERRMES='Illegal segment code. Data file corrupted'
      GOTO 87
*     no buffer bank available
   95 IER = 5
      ERRMES='No buffer bank available'
   87 CALL FQCOND('PRINT',IPRTFL)
      IF(IPRTFL.GT.0) WRITE(*,*) ERRMES
      GOTO 89
*
*     end-of-file
   99 IER = -1
*
*     if the last logical record was incomplete (infu(irstu+3,lr)=2)
*     at the end of previous file (infu(isgen+1,lr)=2) then
*     forget about it (simulate, that log. rec. is OK).
      IF(INFU(32,LR).EQ.1 .AND. INFU(ISGEN+1,LR).EQ.2 .AND.
     +   (INFU(IRSTU+3,LR).GT.0 .OR. INFU(IRSTU+2,LR).NE.0))
     +  INFU(ISGEN+1,LR)=1
*
*     store code of actual logical record
   89 CONTINUE
      IF(IER.GT.0) CALL FPWMSG(IER,'FRKEX',ERRMES)
      INFU(IRSTU+3,LR)=IER
*
*     restore record pointer
      CALL FIPCOP(IP,JP,KP,-LR)
*
  100 RETURN
      END
