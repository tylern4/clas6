#ifndef Linux
*CMZU:  1.00/16 07/03/96  18.14.48  by  Wiesiek Dlugosz
#endif /* not Linux */
*CMZ :  1.00/00 16/12/94  17.00.00  by  Pavel Binko
*-- Author :    Volker Blobel   16/12/94
      SUBROUTINE FRHDR(NAMDAT,NUM,NCOL,NROW,FORMAT,NCH,IER)
*
      CHARACTER*(*) NAMDAT, FORMAT
*
************************************************************************
*
*     Get data header (read)
*
************************************************************************
*
#ifndef Linux
      PARAMETER (NW5 = 5)
#endif /* not Linux */
      CHARACTER NAMEC*8, CHA*4, STRING*256, FTEXT*16
      LOGICAL IS01,IS03,ILLG
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
*     statement functions
      NEXTIP(IP)=IP+IBUF(INDR+IP+11)+11
      NEXTJP(JP)=JP+IBUF(INDR+JP+ 1)+IBUF(INDR+JP+ 9)
      ILLG(I) =I.LT.0.OR.I.GT.3
      IS01(I) =I.EQ.0.OR.I.EQ.1
      IS03(I) =I.EQ.0.OR.I.EQ.3
*     ...
*
      LRSAV = LR
      LUNRSAV = LUNR
      ITRY = 0
   01 IER=8
*     error if no read unit defined
      IF(LR.EQ.0) THEN
         CALL FPWMSG(IER,'FRHDR','No read unit defined')
         GOTO 100
      ENDIF
#ifndef Linux
*     ordered access
      IF(INFU(3,LR).EQ.1.AND.INFU(25,LR).GT.0) THEN
         CALL FRHDRO(NAMDAT,NUM,NCOL,NROW,FORMAT,NCH,IER)
         GOTO 200
      ENDIF
#endif /* not Linux */
*
*     reset key flag
      INFU(IRSTU+4,LR)=0
      IER=0
      IF( INFU(7,LR).NE.0 ) THEN
*     read header through index
         IF( INFU(7,LR).LT.0 ) THEN
*           no data file yet
            IF( DTFLNM(LR).EQ.' ' ) GOTO 100
*     open data file through the index file
            CALL MKIXOP(IER)
            IF(IER.LT.0) GOTO 99
            IF(IER.GT.0) GOTO 96
         ENDIF
*        change current LR, LUNR (from index file to data file)
         CALL FRUNIT(LUNBK(INFU(7,LR)))
      ENDIF
*
*     check status of unit ---------------------------------------------
*
*     get record pointers for binary input
      CALL FIPCOP(IP,JP,KP,LR)
*
*     important status words
*        INFU(ISGEN+1,LR) = read write etc
*        INFU(IRSTU+2,LR) = error code of last read operation
*        INFU(IRSTU+3,LR) = error code of actual logical record
*
#ifdef Linux
      IF( INFU(25,LR).EQ.0 ) THEN
#endif /* Linux */
*        general status of unit
      IF(INFU(ISGEN+1,LR).EQ.2) THEN
*        status is end-of-file at reading
         GOTO 99
      ELSE IF(INFU(ISGEN+1,LR).NE.1) THEN
         ERRMES='Status is undef, write or rewind'
         GOTO 93
      END IF
#ifdef Linux
      END IF
#endif /* Linux */
*     check last read operation
      IF(INFU(IRSTU+2,LR).NE.0) THEN
         ERRMES='Last read operation was in error'
         GOTO 91
      END IF
*     check status of actual logical record
      IF(INFU(IRSTU+3,LR).NE.0) THEN
         ERRMES='Actual logical record was in error'
         IER=INFU(IRSTU+3,LR)
         GOTO 90
      END IF
*     branch for text type
      IF(INFU(3,LR).EQ.2) GOTO 60
*
*     binary input -----------------------------------------------------
*
*     value of IP
*              IP > 1   pointing to current record header
*              IP = 1   record is read in, IP not yet defined
*              IP = 0   no record read in
*              IP < 0   ?
      NUMBNK = 1000*LUNR+1
      INDR=INDXB(IBUF,NUMBNK)
      IF( INDR.EQ.0 .AND. INFU(10,LR).GT.0 )
*        data file, it's direct, so we know the bufsize
*        and for cartridge data file we have got it from index.
     +   INDR=INDXA(IBUF,NUMBNK,INFU(1,LR)/4)
      IF(INDR.EQ.0) THEN
         ERRMES='No buffer bank existing'
         GOTO 93
      END IF
*
#ifdef Linux
      IF(INFU(25,LR).NE.0) THEN
         IF( INFU(29,LR).EQ.0 ) THEN
*        reading of the data record is necessary
            CALL FRREC(LUNR,IBUF(INDR),IBUF(INDR+1),IEND)
            INFU(29,LR) = INFU(14,LR)
            INFU(IRSTU+2,LR)=IEND
            INFU(IRSTU+3,LR)=0
            IF(IEND.LT.0) THEN
*              general status is: end-of-file at reading
               INFU(ISGEN+1,LR)=2
               GOTO 92
            ELSE IF(IEND.GT.0) THEN
               ERRMES='Read error'
               GOTO 91
            ELSE
*              eventually convert to local format
               CALL  FSWAP(IBUF(INDR+1),0)
               CALL FRCONV(IBUF(INDR+1),LOCAL,ICER)
               INFU(IRSTU+1,LR)=+1
            END IF
            IP = DTFLPS(LR)
            JP = 0
            DTFLPS(LR) = 2
*        copy record segment
            CALL FPCPRK(LR,IBUF(INDR+IP+1))
         ENDIF
         IF(INFU(IRKEY+10,LR).NE.IBUF(INDR+IP+10)) THEN
            INFU(IRKEY+10,LR) =  IBUF(INDR+IP+10)
         ENDIF
      ENDIF
#endif /* Linux */
*
      IF(IP.LE.1) THEN
         ERRMES='Invalid value of IP pointing to current record header'
        GOTO 93
      ENDIF
*     IP is defined, check conversion of record
      IF(INFU(10,LR).GT.0.AND.JP.EQ.0)THEN
         IF( INFU(14,LR).NE.INFU(27,LR) ) THEN
*           reading is neccessary
            CALL FRREC(LUNR,IBUF(INDR),IBUF(INDR+1),IEND)
            INFU(29,LR) = INFU(14,LR)
            INFU(IRSTU+2,LR)=IEND
            INFU(IRSTU+3,LR)=0
            IF(IEND.LT.0) THEN
*              general status is: end-of-file at reading
               INFU(ISGEN+1,LR)=2
               GOTO 92
            ELSE IF(IEND.GT.0) THEN
               ERRMES='Read error'
               GOTO 91
            ELSE
*              eventually convert to local format
               CALL  FSWAP(IBUF(INDR+1),0)
               CALL FRCONV(IBUF(INDR+1),LOCAL,ICER)
               INFU(IRSTU+1,LR)=+1
            END IF
         END IF
*        check for being inside the array boundaries
         IF((IP+10).GT.INFU(1,LR)/4) GOTO 97
*        copy record segment
         CALL FPCPRK(LR,IBUF(INDR+IP+1))
      END IF
      IF(INFU(IRSTU+1,LR).LT.0) THEN
         CALL FRCONV(IBUF(INDR+1),LOCAL,ICER)
         INFU(IRSTU+1,LR)=1
      END IF
*     check if run name, run number and event number coincide
*     in index and data files
      IF(INFU(10,LR).GT.0) THEN
         DO 125 MI=4,7
 125     IF(INFU(IRKEY+MI,LR).NE.INFU(IRKEY+MI,INFU(10,LR)))  GOTO 97
      ENDIF
      IF(JP.EQ.0) GOTO 30
*     JP is defined
   10 CONTINUE
      IF(NEXTIP(IP).EQ.NEXTJP(JP)) THEN
*        reset JP (was last data segment inside record segment)
         JP=0
*        check illegal segment code
         IF(ILLG(INFU(IRKEY+10,LR))) GOTO 94
*        check end-of-logical-record
         IF(IS03(INFU(IRKEY+10,LR))) GOTO 99
*        continuation of logical record in next record segment
         IF(NEXTIP(IP).EQ.IBUF(INDR+2)) THEN
*           reset IP (was last record segment in record)
            IP=0
            IF( INFU(10,LR).GT.0 ) THEN
               INFU(27,LR) = INFU(14,LR)+1
#ifdef Linux
            ELSE IF( INFU(25,LR).NE.0 ) THEN
               INFU(27,LR) = INFU(29,LR)+1
#endif /* Linux */
            ENDIF
            CALL FRREC(LUNR,IBUF(INDR),IBUF(INDR+1),IEND)
            INFU(29,LR) = INFU(14,LR)
            INFU(IRSTU+2,LR)=IEND
            INFU(IRSTU+3,LR)=0
            IF(IEND.LT.0) THEN
*              general status is: end-of-file at reading
               INFU(ISGEN+1,LR)=2
               GOTO 92
            ELSE IF(IEND.GT.0) THEN
               ERRMES='Read error'
               GOTO 91
            ELSE
*              eventually convert to local format
               CALL  FSWAP(IBUF(INDR+1),0)
               CALL FRCONV(IBUF(INDR+1),LOCAL,ICER)
               INFU(IRSTU+1,LR)=+1
            END IF
*           successful read - reset pointer
            IP=2
         ELSE
*           get next IP in same record (but save previous IP before)
            LIP=IP
            IP=NEXTIP(IP)
         END IF
*        copy record segment
         CALL FPCPRK(LR,IBUF(INDR+IP+1))
*        check segment code of new record segment
         IPS=INFU(IRKEY+10,LR)
         IF(ILLG(IPS)) GOTO 94
         IF(IS01(IPS)) THEN
*           segment is a start segment - error
            IF(IP.EQ.2) THEN
*              status: record read in, IP not yet defined
               IP=1
            ELSE
*              restore saved value
               IP=LIP
            END IF
            GOTO 94
         END IF
      ELSE
         JP=NEXTJP(JP)
         GOTO 40
      END IF
*     first data segment header after record header
   30 JP=IP+11
   40 CONTINUE
*     copy data segment header ...

*     corrupted file might provide unreasonable JP
      IF (INDR+JP+1.GT.NBUF)              GOTO 991
      IF (INDR+JP.LT.0)                   GOTO 991
      IF (IRHDR+IBUF(INDR+JP+1).GT.NLDIM) GOTO 991
                          
      DO 50 I=1,IBUF(INDR+JP+1)
   50 INFU(IRHDR+I,LR)=IBUF(INDR+JP+I)
      IF( INFU(10,LR).GT.0) THEN
         INFU(IRHDR+2,INFU(10,LR)) = INFU(IRHDR+2,LR)
         INFU(IRHDR+3,INFU(10,LR)) = INFU(IRHDR+3,LR)
      ENDIF
*     check data segment header segment code
      JPS=INFU(IRHDR+7,LR)
      IF(ILLG(JPS)) GOTO 94
      IF(.NOT.IS01(JPS)) GOTO 10
*     data segment is starting (0 or 1) - reset data pointer
      KP=0
      GOTO 55
*
*     text input -------------------------------------------------------
*
   60 IF(IP.LE.1) THEN
         ERRMES='Invalid value of IP pointing to current record header'
         GOTO 93
      ENDIF
*     key was defined
   70 NSTR=INFU(1,LR)
      IF(NSTR.LT.72.OR.NSTR.GT.256) NSTR=80
*     read or copy from buffer
      INDR=INDXB(IBUF,1000*LUNR+1)
      IF(INDR.EQ.0) GOTO 95
      IF(IBUF(INDR).EQ.0) THEN
         READ(LUNR,'(A)',END=72) STRING(1:NSTR)
         GOTO 74
*        general status: end-of-file at reading
   72    INFU(ISGEN+1,LR)=2
         GOTO 99
   74    CONTINUE
      ELSE
         STRING=' '
         CALL HLTOTX(IBUF(INDR+1),(NSTR+3)/4,STRING,NCH)
         IBUF(INDR) = 0
      END IF
*     translate and interprete
      CALL LINEIN(STRING(1:NSTR))
      CALL STMTST(NTEL,FTEXT)
      CALL STMTNU
c     CALL STMTPR
      IF(NTX.LT.1)      GOTO 70
*     first text may be "RECL" or "RECORD"
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
      ELSE IF(STMT(ITX(1,1):ITX(2,1)).EQ.'END') THEN
         GOTO 99
      ELSE IF(STMT(ITX(1,1):ITX(2,1)).EQ.'RECORD') THEN
*        copy to buffer
         CALL TXTOHL(STMT(1:NSTR),IBUF(INDR+1),NHL)
         IBUF(INDR) = NHL
         GOTO 99
      END IF
*     store data segment header
      NAMEC=STMT(ITX(1,1):ITX(2,1))
      NUM=0
      NCOL=0
      NROW=0
      FORMAT=' '
      NCH=1
      DO 76 I=2,MIN0(4,NTX)
*     check hexadecimal number
*     check integer
      IF(ITX(3,I).NE.4) GOTO 77
      IF(I.EQ.2) NUM =ITX(4,I)
      IF(I.EQ.3) NCOL=ITX(4,I)
      IF(I.EQ.4) NROW=ITX(4,I)
   76 CONTINUE
      I=5
*     check format information
   77 IF(I+2.LE.NTX) THEN
*     check format information
         IF(ITX(3,I).EQ.3) THEN
            CALL UCASE(STMT(ITX(1,I):ITX(2,I)))
            IF(STMT(ITX(1,I):ITX(2,I)).EQ.'FMT') THEN
               IF(STMT(ITX(1,I+1):ITX(2,I+1)).EQ.'=') THEN
                  FORMAT=STMT(ITX(1,I+2):ITX(2,NTX))
                  NCH=ITX(2,NTX)-ITX(1,I+2)+1
               END IF
            END IF
         END IF
      END IF
      INFU(IRHDR+1,LR)=0
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
      INFU(IRHDR+2,LR)=ICH
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
      INFU(IRHDR+3,LR)=ICH
      INFU(IRHDR+4,LR)=NUM
      INFU(IRHDR+5,LR)=NCOL
      INFU(IRHDR+6,LR)=NROW
      INFU(IRHDR+7,LR)=0
      INFU(IRHDR+8,LR)=0
      INFU(IRHDR+9,LR)=0
      CALL TXTOHL(FORMAT(1:NCH),INFU(IRHDR+10,LR),NHL)
      INFU(IRHDR+1,LR)=9+NHL
      IF( LRSAV.NE.LR ) THEN
         INFU(2,LRSAV) = INFU(2,LR)
         INFU(3,LRSAV) = INFU(3,LR)
      ENDIF
      JP=2
*
*     code common to binary and formatted read
*
*     ... and convert to character ...
   55 ICH=INFU(IRHDR+2,LR)
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
      ICH=INFU(IRHDR+3,LR)
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
      NAMDAT=NAMEC
*     ... and copy numerical information
      NUM =INFU(IRHDR+4,LR)
      NCOL=INFU(IRHDR+5,LR)
      NROW=INFU(IRHDR+6,LR)
*     copy format
      CALL HLTOTX(INFU(IRHDR+10,LR),INFU(IRHDR+1,LR)-9,FORMAT,NCH)
      IER=0
      ERRMES=' '
      GOTO 90
*     all error conditions are set here
*               only argument IER is returned
   91 IER = 1
      GOTO 90
  991 IER = 1
      ERRMES='Data segment header corrupt'
      GOTO 90
   92 IER = 2
      ERRMES='Unexpected end-of-data (record incomplete)'
      GOTO 90
   93 IER = 3
*     Error in calling order
      GOTO 90
*     illegal segment code
   94 IER = 4
      ERRMES='Illegal segment code'
      GOTO 90
*
   95 IER = 5
      ERRMES='Input buffer not available'
      GOTO 90
*  DUNIT parameter not defined in OPEN statement for index file
*     which refers to cartridge data file
   96 IER = 6
      ERRMES='DUNIT not defined or wrong using of access method'
      GOTO 90
*     index or data file corrupted
   97 IER = 7
      ERRMES='Index and data file do not conform to each other'
      GOTO 90
*     end-of-record reached (no further data for this logical record)
   99 IER = -1
*     this is the normal end of a logical record
 
*     common code for all returns (except unit undefined) --------------
*     store code of actual logical record
   90 IF(IER.GT.0) CALL FPWMSG(IER,'FRHDR',ERRMES)
      INFU(IRSTU+3,LR)=IER
*     restore record pointer
      CALL FIPCOP(IP,JP,KP,-LR)
*
  100 CONTINUE
      IF(IER.EQ.0) THEN
*        select
         IF(INFU(10,LR).GT.0) THEN
            CALL FSELCT(2,LUNBK(INFU(10,LR)),IDEC)
         ELSE
            CALL FSELCT(2,LUNBK(LR),IDEC)
         ENDIF
         IF(IDEC.EQ.1) GOTO 01
         IF(IDEC.EQ.2) GOTO 99
         INFU(IRSTA+5,LR)=INFU(IRSTA+5,LR)+1
         IF(INFU(10,LR).GT.0)
     +      INFU(IRSTA+5,INFU(10,LR))=INFU(IRSTA+5,INFU(10,LR))+1
      END IF
*
      LR = LRSAV
      LUNR = LUNRSAV
*
#ifndef Linux
200   RETURN
#else /* Linux */
      RETURN
#endif /* Linux */
      END
