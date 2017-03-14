#ifndef Linux
*CMZU:  1.00/16 07/03/96  18.17.35  by  Wiesiek Dlugosz
#endif /* not Linux */
*CMZ :  1.00/00 16/12/94  17.00.00  by  Pavel Binko
*-- Author :    Volker Blobel   16/12/94
      SUBROUTINE FRDAT(NR,IARRAY,NDIM)
*
      INTEGER IARRAY(*)
*
************************************************************************
*
*     Read data
*
************************************************************************
*
      CHARACTER CHA*4, STRING*256, FTEXT*16
      LOGICAL IS01,IS03,ILLG,START
      DATA START/.TRUE./
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
         ILLG(I)   =I.LT.0.OR.I.GT.3
         IS01(I)   =I.EQ.0.OR.I.EQ.1
         IS03(I)   =I.EQ.0.OR.I.EQ.3
*
      IF(START) THEN
         START=.FALSE.
      END IF
      NR=0
*
*     general tests
*
*     error if no read unit defined
      IER=8
      IF(LR.EQ.0) THEN
         CALL FPWMSG(IER,'FRDAT','Input unit not defined')
         GOTO 100
      ENDIF
*
#ifndef Linux
*     Ordered access ...
      IF(INFU(3,LR).EQ.1.AND.INFU(25,LR).GT.0) THEN
         CALL FRDATO(NR,IARRAY,NDIM,IER)
*        Set 'Locked record' return code
         IF(IER.EQ.-911) NR=IER
         GOTO 100
      ENDIF
*
#endif /* not Linux */
*     switch eventually from select to data file
      LRSAV = LR
      LUNRSAV = LUNR
*     reset key flag
      INFU(IRSTU+4,LR)=0
*
      IF(INFU(7,LR).GT.0) CALL FRUNIT(LUNBK(INFU(7,LR)))
*
*     reset key flag
      INFU(IRSTU+4,LR)=0
      IER=0
      IF(NDIM.LE.0) GOTO 98
*
*     get record pointers for binary input
      CALL FIPCOP(IP,JP,KP,LR)
*
*     important status words
*        INFU(ISGEN+1,LR) = read write etc
*        INFU(IRSTU+2,LR) = error code of last read operation
*        INFU(IRSTU+3,LR) = error code of actual logical record
*
*     general status of unit
#ifdef Linux
      IF(INFU(25,LR).EQ.0) THEN
#endif /* Linux */
      IF(INFU(ISGEN+1,LR).EQ.2) THEN
         ERRMES='Status is end-of-file at reading'
         GOTO 97
      ELSE IF(INFU(ISGEN+1,LR).NE.1) THEN
         ERRMES='Status is undef, write or rewind'
         GOTO 93
      END IF
#ifdef Linux
      END IF
#endif /* Linux */
*
*     check last read operation
      IF(INFU(IRSTU+2,LR).NE.0) THEN
         ERRMES='Last read operation was in error'
         GOTO 91
      END IF
*
*     check status of actual logical record
      IF(INFU(IRSTU+3,LR).NE.0) THEN
         ERRMES='Actual logical record was in error'
         IER=INFU(IRSTU+3,LR)
         GOTO 90
      END IF
      IF(INFU(3,LR).EQ.2) GOTO 60
*
*     code for binary input --------------------------------------------
*
*     value of IP
*              IP > 1   pointing to current record header
*              IP = 1   record is read in, IP not yet defined
*              IP = 0   no record read in
*              IP < 0   ?
*
      NUMBNK = 1000*LUNR+1
      INDR=INDXB(IBUF,NUMBNK)
      IF(INDR.EQ.0) THEN
         ERRMES='No buffer bank existing'
         GOTO 93
      END IF
*
*     IP and JP have to be defined
      IF(IP.LE.1) THEN
         ERRMES='Invalid value of IP pointer. It''s likely wrong record'
         GOTO 93
      ENDIF
      IF(JP.EQ.0) THEN
         ERRMES='Invalid value of JP pointer. It''s likely wrong record'
         GOTO 93
      ENDIF
*
*     IP is defined, check conversion of record
      IF(INFU(IRSTU+1,LR).LT.0) THEN
         CALL FRCONV(IBUF(INDR+1),LOCAL,ICER)
         INFU(IRSTU+1,LR)=1
      END IF
*
*     number of words already copied
      IS=0
   11 IF(KP.EQ.0) KP=JP+INFU(IRHDR+1,LR)
*
*     get index of last data word
      KPL=JP+INFU(IRHDR+1,LR)+INFU(IRHDR+9,LR)
*
*     copy as many as possible
      NCOP=MIN0(NDIM-IS,KPL-KP)
      CALL UCOPY(IBUF(INDR+KP+1),IARRAY(IS+1),NCOP)
*
*     update pointer
      KP=KP+NCOP
      IS=IS+NCOP
      IF(IS.EQ.NDIM.OR.IS03(INFU(IRHDR+7,LR))) THEN
*        data transfer finished for this call
         NR=IS
         GOTO 90
      END IF
*
*     next part required
      IF(NEXTIP(IP).EQ.NEXTJP(JP)) THEN
*        reset JP (was last data segment inside record segment)
         JP=0
*        check illegal segment code
         IF(ILLG(INFU(IRKEY+10,LR))) THEN
            ERRMES='Illegal segment code'
            GOTO 94
         ENDIF
*        check end-of-logical-record
         IF(IS03(INFU(IRKEY+10,LR))) GOTO 99
*
*        continuation of logical record in next record segment
         IF(NEXTIP(IP).EQ.IBUF(INDR+2)) THEN
*           reset IP (was last record segment in record)
            IP=0
            IF(INFU(10,LR).GT.0) THEN
               INFU(27,LR) = INFU(14,LR)+1
#ifdef Linux
            ELSE IF( INFU(25,LR).NE.0 ) THEN
               INFU(27,LR) = INFU(29,LR)+1
#endif /* Linux */
            ENDIF
            CALL FRREC(LUNR,IBUF(INDR),IBUF(INDR+1),IEND)
            INFU(29,LR) = INFU(14,LR)
            INFU(IRSTU+2,LR)=IEND
            IF(IEND.LT.0) THEN
*              general status is: end-of-file at reading
               INFU(ISGEN+1,LR)=2
               ERRMES='Unexpected end-of-block (block incomplete)'
               GOTO 92
            ELSE IF(IEND.GT.0) THEN
               ERRMES='Read error'
               GOTO 91
            ELSE
*              eventually convert to local format
               CALL  FSWAP(IBUF(INDR+1),0)
               CALL FRCONV(IBUF(INDR+1),LOCAL,ICER)
               INFU(IRSTU+1,LR)=+1
*              store actual record number
*--BUG-->      INFU(IRSTU+5,LR)=IBUF(INDR+5)/100
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
         IF(ILLG(IPS)) THEN
            ERRMES='Illegal segment code'
            GOTO 94
         ENDIF
         IF(IS01(IPS)) THEN
*           segment is a start segment - error
            IF(IP.EQ.2) THEN
*              status: record read in, IP not yet defined
               IP=1
            ELSE
*              restore saved value
               IP=LIP
            END IF
            ERRMES='Illegal segment code'
            GOTO 94
         END IF
      ELSE
         JP=NEXTJP(JP)
         GOTO 40
      END IF
*
*     first data segment header after record header
   30 JP=IP+11
*     copy data segment header ...
   40 DO 50 I=1,IBUF(INDR+JP+1)
   50 INFU(IRHDR+I,LR)=IBUF(INDR+JP+I)
*     ... convert to local format ...
C     CALL CNVR(INFU(IRHDR+1,LR),...
*     check data segment header segment code
      JPS=INFU(IRHDR+7,LR)
      IF(ILLG(JPS) .OR. IS01(JPS)) THEN
         ERRMES='Illegal data segment header segment code'
         GOTO 94
      ENDIF
*     data segment is starting (0 or 1) - reset data pointer
      KP=0
      GOTO 11
*
*     code for text input ----------------------------------------------
*
   60 IF(IP.LE.1 .OR. JP.LE.1) THEN
         ERRMES='Error in calling order'
         GOTO 93
      ENDIF
*     key data segment was defined
      INFU(IRSTU+4,LR)=0
   70 NSTR=INFU(1,LR)
      IF(NSTR.LT.72.OR.NSTR.GT.256) NSTR=80
*     read or copy from buffer
      INDR=INDXB(IBUF,1000*LUNR+1)
      IF(INDR.EQ.0) GOTO 95
      IF(IBUF(INDR).EQ.0) THEN
         READ(LUNR,101,END=72) STRING(1:NSTR)
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
C     WRITE(6,138) STRING(1:NSTR)
  138 FORMAT(' FRDAT READS:',A)
*     translate and interprete
      CALL LINEIN(STRING(1:NSTR))
      CALL STMTST(NTEL,FTEXT)
      CALL STMTNU
c     CALL STMTPR
      IF(NTX.LT.1)      GOTO 70
      IF(ITX(3,1).EQ.3) THEN
*        text found - fill to buffer
         INDR=INDXB(IBUF,1000*LUNR+1)
         IF(INDR.EQ.0) THEN
            INDR=INDXA(IBUF,1000*LUNR+1,64)
            IF(INDR.EQ.0) GOTO 95
         END IF
         CALL TXTOHL(STMT(1:NSTR),IBUF(INDR+1),NHL)
         IBUF(INDR) = NHL
C        WRITE(6,143) STMT(1:NSTR),NHL
  143    FORMAT(' FRDAT TO BUFFER:',A,I6)
         GOTO 99
      END IF
*     copy to array
      DO 76 I=1,NTX
      IF(ITX(3,I).EQ.2) THEN
*        special case of text in parentheses
         JA=ITX(1,I)
         JE=ITX(2,I)
         DO 75 J=JA,JE,4
         IF(NR.EQ.NDIM) THEN
*           blank out already stored data ...
            STMT(1:J-1)=' '
            STMT(J-1:J-1)=''''
*           ...  and store in buffer for next call
            INDR=INDXB(IBUF,1000*LUNR+1)
            IF(INDR.EQ.0) THEN
               INDR=INDXA(IBUF,1000*LUNR+1,64)
               IF(INDR.EQ.0) GOTO 95
            END IF
            CALL TXTOHL(STMT(1:NSTR),IBUF(INDR+1),NHL)
            IBUF(INDR) = NHL
            GOTO 99
         END IF
         JB=MIN0(J+3,JE)
         CHA=STMT(J:JB)
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
         NR=NR+1
         IARRAY(NR)=ICH
   75    CONTINUE
         GOTO 76
      END IF
      IF(ITX(3,I).LT.4.OR.ITX(3,I).GT.6) GOTO 76
      IF(NR.EQ.NDIM) THEN
*        blank out already stored data ...
         IF(ITX(1,I).GT.1) STMT(1:(ITX(1,I)-1))=' '
*        ...  and store in buffer for next call
         INDR=INDXB(IBUF,1000*LUNR+1)
         IF(INDR.EQ.0) THEN
            INDR=INDXA(IBUF,1000*LUNR+1,64)
            IF(INDR.EQ.0) GOTO 95
         END IF
         CALL TXTOHL(STMT(1:NSTR),IBUF(INDR+1),NHL)
         IBUF(INDR) = NHL
         GOTO 99
      END IF
      NR=NR+1
      IARRAY(NR)=ITX(4,I)
   76 CONTINUE
C     WRITE(6,137) NR
  137 FORMAT(' ONE LINE ENDS WITH NR=',I5)
      GOTO 70
*     all error conditions are set here
*               only argument IER is returned
*     pure read error
   91 IER = 1
      GOTO 90
*     unexpected end-of-data (record incomplete)
   92 IER = 2
      GOTO 90
*     error in calling order
   93 IER = 3
      GOTO 90
*     illegal segment code
   94 IER = 4
      GOTO 90
   95 IER = 5
      ERRMES='Input buffer not available'
      GOTO 90
C  96 IER = 6
C     GOTO 90
*     read attempt after end-of-file
   97 IER = 7
      GOTO 90
*     dimension incorrect
   98 IER = 8
      ERRMES='Dimension incorrect'
      GOTO 90
*     end-of-record reached (no further data for this logical record)
   99 IER = 0
      ERRMES=' '
      IF(INFU(ISGEN+1,LR).EQ.2) IER=-1
*     this is the normal end of a logical record
*
*     common code ------------------------------------------------------
*
*     store code of actual logical record
   90 INFU(IRSTU+3,LR)=IER
*     restore record pointer
      INFU(IRSTU+ 8,LR)=IP
      INFU(IRSTU+ 9,LR)=JP
      INFU(IRSTU+10,LR)=KP
      CALL FIPCOP(IP,JP,KP,-LR)
      LR = LRSAV
      LUNR = LUNRSAV
*
      IF(IER.GT.0) CALL FPWMSG(IER,'FRDAT',ERRMES)
C     WRITE(6,107) IER
*
  100 RETURN
  101 FORMAT(A)
  107 FORMAT(' FRDAT IER= ',I6)
      END
