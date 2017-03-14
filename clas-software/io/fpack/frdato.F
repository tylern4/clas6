#ifdef HPUX
*CMZU:  1.00/16 29/02/96  21.44.38  by  Unknown
*-- Author :    Sergey Esenov   27/02/96
      SUBROUTINE FRDATO(NR,IARRAY,NDIM,IER)
*
      INTEGER IARRAY(*)
*
************************************************************************
*
*     Read data
*
************************************************************************
*
      PARAMETER (NW5 = 5)
      CHARACTER CHA*4
      INTEGER IFREE(2)
      LOGICAL IS01,IS03,ILLG,START
      DATA START/.TRUE./
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
      CHARACTER  ERRMES*256
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
*     statement functions
         NEXTIP(IP)=IP+IBUF(INDR+IP+11)+11
         NEXTJP(JP)=JP+IBUF(INDR+JP+ 1)+IBUF(INDR+JP+ 9)
         ILLG(I)   =I.LT.0.OR.I.GT.3
         IS01(I)   =I.EQ.0.OR.I.EQ.1
         IS03(I)   =I.EQ.0.OR.I.EQ.3
*
      IF(START) THEN
         CHA='#-FR'
*KEEP,ICHCHA.
*     get hollerith 4H from char*4
      ICH=IOR(ISHFT(ICHAR(CHA(1:1)),24),IOR(ISHFT(ICHAR(CHA(2:2)),16),
     +    IOR(ISHFT(ICHAR(CHA(3:3)), 8),ICHAR(          CHA(4:4)))))
*KEND.
         IFREE(1) = ICH
         CHA='EE-#'
*KEEP,ICHCHA.
*     get hollerith 4H from char*4
      ICH=IOR(ISHFT(ICHAR(CHA(1:1)),24),IOR(ISHFT(ICHAR(CHA(2:2)),16),
     +    IOR(ISHFT(ICHAR(CHA(3:3)), 8),ICHAR(          CHA(4:4)))))
*KEND.
         IFREE(2) = ICH
         START=.FALSE.
      END IF
      NR=0
*
*     general tests
*
*     error if no read unit defined
      IER=8
      IF(LR.EQ.0) THEN
         CALL FPWMSG(IER,'FRDATO','Input unit not defined')
         GOTO 100
      ENDIF
*
*     switch eventually from select to data file
      LRSAV = LR
      LUNRSAV = LUNR
*     reset key flag
      INFU(IRSTU+4,LR)=0
      IER=0
      IF(NDIM.LE.0) GOTO 98
*
*     get record pointers for binary input
      CALL FIPCOP(IP,JP,KP,LR)
*      PRINT '(1X,''FRDATO.Entry IP,JP,KP'',3I8)',IP,JP,KP
*
*     important status words
*        INFU(ISGEN+1,LR) = read write etc
*        INFU(IRSTU+2,LR) = error code of last read operation
*        INFU(IRSTU+3,LR) = error code of actual logical record
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
      NXTIP = NEXTIP(IP)
      NTAIL = NXTIP - NW5
      IF(NTAIL.EQ.NEXTJP(JP)) THEN
*        reset JP (was last data segment inside record segment)
         JP=0
*        check illegal segment code
         IF(ILLG(INFU(IRKEY+10,LR))) GOTO 94
*        check end-of-logical-record
         IF(IS03(INFU(IRKEY+10,LR))) GOTO 99
         INFU(27,LR) = IBUF(INDR+NTAIL+3)
         DTFLPS(LR)  = IBUF(INDR+NTAIL+4)
         IF(INFU(27,LR).EQ.0)  GOTO 99
*        continuation of logical record in next record segment
*        reset IP (was last record segment in record)
         IP=0
         CALL FRREC(LUNR,IBUF(INDR),IBUF(INDR+1),IEND)
         INFU(29,LR) = INFU(14,LR)
         INFU(IRSTU+2,LR)=IEND
         IF(IEND.LT.0) THEN
*           general status is: end-of-file at reading
            INFU(ISGEN+1,LR)=2
            ERRMES='Unexpected end-of-block (block incomplete)'
            GOTO 92
         ELSE IF(IEND.GT.0) THEN
            ERRMES='Read error'
            GOTO 91
         ENDIF
*        convert to local format
         CALL  FSWAP(IBUF(INDR+1),0)
         CALL FRCONV(IBUF(INDR+1),LOCAL,ICER)
         INFU(IRSTU+1,LR)=+1
*
*        successful read - reset pointer
         IP = DTFLPS(LR)
         IF(IP.EQ.0) IP=2
*        Check the data integrity ...
*        Get version#1 in the top of segment
         IVERS1 = IBUF(INDR+IP+10)/16
*        if block was locked -> goto 911 (warn 'master')
         IF( IVERS1.EQ.0 ) GOTO 911
*        Strip segment code from version#
         IBUF(INDR+IP+10)=MOD(IBUF(INDR+IP+10),16)
*        Get version#2 in bottom
         NXTIP = NEXTIP(IP)
         NTAIL = NXTIP - NW5
         IVERS2 = IBUF(INDR+NTAIL+5)
*        IVERS1 must be identical to IVERS2
         IF(IVERS1.NE.IVERS2) GOTO 911
*        If version# has not been initialized yet ...
         IF(INFU(IRSTU+21,LR).EQ.0) INFU(IRSTU+21,LR)=IVERS1
*        version of previous block is not equal the current one
         IF(INFU(IRSTU+21,LR).NE.IVERS1) GOTO 911
*
*        Extract the address of the next part (extent) of a logical record
*        ... block# & offset in block
*         INFU(IRSTU+22,LR) = IBUF(INDR+NTAIL+3)
*         INFU(IRSTU+23,LR) = IBUF(INDR+NTAIL+4)
         IF(IP.LE.0 .AND. IP.GE.NTAIL)
     +            CALL LOGERROR('FRDATO. BAD IP VALUE')
*        copy record segment
         CALL FPCPRK(LR,IBUF(INDR+IP+1))
         INFU(IRKEY+10,LR) = IBUF(INDR+IP+10)
*        check segment code of new record segment
         IPS=INFU(IRKEY+10,LR)
         IF(ILLG(IPS)) GOTO 94
         IF(IS01(IPS)) GOTO 94
         GOTO 30
      ENDIF
      IF(JP.LE.0) CALL LOGERROR('FRDATO. INVALID JP(=0)')
      JP=NEXTJP(JP)
      IF(JP.GT.NTAIL) CALL LOGERROR('FRDATO. INVALID JP(>NTAIL)')
*     Check that header was not marked as free ...
      NMORE = NTAIL - JP
      IF(NMORE.GT.2) THEN
         IF(IBUF(INDR+JP+2).EQ.IFREE(1).AND.
     +      IBUF(INDR+JP+3).EQ.IFREE(2).AND.
     +      IBUF(INDR+JP+1).EQ.NMORE)       GOTO 99
      ELSEIF(NMORE.EQ.2) THEN
         IF(IBUF(INDR+JP+2).EQ.IFREE(1).AND.
     +      IBUF(INDR+JP+1).EQ.NMORE)       GOTO 99
      ELSEIF(NMORE.EQ.1) THEN
         IF(IBUF(INDR+JP+1).EQ.NMORE)       GOTO 99
      ENDIF
      GOTO 40
*
*     first data segment header after record header
   30 JP=IP+11
*     copy data segment header ...
   40 DO 50 I=1,IBUF(INDR+JP+1)
   50 INFU(IRHDR+I,LR)=IBUF(INDR+JP+I)
*     check data segment header segment code
      JPS=INFU(IRHDR+7,LR)
      IF(ILLG(JPS) .OR. IS01(JPS)) GOTO 94
*     data segment is starting (0 or 1) - reset data pointer
      KP=0
      GOTO 11
*
*     all error conditions are set here
*               only argument IER is returned
*     pure read error
   91 IER = 1
      GOTO 90
*     Record was locked or changed by another job.
  911 IER = -911
*     Index path for read incorrect
      INFU(ISGEN+9,LR) = 1
      GOTO 100
*     unexpected end-of-data (record incomplete)
   92 IER = 2
      GOTO 90
*     error in calling order
   93 IER = 3
      GOTO 90
*     illegal segment code
   94 IER = 4
      ERRMES='Illegal segment code'
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
*     save record pointer
      INFU(IRSTU+ 8,LR)=IP
      INFU(IRSTU+ 9,LR)=JP
      INFU(IRSTU+10,LR)=KP
      CALL FIPCOP(IP,JP,KP,-LR)
      LR = LRSAV
      LUNR = LUNRSAV
*
      IF(IER.GT.0) CALL FPWMSG(IER,'FRDATO',ERRMES)

C     WRITE(6,107) IER
*
  100 RETURN
  101 FORMAT(A)
  107 FORMAT(' FRDAT IER= ',I6)
      END
#elif defined Linux
      subroutine dummy_frdato
      implicit none
      return
      end
#elif defined SunOS
*CMZU:  1.00/16 29/02/96  21.44.38  by  Unknown
*-- Author :    Sergey Esenov   27/02/96
      SUBROUTINE FRDATO(NR,IARRAY,NDIM,IER)
*
      INTEGER IARRAY(*)
*
************************************************************************
*
*     Read data
*
************************************************************************
*
      PARAMETER (NW5 = 5)
      CHARACTER CHA*4
      INTEGER IFREE(2)
      LOGICAL IS01,IS03,ILLG,START
      DATA START/.TRUE./
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
*KEEP,FBUFFER.
*     buffer common
      PARAMETER (NBUF=100000)
      COMMON/FBUF/IBUF(NBUF)
      SAVE/FBUF/
*     d.p. weygand - 06/1998 - add definition of errmes for SunOS
      CHARACTER  ERRMES*256
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
         CHA='#-FR'
*KEEP,ICHCHA.
*     get hollerith 4H from char*4
      ICH=IOR(ISHFT(ICHAR(CHA(1:1)),24),IOR(ISHFT(ICHAR(CHA(2:2)),16),
     +    IOR(ISHFT(ICHAR(CHA(3:3)), 8),ICHAR(          CHA(4:4)))))
*KEND.
         IFREE(1) = ICH
         CHA='EE-#'
*KEEP,ICHCHA.
*     get hollerith 4H from char*4
      ICH=IOR(ISHFT(ICHAR(CHA(1:1)),24),IOR(ISHFT(ICHAR(CHA(2:2)),16),
     +    IOR(ISHFT(ICHAR(CHA(3:3)), 8),ICHAR(          CHA(4:4)))))
*KEND.
         IFREE(2) = ICH
         START=.FALSE.
      END IF
      NR=0
*
*     general tests
*
*     error if no read unit defined
      IER=8
      IF(LR.EQ.0) THEN
         CALL FPWMSG(IER,'FRDATO','Input unit not defined')
         GOTO 100
      ENDIF
*
*     switch eventually from select to data file
      LRSAV = LR
      LUNRSAV = LUNR
*     reset key flag
      INFU(IRSTU+4,LR)=0
      IER=0
      IF(NDIM.LE.0) GOTO 98
*
*     get record pointers for binary input
      CALL FIPCOP(IP,JP,KP,LR)
*      PRINT '(1X,''FRDATO.Entry IP,JP,KP'',3I8)',IP,JP,KP
*
*     important status words
*        INFU(ISGEN+1,LR) = read write etc
*        INFU(IRSTU+2,LR) = error code of last read operation
*        INFU(IRSTU+3,LR) = error code of actual logical record
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
      NXTIP = NEXTIP(IP)
      NTAIL = NXTIP - NW5
      IF(NTAIL.EQ.NEXTJP(JP)) THEN
*        reset JP (was last data segment inside record segment)
         JP=0
*        check illegal segment code
         IF(ILLG(INFU(IRKEY+10,LR))) GOTO 94
*        check end-of-logical-record
         IF(IS03(INFU(IRKEY+10,LR))) GOTO 99
         INFU(27,LR) = IBUF(INDR+NTAIL+3)
         DTFLPS(LR)  = IBUF(INDR+NTAIL+4)
         IF(INFU(27,LR).EQ.0)  GOTO 99
*        continuation of logical record in next record segment
*        reset IP (was last record segment in record)
         IP=0
         CALL FRREC(LUNR,IBUF(INDR),IBUF(INDR+1),IEND)
         INFU(29,LR) = INFU(14,LR)
         INFU(IRSTU+2,LR)=IEND
         IF(IEND.LT.0) THEN
*           general status is: end-of-file at reading
            INFU(ISGEN+1,LR)=2
            ERRMES='Unexpected end-of-block (block incomplete)'
            GOTO 92
         ELSE IF(IEND.GT.0) THEN
            ERRMES='Read error'
            GOTO 91
         ENDIF
*        convert to local format
         CALL  FSWAP(IBUF(INDR+1),0)
         CALL FRCONV(IBUF(INDR+1),LOCAL,ICER)
         INFU(IRSTU+1,LR)=+1
*
*        successful read - reset pointer
         IP = DTFLPS(LR)
         IF(IP.EQ.0) IP=2
*        Check the data integrity ...
*        Get version#1 in the top of segment
         IVERS1 = IBUF(INDR+IP+10)/16
*        if block was locked -> goto 911 (warn 'master')
         IF( IVERS1.EQ.0 ) GOTO 911
*        Strip segment code from version#
         IBUF(INDR+IP+10)=MOD(IBUF(INDR+IP+10),16)
*        Get version#2 in bottom
         NXTIP = NEXTIP(IP)
         NTAIL = NXTIP - NW5
         IVERS2 = IBUF(INDR+NTAIL+5)
*        IVERS1 must be identical to IVERS2
         IF(IVERS1.NE.IVERS2) GOTO 911
*        If version# has not been initialized yet ...
         IF(INFU(IRSTU+21,LR).EQ.0) INFU(IRSTU+21,LR)=IVERS1
*        version of previous block is not equal the current one
         IF(INFU(IRSTU+21,LR).NE.IVERS1) GOTO 911
*
*        Extract the address of the next part (extent) of a logical record
*        ... block# & offset in block
*         INFU(IRSTU+22,LR) = IBUF(INDR+NTAIL+3)
*         INFU(IRSTU+23,LR) = IBUF(INDR+NTAIL+4)
         IF(IP.LE.0 .AND. IP.GE.NTAIL)
     +            CALL LOGERROR('FRDATO. BAD IP VALUE')
*        copy record segment
         CALL FPCPRK(LR,IBUF(INDR+IP+1))
         INFU(IRKEY+10,LR) = IBUF(INDR+IP+10)
*        check segment code of new record segment
         IPS=INFU(IRKEY+10,LR)
         IF(ILLG(IPS)) GOTO 94
         IF(IS01(IPS)) GOTO 94
         GOTO 30
      ENDIF
      IF(JP.LE.0) CALL LOGERROR('FRDATO. INVALID JP(=0)')
      JP=NEXTJP(JP)
      IF(JP.GT.NTAIL) CALL LOGERROR('FRDATO. INVALID JP(>NTAIL)')
*     Check that header was not marked as free ...
      NMORE = NTAIL - JP
      IF(NMORE.GT.2) THEN
         IF(IBUF(INDR+JP+2).EQ.IFREE(1).AND.
     +      IBUF(INDR+JP+3).EQ.IFREE(2).AND.
     +      IBUF(INDR+JP+1).EQ.NMORE)       GOTO 99
      ELSEIF(NMORE.EQ.2) THEN
         IF(IBUF(INDR+JP+2).EQ.IFREE(1).AND.
     +      IBUF(INDR+JP+1).EQ.NMORE)       GOTO 99
      ELSEIF(NMORE.EQ.1) THEN
         IF(IBUF(INDR+JP+1).EQ.NMORE)       GOTO 99
      ENDIF
      GOTO 40
*
*     first data segment header after record header
   30 JP=IP+11
*     copy data segment header ...
   40 DO 50 I=1,IBUF(INDR+JP+1)
   50 INFU(IRHDR+I,LR)=IBUF(INDR+JP+I)
*     check data segment header segment code
      JPS=INFU(IRHDR+7,LR)
      IF(ILLG(JPS) .OR. IS01(JPS)) GOTO 94
*     data segment is starting (0 or 1) - reset data pointer
      KP=0
      GOTO 11
*
*     all error conditions are set here
*               only argument IER is returned
*     pure read error
   91 IER = 1
      GOTO 90
*     Record was locked or changed by another job.
  911 IER = -911
*     Index path for read incorrect
      INFU(ISGEN+9,LR) = 1
      GOTO 100
*     unexpected end-of-data (record incomplete)
   92 IER = 2
      GOTO 90
*     error in calling order
   93 IER = 3
      GOTO 90
*     illegal segment code
   94 IER = 4
      ERRMES='Illegal segment code'
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
*     save record pointer
      INFU(IRSTU+ 8,LR)=IP
      INFU(IRSTU+ 9,LR)=JP
      INFU(IRSTU+10,LR)=KP
      CALL FIPCOP(IP,JP,KP,-LR)
      LR = LRSAV
      LUNR = LUNRSAV
*
      IF(IER.GT.0) CALL FPWMSG(IER,'FRDATO',ERRMES)
 
C     WRITE(6,107) IER
*
  100 RETURN
  101 FORMAT(A)
  107 FORMAT(' FRDAT IER= ',I6)
      END
#else
*CMZU:  1.00/16 29/02/96  21.44.38  by  Unknown
*-- Author :    Sergey Esenov   27/02/96
      SUBROUTINE FRDATO(NR,IARRAY,NDIM,IER)
*
      INTEGER IARRAY(*)
*
************************************************************************
*
*     Read data
*
************************************************************************
*
      PARAMETER (NW5 = 5)
      CHARACTER CHA*4
      INTEGER IFREE(2)
      LOGICAL IS01,IS03,ILLG,START
      DATA START/.TRUE./
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
*
*     jam - 04/1996 - define errmes for aix
      CHARACTER*132 ERRMES 
*
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
*     statement functions
         NEXTIP(IP)=IP+IBUF(INDR+IP+11)+11
         NEXTJP(JP)=JP+IBUF(INDR+JP+ 1)+IBUF(INDR+JP+ 9)
         ILLG(I)   =I.LT.0.OR.I.GT.3
         IS01(I)   =I.EQ.0.OR.I.EQ.1
         IS03(I)   =I.EQ.0.OR.I.EQ.3
*
      IF(START) THEN
         CHA='#-FR'
*KEEP,ICHCHA.
*     get hollerith 4H from char*4
      ICH=IOR(ISHFT(ICHAR(CHA(1:1)),24),IOR(ISHFT(ICHAR(CHA(2:2)),16),
     +    IOR(ISHFT(ICHAR(CHA(3:3)), 8),ICHAR(          CHA(4:4)))))
*KEND.
         IFREE(1) = ICH
         CHA='EE-#'
*KEEP,ICHCHA.
*     get hollerith 4H from char*4
      ICH=IOR(ISHFT(ICHAR(CHA(1:1)),24),IOR(ISHFT(ICHAR(CHA(2:2)),16),
     +    IOR(ISHFT(ICHAR(CHA(3:3)), 8),ICHAR(          CHA(4:4)))))
*KEND.
         IFREE(2) = ICH
         START=.FALSE.
      END IF
      NR=0
*
*     general tests
*
*     error if no read unit defined
      IER=8
      IF(LR.EQ.0) THEN
         CALL FPWMSG(IER,'FRDATO','Input unit not defined')
         GOTO 100
      ENDIF
*
*     switch eventually from select to data file
      LRSAV = LR
      LUNRSAV = LUNR
*     reset key flag
      INFU(IRSTU+4,LR)=0
      IER=0
      IF(NDIM.LE.0) GOTO 98
*
*     get record pointers for binary input
      CALL FIPCOP(IP,JP,KP,LR)
*      PRINT '(1X,''FRDATO.Entry IP,JP,KP'',3I8)',IP,JP,KP
*
*     important status words
*        INFU(ISGEN+1,LR) = read write etc
*        INFU(IRSTU+2,LR) = error code of last read operation
*        INFU(IRSTU+3,LR) = error code of actual logical record
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
      NXTIP = NEXTIP(IP)
      NTAIL = NXTIP - NW5
      IF(NTAIL.EQ.NEXTJP(JP)) THEN
*        reset JP (was last data segment inside record segment)
         JP=0
*        check illegal segment code
         IF(ILLG(INFU(IRKEY+10,LR))) GOTO 94
*        check end-of-logical-record
         IF(IS03(INFU(IRKEY+10,LR))) GOTO 99
         INFU(27,LR) = IBUF(INDR+NTAIL+3)
         DTFLPS(LR)  = IBUF(INDR+NTAIL+4)
         IF(INFU(27,LR).EQ.0)  GOTO 99
*        continuation of logical record in next record segment
*        reset IP (was last record segment in record)
         IP=0
         CALL FRREC(LUNR,IBUF(INDR),IBUF(INDR+1),IEND)
         INFU(29,LR) = INFU(14,LR)
         INFU(IRSTU+2,LR)=IEND
         IF(IEND.LT.0) THEN
*           general status is: end-of-file at reading
            INFU(ISGEN+1,LR)=2
            ERRMES='Unexpected end-of-block (block incomplete)'
            GOTO 92
         ELSE IF(IEND.GT.0) THEN
            ERRMES='Read error'
            GOTO 91
         ENDIF
*        convert to local format
         CALL  FSWAP(IBUF(INDR+1),0)
         CALL FRCONV(IBUF(INDR+1),LOCAL,ICER)
         INFU(IRSTU+1,LR)=+1
*
*        successful read - reset pointer
         IP = DTFLPS(LR)
         IF(IP.EQ.0) IP=2
*        Check the data integrity ...
*        Get version#1 in the top of segment
         IVERS1 = IBUF(INDR+IP+10)/16
*        if block was locked -> goto 911 (warn 'master')
         IF( IVERS1.EQ.0 ) GOTO 911
*        Strip segment code from version#
         IBUF(INDR+IP+10)=MOD(IBUF(INDR+IP+10),16)
*        Get version#2 in bottom
         NXTIP = NEXTIP(IP)
         NTAIL = NXTIP - NW5
         IVERS2 = IBUF(INDR+NTAIL+5)
*        IVERS1 must be identical to IVERS2
         IF(IVERS1.NE.IVERS2) GOTO 911
*        If version# has not been initialized yet ...
         IF(INFU(IRSTU+21,LR).EQ.0) INFU(IRSTU+21,LR)=IVERS1
*        version of previous block is not equal the current one
         IF(INFU(IRSTU+21,LR).NE.IVERS1) GOTO 911
*
*        Extract the address of the next part (extent) of a logical record
*        ... block# & offset in block
*         INFU(IRSTU+22,LR) = IBUF(INDR+NTAIL+3)
*         INFU(IRSTU+23,LR) = IBUF(INDR+NTAIL+4)
         IF(IP.LE.0 .AND. IP.GE.NTAIL)
     +            CALL LOGERROR('FRDATO. BAD IP VALUE')
*        copy record segment
         CALL FPCPRK(LR,IBUF(INDR+IP+1))
         INFU(IRKEY+10,LR) = IBUF(INDR+IP+10)
*        check segment code of new record segment
         IPS=INFU(IRKEY+10,LR)
         IF(ILLG(IPS)) GOTO 94
         IF(IS01(IPS)) GOTO 94
         GOTO 30
      ENDIF
      IF(JP.LE.0) CALL LOGERROR('FRDATO. INVALID JP(=0)')
      JP=NEXTJP(JP)
      IF(JP.GT.NTAIL) CALL LOGERROR('FRDATO. INVALID JP(>NTAIL)')
*     Check that header was not marked as free ...
      NMORE = NTAIL - JP
      IF(NMORE.GT.2) THEN
         IF(IBUF(INDR+JP+2).EQ.IFREE(1).AND.
     +      IBUF(INDR+JP+3).EQ.IFREE(2).AND.
     +      IBUF(INDR+JP+1).EQ.NMORE)       GOTO 99
      ELSEIF(NMORE.EQ.2) THEN
         IF(IBUF(INDR+JP+2).EQ.IFREE(1).AND.
     +      IBUF(INDR+JP+1).EQ.NMORE)       GOTO 99
      ELSEIF(NMORE.EQ.1) THEN
         IF(IBUF(INDR+JP+1).EQ.NMORE)       GOTO 99
      ENDIF
      GOTO 40
*
*     first data segment header after record header
   30 JP=IP+11
*     copy data segment header ...
   40 DO 50 I=1,IBUF(INDR+JP+1)
   50 INFU(IRHDR+I,LR)=IBUF(INDR+JP+I)
*     check data segment header segment code
      JPS=INFU(IRHDR+7,LR)
      IF(ILLG(JPS) .OR. IS01(JPS)) GOTO 94
*     data segment is starting (0 or 1) - reset data pointer
      KP=0
      GOTO 11
*
*     all error conditions are set here
*               only argument IER is returned
*     pure read error
   91 IER = 1
      GOTO 90
*     Record was locked or changed by another job.
  911 IER = -911
*     Index path for read incorrect
      INFU(ISGEN+9,LR) = 1
      GOTO 100
*     unexpected end-of-data (record incomplete)
   92 IER = 2
      GOTO 90
*     error in calling order
   93 IER = 3
      GOTO 90
*     illegal segment code
   94 IER = 4
      ERRMES='Illegal segment code'
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
*     save record pointer
      INFU(IRSTU+ 8,LR)=IP
      INFU(IRSTU+ 9,LR)=JP
      INFU(IRSTU+10,LR)=KP
      CALL FIPCOP(IP,JP,KP,-LR)
      LR = LRSAV
      LUNR = LUNRSAV
*
      IF(IER.GT.0) CALL FPWMSG(IER,'FRDATO',ERRMES)
 
C     WRITE(6,107) IER
*
  100 RETURN
  101 FORMAT(A)
  107 FORMAT(' FRDAT IER= ',I6)
      END
#endif
