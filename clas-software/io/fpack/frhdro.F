#ifdef HPUX
*CMZU:  1.00/16 01/03/96  01.30.45  by  Unknown
*-- Author :    Sergey Esenov   27/02/96
      SUBROUTINE FRHDRO(NAMDAT,NUM,NCOL,NROW,FORMAT,NCH,IER)
*
      CHARACTER*(*) NAMDAT, FORMAT
*
************************************************************************
*
*     Get data header (read)
*
************************************************************************
*
      PARAMETER (NW5 = 5)
      CHARACTER NAMEC*8, CHA*4
      LOGICAL IS01,IS03,ILLG
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
      CHARACTER  ERRMES*256
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
      INTEGER IFREE(2)
      LOGICAL START
      SAVE    START
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
      IF(INFU(25,LR).EQ.0) GOTO 200
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
         START = .FALSE.
      ENDIF
*
*     reset key flag
      INFU(IRSTU+4,LR)=0
      IER=0
*
*     check status of unit ---------------------------------------------
*
*     get record pointers for binary input
      CALL FIPCOP(IP,JP,KP,LR)
*
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
*
      NUMBNK = 1000*LUNR+1
      INDR=INDXB(IBUF,NUMBNK)
      IF(INDR.EQ.0) THEN
         ERRMES='No buffer bank existing'
         GOTO 93
      END IF
*      PRINT *,'FRHDRO. Entry'
*
*     value of IP
*              IP > 1   pointing to current record header
*              IP = 1   record is read in, IP not yet defined
*              IP = 0   no record read in
*              IP < 0   ?
*
      INFU29 = INFU(29,LR)
*     If data record is not loaded yet ... load it
      IF(INFU(29,LR).EQ.0) GOTO 20
*
      IF(INFU(IRKEY+10,LR).NE.MOD(IBUF(INDR+IP+10),16)) THEN
            INFU(IRKEY+10,LR) =  MOD(IBUF(INDR+IP+10),16)
      ENDIF
      IF(IP.LE.1) THEN
         ERRMES='Invalid value of IP pointing to current record header'
         GOTO 93
      ENDIF
*     End of segment
      NXTIP = NEXTIP(IP)
*     Tail of segment
      NTAIL = NXTIP - NW5
*
*     IP is defined, check conversion of record
      IF(INFU(IRSTU+1,LR).LT.0) THEN
         CALL FRCONV(IBUF(INDR+1),LOCAL,ICER)
         INFU(IRSTU+1,LR)=1
      END IF
      IF(JP.EQ.0) GOTO 30
*     JP is defined
   10 CONTINUE
*     End of segment
      NXTIP = NEXTIP(IP)
*     Tail of segment
      NTAIL = NXTIP - NW5
      NXTJP = NEXTJP(JP)
*      PRINT '(1X,I10,1X,2A4)',(IBUF(INDR+NXTJP+J),J=1,3)
      IF(NTAIL.EQ.NEXTJP(JP).OR.
     +       NTAIL.EQ.NXTJP+IBUF(INDR+NXTJP+1)) THEN
*        reset JP (was last data segment inside record segment)
         JP=0
*        check illegal segment code
         IF(ILLG(INFU(IRKEY+10,LR))) THEN
*            PRINT *,'1. ILLEGAL SEGMENT CODE IPS=',INFU(IRKEY+10,LR)
            GOTO 94
         ENDIF
*        check end-of-logical-record
         IF(IS03(INFU(IRKEY+10,LR))) GOTO 99
*        continuation of logical record in next record segment
         INFU(27,LR) = IBUF(INDR+NTAIL+3)
         DTFLPS(LR)  = IBUF(INDR+NTAIL+4)
*        no continuation ...
         IF(INFU(27,LR).EQ.0)  GOTO 92
         GOTO 20
      ENDIF
      JP=NEXTJP(JP)
*     Check that header was not marked as free ...
      NMORE = NTAIL - JP

*      PRINT '(1X,I10,1X,2A4)',(IBUF(INDR+JP+J),J=1,3)
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
20    CONTINUE
*     reading of the data record is necessary
      IF(INFU(27,LR).EQ.0) GOTO 92
      CALL FRREC(LUNR,IBUF(INDR),IBUF(INDR+1),IEND)
      INFU(IRSTU+2,LR)=IEND
      INFU(IRSTU+3,LR)=0
      IF(IEND.LT.0) THEN
*        general status is: end-of-file at reading
         INFU(ISGEN+1,LR)=2
         GOTO 92
      ELSE IF(IEND.GT.0) THEN
            ERRMES='Read error'
            GOTO 91
      ENDIF
      INFU(29,LR) = INFU(14,LR)
*     eventually convert to local format
      CALL  FSWAP(IBUF(INDR+1),0)
      CALL FRCONV(IBUF(INDR+1),LOCAL,ICER)
      INFU(IRSTU+1,LR)=+1
      IP = DTFLPS(LR)
      JP = 0
*     Check the data integrity ...
*     Get version#1 in the top of segment
      IVERS1 = IBUF(INDR+IP+10)/16
*     if block was locked -> goto 911 (warn 'master')
      IF( IVERS1.EQ.0 ) GOTO 911
*     Strip segment code from version#
      IBUF(INDR+IP+10)=MOD(IBUF(INDR+IP+10),16)
*     Get version#2 in bottom
      NXTIP = NEXTIP(IP)
      NTAIL = NXTIP - NW5
      IVERS2 = IBUF(INDR+NTAIL+5)
*
*     IVERS1 must be identical to IVERS2
      IF(IVERS1.NE.IVERS2) GOTO 911
*
*     If version# has not been initialized yet ...
      IF(INFU(IRSTU+21,LR).EQ.0) INFU(IRSTU+21,LR)=IVERS1
*     version of previous block is not equal the current one
      IF(INFU(IRSTU+21,LR).NE.IVERS1) GOTO 911
*
*     Extract the address of the next part (extent) of a logical record
*     ... block# & offset in block
*      INFU(IRSTU+22,LR) = IBUF(INDR+NTAIL+3)
*      INFU(IRSTU+23,LR) = IBUF(INDR+NTAIL+4)
*
*     copy record segment
      CALL FPCPRK(LR,IBUF(INDR+IP+1))
*
*     check segment code of new record segment
      IPS=INFU(IRKEY+10,LR)
*      PRINT *,'FRHDRO. IPS = ',IPS
      IF(ILLG(IPS)) THEN
*         PRINT *,'2. ILLEGAL SEGMENT CODE IPS=',IPS
         GOTO 94
      ENDIF
      IF(INFU29.EQ.0.AND..NOT.IS01(IPS)) THEN
*         PRINT *,'3. ILLEGAL SEGMENT CODE IPS=',IPS
         GOTO 94
      ENDIF
*
*     first data segment header after record header
   30 JP=IP+11
   40 CONTINUE
*     copy data segment header ...
      DO 50 I=1,IBUF(INDR+JP+1)
   50 INFU(IRHDR+I,LR)=IBUF(INDR+JP+I)
*     check data segment header segment code
      JPS=INFU(IRHDR+7,LR)
      IF(ILLG(JPS)) THEN
*         PRINT *,'4. ILLEGAL SEGMENT CODE JPS=',JPS
         GOTO 94
      ENDIF
      IF(.NOT.IS01(JPS)) GOTO 10
*     data segment is starting (0 or 1) - reset data pointer
      KP=0
*     ... and convert to character ...
      ICH=INFU(IRHDR+2,LR)
*KEEP,CHAICH.
*     get char*4 from hollerith 4H
      CHA=CHAR(IAND(ISHFT(ICH,-24),255))//CHAR(IAND(ISHFT(ICH,-16),255))
     +  //CHAR(IAND(ISHFT(ICH,- 8),255))//CHAR(IAND      (ICH     ,255))
*KEND.
      NAMEC(1:4)=CHA
      ICH=INFU(IRHDR+3,LR)
*KEEP,CHAICH.
*     get char*4 from hollerith 4H
      CHA=CHAR(IAND(ISHFT(ICH,-24),255))//CHAR(IAND(ISHFT(ICH,-16),255))
     +  //CHAR(IAND(ISHFT(ICH,- 8),255))//CHAR(IAND      (ICH     ,255))
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
*
*     all error conditions are set here
*               only argument IER is returned
   91 IER = 1
      GOTO 90
*     Logical record was changed or locked by another job
*     It is necessary to set position and call FRKEY again
  911 IER = -911
      INFU(IRSTU+8,LR)  = 0
      INFU(IRSTU+9,LR)  = 0
      INFU(IRSTU+10,LR) = 0
      INFU(ISGEN+9,LR)  = 1
      GOTO 200
   92 IER = 2
      ERRMES='Unexpected end-of-data (record incomplete)'
      GOTO 90
   93 IER = 3
*     Error in calling order
      GOTO 90
*     illegal segment code
   94 IER = 4
      ERRMES='Illegal segment code'
*         PRINT *,'------------------------------------------'
*         PRINT '(1X,''BLK#='',2I6,1X,''VERS='',2I7)',
*     +      INFU(29,LR),INFU29,IVERS1,IVERS2
*         PRINT '(1X,''IP,JP,KP'',3I6)',IP,JP,KP
*         PRINT *,'------------------------------------------'
*         PRINT *,'RECORD SEGMENT HEADER'
*         PRINT '(1X,Z8,1X,Z8,1X,I10,1X,/,2A4,5I10)',
*     +     (INFU(IRKEY+J,LR),J=1,10)
*         PRINT *,'------------------------------------------'
*         PRINT *,'DATA SEGMENT HEADER'
*         PRINT '(1X,I3,1X,2A4,6I9)',
*     +     (INFU(IRHDR+J,LR),J=1,9)
*         PRINT *,'------------------------------------------'
*         PRINT '(1X,''NXTIP,NTAIL,ALL,USED'',2I6)',NXTIP,NTAIL,
*     +       IBUF(INDR+1),IBUF(INDR+2)
*         PRINT *,'BUFFER FROM IP. INDR=',INDR
*         DO I=1,NTAIL,6
*         PRINT '(1X,6I10)',(IBUF(INDR+IP+J),J=I,I+5)
*         PRINT '(1X,6(2X,Z8))',(IBUF(INDR+IP+J),J=I,I+5)
*         PRINT '(1X,6(4X,A4))',(IBUF(INDR+IP+J),J=I,I+5)
*         ENDDO
*         PRINT *,'------------------------------------------'
*         PRINT *,'TAIL BLOCK'
*         PRINT '(1X,5I10)',(IBUF(INDR+TAIL+J),J=1,5)
*         PRINT *,'------------------------------------------'
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
*     save record pointer
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
200   CONTINUE
*      PRINT *,'FRHDRO. Exit. IP,JP,KP,IER',IP,JP,KP,IER
*      PRINT '(1X,Z8,1X,Z8,I8,1X,2A4,6I9)',(INFU(IRKEY+J,LR),J=1,10)
*      PRINT '(1X,I3,1X,2A4,I10,3I6,2I8)',(INFU(IRHDR+J,LR),J=1,9)
      LR = LRSAV
      LUNR = LUNRSAV
*
      RETURN
      END
#elif defined Linux
      subroutine dummy_frhdro
      implicit none
      return
      end
#elif defined SunOS
*CMZU:  1.00/16 01/03/96  01.30.45  by  Unknown
*-- Author :    Sergey Esenov   27/02/96
      SUBROUTINE FRHDRO(NAMDAT,NUM,NCOL,NROW,FORMAT,NCH,IER)
*
      CHARACTER*(*) NAMDAT, FORMAT
*
************************************************************************
*
*     Get data header (read)
*
************************************************************************
*
      PARAMETER (NW5 = 5)
      CHARACTER NAMEC*8, CHA*4
      LOGICAL IS01,IS03,ILLG
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
*KEND.
      INTEGER IFREE(2)
      LOGICAL START
      SAVE    START
** 
*    d.p. weygand - 06/1998 - add definition of errmes for SunOS
      CHARACTER  ERRMES*256
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
      IF(INFU(25,LR).EQ.0) GOTO 200
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
         START = .FALSE.
      ENDIF
*
*     reset key flag
      INFU(IRSTU+4,LR)=0
      IER=0
*
*     check status of unit ---------------------------------------------
*
*     get record pointers for binary input
      CALL FIPCOP(IP,JP,KP,LR)
*
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
*
      NUMBNK = 1000*LUNR+1
      INDR=INDXB(IBUF,NUMBNK)
      IF(INDR.EQ.0) THEN
         ERRMES='No buffer bank existing'
         GOTO 93
      END IF
*      PRINT *,'FRHDRO. Entry'
*
*     value of IP
*              IP > 1   pointing to current record header
*              IP = 1   record is read in, IP not yet defined
*              IP = 0   no record read in
*              IP < 0   ?
*
      INFU29 = INFU(29,LR)
*     If data record is not loaded yet ... load it
      IF(INFU(29,LR).EQ.0) GOTO 20
*
      IF(INFU(IRKEY+10,LR).NE.MOD(IBUF(INDR+IP+10),16)) THEN
            INFU(IRKEY+10,LR) =  MOD(IBUF(INDR+IP+10),16)
      ENDIF
      IF(IP.LE.1) THEN
         ERRMES='Invalid value of IP pointing to current record header'
         GOTO 93
      ENDIF
*     End of segment
      NXTIP = NEXTIP(IP)
*     Tail of segment
      NTAIL = NXTIP - NW5
*
*     IP is defined, check conversion of record
      IF(INFU(IRSTU+1,LR).LT.0) THEN
         CALL FRCONV(IBUF(INDR+1),LOCAL,ICER)
         INFU(IRSTU+1,LR)=1
      END IF
      IF(JP.EQ.0) GOTO 30
*     JP is defined
   10 CONTINUE
*     End of segment
      NXTIP = NEXTIP(IP)
*     Tail of segment
      NTAIL = NXTIP - NW5
      NXTJP = NEXTJP(JP)
*      PRINT '(1X,I10,1X,2A4)',(IBUF(INDR+NXTJP+J),J=1,3)
      IF(NTAIL.EQ.NEXTJP(JP).OR.
     +       NTAIL.EQ.NXTJP+IBUF(INDR+NXTJP+1)) THEN
*        reset JP (was last data segment inside record segment)
         JP=0
*        check illegal segment code
         IF(ILLG(INFU(IRKEY+10,LR))) THEN
*            PRINT *,'1. ILLEGAL SEGMENT CODE IPS=',INFU(IRKEY+10,LR)
            GOTO 94
         ENDIF
*        check end-of-logical-record
         IF(IS03(INFU(IRKEY+10,LR))) GOTO 99
*        continuation of logical record in next record segment
         INFU(27,LR) = IBUF(INDR+NTAIL+3)
         DTFLPS(LR)  = IBUF(INDR+NTAIL+4)
*        no continuation ...
         IF(INFU(27,LR).EQ.0)  GOTO 92
         GOTO 20
      ENDIF
      JP=NEXTJP(JP)
*     Check that header was not marked as free ...
      NMORE = NTAIL - JP
 
*      PRINT '(1X,I10,1X,2A4)',(IBUF(INDR+JP+J),J=1,3)
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
20    CONTINUE
*     reading of the data record is necessary
      IF(INFU(27,LR).EQ.0) GOTO 92
      CALL FRREC(LUNR,IBUF(INDR),IBUF(INDR+1),IEND)
      INFU(IRSTU+2,LR)=IEND
      INFU(IRSTU+3,LR)=0
      IF(IEND.LT.0) THEN
*        general status is: end-of-file at reading
         INFU(ISGEN+1,LR)=2
         GOTO 92
      ELSE IF(IEND.GT.0) THEN
            ERRMES='Read error'
            GOTO 91
      ENDIF
      INFU(29,LR) = INFU(14,LR)
*     eventually convert to local format
      CALL  FSWAP(IBUF(INDR+1),0)
      CALL FRCONV(IBUF(INDR+1),LOCAL,ICER)
      INFU(IRSTU+1,LR)=+1
      IP = DTFLPS(LR)
      JP = 0
*     Check the data integrity ...
*     Get version#1 in the top of segment
      IVERS1 = IBUF(INDR+IP+10)/16
*     if block was locked -> goto 911 (warn 'master')
      IF( IVERS1.EQ.0 ) GOTO 911
*     Strip segment code from version#
      IBUF(INDR+IP+10)=MOD(IBUF(INDR+IP+10),16)
*     Get version#2 in bottom
      NXTIP = NEXTIP(IP)
      NTAIL = NXTIP - NW5
      IVERS2 = IBUF(INDR+NTAIL+5)
*
*     IVERS1 must be identical to IVERS2
      IF(IVERS1.NE.IVERS2) GOTO 911
*
*     If version# has not been initialized yet ...
      IF(INFU(IRSTU+21,LR).EQ.0) INFU(IRSTU+21,LR)=IVERS1
*     version of previous block is not equal the current one
      IF(INFU(IRSTU+21,LR).NE.IVERS1) GOTO 911
*
*     Extract the address of the next part (extent) of a logical record
*     ... block# & offset in block
*      INFU(IRSTU+22,LR) = IBUF(INDR+NTAIL+3)
*      INFU(IRSTU+23,LR) = IBUF(INDR+NTAIL+4)
*
*     copy record segment
      CALL FPCPRK(LR,IBUF(INDR+IP+1))
*
*     check segment code of new record segment
      IPS=INFU(IRKEY+10,LR)
*      PRINT *,'FRHDRO. IPS = ',IPS
      IF(ILLG(IPS)) THEN
*         PRINT *,'2. ILLEGAL SEGMENT CODE IPS=',IPS
         GOTO 94
      ENDIF
      IF(INFU29.EQ.0.AND..NOT.IS01(IPS)) THEN
*         PRINT *,'3. ILLEGAL SEGMENT CODE IPS=',IPS
         GOTO 94
      ENDIF
*
*     first data segment header after record header
   30 JP=IP+11
   40 CONTINUE
*     copy data segment header ...
      DO 50 I=1,IBUF(INDR+JP+1)
   50 INFU(IRHDR+I,LR)=IBUF(INDR+JP+I)
*     check data segment header segment code
      JPS=INFU(IRHDR+7,LR)
      IF(ILLG(JPS)) THEN
*         PRINT *,'4. ILLEGAL SEGMENT CODE JPS=',JPS
         GOTO 94
      ENDIF
      IF(.NOT.IS01(JPS)) GOTO 10
*     data segment is starting (0 or 1) - reset data pointer
      KP=0
*     ... and convert to character ...
      ICH=INFU(IRHDR+2,LR)
*KEEP,CHAICH.
*     get char*4 from hollerith 4H
      CHA=CHAR(IAND(ISHFT(ICH,-24),255))//CHAR(IAND(ISHFT(ICH,-16),255))
     +  //CHAR(IAND(ISHFT(ICH,- 8),255))//CHAR(IAND      (ICH     ,255))
*KEND.
      NAMEC(1:4)=CHA
      ICH=INFU(IRHDR+3,LR)
*KEEP,CHAICH.
*     get char*4 from hollerith 4H
      CHA=CHAR(IAND(ISHFT(ICH,-24),255))//CHAR(IAND(ISHFT(ICH,-16),255))
     +  //CHAR(IAND(ISHFT(ICH,- 8),255))//CHAR(IAND      (ICH     ,255))
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
*
*     all error conditions are set here
*               only argument IER is returned
   91 IER = 1
      GOTO 90
*     Logical record was changed or locked by another job
*     It is necessary to set position and call FRKEY again
  911 IER = -911
      INFU(IRSTU+8,LR)  = 0
      INFU(IRSTU+9,LR)  = 0
      INFU(IRSTU+10,LR) = 0
      INFU(ISGEN+9,LR)  = 1
      GOTO 200
   92 IER = 2
      ERRMES='Unexpected end-of-data (record incomplete)'
      GOTO 90
   93 IER = 3
*     Error in calling order
      GOTO 90
*     illegal segment code
   94 IER = 4
      ERRMES='Illegal segment code'
*         PRINT *,'------------------------------------------'
*         PRINT '(1X,''BLK#='',2I6,1X,''VERS='',2I7)',
*     +      INFU(29,LR),INFU29,IVERS1,IVERS2
*         PRINT '(1X,''IP,JP,KP'',3I6)',IP,JP,KP
*         PRINT *,'------------------------------------------'
*         PRINT *,'RECORD SEGMENT HEADER'
*         PRINT '(1X,Z8,1X,Z8,1X,I10,1X,/,2A4,5I10)',
*     +     (INFU(IRKEY+J,LR),J=1,10)
*         PRINT *,'------------------------------------------'
*         PRINT *,'DATA SEGMENT HEADER'
*         PRINT '(1X,I3,1X,2A4,6I9)',
*     +     (INFU(IRHDR+J,LR),J=1,9)
*         PRINT *,'------------------------------------------'
*         PRINT '(1X,''NXTIP,NTAIL,ALL,USED'',2I6)',NXTIP,NTAIL,
*     +       IBUF(INDR+1),IBUF(INDR+2)
*         PRINT *,'BUFFER FROM IP. INDR=',INDR
*         DO I=1,NTAIL,6
*         PRINT '(1X,6I10)',(IBUF(INDR+IP+J),J=I,I+5)
*         PRINT '(1X,6(2X,Z8))',(IBUF(INDR+IP+J),J=I,I+5)
*         PRINT '(1X,6(4X,A4))',(IBUF(INDR+IP+J),J=I,I+5)
*         ENDDO
*         PRINT *,'------------------------------------------'
*         PRINT *,'TAIL BLOCK'
*         PRINT '(1X,5I10)',(IBUF(INDR+TAIL+J),J=1,5)
*         PRINT *,'------------------------------------------'
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
*     save record pointer
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
200   CONTINUE
*      PRINT *,'FRHDRO. Exit. IP,JP,KP,IER',IP,JP,KP,IER
*      PRINT '(1X,Z8,1X,Z8,I8,1X,2A4,6I9)',(INFU(IRKEY+J,LR),J=1,10)
*      PRINT '(1X,I3,1X,2A4,I10,3I6,2I8)',(INFU(IRHDR+J,LR),J=1,9)
      LR = LRSAV
      LUNR = LUNRSAV
*
      RETURN
      END
#else
*CMZU:  1.00/16 01/03/96  01.30.45  by  Unknown
*-- Author :    Sergey Esenov   27/02/96
      SUBROUTINE FRHDRO(NAMDAT,NUM,NCOL,NROW,FORMAT,NCH,IER)
*
      CHARACTER*(*) NAMDAT, FORMAT
*
************************************************************************
*
*     Get data header (read)
*
************************************************************************
*
      PARAMETER (NW5 = 5)
      CHARACTER NAMEC*8, CHA*4
      LOGICAL IS01,IS03,ILLG
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
*     jam - 04/1996 - define ERRMES for aix
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
      INTEGER IFREE(2)
      LOGICAL START
      SAVE    START
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
      IF(INFU(25,LR).EQ.0) GOTO 200
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
         START = .FALSE.
      ENDIF
*
*     reset key flag
      INFU(IRSTU+4,LR)=0
      IER=0
*
*     check status of unit ---------------------------------------------
*
*     get record pointers for binary input
      CALL FIPCOP(IP,JP,KP,LR)
*
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
*
      NUMBNK = 1000*LUNR+1
      INDR=INDXB(IBUF,NUMBNK)
      IF(INDR.EQ.0) THEN
         ERRMES='No buffer bank existing'
         GOTO 93
      END IF
*      PRINT *,'FRHDRO. Entry'
*
*     value of IP
*              IP > 1   pointing to current record header
*              IP = 1   record is read in, IP not yet defined
*              IP = 0   no record read in
*              IP < 0   ?
*
      INFU29 = INFU(29,LR)
*     If data record is not loaded yet ... load it
      IF(INFU(29,LR).EQ.0) GOTO 20
*
      IF(INFU(IRKEY+10,LR).NE.MOD(IBUF(INDR+IP+10),16)) THEN
            INFU(IRKEY+10,LR) =  MOD(IBUF(INDR+IP+10),16)
      ENDIF
      IF(IP.LE.1) THEN
         ERRMES='Invalid value of IP pointing to current record header'
         GOTO 93
      ENDIF
*     End of segment
      NXTIP = NEXTIP(IP)
*     Tail of segment
      NTAIL = NXTIP - NW5
*
*     IP is defined, check conversion of record
      IF(INFU(IRSTU+1,LR).LT.0) THEN
         CALL FRCONV(IBUF(INDR+1),LOCAL,ICER)
         INFU(IRSTU+1,LR)=1
      END IF
      IF(JP.EQ.0) GOTO 30
*     JP is defined
   10 CONTINUE
*     End of segment
      NXTIP = NEXTIP(IP)
*     Tail of segment
      NTAIL = NXTIP - NW5
      NXTJP = NEXTJP(JP)
*      PRINT '(1X,I10,1X,2A4)',(IBUF(INDR+NXTJP+J),J=1,3)
      IF(NTAIL.EQ.NEXTJP(JP).OR.
     +       NTAIL.EQ.NXTJP+IBUF(INDR+NXTJP+1)) THEN
*        reset JP (was last data segment inside record segment)
         JP=0
*        check illegal segment code
         IF(ILLG(INFU(IRKEY+10,LR))) THEN
*            PRINT *,'1. ILLEGAL SEGMENT CODE IPS=',INFU(IRKEY+10,LR)
            GOTO 94
         ENDIF
*        check end-of-logical-record
         IF(IS03(INFU(IRKEY+10,LR))) GOTO 99
*        continuation of logical record in next record segment
         INFU(27,LR) = IBUF(INDR+NTAIL+3)
         DTFLPS(LR)  = IBUF(INDR+NTAIL+4)
*        no continuation ...
         IF(INFU(27,LR).EQ.0)  GOTO 92
         GOTO 20
      ENDIF
      JP=NEXTJP(JP)
*     Check that header was not marked as free ...
      NMORE = NTAIL - JP
 
*      PRINT '(1X,I10,1X,2A4)',(IBUF(INDR+JP+J),J=1,3)
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
20    CONTINUE
*     reading of the data record is necessary
      IF(INFU(27,LR).EQ.0) GOTO 92
      CALL FRREC(LUNR,IBUF(INDR),IBUF(INDR+1),IEND)
      INFU(IRSTU+2,LR)=IEND
      INFU(IRSTU+3,LR)=0
      IF(IEND.LT.0) THEN
*        general status is: end-of-file at reading
         INFU(ISGEN+1,LR)=2
         GOTO 92
      ELSE IF(IEND.GT.0) THEN
            ERRMES='Read error'
            GOTO 91
      ENDIF
      INFU(29,LR) = INFU(14,LR)
*     eventually convert to local format
      CALL  FSWAP(IBUF(INDR+1),0)
      CALL FRCONV(IBUF(INDR+1),LOCAL,ICER)
      INFU(IRSTU+1,LR)=+1
      IP = DTFLPS(LR)
      JP = 0
*     Check the data integrity ...
*     Get version#1 in the top of segment
      IVERS1 = IBUF(INDR+IP+10)/16
*     if block was locked -> goto 911 (warn 'master')
      IF( IVERS1.EQ.0 ) GOTO 911
*     Strip segment code from version#
      IBUF(INDR+IP+10)=MOD(IBUF(INDR+IP+10),16)
*     Get version#2 in bottom
      NXTIP = NEXTIP(IP)
      NTAIL = NXTIP - NW5
      IVERS2 = IBUF(INDR+NTAIL+5)
*
*     IVERS1 must be identical to IVERS2
      IF(IVERS1.NE.IVERS2) GOTO 911
*
*     If version# has not been initialized yet ...
      IF(INFU(IRSTU+21,LR).EQ.0) INFU(IRSTU+21,LR)=IVERS1
*     version of previous block is not equal the current one
      IF(INFU(IRSTU+21,LR).NE.IVERS1) GOTO 911
*
*     Extract the address of the next part (extent) of a logical record
*     ... block# & offset in block
*      INFU(IRSTU+22,LR) = IBUF(INDR+NTAIL+3)
*      INFU(IRSTU+23,LR) = IBUF(INDR+NTAIL+4)
*
*     copy record segment
      CALL FPCPRK(LR,IBUF(INDR+IP+1))
*
*     check segment code of new record segment
      IPS=INFU(IRKEY+10,LR)
*      PRINT *,'FRHDRO. IPS = ',IPS
      IF(ILLG(IPS)) THEN
*         PRINT *,'2. ILLEGAL SEGMENT CODE IPS=',IPS
         GOTO 94
      ENDIF
      IF(INFU29.EQ.0.AND..NOT.IS01(IPS)) THEN
*         PRINT *,'3. ILLEGAL SEGMENT CODE IPS=',IPS
         GOTO 94
      ENDIF
*
*     first data segment header after record header
   30 JP=IP+11
   40 CONTINUE
*     copy data segment header ...
      DO 50 I=1,IBUF(INDR+JP+1)
   50 INFU(IRHDR+I,LR)=IBUF(INDR+JP+I)
*     check data segment header segment code
      JPS=INFU(IRHDR+7,LR)
      IF(ILLG(JPS)) THEN
*         PRINT *,'4. ILLEGAL SEGMENT CODE JPS=',JPS
         GOTO 94
      ENDIF
      IF(.NOT.IS01(JPS)) GOTO 10
*     data segment is starting (0 or 1) - reset data pointer
      KP=0
*     ... and convert to character ...
      ICH=INFU(IRHDR+2,LR)
*KEEP,CHAICH.
*     get char*4 from hollerith 4H
      CHA=CHAR(IAND(ISHFT(ICH,-24),255))//CHAR(IAND(ISHFT(ICH,-16),255))
     +  //CHAR(IAND(ISHFT(ICH,- 8),255))//CHAR(IAND      (ICH     ,255))
*KEND.
      NAMEC(1:4)=CHA
      ICH=INFU(IRHDR+3,LR)
*KEEP,CHAICH.
*     get char*4 from hollerith 4H
      CHA=CHAR(IAND(ISHFT(ICH,-24),255))//CHAR(IAND(ISHFT(ICH,-16),255))
     +  //CHAR(IAND(ISHFT(ICH,- 8),255))//CHAR(IAND      (ICH     ,255))
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
*
*     all error conditions are set here
*               only argument IER is returned
   91 IER = 1
      GOTO 90
*     Logical record was changed or locked by another job
*     It is necessary to set position and call FRKEY again
  911 IER = -911
      INFU(IRSTU+8,LR)  = 0
      INFU(IRSTU+9,LR)  = 0
      INFU(IRSTU+10,LR) = 0
      INFU(ISGEN+9,LR)  = 1
      GOTO 200
   92 IER = 2
      ERRMES='Unexpected end-of-data (record incomplete)'
      GOTO 90
   93 IER = 3
*     Error in calling order
      GOTO 90
*     illegal segment code
   94 IER = 4
      ERRMES='Illegal segment code'
*         PRINT *,'------------------------------------------'
*         PRINT '(1X,''BLK#='',2I6,1X,''VERS='',2I7)',
*     +      INFU(29,LR),INFU29,IVERS1,IVERS2
*         PRINT '(1X,''IP,JP,KP'',3I6)',IP,JP,KP
*         PRINT *,'------------------------------------------'
*         PRINT *,'RECORD SEGMENT HEADER'
*         PRINT '(1X,Z8,1X,Z8,1X,I10,1X,/,2A4,5I10)',
*     +     (INFU(IRKEY+J,LR),J=1,10)
*         PRINT *,'------------------------------------------'
*         PRINT *,'DATA SEGMENT HEADER'
*         PRINT '(1X,I3,1X,2A4,6I9)',
*     +     (INFU(IRHDR+J,LR),J=1,9)
*         PRINT *,'------------------------------------------'
*         PRINT '(1X,''NXTIP,NTAIL,ALL,USED'',2I6)',NXTIP,NTAIL,
*     +       IBUF(INDR+1),IBUF(INDR+2)
*         PRINT *,'BUFFER FROM IP. INDR=',INDR
*         DO I=1,NTAIL,6
*         PRINT '(1X,6I10)',(IBUF(INDR+IP+J),J=I,I+5)
*         PRINT '(1X,6(2X,Z8))',(IBUF(INDR+IP+J),J=I,I+5)
*         PRINT '(1X,6(4X,A4))',(IBUF(INDR+IP+J),J=I,I+5)
*         ENDDO
*         PRINT *,'------------------------------------------'
*         PRINT *,'TAIL BLOCK'
*         PRINT '(1X,5I10)',(IBUF(INDR+TAIL+J),J=1,5)
*         PRINT *,'------------------------------------------'
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
*     save record pointer
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
200   CONTINUE
*      PRINT *,'FRHDRO. Exit. IP,JP,KP,IER',IP,JP,KP,IER
*      PRINT '(1X,Z8,1X,Z8,I8,1X,2A4,6I9)',(INFU(IRKEY+J,LR),J=1,10)
*      PRINT '(1X,I3,1X,2A4,I10,3I6,2I8)',(INFU(IRHDR+J,LR),J=1,9)
      LR = LRSAV
      LUNR = LUNRSAV
*
      RETURN
      END
#endif
