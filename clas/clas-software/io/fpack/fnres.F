*CMZ :  1.00/00 16/12/94  17.00.00  by  Pavel Binko
*-- Author :    Zbigniew Szkutnik   16/12/94
      SUBROUTINE FNRES(CLIENT,STREAM,MODE,RET)
*
      INTEGER CLIENT, STREAM, RET
*
************************************************************************
*
*     Reopen a remote file and reset its status according to MODE
*     MODE=  0  just open
*         =  1  open and reset file status
*     RET = -1  failure
*         =  0  reconnect succesful
*
************************************************************************
*
      INTEGER BUFFER(12), BCOUNT
      CHARACTER ACTION*9, NAMKEY*8, KEY*20
      INTEGER*2 STRLEN
      PARAMETER(MAXMAL=20)
      COMMON/KEYSTAT/BCOUNT
      COMMON/LOCKSTAT/LOCKFLAG
*KEEP,NETCOM.
*     Up to MAXSERV remote connections can internally be stored
*     and reused. A connection is being closed when all files on
*     a given host are closed.
*     REMHOST(I) : name of the i-th remote host
*     HOSTLEN(I) : length of the name
*     ICLIENT(1,I) : RPC client handle for the host
*     ICLIENT(2,I) : number of files opened on the host
      PARAMETER (MAXSERV=10)
      CHARACTER*127 REMHOST(MAXSERV)
      INTEGER ICLIENT(2,MAXSERV), HOSTLEN(MAXSERV)
      COMMON/NUMNET/HOSTLEN, ICLIENT, NCON
      COMMON/CHANET/REMHOST
      SAVE/NUMNET/,/CHANET/
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
*KEND.
 
      RET=-1
 
* Find the corresponding unit
      DO 10 I=1,99
      IF(LUNIT(I).LE.0) GOTO 10
      IND=LUNIT(I)
      IF(INFU(9,IND).EQ.CLIENT.AND.INFU(8,IND).EQ.STREAM) THEN
        LUN=I
        GOTO 20
      ENDIF
  10  CONTINUE
* Unit not found
      RETURN
 
* Unit found; try to reopen.
  20  IF(CLIENT.GT.0) THEN
*       This is the first time network problem is found for
*       the channel.
*       Remove the entry for host from the control tables first...
        DO 30 I=1,NCON
        IF(CLIENT.EQ.ICLIENT(1,I)) GOTO 40
  30    CONTINUE
  40    IF(I.LT.NCON) THEN
          DO 50 J=I,NCON-1
          REMHOST(J)=REMHOST(J+1)
          HOSTLEN(J)=HOSTLEN(J+1)
          ICLIENT(1,J)=ICLIENT(1,J+1)
  50      ICLIENT(2,J)=ICLIENT(2,J+1)
        END IF
        NCON=NCON-1
*       ...and mark all units opened through this channel as
*          unusable.
        DO 60 I=1,99
        IF(LUNIT(I).LE.0) GOTO 60
        IND1=LUNIT(I)
        IF(INFU(9,IND1).EQ.CLIENT) INFU(9,IND1)=-1*INFU(9,IND1)
  60    CONTINUE
      END IF
 
      IF(INFU(4,IND).EQ.1) THEN
         ACTION='READ'
      ELSE IF(INFU(4,IND).EQ.2) THEN
*        if original ACTION was WRITE, reopen with ACTION MODIFY not to
*        destroy already written data!
         ACTION='MODIFY'
      ELSE IF(INFU(4,IND).EQ.3) THEN
         ACTION='READWRITE'
      ELSE
         ACTION='MODIFY'
      END IF
      ILENF=LENB(FILNAM(IND))
      ILENH=LENB(HOSNAM(IND))
      WRITE(6,'(''Trying to recover...'')')
      CALL FPOPEN(LUN,HOSNAM(IND)(1:ILENH),
     +     FILNAM(IND)(1:ILENF),ACTION,INFU(1,IND),0,0,IOS)
      IF(IOS.NE.0) THEN
*        reconnect failed
         RETURN
      ELSE
*        reconnect successful
         CLIENT=INFU(9,IND)
         STREAM=INFU(8,IND)
      END IF
 
* File successfully reopened; reset status now.
      IF(MODE.EQ.0) THEN
        RET=0
        RETURN
      END IF
 
      IF(INFU(2,IND).EQ.2) THEN
*       Direct access file, no reset necessary <===========
        RET=0
        RETURN
      ELSE IF(INFU(2,IND).GT.3) THEN
*       Access special or ordered, cannot reset <==========
        RETURN
      ELSE IF(INFU(2,IND).EQ.1) THEN
*       Sequential file <==================================
        IF(INFU(ISGEN+1,IND).EQ.1.OR.INFU(ISGEN+1,IND).EQ.3) THEN
*         general status was 'reading' or 'writing'
          IF(INFU(ISGEN+1,IND).EQ.1) THEN
*           'reading'
            NSKIP=INFU(1,IND)*INFU(IRSTU+5,IND)
          ELSE
*           'writing'
            NSKIP=INFU(1,IND)*INFU(IWSTA+1,IND)
          END IF
          NBYTES=0
          CALL READC(CLIENT,STREAM,NSKIP,NBYTES,BUFFER,NREAD)
          IF(NREAD.EQ.NBYTES) RET=0
          RETURN
        ELSE
*         general status was 'rewind','undef','eofreading' or 'eofwr';
*         no reset necessary (really?)
          RET=0
          RETURN
        END IF
      ELSE
*       Keyed access file <================================
        IF(INFU(ISGEN+1,IND).EQ.1.AND.INFU(IRSTU+14,LR).NE.0) THEN
*         general status was 'reading'
          CALL TRAKEY('HC',KEY,INFU(IRSTU+14,LR),NAMKEY,NUM1,NUM2,NRD)
          STRLEN=LEN(KEY)
          NBYTES=48
          KEYCODE=2
          CALL KREADC(CLIENT,STREAM,NBYTES,BUFFER,KEYCODE,
     +            KEY,IRET,STRLEN)
          IF(IRET.EQ.0) THEN
            IOS=0
            DO 70 I=1,BCOUNT
            CALL KBACKC(CLIENT,STREAM,IOS)
  70        CONTINUE
            IF(IOS.EQ.0) RET=0
          END IF
        ELSE
*         general status was 'rewind','undef','eofreading','eofwr' or
*         'writing'; no reset necessary (really?)
*         reestablish the lock on the current file, if necessary
          IF(LOCKFLAG.NE.1) THEN
            RET=0
            RETURN
          END IF
          LMODE=1
          STRLEN=LENB(FILNAM(IND))
          DO 80 I=1,MAXMAL
          CALL CPLOC(FILNAM(IND)(1:STRLEN),LMODE,
     +                CLIENT,STREAM,IRET,STRLEN)
          IF(IRET.EQ.0) THEN
            RET=0
            RETURN
          END IF
          IF(I.EQ.MAXMAL) RETURN
          CALL FPWAIT(I)
  80      CONTINUE
          RETURN
        END IF
      END IF
*
      END
