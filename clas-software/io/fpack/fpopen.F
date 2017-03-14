*CMZ :  1.00/00 16/12/94  17.00.00  by  Pavel Binko
*-- Author :    Zbigniew Szkutnik   16/12/94
      SUBROUTINE FPOPEN(LUN,HOST,FILENAME,ACTION,LRECL,NR1,NR2,IRET)
*
      CHARACTER*(*) HOST, FILENAME, ACTION
*
************************************************************************
*
*     Open a remote file. Start server if necessary.
*
************************************************************************
*
      INTEGER CLIENT, STREAM
      CHARACTER HOST1*127
      LOGICAL START
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
      DATA START/.TRUE./
*
      IF(START) THEN
         NCON=0
         DO 1 I=1,MAXSERV
         HOSTLEN(I)=0
         ICLIENT(1,I)=0
   1     ICLIENT(2,I)=0
         START=.FALSE.
      END IF
*
   2  CONTINUE
* for connections book-keeping ignore the option part of HOST
      IND = INDEX(HOST,'$') - 1
      IF(IND.EQ.-1) THEN
         HOST1 = HOST
      ELSE
         HOST1 = HOST(1:IND)
      END IF
      ILEN = LENB(HOST1)
      IF(INFU(2,LUNIT(LUN)).EQ.3) THEN
* keyed access file
         STREAM=LUN
      ELSE IF(INFU(12,LUNIT(LUN)).EQ.3.OR.
     +        INFU(12,LUNIT(LUN)).EQ.4) THEN
* remote stage requested
         STREAM=-1*LUN
      ELSE
         STREAM=0
      END IF
      IF(NCON.GT.0) THEN
         DO 10 I=1,NCON
         IF(HOST1(1:ILEN).EQ.REMHOST(I)(1:HOSTLEN(I))) THEN
            CLIENT=ICLIENT(1,I)
            CALL OPENF(CLIENT,STREAM,HOST,FILENAME,ACTION,LRECL,
     +                 'REC',NR1,NR2)
            IF(STREAM.EQ.0) THEN
               IF(CLIENT.EQ.0) THEN
*                 old connection is dead, try to recover
                  WRITE(6,'(''Trying to recover...'')')
                  IF(I.LT.NCON) THEN
                    DO 50 J=I,NCON-1
                    REMHOST(J)=REMHOST(J+1)
                    HOSTLEN(J)=HOSTLEN(J+1)
                    ICLIENT(1,J)=ICLIENT(1,J+1)
  50                ICLIENT(2,J)=ICLIENT(2,J+1)
                  END IF
                  NCON=NCON-1
*                 mark all units opened through this channel
*                 as unusable.
                  DO 60 J=1,99
                  IF(LUNIT(J).LE.0) GOTO 60
                  IND1=LUNIT(J)
                  IF(INFU(9,IND1).EQ.CLIENT)
     +                             INFU(9,IND1)=-1*INFU(9,IND1)
  60              CONTINUE
                  GO TO 2
               ELSE
*                 open failed
                  IRET=-4
                  GOTO 100
               END IF
            ELSE IF(STREAM.LT.0.AND.STREAM.GT.-100000) THEN
*              remote stage failed (stream is a FILE pointer casted to int
*              which can result in a negative number with a very large module;
*              error return, on the other hand, is a smaller number sent from
*              the FPserver as a negative number.)
               IRET=-1*STREAM
               GOTO 100
            ELSE
               ICLIENT(2,I)=ICLIENT(2,I)+1
               INFU(8,LUNIT(LUN))=STREAM
               INFU(9,LUNIT(LUN))=CLIENT
               IRET=0
               GOTO 100
            END IF
         END IF
  10     CONTINUE
      END IF
      CLIENT=0
      CALL OPENF(CLIENT,STREAM,HOST,FILENAME,ACTION,LRECL,
     +           'REC',NR1,NR2)
      IF(CLIENT.EQ.0) THEN
*        no connection to server
         IRET=-3
         GOTO 100
      ELSE
         IF(STREAM.LE.0.AND.STREAM.GT.-100000) THEN
*           open failed
            CALL STOPSF(CLIENT,IRET)
            IF(STREAM.LT.0) THEN
*              remote stage failed (stream is a FILE pointer casted to int
*              which can result in a negative number with a very large module;
*              error return, on the other hand, is a smaller number sent from
*              the FPserver as a negative number.)
               IRET=-1*STREAM
               GOTO 100
            END IF
            IRET=-4
            GOTO 100
         ELSE
*           success
            INFU(8,LUNIT(LUN))=STREAM
            INFU(9,LUNIT(LUN))=CLIENT
            IF(NCON.LT.MAXSERV) THEN
               NCON=NCON+1
               REMHOST(NCON)=HOST1
               HOSTLEN(NCON)=ILEN
               ICLIENT(2,NCON)=1
               ICLIENT(1,NCON)=CLIENT
            END IF
            IRET=0
            GOTO 100
         END IF
      END IF
*
  100 RETURN
      END
