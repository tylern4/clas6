*CMZ :  1.00/00 16/12/94  17.00.00  by  Pavel Binko
*-- Author :    Zbigniew Szkutnik   16/12/94
      SUBROUTINE FPCLOS(LUN,IRET)
*
************************************************************************
*
*     Close a remote file. Stop server if necessary.
*
************************************************************************
*
      INTEGER CLIENT
      LOGICAL CONFLAG
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
*KEEP,FBUFFER.
*     buffer common
      PARAMETER (NBUF=100000)
      COMMON/FBUF/IBUF(NBUF)
      SAVE/FBUF/
*KEND.
*
      JRET=0
      CONFLAG=.TRUE.
      CLIENT=INFU(9,LUNIT(LUN))
      NC=1
      DO 10 I=1,NCON
       IF(CLIENT.EQ.ICLIENT(1,I)) THEN
         NC=ICLIENT(2,I)
         GOTO 40
       END IF
  10  CONTINUE
*
  40  IF(INFU(4,LUNIT(LUN)).EQ.1) THEN
        IF(INDXB(IBUF,1000*LUNR+6).GT.0) THEN
*         do not stop the server for concatenated input
          CONFLAG=.FALSE.
        ENDIF
      ENDIF
      IF(NC.EQ.1.AND.CONFLAG) THEN
        JRET=1
        IF(I.LE.NCON) THEN
          DO 20 J=I,NCON-1
            REMHOST(J)=REMHOST(J+1)
            HOSTLEN(J)=HOSTLEN(J+1)
            ICLIENT(1,J)=ICLIENT(1,J+1)
  20      ICLIENT(2,J)=ICLIENT(2,J+1)
          NCON=NCON-1
        END IF
      ELSE
        ICLIENT(2,I)=ICLIENT(2,I)-1
      END IF
*
      CALL CLOSEF(CLIENT,INFU(8,LUNIT(LUN)),JRET)
      INFU(9,LUNIT(LUN))=0
      INFU(8,LUNIT(LUN))=0
      IRET=JRET
*
      RETURN
      END
