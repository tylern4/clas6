*CMZ :  1.00/00 16/12/94  17.00.00  by  Pavel Binko
*-- Author :    Pavel Binko   16/12/94
      SUBROUTINE FPINFU(IOPEN,LX,FNS)
*
      CHARACTER*(*) FNS
*
************************************************************************
*
*     Restore INFU from IOPEN
*
************************************************************************
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
*KEND.
      INTEGER IOPEN(IOPMAX)
*
*     test the logical unit number
      IF(LX.NE.LUNIT(IOPEN(1))) THEN
         WRITE(*,*) 'FPINFU. Wrong logical unit number'
         WRITE(*,*) '        IOPEN(1)     = ',IOPEN(1)
         WRITE(*,*) '  LX LUNIT(IOPEN(1)) = ',LX,LUNBK(IOPEN(1))
         GOTO 100
      ENDIF
*
*     physical record length in bytes
      INFU( 1,LX) = IOPEN(2)
*
*     primary space
      INFU(17,LX) = IOPEN(3)
*
*     secondary space
      INFU(18,LX) = IOPEN(4)
*
*     blocking factor
      INFU(19,LX) = IOPEN(5)
*
*     number of buffers
      INFU(20,LX) = IOPEN(6)
*
*     SPLITMB or SPLITEV
      IF(IOPEN(8).GT.0 .OR. IOPEN(23).GT.0) THEN
         IF(IOPEN(8).GT.0) THEN
*           for SPLITMB
            INFU(21,LX)=IOPEN(8)
         ELSE
*           for SPLITEV
            INFU(39,LX)=IOPEN(23)
         ENDIF
*
*        no suffix for UNIXTAPE
         IF(IOPEN(15).EQ.5) THEN
            INFU(22,LX)=0
         ELSE
            L = LENB(FNS)
            IF( (FNS(L-3:L-3).EQ.'.') .AND.
     +        (IFPWNM(FNS(L-2:L)).GE.0) ) INFU(22,LX)=IFPWNM(FNS(L-2:L))
            IF( (FNS(L-8:L-8).EQ.'.') .AND.
     +        (IFPWNM(FNS(L-7:L)).GE.0) ) INFU(22,LX)=IFPWNM(FNS(L-7:L))
         ENDIF
         INFU(23,LX)=INFU(22,LX)
         IF(INFU(22,LX).LT.0) CALL LOGERROR('BAD POSTFIX FOR SPLITMB')
      END IF
*
*     action (read, write, readwrite, modify)
      INFU( 4,LX) = IOPEN(9)
*
*     status (old, new, scratch)
      INFU(16,LX) = IOPEN(10)
*
*     access (seq, dir, keyed, spaecial, ordered)
      INFU( 2,LX) = IOPEN(11)
*
*     form (binary, text)
      INFU( 3,LX) = IOPEN(12)
*
*     wordfmt (local, IEEE, IBM, VAX, DEC)
      INFU( 5,LX) = IOPEN(13)
*
*     logical record separation
      INFU(11,LX) = IOPEN(14)
*
*     NOOPEN or DEFER or STAGE or STAGEKEEP or UNIXTAPE
      INFU(12,LX) = IOPEN(15)
*
*     compress
      INFU(13,LX) = IOPEN(16)
*
*     medium (0/1/2=disk/unixtape/rfio)
      IF(IOPEN(17).EQ.1) INFU(15,LX)=2
      IF(IOPEN(15).EQ.5) INFU(15,LX)=1
*
*     SKIPCOREV - skip corrupted event at the end of file
      INFU(32,LX) = IOPEN(18)
*
*     DUNIT=lun - log unit for data file accessed from index file
*     (specifically for DSYIBM cartridge access)
      IF(IOPEN(19).NE.0 .AND. INFU(35,LX).EQ.0) INFU(35,LX) = IOPEN(19)
*
*     parameter GENER="list_of_generic_names" given
      INFU(33,LX) = IOPEN(20)
*
*     set flag "DSTAGE parameter has been set"
      INFU(37,LX) = IOPEN(21)
*
*     set flag "DDEFER parameter has been set"
      INFU(38,LX) = IOPEN(22)
*
*     tape server ( 0=default IBM tape server
*                   1=AMPEX tape server
*                   2=ACS access using OSM service commands )
      INFU(41,LX) = IOPEN(24)
*
  100 RETURN
      END
