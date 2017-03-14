*CMZ :  1.00/00 16/12/94  17.00.00  by  Pavel Binko
*-- Author :    Volker Blobel   16/12/94
      SUBROUTINE FRKEY2(NAMKEY,NUM1,NUM2,NDATWD,IER)
*
      CHARACTER*(*) NAMKEY
*
************************************************************************
*
*     Return next key block (segment code 0 or 1)
*     (read before, if not yet done), additional argument
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
*
      CHARACTER NAMEC*8, CHA*4
      CHARACTER*72 ERRSTR
*
*     error if no read unit defined
      IF(LR.EQ.0) GOTO 98
*
*     important status words
*        INFU(ISGEN+1,LR) = read write etc
*        INFU(IRSTU+2,LR) = error code of last read operation
*        INFU(IRSTU+3,LR) = error code of actual logical record
*        INFU(IRSTU+4,LR) = 1 if key just read
*
*     check status for sequential IO only
      IF(INFU(2,LR).NE.1) GOTO 5
*
*     general status of unit
      IF(INFU(ISGEN+1,LR).EQ.0.OR.INFU(ISGEN+1,LR).EQ.5) THEN
*        status is undef or rewind - switch to reading
         INFU(ISGEN+1,LR)=1
         INFU(IRSTU+2,LR)=0
         INFU(IRSTU+3,LR)=0
         INFU(IRSTU+4,LR)=0
*        drop bank too
      ELSE IF(INFU(ISGEN+1,LR).EQ.2) THEN
*        status is unexpected end-of-file at reading
         WRITE(ERRSTR,7777) DTFLNM(LR)(1:LENB(DTFLNM(LR)))
 7777 FORMAT('Unexpected end-of-file on input "',A,'"')
         WRITE(*,*) ERRSTR
         IF(INFU(32,LR).EQ.1 .AND. INFU(IRSTU+2,LR).NE.0) THEN
*           ... and read status is incomplete logical record, i.e.
*           unexpected EOF - trasfer control to FRKEY.
            INFU(ISGEN+1,LR)=1
            INFU(IRSTU+2,LR)=0
            INFU(IRSTU+3,LR)=0
            INFU(IRSTU+4,LR)=0
            CALL FPWMSG(99,'FRKEY2',ERRSTR)
            GOTO 5
         ENDIF
         WRITE(ERRSTR,7778) INFU(IRKEY+4,LR),INFU(IRKEY+5,LR),
     +                 INFU(IRKEY+6,LR),INFU(IRKEY+7,LR)
 7778    FORMAT('Incomplete logical record "',2A4,2I12,'"')
         WRITE(*,*) ERRSTR
         WRITE(*,*) '*** Stop reading ***'
         CALL FPWMSG(99,'FRKEY2',ERRSTR)
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
    5 IF(INFU(IRSTU+4,LR).NE.1) THEN
*        reading necessary
         CALL FRKEY(NAMKEY,NUM1,NUM2,NDATWD,IER)
      ELSE
*        return previous key information
         IER=0
*        copy from INFU
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
         NAMKEY=NAMEC
         NUM1  =INFU(IRKEY+6,LR)
         NUM2  =INFU(IRKEY+7,LR)
         NDATWD=INFU(IRKEY+9,LR)
      END IF
*     reset key-flag
      INFU(IRSTU+4,LR)=0
      GOTO 100
*
*     all error conditions are set here
*               only argument IER is returned
*     error in calling order
   93 IER = 3
      CALL FPWMSG(IER,'FRKEY2','Error in calling order')
      GOTO 100
*     unit undefined
   98 IER = 8
      CALL FPWMSG(IER,'FRKEY2','No read unit defined')
      GOTO 100
*     end-of-record reached (no further data for this logical record)
   99 IER = -1
*
  100 RETURN
      END
