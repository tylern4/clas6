*CMZ :  1.00/00 16/12/94  17.00.00  by  Pavel Binko
*-- Author :    Volker Blobel   16/12/94
      SUBROUTINE FSELCR(ICASE,LUNDEC,IDEC)
*
************************************************************************
*
*     Decide on the basis of the RSELECT statements
*
*     to be called in FRKEY with ICASE = 1
*                     FRHDR              2
*                     FWKEY              3
*                     FWHDR              4
*
*     LUNDEC = unit
*
*     result : IDEC = 0     no action
*                   = 1     skip this data
*                   = 2     STOP
*
************************************************************************
*
      LOGICAL RCKEXIST,RCKACCEPT
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
 
*     variables for RECKEY selections
      RCKEXIST = .FALSE.
      RCKACCEPT = .FALSE.
*
      IDEC=0
      IF(ICASE.LT.1.OR.ICASE.GT.4) GOTO 100
      IF(LUNDEC.LT.1.OR.LUNDEC.GT.99) GOTO 100
      LL=LUNIT(LUNDEC)
      IF(LL.EQ.0) GOTO 100
      IF(INFU(24,LL).NE.0) GOTO 100
      IND20=INDXB(IBUF,1000*LUNDEC+20 )
      IF(IND20.EQ.0) GOTO 100
*
*     define index to key
      IF(ICASE.LE.2) THEN
         IKEY=IRKEY
         IHDR=IRHDR
         IOST=IRSTA
      ELSE
         IKEY=IWKEY
         IHDR=IWHDR
         IOST=IWSTA
      END IF
*
*     some selection is existing - go into details
*
*     ... check date
      IF(IBUF(IND20+2).NE.0.AND.MOD(ICASE,2).EQ.1) THEN
*        after date
         IF(INFU(IKEY+7,LL).LT.IBUF(IND20+2)) GOTO 90
      END IF
      IF(IBUF(IND20+3).NE.0.AND.MOD(ICASE,2).EQ.1) THEN
*        before date
         IF(INFU(IKEY+7,LL).GT.IBUF(IND20+3)) GOTO 90
      END IF
*
*     ... check STOPMB
      IF(IBUF(IND20+4).NE.0.AND.MOD(ICASE,2).EQ.1) THEN
         MB=IBUF(IND20+4)
         F1MB = 1000000.0
         FMB = (INFU(1,LL)/F1MB)*INFU(IOST+2,LL)
*        where are the megabytes stored? assume in statistic place 1
         IF(MB.GT.FMB) GOTO 95
      END IF
*
*     ... check STOPSEC
      IF(IBUF(IND20+5).NE.0.AND.MOD(ICASE,2).EQ.1) THEN
         SC=IBUF(IND20+5)
*        which function to get the time? Assume TIML
c        IF(SC.GT.TIML(DUMMY)) GOTO 95
      END IF
*
*     now check the remaining selection possibilities
      IND=IND20
*     get index of next bank
   10 IND=INDXC(IBUF,IND)
      IF(IND.EQ.0) GOTO 85
      IDENT=IBUF(IND-2)
      IF(IDENT/1000.NE.LUNDEC) GOTO 85
      ISEL=IBUF(IND+1)
 
*     ... check class
      IF(ISEL.EQ.1 .OR. ISEL.EQ.-1) THEN
         IF(MOD(ICASE,2).NE.1) GOTO 10
*        check class - compare record name
         IF(IBUF(IND+2).EQ.INFU(IKEY+4,LL).AND.
     +      IBUF(IND+3).EQ.INFU(IKEY+5,LL)) THEN
*           ... and check bits
            IF(ISEL.GT.0) THEN
               IF(IAND(IBUF(IND+4),INFU(IKEY+9,LL)).EQ.0) GOTO 90
            ELSE
               IF(IAND(IBUF(IND+4),INFU(IKEY+9,LL)).NE.0) GOTO 90
            END IF
         END IF
      ELSE IF(ISEL.EQ.2 .OR. ISEL.EQ.-2) THEN
*
*     RECORD/NOTREORD
*     1      isel
*     2      group of integers
*
*        RECORD/NOTRECORD - only for input -----------------------------
         IF(ICASE.GT.1) GOTO 10
*        ... using last read record number
         NUMBER=INFU(IOST+6,LL)
         JDEC=NUMTAG(NUMBER,IBUF(IND+2),IBUF(IND)-1)
         IF(ISEL.GT.0) THEN
*           RECORD
            IF(JDEC.EQ.0.OR.JDEC.EQ.2) GOTO 90
            IF(JDEC.EQ.3) GOTO 95
         ELSE
*           NOTRECORD
            IF(JDEC.EQ.1) GOTO 90
         END IF
      ELSE IF(ISEL.EQ.3 .OR. ISEL.EQ.-3) THEN
*
*     RECNAME/NOTRECNAME
*     1   isel
*     2   ) first
*     3   )    name
*     4   ) second
*     5   )    name
*
         IF(MOD(ICASE,2).NE.1) GOTO 10
         NBLKS=(IBUF(IND)-1)/2
         JDEC=0
         DO 15 I=1,NBLKS
         IF(IBUF(IND+2*I  ).EQ.INFU(IKEY+4,LL).AND.
     +      IBUF(IND+2*I+1).EQ.INFU(IKEY+5,LL)) THEN
*           record name found
            IF(ISEL.GT.0) GOTO 10
            GOTO 90
         END IF
   15   CONTINUE
*        record name not found
         IF(ISEL.LT.0) GOTO 10
         GOTO 90
      ELSE IF(ISEL.EQ.4 .OR. ISEL.EQ.-4) THEN
*
*     RECRANGE
*     1   isel
*     2   ) record(1:4)
*     3   )   name(5:8)
*     4   ) group of integers
*
         IF(MOD(ICASE,2).NE.1) GOTO 10
         IF(IBUF(IND+2).EQ.INFU(IKEY+4,LL).AND.
     +      IBUF(IND+3).EQ.INFU(IKEY+5,LL)) THEN
*           correct record name found
            NUMBER=INFU(IKEY+6,LL)
            JDEC=NUMTAG(NUMBER,IBUF(IND+4),IBUF(IND)-3)
            IF(ISEL.GT.0) THEN
*              RECRANGE
               IF(JDEC.EQ.0.OR.JDEC.EQ.2) GOTO 90
               IF(JDEC.EQ.3) GOTO 95
            ELSE
*              NOTRECRANGE
               IF(JDEC.EQ.1) GOTO 90
            END IF
         END IF
      ELSE IF(ISEL.EQ.5 .OR. ISEL.EQ.-5) THEN
*
*     1   isel
*     2   ) record
*     3   )    name
*     3   ) first
*     4   )    block name
*     5   ) second
*     6   )    block name
*
         IF(MOD(ICASE,2).NE.0) GOTO 10
         IF(IBUF(IND+2).EQ.INFU(IKEY+4,LL).AND.
     +      IBUF(IND+3).EQ.INFU(IKEY+5,LL)) THEN
*           correct record name found
            NBLKS=(IBUF(IND)-3)/2
            JDEC=0
 
            DO 20 I=1,NBLKS
            IF(IBUF(IND+2*I+2).EQ.INFU(IHDR+2,LL).AND.
     +         IBUF(IND+2*I+3).EQ.INFU(IHDR+3,LL)) THEN
*              block name found
               IF(ISEL.GT.0) GOTO 10
               GOTO 90
            END IF
   20       CONTINUE
*           block name not found
            IF(ISEL.LT.0) GOTO 10
            GOTO 90
         END IF
      ELSE IF(ISEL.EQ.6 .OR. ISEL.EQ.-6) THEN
*        RECKEY --------------------------------------------------------
*
*     1   isel
*     2   ) record
*     3   )    name
*     4     record number A
*     5     group of integers record number B
*
         IF(MOD(ICASE,2).NE.1) GOTO 10
         IF(ISEL.GT.0) THEN
            IF( RCKACCEPT ) GOTO 10
*           record is accepted already
            RCKEXIST = .TRUE.
         ENDIF
         IF(IBUF(IND+2).EQ.INFU(IKEY+4,LL).AND.
     +      IBUF(IND+3).EQ.INFU(IKEY+5,LL).AND.
     +      IBUF(IND+4).EQ.INFU(IKEY+6,LL)) THEN
*           correct record name and first number found
            NUMBER=INFU(IKEY+7,LL)
*           check second number
            JDEC=NUMTAG(NUMBER,IBUF(IND+5),IBUF(IND)-4)
            IF(ISEL.GT.0) THEN
*              RECKEY
               IF(JDEC.EQ.1) RCKACCEPT = .TRUE.
            ELSE
*              NOTRECKEY
               IF(JDEC.EQ.1) GOTO 90
            END IF
         END IF
      END IF
      GOTO 10
*
   85 CONTINUE
*     test RECKEY selection results
      IF(RCKEXIST.AND..NOT.RCKACCEPT) IDEC=1
      GOTO 100
 
*     set the skip flag
   90 IDEC=1
      GOTO 100
*
*     set the STOP flag
   95 IDEC=2
      CALL FQCSET('STOP',2)
*
  100 RETURN
      END
