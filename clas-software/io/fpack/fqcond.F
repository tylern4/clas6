*CMZ :  1.00/00 16/12/94  17.00.00  by  Pavel Binko
*-- Author :    Volker Blobel   16/12/94
      SUBROUTINE FQCOND(CASET,JARG)
*
      CHARACTER*(*) CASET
*
************************************************************************
*
*     Communication subroutine of conditions
*                 _____
*     CALL FQCOND(CASET,JARG)   get condition code
*                       ----
*
*                 _____ ____
*     CALL FQCSET(CASET,JARG)   set condition code
*
*     Inquiry on conditions
*     CASET text    IANS value    explanation
*      TIME         1 if true     left time less than limit
*      STOP        >0 if true     any stop condition
*      OPENERROR   >0 if true     any OPEN error (reset!)
*      PRINT        print_level
*
************************************************************************
*
* Event Analyses TIME
      PARAMETER (NCNTMX = 5)
      INTEGER   ITCNT
      REAL      EAMAX,EATIME(NCNTMX)
      LOGICAL   START
*
      CHARACTER CASEC*16
      EQUIVALENCE (IARG,FARG)
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
*     initial/default values
      DATA SECST/1.05/,ISTOP/0/,IOPERR/0/,IPRFLG/0/
      DATA ITCNT/0/,START/.TRUE./
      SAVE EAMAX,EATIME,ITCNT,START
*
      JARG=0
      CASEC=CASET
*
*     convert to upper case
      CALL UCASE(CASEC)
*
*     analyse text
      IF(CASEC.EQ.'TIME') THEN
         IF(START) THEN
            START = .FALSE.
            DO 10 I=1,NCNTMX
  10        EATIME(I)=0.
            ITCNT=0
            CALL TIMED(EATIME(1))
         ENDIF
         ITCNT = ITCNT+1
         IF(ITCNT.GT.NCNTMX) ITCNT=1
*        check Event Analyses TIME interval and ...
         CALL TIMED(EATIME(ITCNT))
*        find max. time interval for last NCNTMX events.
         EAMAX = 0.
         DO 20 I=1,NCNTMX
  20     IF(EATIME(I).GT.EAMAX) EAMAX = EATIME(I)
*
*        check remaining seconds
         CALL TIMEL(TLEFT)
         IF(TLEFT-EAMAX.LE.SECST) JARG=1
      ELSE IF(CASEC.EQ.'STOP') THEN
*        return stop condition
         JARG=ISTOP
      ELSE IF(CASEC.EQ.'OPENERROR') THEN
*        return number of open errors and reset
         JARG=IOPERR
         IOPERR=0
      ELSE IF(CASEC.EQ.'PRINT') THEN
*        return print flag
         JARG=IPRFLG
      END IF
*
      GOTO 100
*
************************************************************************
*
      ENTRY FQCSET(CASET,JARG)
*
************************************************************************
*
*     set condition CASET to code JARG
      IARG=JARG
      CASEC=CASET
*
*     convert to upper case
      CALL UCASE(CASEC)
*
*     analyse text
      IF(CASEC.EQ.'TIME') THEN
*        get seconds (in floating point)
         SECST=FARG
      ELSE IF(CASEC.EQ.'STOP') THEN
*        set stop condition to argument
         IF(ISTOP.EQ.0) ISTOP=IARG
      ELSE IF(CASEC.EQ.'OPENERROR') THEN
*        increase counter for open errors
         IOPERR=IOPERR+1
         IF(IARG.EQ.0) IOPERR=0
      ELSE IF(CASEC.EQ.'PRINT') THEN
*        store print flag
         IPRFLG=IARG
      END IF
*
  100 RETURN
      END
