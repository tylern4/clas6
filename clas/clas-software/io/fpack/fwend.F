#ifndef Linux
*CMZU:  1.00/16 26/02/96  11.55.55  by  Sergey Esenov
#endif /* not Linux */
*CMZ :  1.00/00 16/12/94  17.00.00  by  Pavel Binko
*-- Author :    Volker Blobel   16/12/94
      SUBROUTINE FWEND(IERR)
*
************************************************************************
*
*     End of ...
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
*KEEP,FBUFFER.
*     buffer common
      PARAMETER (NBUF=100000)
      COMMON/FBUF/IBUF(NBUF)
      SAVE/FBUF/
*KEND.
*
      IERR=0
      IF(LW.EQ.0) GOTO 100
*     test, if something is output
      IF(INFU(IWSTU+4,LW).NE.2) GOTO 100
      IF(INFU(3,LW).EQ.1) THEN
*        binary
         INDW = INDXB(IBUF,1000*LUNW+2)
         IF(INDW.LE.0)CALL LOGERROR('NO OUTPUT BUFFER')
         IF(INFU(IWSTU+5,LW).EQ.2) THEN
*           previous data segment header was added to buffer
#ifndef Linux
            IF(INFU(25,LW).NE.0.AND.INFU(ISGEN+11,LW).NE.0) THEN
               JP=INFU(IWSTU+19,LW)
            ELSE
#endif /* not Linux */
               JP=INFU(IWSTU+9,LW)
#ifndef Linux
            ENDIF
#endif /* not Linux */
*           update data segment header
            IF(IBUF(INDW+JP+7).NE.0) IBUF(INDW+JP+7)=3
         END IF
#ifndef Linux
         IF(INFU(25,LW).NE.0.AND.INFU(ISGEN+11,LW).NE.0) THEN
            IG=INDW+INFU(IWSTU+18,LW)
         ELSE
#endif /* not Linux */
            IG=INDW+INFU(IWSTU+8,LW)
#ifndef Linux
         ENDIF
#endif /* not Linux */
*        set segment code to 3, if spanned record
         IF(IBUF(IG+10).NE.0) IBUF(IG+10)=3
*        keyed access and RECSEP require output
*        of last physical record in each logical record
         IF(INFU(2,LW).EQ.3.OR.INFU(11,LW).EQ.1) CALL FPFLSH(IERR)
         IF( INFU(25,LW).NE.0 ) CALL FWKUPD(0,IERR)
      ELSE
*        text-file
         CALL TEXTD(' ')
         CALL TEXTD('END-OF-RECORD')
         CALL TEXTD(' ')
      END IF
*     key becomes undefined
   90 INFU(IWSTU+4,LW)=0
*     no data file
      INFU(7,LW) = 0
*
  100 RETURN
      END
