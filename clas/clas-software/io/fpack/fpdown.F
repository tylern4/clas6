#ifndef Linux
*CMZU:  1.00/16 27/02/96  16.39.15  by  Unknown
#endif /* not Linux */
*CMZ :  1.00/00 16/12/94  17.00.00  by  Pavel Binko
*-- Author :    Ruten Gurin   16/12/94
      SUBROUTINE FPDOWN(IMODE,IFOUND)
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
#ifndef Linux
*
*      PRINT *,'----------- FPDOWN ----------'
#endif /* not Linux */
*     save read position
      ISV27 = INFU(27,LR)
*     let's find the index block
      LEVEL = INFU(25,LR)
      INDX = INDXB(IBUF,1000*LUNR+9+LEVEL)
      IF( LEVEL.LE.0 .OR. INDX.LE.0 ) CALL LOGERROR('FPDOWN')
#ifndef Linux
*      PRINT '(1X,''LEVEL='',I2,1X,''TYP='',I2,1X,''B#'',I5)',
*     +    LEVEL,IBUF(INDX+17),IBUF(INDX+11)
*
*      DO I=1,IBUF(INDX+22),7
*      PRINT '(1X,2A4,5I9)',(IBUF(INDX+23+I+J-1),J=1,7)
*      ENDDO
 
#endif /* not Linux */
  10  CONTINUE
*     current position
      NPOS = IBUF(INDX+15)
#ifndef Linux
*      PRINT '(1X,''NPOS='',I5,2X,''TYPE='',I2)',NPOS,IBUF(INDX+17)
#endif /* not Linux */
      IF( IBUF(INDX+17).LE.1 ) THEN
*        no more index levels
         GOTO 100
      ENDIF
*     data segment header position in index block
      JP = 13
*     data position
      KP = JP + IBUF(INDX+JP+1)
*     index block number
      INFU(27,LR) = IBUF(INDX+KP+NPOS+6)
*     increase level
      LEVEL = LEVEL + 1
      INFU(25,LR) = LEVEL
      INDX = INDXB(IBUF,1000*LUNR+9+LEVEL)
      IF( INDX.LE.0 ) THEN
         INDX = INDXA(IBUF,1000*LUNR+9+LEVEL,INFU(1,LR)/4)
         IF( INDX.LE.0 ) CALL LOGERROR('FPDOWN. Out of memory.')
      ENDIF
#ifndef Linux
*      PRINT '(3X,''LEVEL='',I3,1X,2A4,5I10)',
*     +      LEVEL,(IBUF(INDX+KP+NPOS+J),J=1,7)
#endif /* not Linux */
*     read the record
      CALL FRRECX(LUNR,IBUF(INDX),IBUF(INDX+1),IER)
      IF( IER.NE.0 ) CALL LOGERROR('FPDOWN. Corrupted ordered file.')
*     set the right position
      CALL FPSRCH(IMODE,IFOUND)
      GOTO 10
100   CONTINUE
      INFU(27,LR) = ISV27
#ifndef Linux
*      PRINT *,'------------- END OF FPDOWN ----------------'
#endif /* not Linux */
      RETURN
      END
