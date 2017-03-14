*CMZU:  1.00/02 08/02/95  13.18.59  by  Unknown
*-- Author :    Sergei Esenov   08/02/95
      SUBROUTINE FPRPTH
*     Print current index path for ordered access
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
      IF(LR.EQ.0) GOTO 100
      IF(INFU(2,LR).NE.5) GOTO 100
      IF(INFU(25,LR).LE.0) GOTO 100	
*     get current level (must be bottom level - leaves)
      LEVELX= INFU(25,LR)
      IELSZ = 7
      DO 50 LEVEL=1,LEVELX
         INDX = INDXB(IBUF,1000*LUNR+9+LEVEL)
         IF( LEVEL.LE.0 .OR. INDX.LE.0 ) CALL LOGERROR('FPRPTH')
         PRINT '(1X,30(''=''),'' LEVEL='',I2,1X,30(''=''))',LEVEL
         PRINT *,'Phys. rec',IBUF(INDX+11),' FLAG ',IBUF(INDX+17),
     +        ' (bottom=1, others=2)'
*     data segment header position in index block
         JP = 13
*     data position
         KP = JP + IBUF(INDX+JP+1)
*     number of data words
         NW = IBUF(INDX+22)
         PRINT *,'USED',NW,' word(s),',NW/7,' element(s)'
*     current position
         NPOS = IBUF(INDX+15)
*
         ICUR=0
         DO 40 J=1,NW,IELSZ
            ICUR = ICUR+1
            IF(J.EQ.NPOS+1) THEN
               PRINT '(1X,I5,1X,2A4,3(1X,I9),2(1X,I12),'' current'')',
     +              ICUR,(IBUF(INDX+KP+J+K-1),K=1,7)
            ELSE
               PRINT '(1X,I5,1X,2A4,3(1X,I9),2(1X,I12))',
     +              ICUR,(IBUF(INDX+KP+J+K-1),K=1,7)
            ENDIF
 40      CONTINUE
         PRINT *,'CURRENT POSITION NPOS',NPOS,' NUM',NPOS/7
         PRINT '(1X,2A4,3(1X,I9),2(1X,I12))',
     +           (IBUF(INDX+KP+NPOS+K),K=1,7)
 50   CONTINUE
      PRINT '(1X,70(''!''))'
100   RETURN
      END
