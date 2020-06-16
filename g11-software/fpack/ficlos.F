#ifndef Linux
*CMZ :  1.00/14 02/03/96  11.29.04  by  Wiesiek Dlugosz
*CMZ :  1.00/13 21/02/96  22.19.17  by  Wiesiek Dlugosz
#endif /* not Linux */
*CMZ :  1.00/00 16/12/94  17.00.00  by  Pavel Binko
*-- Author :    Pavel Binko   16/12/94
      SUBROUTINE FICLOS(LUNPAR)
*
************************************************************************
*
*     Close  (by unit number), drop the read buffer
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
      LUN = LUNPAR
*
      IF(LUN.EQ.0) GOTO 100
      LL=LUNIT(LUN)
      IF(LL.EQ.0) GOTO 100
      IF(INFU(ISGEN+1,LL).EQ.3) THEN
*        status was writing - save settings
         LUNWS=LUNW
         LWSAV=LW
         CALL FWUNIT(LUN)
         CALL FWEOD
*        restore
         LUNW=LUNWS
         LW  =LWSAV
      END IF
      IF( INFU(7,LL).NE.0 ) THEN
*        it's an index, close data file, if connected.
         IF( INFU(7,LL).GT.0 ) CALL FUCLOX(LUNBK(INFU(7,LL)))
         INFU(7,LL) = 0
*        drop data file name
         DTFLNM(LL) = ' '
      ENDIF
*     drop read buffer, next file may have another record length
      IBLA = INDXA(IBUF,1000*LUN+1,-1)
*
*     if NOOPEN & SPECIAL, either CLOSE or ENDFILE should not be done
      IF(INFU(12,LL).EQ.1 .AND. INFU(2,LL).EQ.4) GOTO 100
*
      IF(INFU(12,LL).EQ.1 .AND. INFU(4,LL).NE.1) THEN
         ENDFILE(LUN)
         WRITE(6,'(''===> EOF mark written for unit'',I2)') LUN
         GOTO 90
      ENDIF
      IF(INFU(9,LL).NE.0) THEN
*        remote file
         CALL FPCLOS(LUN,IRET)
      ELSE
*        local file
#ifndef Linux
*+SELF, IF=UNIX, IF=LYON.
*         IF(INFU(8,LL).LE.0.OR.INFU(3,LL).EQ.2) THEN
#endif /* not Linux */
         IF(INFU(8,LL).EQ.-1.OR.INFU(3,LL).EQ.2) THEN
            CLOSE(LUN)
            INFU(8,LL)=0
         ELSE
            CALL FPSCLO(LUN)
         END IF
      END IF
   90 CONTINUE
*
  100 RETURN
      END
