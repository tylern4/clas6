*CMZ :  1.00/00 16/12/94  17.00.00  by  Pavel Binko
*-- Author :    Pavel Binko   16/12/94
      SUBROUTINE MKNXOP(IRC)
*
************************************************************************
*
*     Open the next file on input if any
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
*KEND.
*
      CHARACTER DANAME*16, FINAME*256, HONAME*127, VONAME*6
      INTEGER IOPEN(IOPMAX), LUNPAR(ILPMAX)
*
      CALL FQCOND('PRINT',IPFLAG)
*
      ISTAT = INFU(ISGEN+1,LR)
*     general status is: end-of-file at reading
      INFU(ISGEN+1,LR)=2
*
*
      IF( INFU(4,LR).NE.1 ) THEN
         CALL FPWMSG(78,'MKNXOP',
     +        'File has been opened NOT for reading')
         GOTO 99
      ENDIF
*
      III=IFRST(DANAME,-LUNBK(LR))
      IF(III.EQ.0) THEN
         CALL FPWMSG(79,'MKNXOP',
     +      'No unit found for INFU control block')
         GOTO 99
      ENDIF
*
*     Restore IOPEN from INFU
      CALL FPIOPEN(IOPEN,LR)
*
*     initialise voname
      VONAME=' '
*
******************************************************************
*
*     Open the next file with old status
*
******************************************************************
*
*
*        Check, whether next file_name exists
         IND6 = INDXB(IBUF,1000*LUNR+6)
*        Check, whether next serial_volume exists
         IF( INFU(31,LR).EQ.1 ) THEN
            IND7 = INDXB(IBUF,1000*LUNR+7)
         ENDIF
         IF( IND6.LE.0 ) GOTO 98
         CALL FQFILE(DANAME,FINAME,HONAME,LUNPAR,IRC)
         IF(IRC.NE.0) CALL LOGERROR('WRONG DATANAME')
*        Get the next file_name
         CALL FPNXTN(LUNR,FINAME)
*
*        Check, whether next serial_volume exists
         IF( INFU(31,LR).EQ.1 ) THEN
            IF( IND7.LE.0 ) GOTO 98
*           Get the next serial_volume
            CALL FPNXTV(LUNR,VONAME)
         ENDIF
*
*     Close the file without deleting its status
      CALL FICLOS(LUNBK(LR))
*
      INFU(ISGEN+1,LR)=ISTAT
*
*     Restore IOPEN from INFU
      CALL FPIOPEN(IOPEN,LR)
*
*     open the next file with old status
      IRC=0
      CALL MKOSPL(IOPEN,FINAME,DANAME,HONAME,VONAME,
     +             IRC)
      IF( IRC.NE.0 ) GOTO 99
*     reset the pointer to record segment header
      INFU(IRSTU+8,LR) = 0
*
      GOTO 100
*
   98 IF( (IND6.NE.IND7) .AND. INFU(31,LR).EQ.1 ) THEN
         WRITE(6,*)
     +        '++++++++++++++++++++++++++++++++++++++++++++++++++++++'
         WRITE(6,*)
     +        '+                                                    +'
         WRITE(6,*)
     +        '+   WARNING :                                        +'
         WRITE(6,*)
     +        '+                                                    +'
         WRITE(6,*)
     +        '+   Number of input file names do not correspond     +'
         WRITE(6,*)
     +        '+         to number of volume serial numbers         +'
         WRITE(6,*)
     +        '+                                                    +'
         WRITE(6,*)
     +        '++++++++++++++++++++++++++++++++++++++++++++++++++++++'
      END IF
 99   IRC=-1
*
 100  CONTINUE
*
      RETURN
      END
