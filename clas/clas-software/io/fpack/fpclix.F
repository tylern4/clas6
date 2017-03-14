*CMZ :  1.00/05 06/08/95  18.00.23  by  Wiesiek Dlugosz
*CMZU:  1.00/02 10/02/95  16.15.38  by  Wiesiek Dlugosz
*CMZ :  1.00/00 16/12/94  17.00.00  by  Pavel Binko
*-- Author :    Ruten Gurin   16/12/94
      SUBROUTINE FPCLIX(FROM,TO,IC,IERR)
*
      CHARACTER*(*) FROM,TO
*
************************************************************************
*
*     Write one entry into index file, change the classification word
*
************************************************************************
*
      INTEGER IHL(100),ICL
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
      CHARACTER*512 TMPBUF, HONAME*127
      CHARACTER*12 STR, MODA(0:4)*9
      DATA MODA/'Unknown','READ','WRITE','READWRITE','MODIFY'/
*
      ICL = IC
      GOTO 10
*
************************************************************************
*
      ENTRY FPCURX(FROM,TO,IERR)
*
************************************************************************
*
*     Write one entry into index file
*
************************************************************************
*
      ICL = -1
*
   10 IERR = 0
      LRSAV = LR
      LUNRSAV = LUNR
      LWSAV = LW
      LUNWSAV = LUNW
      LFR = LENB(FROM)
      LTO = LENB(TO)
      IF(FROM(:LFR).EQ.TO(:LTO)) GOTO 90
      LUNR = IFRST(FROM,0)
      IF( LUNR.LE.0 .OR. LUNR.GT.99 ) GOTO 91
      LR = LUNIT(LUNR)
      IF( LR.LE.0 ) GOTO 92
      IF(INFU(4,LR).NE.1 .AND. INFU(4,LR).NE.3) GOTO 93
*
      LUNW = IFRST(TO,0)
      IF( LUNW.LE.0 .OR. LUNW.GT.99 ) GOTO 94
      LW = LUNIT(LUNW)
      IF( LW.LE.0 ) GOTO 95
*     access must be on 'write' or 'readwrite'
      IF(INFU(4,LW).NE.2 .AND. INFU(4,LW).NE.3) GOTO 96
*
      IF(LR.EQ.LW .OR. LUNR.EQ.LUNW) THEN
         WRITE(6,*) 'FPCLIX/FPCURX. Internal FPACK error.'
         WRITE(6,*) 'LR=',LR,' LW=',LW,' LUNR=',LUNR,' LUNW=',LUNW
         STOP
      ENDIF
*
      IF( DTFLNM(LR).EQ.' ') GOTO 97
*
      IF( INFU(7,LW).EQ.0 .OR. DTFLNM(LR).NE.DTFLNM(LW) .OR.
     +                         DTFLHO(LR).NE.DTFLHO(LW) .OR.
     +                         DTFLMD(LR).NE.DTFLMD(LW)) THEN
         DTFLNM(LW) = DTFLNM(LR)
         DTFLHO(LW) = DTFLHO(LR)
         DTFLMD(LW) = DTFLMD(LR)
         RLFLNM(LW) = RLFLNM(LR)
         RLFLHO(LW) = RLFLHO(LR)
         RLFLMD(LW) = RLFLMD(LR)
*
*
         IF( IDXNEW ) THEN
           L = LENB(RLFLNM(LW))
         ELSE
           L = LENB(DTFLNM(LW))
         ENDIF
 
         ISV24 = INFU(24,LW)
         INFU(24,LW) = 1
         INFU(7,LW) = 0
*     install a sign of index file
         CALL FWKEY('FPIDXNEW',1,2,3)
*
*     write FPINDEX0 bank in the following format:
*        "file_name;host_name;mediatype;;"
         IF(INFU(34,LR).EQ.0) THEN
*     LR refers to data file then get its record length
            ILRECL=INFU(1,LR)
         ELSE
*     LR refers to index file keeping in INFU(34,LR) record length of
*     data file
            ILRECL=INFU(34,LR)
         ENDIF
*                                SHIFT      RECL
         CALL FWHDR('FPINDEX0',INFU(28,LR),INFU(1,LR),6,'A')
*     data file name
 
         IF( IDXNEW) THEN
            TMPBUF = RLFLNM(LW)(1:L)//';'
         ELSE
            TMPBUF = DTFLNM(LW)(1:L)//';'
         ENDIF
 
         LBUF = L+1
*     get actual host name and ...
         HONAME=' '
 
         IF( IDXNEW ) THEN
          L = LENB(RLFLHO(LW))
          IF(L.EQ.0 .OR. (L.NE.0 .AND. RLFLHO(LW)(1:L).EQ.'LOCAL')) THEN
            CALL FPGTH(HONAME,L)
          ELSE
            HONAME=RLFLHO(LW)(1:L)
          ENDIF
         ELSE
          L = LENB(DTFLHO(LW))
          IF(L.EQ.0 .OR. (L.NE.0 .AND. DTFLHO(LW)(1:L).EQ.'LOCAL')) THEN
            CALL FPGTH(HONAME,L)
          ELSE
            HONAME=DTFLHO(LW)(1:L)
          ENDIF
         ENDIF
 
*     ... concatenate it
         TMPBUF=TMPBUF(:LBUF)//HONAME(1:L)//';'
         LBUF=LBUF+L+1
*     concatenate media type
 
         IF( IDXNEW ) THEN
            L = LENB(RLFLMD(LW))
         ELSE
            L = LENB(DTFLMD(LW))
         ENDIF
 
         IF( L.EQ.0 )      GOTO 97
 
         IF( IDXNEW) THEN
            TMPBUF=TMPBUF(:LBUF)//RLFLMD(LW)(:L)//';;'
         ELSE
            TMPBUF=TMPBUF(:LBUF)//DTFLMD(LW)(:L)//';;'
         ENDIF
 
         LBUF=LBUF+L+2
*
         CALL TXTOHL(TMPBUF(1:LBUF),IHL,NHL)
         CALL FWDAT(NHL,IHL)
*
         CALL FWHDR('FPINDEX1',7,8,9,'I')
         INFU(24,LW) = ISV24
*        initialize run name and run number to force FPACK
*        writing them out to index file
         CALL TXTOHL('        ',DTCRNM(1,LW),2)
         DTCRRN(LW)=-1
      ENDIF
      INFU(7,LW) = 0
      NHL=0
*     As soon as  run name is kept as the hollerits ...
      IF( DTCRNM(1,LW).NE.INFU(IRKEY+4,LR) .OR.
     +    DTCRNM(2,LW).NE.INFU(IRKEY+5,LR) ) THEN
*         ... copy markers of changing both run name and run number
          IHL(NHL+1)=-2
          NHL=NHL+1
*         ... copy 2 words of new run name to IHL-buffer
          DO 80 J=1,2
             DTCRNM(J,LW) = INFU(IRKEY+3+J,LR)
             IHL(NHL+J)=DTCRNM(J,LW)
80        CONTINUE
*         and copy 1 word of new run number
          DTCRRN(LW) = INFU(IRKEY+6,LR)
          IHL(NHL+3) = DTCRRN(LW)
          NHL=NHL+3
      ENDIF
      IF( DTCRRN(LW).NE.INFU(IRKEY+6,LR) ) THEN
*         ... set marker of changing a run number
          IHL(NHL+1) = -1
*         ... copy 1 word of new run number
          DTCRRN(LW) = INFU(IRKEY+6,LR)
          IHL(NHL+2) = DTCRRN(LW)
          NHL = NHL+2
      ENDIF
*     copy 1 word of event number
      IHL(NHL+1) = INFU(IRKEY+7,LR)
*     check classword given by user
      IF( ICL.EQ.-1 ) THEN
*        ... copy classword from datafile
         IHL(NHL+2) = INFU(IRKEY+9,LR)
      ELSE
*        ... copy classword of user
         IHL(NHL+2) = ICL
      ENDIF
*     ... copy address of event on data file
      IHL(NHL+3) = DTFLKB(LR)
      IHL(NHL+4) = DTFLPS(LR)
*     ... increment counter
      NHL=NHL+4
*
      CALL FWDAT(NHL,IHL)
      INFU(7,LW) = 1
      GOTO 100
*
90    ERRMES='Input "'//FROM(:LFR)//
     +          '" and output "'//TO(:LTO)//'" are the same'
      GOTO 99
91    CALL PNVI(LUNR,STR,J)
      ERRMES='Input unit '//STR(:J)//' out of range'
      GOTO 99
92    CALL PNVI(LR,STR,J)
      ERRMES='Input unit index '//STR(:J)
     +       //' out of range'
      GOTO 99
93    I=INFU(4,LR)
      CALL PNVI(I,STR,J)
      IF(I.LT.1 .OR.I.GT.4) I=0
      ERRMES='Invalid input mode'//STR(:J)//' ('//
     +         MODA(I)(:LENB(MODA(I)))//')'
      GOTO 99
94    CALL PNVI(LUNW,STR,J)
      ERRMES='Output unit '//STR(:J)//' out of range'
      GOTO 99
95    CALL PNVI(LW,STR,J)
      ERRMES='Output unit index '//STR(:J)
     +       //' out of range'
      GOTO 99
96    I=INFU(4,LW)
      CALL PNVI(I,STR,J)
      ERRMES='Invalid output mode'//STR(:J)//' ('//
     +         MODA(I)(:LENB(MODA(I)))//')'
      GOTO 99
97    ERRMES='Invalid input data file name'
*
99    IERR=1
      CALL FPWMSG(IERR,'FPCLIX',ERRMES)
*
100   CONTINUE
      LR = LRSAV
      LUNR = LUNRSAV
      LW = LWSAV
      LUNW = LUNWSAV
*
      RETURN
      END
