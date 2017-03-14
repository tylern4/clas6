*CMZ :  1.00/00 16/12/94  17.00.00  by  Pavel Binko
*-- Author :    Volker Blobel   16/12/94
      SUBROUTINE FSELCF(ICASE,LUNDEC,IDEC)
*
************************************************************************
*
*     Decide on the basis of the FSELECT statements
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
*                   = 2     stop
*
************************************************************************
*
      LOGICAL STOPFLAG,STOPEXIST,STOPGLOB,BLEXIST,RKEXIST
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
*
      IDEC=0
      IF(ICASE.LT.1.OR.ICASE.GT.4) GOTO 100
      IF(LUNDEC.LT.1.OR.LUNDEC.GT.99) GOTO 100
      LL=LUNIT(LUNDEC)
      IF(LL.EQ.0) GOTO 100
      IF(INFU(24,LL).NE.0) GOTO 100
      IND20=INDXB(IBUF,1000*LUNDEC+20 )
      IF( IND20.EQ.0 ) GOTO 100
      ICASE2 = MOD(ICASE,2)
*     test BLOCK/NOTBLOCK, RECBLOCK/NOTRECBLOCK
      IF( ICASE2.EQ.0 .AND. IBUF(IND20+6).EQ.0 ) GOTO 100
      IF( ICASE2.EQ.1 .AND. IBUF(IND20+7).EQ.0 ) GOTO 100
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
*     check STOPMB
      IF(IBUF(IND20+4).NE.0.AND.ICASE2.EQ.1) THEN
         MB=IBUF(IND20+4)
         F1MB = 1000000.0
         FMB = (INFU(1,LL)/F1MB)*INFU(IOST+2,LL)
*        where are the megabytes stored? assume in statistic place 1
         IF(MB.GT.FMB) GOTO 95
      END IF
*
*     check STOPSEC
      IF(IBUF(IND20+5).NE.0.AND.ICASE2.EQ.1) THEN
         SC=IBUF(IND20+5)
*        which function to get the time? Assume TIML
c        IF(SC.GT.TIML(DUMMY)) GOTO 95
      END IF
*
*     now check the remaining selection possibilities
      IND=INDXB(IBUF,1000*LUNDEC+21 )
      IF( IND.EQ.0 ) GOTO 100
      MAXID = 1000*LUNDEC+199
      STOPGLOB = ICASE2.EQ.1
10    CONTINUE
      LIM = IND+IBUF(IND)
      I = IND+1
      STOPFLAG = STOPGLOB
      STOPEXIST = .FALSE.
      BLEXIST = .FALSE.
      RKEXIST = .FALSE.
20    CONTINUE
      IF( I.GT.LIM ) THEN
         IF( ICASE2.EQ.0 .AND. .NOT.BLEXIST ) GOTO 15
         IF( ICASE2.EQ.1 .AND. .NOT.RKEXIST ) GOTO 15
         GOTO 100
      ENDIF
      KW = IBUF(I)
      I=I+1
      GOTO(1100,1200,1300,1400,1500,1600,1700,1800,1900,2000,2100,2200),
     +    IABS(KW)
      CALL LOGERROR('Invalid information in SELECT bank')
*
1100  CONTINUE
*     RECORD/NOTRECORD
      IF( ICASE.EQ.1 ) THEN
         RKEXIST = .TRUE.
         STOPEXIST = .TRUE.
         IFLAG = NUMTG3(IBUF,I,INFU(IOST+6,LL))
         IF( KW.GT.0 ) THEN
            IF( IFLAG.NE.2 ) STOPFLAG = .FALSE.
            IF( IFLAG.NE.0 ) GOTO 15
         ELSE
            STOPFLAG = .FALSE.
            IF( IFLAG.EQ.0 ) GOTO 15
         ENDIF
      ENDIF
      I = I + IBUF(I) + 1
      GOTO 20
*
1200  CONTINUE
*     RECKEY/NOTRECKEY
*
*    -1   isel
*     0   ) record
*     1   )    name
*     2     record number A
*     3     count, group of integers record number B
*
      IF(ICASE2.EQ.1) THEN
         RKEXIST = .TRUE.
         IF(IBUF(I+0).EQ.INFU(IKEY+4,LL).AND.
     +      IBUF(I+1).EQ.INFU(IKEY+5,LL) ) THEN
*           correct record name found
            IF( IBUF(I+2).GT.INFU(IKEY+6,LL)) THEN
               IFLAG = 1
            ELSE IF( IBUF(I+2).EQ.INFU(IKEY+6,LL)) THEN
*              correct first number found
               IFLAG = NUMTG3(IBUF,I+3,INFU(IKEY+7,LL))
            ELSE
               IFLAG = 2
            ENDIF
         ELSE
*           accept, if another recname
            IFLAG = 0
         ENDIF
         IF( KW.GT.0 ) THEN
            IF( IFLAG.NE.0 ) GOTO 15
         ELSE
            IF( IFLAG.EQ.0 ) GOTO 15
         ENDIF
      ENDIF
      I = I+3
      I = I + IBUF(I) + 1
      GOTO 20
*
1300  CONTINUE
*     RECNAME/NOTRECNAME
      IF(ICASE2.EQ.1) THEN
         RKEXIST = .TRUE.
         IFLAG = 1
         DO 1301 J = I+1,I+IBUF(I),2
            IF(IBUF(J+0).EQ.INFU(IKEY+4,LL).AND.
     +         IBUF(J+1).EQ.INFU(IKEY+5,LL)) THEN
*              record name found
               IFLAG = 0
               GOTO 1302
            END IF
1301     CONTINUE
1302     CONTINUE
         IF( KW.GT.0 ) THEN
            IF( IFLAG.NE.0 ) GOTO 15
         ELSE
            IF( IFLAG.EQ.0 ) GOTO 15
         ENDIF
      ENDIF
      I = I + IBUF(I) + 1
      GOTO 20
*
1400  CONTINUE
*     NUMRA/NOTNUMRA
      IF( ICASE2.EQ.1 ) THEN
         RKEXIST = .TRUE.
         IFLAG = NUMTG3(IBUF,I,INFU(IKEY+6,LL))
         IF( KW.GT.0 ) THEN
            IF( IFLAG.NE.0 ) GOTO 15
         ELSE
            IF( IFLAG.EQ.0 ) GOTO 15
         ENDIF
      ENDIF
      I = I + IBUF(I) + 1
      GOTO 20
*
1500  CONTINUE
*     NUMRB/NOTNUMRB
      IF( ICASE2.EQ.1 ) THEN
         RKEXIST = .TRUE.
         IFLAG = NUMTG3(IBUF,I,INFU(IKEY+7,LL))
         IF( KW.GT.0 ) THEN
            IF( IFLAG.NE.0 ) GOTO 15
         ELSE
            IF( IFLAG.EQ.0 ) GOTO 15
         ENDIF
      ENDIF
      I = I + IBUF(I) + 1
      GOTO 20
*
1600  CONTINUE
*     CLASS/NOTCLASS
      IF( ICASE2.EQ.1 ) THEN
         RKEXIST = .TRUE.
         IFLAG = IAND(IBUF(I),INFU(IKEY+9,LL))
*        note inversed value for flag !
         IF( KW.GT.0 ) THEN
            IF( IFLAG.EQ.0 ) GOTO 15
         ELSE
            IF( IFLAG.NE.0 ) GOTO 15
         ENDIF
      ENDIF
      I = I + 1
      GOTO 20
*
1700  CONTINUE
*     BLOCK/NOTBLOCK
      IF(ICASE2.NE.1) THEN
         BLEXIST = .TRUE.
         IFLAG = 1
         DO 1701 J = I+1,I+IBUF(I),2
            IF(IBUF(J+0).EQ.INFU(IHDR+2,LL).AND.
     +         IBUF(J+1).EQ.INFU(IHDR+3,LL)) THEN
*              block name found
               IFLAG = 0
               GOTO 1702
            END IF
1701     CONTINUE
*        IFLAG = 1
1702     CONTINUE
         IF( KW.GT.0 ) THEN
            IF( IFLAG.NE.0 ) GOTO 15
         ELSE
            IF( IFLAG.EQ.0 ) GOTO 15
         ENDIF
      ENDIF
      I = I + IBUF(I) + 1
      GOTO 20
*
1800  CONTINUE
*     BEFOREDATE/AFTERDATE
      IF( ICASE2.EQ.1 ) THEN
         RKEXIST = .TRUE.
         IF( KW.GT.0 ) THEN
            IF(INFU(IKEY+7,LL).GT.IBUF(I)) GOTO 15
         ELSE
            IF(INFU(IKEY+7,LL).LT.IBUF(I)) GOTO 15
         ENDIF
      ENDIF
      I = I + 1
      GOTO 20
*
1900  CONTINUE
*     RECRANGE/NOTRECRANGE
*
*    -1   isel
*     0   ) record
*     1   )    name
*     2     count, group of integers record number A
*
      IF(ICASE2.EQ.1) THEN
         RKEXIST = .TRUE.
         IF(IBUF(I+0).EQ.INFU(IKEY+4,LL).AND.
     +      IBUF(I+1).EQ.INFU(IKEY+5,LL) ) THEN
*           correct record name found
            IFLAG = NUMTG3(IBUF,I+2,INFU(IKEY+6,LL))
         ELSE
*           accept, if another recname
            IFLAG = 0
         ENDIF
         IF( KW.GT.0 ) THEN
            IF( IFLAG.NE.0 ) GOTO 15
         ELSE
            IF( IFLAG.EQ.0 ) GOTO 15
         ENDIF
      ENDIF
      I = I+2
      I = I + IBUF(I) + 1
      GOTO 20
*
2000  CONTINUE
*     RECCLASS/NOTRECCLASS
*
*    -1   isel
*     0   ) record
*     1   )    name
*     2     mask
*
      IF( ICASE2.EQ.1 ) THEN
         RKEXIST = .TRUE.
*        note inversed value for flag !
         IF(IBUF(I+0).EQ.INFU(IKEY+4,LL).AND.
     +      IBUF(I+1).EQ.INFU(IKEY+5,LL) ) THEN
*           correct record name found
            IFLAG = IAND(IBUF(I+2),INFU(IKEY+9,LL))
         ELSE
*           accept, if another recname
*           note inversed value for flag !
            IFLAG = 1
         ENDIF
         IF( KW.GT.0 ) THEN
            IF( IFLAG.EQ.0 ) GOTO 15
         ELSE
            IF( IFLAG.NE.0 ) GOTO 15
         ENDIF
      ENDIF
      I = I + 3
      GOTO 20
*
2100  CONTINUE
*     RECBLOCK/NOTRECBLOCK
*
*    -1   isel
*     0   ) record
*     1   )    name
*     2     count, block_names
*
      IF(ICASE2.NE.1) THEN
         BLEXIST = .TRUE.
         IFLAG = 1
         IF(IBUF(I+0).NE.INFU(IKEY+4,LL).OR.
     +      IBUF(I+1).NE.INFU(IKEY+5,LL) ) THEN
*           record name mismatch, so accept the block
            IFLAG = 0
            GOTO 2102
         ENDIF
         DO 2101 J = I+3,I+2+IBUF(I+2),2
            IF(IBUF(J+0).EQ.INFU(IHDR+2,LL).AND.
     +         IBUF(J+1).EQ.INFU(IHDR+3,LL)) THEN
*              block name found
               IFLAG = 0
               GOTO 2102
            END IF
2101     CONTINUE
*        IFLAG = 1
2102     CONTINUE
         IF( KW.GT.0 ) THEN
            IF( IFLAG.NE.0 ) GOTO 15
         ELSE
            IF( IFLAG.EQ.0 ) GOTO 15
         ENDIF
      ENDIF
      I = I+2
      I = I + IBUF(I) + 1
      GOTO 20
*
2200  CONTINUE
*     KEY/NOTKEY
*
*    -1   isel
*     0   ) record
*     1   )    name
*     2     record number A
*     3     count, group of integers record number B
*
      IF(ICASE2.EQ.1) THEN
         RKEXIST = .TRUE.
         IF(IBUF(I+0).EQ.INFU(IKEY+4,LL).AND.
     +      IBUF(I+1).EQ.INFU(IKEY+5,LL) ) THEN
*           correct record name found
            IF( IBUF(I+2).GT.INFU(IKEY+6,LL)) THEN
               IFLAG = 1
            ELSE IF( IBUF(I+2).EQ.INFU(IKEY+6,LL)) THEN
*              correct first number found
               IFLAG = NUMTG3(IBUF,I+3,INFU(IKEY+7,LL))
            ELSE
               IFLAG = 2
            ENDIF
         ELSE
            IFLAG = 1
         ENDIF
         IF( KW.GT.0 ) THEN
            IF( IFLAG.NE.0 ) GOTO 15
         ELSE
            IF( IFLAG.EQ.0 ) GOTO 15
         ENDIF
      ENDIF
      I = I+3
      I = I + IBUF(I) + 1
      GOTO 20
 
*     get index of next bank
   15 CONTINUE
      IF( RKEXIST ) THEN
         IF( .NOT.STOPEXIST ) STOPGLOB = .FALSE.
         IF( .NOT.STOPFLAG )  STOPGLOB = .FALSE.
      ENDIF
      IND=INDXC(IBUF,IND)
      IF(IND.EQ.0) GOTO 90
      IDENT=IBUF(IND-2)
      IF(IDENT.GT.MAXID) GOTO 90
      GOTO 10
*
*     set the skip flag
   90 IF( STOPGLOB ) GOTO 95
      IDEC=1
      GOTO 100
*
*     set the STOP flag
   95 IDEC=2
      CALL FQCSET('STOP',2)
*
  100 RETURN
      END
