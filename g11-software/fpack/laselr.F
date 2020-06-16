*CMZ :  1.00/00 16/12/94  17.00.00  by  Pavel Binko
*-- Author :    Volker Blobel   16/12/94
      SUBROUTINE LASELR
*
************************************************************************
*
*     RSELECT statement
*
************************************************************************
*
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
*     indices in ITX
      PARAMETER( JSTA=1,JEND=2,JTYPE=3,JVAL=4 )
*     types in ITX
      PARAMETER( JSIGN=1,JSTRING=2,JWORD=3,JINT=4,JREAL=6 )
      CHARACTER CTEXT*20, DANAME*16
      CHARACTER*8 NAMS(501), KEYRAN
      INTEGER     NUMS(1503), NCLASS(30)
      LOGICAL STLOOP
*
************************************************************************
*
*     Bank number 1000*LUN+20
*
*     collected information
*
*     1     report flag
*     2     afterdate
*     3     beforedate
*     4     mbytes for stop
*     5     seconds for stop
*     6     not used
*     7     not used
*     8     mode, 0=FSELECT, 1=SELECT, 2=RSELECT
*
*     All other RSELECT items are in the banks
*     with nums 1000*LUN+21..199
*     first word of the bank denotes bank type.
*
*     CLASS/NOTCLASS
*     1     1/-1
*     2     RECNAME(1:4)
*     3     RECNAME(5:8)
*     4     mask
*
*     RECORD/NOTREORD
*     1     2/-2
*     2      group of integers
*     ...   .......
*
*     RECNAME/NOTRECNAME
*     1   3/-3
*     2   ) first
*     3   )    name
*     4   ) second
*     5   )    name
*     ...   .......
*
*     RECRANGE/NOTRECRANGE
*     1   4/-4
*     2   ) record(1:4)
*     3   )   name(5:8)
*     4   ) group of integers
*     ...   .......
*
*     BLOCK/NOTBLOCK
*     1   5/-5
*     2   ) record
*     3   )    name
*     3   ) first
*     4   )    block name
*     5   ) second
*     6   )    block name
*     7...
*
*     RECKEY/NOTRECKEY
*     1   6/-6
*     2   ) record
*     3   )    name
*     4     record number A
*     5     group of integers record number B
*     ...   ...
**********************************************************
*
      CALL TEXT32
      CALL STMTNU
C     CALL STMTPR
      IREPRT=0
      IRESET=0
*
      IF(NTX.LT.2)THEN
         ERRMES='Data name missing'
         GOTO 90
      ENDIF
      IF(NTX.EQ.2)THEN
         ERRMES='Selection parameters missing'
         GOTO 90
      ENDIF
      IF(ITX(3,3).EQ.1.AND.ITX(4,3).EQ.6) THEN
*        Equal sign after data name - error
         ERRMES='Data name missing'
         GOTO 90
      ENDIF
      DANAME=STMT(ITX(1,2):ITX(2,2))
*     check DANAME existing?
      LUN=IFRST(DANAME,0)
      IF(LUN.LT.1.OR.LUN.GT.99) THEN
         ERRMES='File not opened'
         GOTO 90
      END IF
      LL=LUNIT(LUN)
      IF(LL.LE.0) THEN
         ERRMES='File not opened'
         GOTO 90
      END IF
*     Maximal allowed bank index
      IBNLIM = 1000*LUN+199
*     Last used bank index
      IBNKUS = 1000*LUN+20
      IND = INDXB(IBUF,IBNKUS)
      IF(IND.EQ.0) GOTO 712
      IF(IBUF(IND+8).NE.2) THEN
*        previous selection was SELECT/FSELECT, clear it
         DO 7141 I=1000*LUN+20,1000*LUN+199
7141     IND=INDXA(IBUF,I,-1)
         GOTO 712
      ENDIF
      DO 711 I=1,1000
         IND = INDXC(IBUF,IND)
         IF(IND.EQ.0) GOTO 712
         IDENT = IBUF(IND-2)
         IF(IDENT/1000.NE.LUN) GOTO 712
         IF(IDENT.GT.IBNKUS)IBNKUS = IDENT
 711  CONTINUE
 712  CONTINUE
*     first loop is check loop, second one store loop
      STLOOP=.FALSE.
*     start/restart analysis
   10 NKEYS =0
      NBNKS =0
      KEYRAN=' '
      ITLEFT=0
      IEP   =0
*     starting with I=3 (I=1,2 is RSELECT daname)
      I=3
*     check = sign in I+1
   11 NEQ=0
      IF(I+1.LE.NTX) THEN
         IF(ITX(3,I+1).EQ.1.AND.ITX(4,I+1).EQ.6) THEN
            NEQ=1
            IF(I+2.GT.NTX) THEN
*              error: no information after = sign
               ERRMES='Nothing after = sign'
               GOTO 90
            END IF
         END IF
      END IF
      CTEXT=STMT(ITX(1,I):ITX(2,I))
      NC=ITX(2,I)-ITX(1,I)+1
*     ------------------------------------------------------------------
*     special case of CLASS = ... and NOTCLASS = ...
*     ------------------------------------------------------------------
      IF((CTEXT(1:5).EQ.'CLASS'.OR.
     +    CTEXT(1:8).EQ.'NOTCLASS').AND.NEQ.EQ.1) THEN
         I=I+2
         IF(ITX(3,I).EQ.2.OR.ITX(3,I).EQ.3) THEN
            KEYRAN=STMT(ITX(1,I):ITX(2,I))
            I=I+1
         ELSE
            GOTO 90
         END IF
         CALL STRNUM(I,NUMS,NNU)
         IF(NNU.EQ.0.OR.NNU.GT.1500) GOTO 90
*        build mask from numbers
         DO 34 J=1,30
  34     NCLASS(J)=0
*        check class range 1:30
*        0 X     means   X        equality
*        1 X     means   X :      lower limit
*        2 Y     means     : Y    upper limit
*        3 X Y   means   X : Y    interval
         K =3
   36    IF(K.GE.NNU) GOTO 31
         IF(NUMS(K+1).EQ.0) THEN
*           check for equality
            IF(NUMS(K+2).LT.1.OR.NUMS(K+2).GT.30) GOTO 90
            NCLASS(NUMS(K+2))=1
         ELSE IF(NUMS(K+1).EQ.1) THEN
*           check for lower limit
            IF(NUMS(K+2).LT.1.OR.NUMS(K+2).GT.30) GOTO 90
            DO 37 L=NUMS(K+2),30
   37       NCLASS(L)=1
         ELSE IF(NUMS(K+1).EQ.2) THEN
*           check for upper limit
            IF(NUMS(K+2).LT.1.OR.NUMS(K+2).GT.30) GOTO 90
            DO 38 L=1,NUMS(K+2)
   38       NCLASS(L)=1
         ELSE
*           interval
            IF(NUMS(K+2).LT.1.OR.NUMS(K+2).GT.30) GOTO 90
            IF(NUMS(K+3).LT.1.OR.NUMS(K+3).GT.30) GOTO 90
            IF(NUMS(K+2).GT.NUMS(K+3)) GOTO 90
            DO 39 L=NUMS(K+2),NUMS(K+3)
   39       NCLASS(L)=1
            K=K+1
         END IF
         K=K+2
         GOTO 36
   31    IF(STLOOP) THEN
*           store mask after packing
            CALL FCLPA(ICLW,NCLASS)
            IBNKUS = IBNKUS+1
            IF(IBNKUS.GT.IBNLIM) GOTO 93
            IND=INDXA(IBUF,IBNKUS,4)
            IF(IND.EQ.0) GOTO 95
            IBUF(IND+2)=0
            IBUF(IND+3)=0
            CALL TXTOHL(KEYRAN,IBUF(IND+2),NBLA)
            IBUF(IND+4)=ICLW
            IF(CTEXT(1:3).EQ.'NOT') THEN
               IBUF(IND+1) = -1
            ELSE
               IBUF(IND+1) = 1
            END IF
         END IF
         GOTO 50
      END IF
*     ------------------------------------------------------------------
*     case of RECORD = ... and NOTRECORD = ...
*     ------------------------------------------------------------------
      IF((CTEXT(1:6).EQ.'RECORD'.OR.CTEXT(1:9).EQ.'NOTRECORD')
     +    .AND.NEQ.EQ.1) THEN
         I=I+2
         CALL STRNUM(I,NUMS,NNU)
         IF(NNU.EQ.0.OR.NNU.GT.1500) GOTO 90
         IF(STLOOP) THEN
*           store in bank
            IBNKUS = IBNKUS+1
            IF(IBNKUS.GT.IBNLIM) GOTO 93
            IND=INDXA(IBUF,IBNKUS,NNU+1)
            IF(IND.EQ.0) GOTO 95
*           content of bank for RECORD/NOTRECORD option:
*            1    3=RECORD; 4=NOTRECORD
*            2... two or three numbers for total range
*            ...  two or three numbers for single range
*            ...  two or three numbers for single range
            IF(CTEXT(1:6).EQ.'RECORD') THEN
               IBUF(IND+1)=2
            ELSE
               IBUF(IND+1)=-2
            END IF
            DO 33 J=1,NNU
   33       IBUF(IND+J+1)=NUMS(J)
         END IF
         GOTO 50
      END IF
*     ------------------------------------------------------------------
*     special case of RECNAME = ... and NOTRECNAME = ...
*     ------------------------------------------------------------------
      IF((CTEXT(1:7).EQ.'RECNAME'.OR.CTEXT(1:10).EQ.'NOTRECNAME')
     +             .AND.NEQ.EQ.1)THEN
         I=I+2
         CALL STRTXT(I,NAMS,NRN)
         IF(NRN.EQ.0.OR.NRN.GT.500) GOTO 90
         IF(STLOOP) THEN
*           store in bank
            IBNKUS = IBNKUS+1
            IF(IBNKUS.GT.IBNLIM) GOTO 93
            IND=INDXA(IBUF,IBNKUS,1+NRN*2)
            IF(IND.EQ.0) GOTO 95
            IBUF(IND+1)=3
            IF(CTEXT(1:10).EQ.'NOTRECNAME') IBUF(IND+1)=-3
            DO 53 J=1,NRN
            CALL TXTOHL(NAMS(J),IBUF(IND+2*J),NBLA)
   53       CONTINUE
         END IF
         GOTO 50
      END IF
*     ------------------------------------------------------------------
*     case of RECRANGE = ... and NOTRECRANGE =
*     ------------------------------------------------------------------
      IF((CTEXT(1:8).EQ.'RECRANGE'.OR.CTEXT(1:11).EQ.'NOTRECRANGE')
     +             .AND.NEQ.EQ.1)THEN
         I=I+2
         IF(ITX(3,I).EQ.2.OR.ITX(3,I).EQ.3) THEN
            KEYRAN=STMT(ITX(1,I):ITX(2,I))
            I=I+1
         ELSE
            GOTO 90
         END IF
         CALL STRNUM(I,NUMS,NNU)
         IF(NNU.EQ.0.OR.NNU.GT.1500) GOTO 90
         IF(STLOOP) THEN
*           store in bank
            IBNKUS = IBNKUS+1
            IF(IBNKUS.GT.IBNLIM) GOTO 93
            IND=INDXA(IBUF,IBNKUS,NNU+3)
            IF(IND.EQ.0) GOTO 95
            IBUF(IND+1)=4
            IF(CTEXT(1:11).EQ.'NOTRECRANGE') IBUF(IND+1)=-4
            IBUF(IND+2)=0
            IBUF(IND+3)=0
            CALL TXTOHL(KEYRAN,IBUF(IND+2),NBLA)
            DO 43 J=1,NNU
   43       IBUF(IND+J+3)=NUMS(J)
         END IF
         GOTO 50
      END IF
*     ------------------------------------------------------------------
*     case of BLOCK = ... and NOTBLOCK = ...
*     ------------------------------------------------------------------
      IF((CTEXT(1:5).EQ.'BLOCK'.OR.CTEXT(1:8).EQ.'NOTBLOCK')
     +             .AND.NEQ.EQ.1)THEN
         I=I+2
         CALL STRTXT(I,NAMS,NRN)
         IF(NRN.EQ.0.OR.NRN.GT.500) GOTO 90
         IF(STLOOP) THEN
*           store in bank
            IBNKUS = IBNKUS+1
            IF(IBNKUS.GT.IBNLIM) GOTO 93
            IND=INDXA(IBUF,IBNKUS,1+NRN*2)
            IF(IND.EQ.0) GOTO 95
            IBUF(IND+1)=5
            IF(CTEXT(1:8).EQ.'NOTBLOCK') IBUF(IND+1)=-5
            DO 63 J=1,NRN
            CALL TXTOHL(NAMS(J),IBUF(IND+2*J),NBLA)
   63       CONTINUE
         END IF
         GOTO 50
      END IF
*     ------------------------------------------------------------------
*     special case of RECKEY = ... and NOTRECKEY = ...
*     ------------------------------------------------------------------
      IF((CTEXT(1:6).EQ.'RECKEY'.OR.CTEXT(1:9).EQ.'NOTRECKEY')
     +             .AND.NEQ.EQ.1)THEN
         I=I+2
*        get record name
         IF(ITX(3,I).EQ.2.OR.ITX(3,I).EQ.3) THEN
            KEYRAN=STMT(ITX(1,I):ITX(2,I))
            I=I+1
         ELSE
            GOTO 90
         END IF
         IF(ITX(3,I).NE.4) GOTO 90
         KEYNUM=ITX(4,I)
         I=I+1
         CALL STRNUM(I,NUMS,NNU)
         IF(NNU.EQ.0.OR.NNU.GT.1500) GOTO 90
         IF(STLOOP) THEN
*           store in bank
            IBNKUS = IBNKUS+1
            IF(IBNKUS.GT.IBNLIM) GOTO 93
            IND=INDXA(IBUF,IBNKUS,NNU+4)
            IF(IND.EQ.0) GOTO 95
            IBUF(IND+1)=6
            IF(CTEXT(1:11).EQ.'NOTRECKEY') IBUF(IND+1)=-6
            IBUF(IND+2)=0
            IBUF(IND+3)=0
            CALL TXTOHL(KEYRAN,IBUF(IND+2),NBLA)
            IBUF(IND+4)=KEYNUM
            DO 42 J=1,NNU
   42       IBUF(IND+J+4)=NUMS(J)
         END IF
         GOTO 50
      END IF
*     ------------------------------------------------------------------
*     case of STOPSEC = ... , STOPMB = ..., BEFOREDATE = ... , AFTER..
*     ------------------------------------------------------------------
      IF(NEQ.NE.0) THEN
*        for = sign compare
         IF(CTEXT(1:10).EQ.'BEFOREDATE'.OR.
     +      CTEXT(1:9).EQ.'AFTERDATE') THEN
*           numeric information
            I=I+2
            IF(ITX(3,I).EQ.4) THEN
               IF(STLOOP) THEN
                  IDATE=ITX(4,I)
                  ITIM =IDATE
                  IF(IABS(IDATE).LT.10000000) THEN
*                    translate noon of the day to UNIX time
                     CALL TIMUYH(3,IDATE,120000,ITIM)
                  END IF
                  IND=INDXB(IBUF,1000*LUN+100)
                  IF(IND.EQ.0) GOTO 95
                  IF(CTEXT(1:10).EQ.'BEFOREDATE') THEN
                     IBUF(IND+3)=ITIM
                  ELSE
                     IBUF(IND+2)=ITIM
                  END IF
               END IF
               GOTO 50
            ELSE
               GOTO 90
            END IF
         END IF
         IF(CTEXT(1:7).EQ.'STOPSEC') THEN
*           numeric information
            I=I+2
            IF(ITX(3,I).EQ.6) THEN
               IF(STLOOP) THEN
                  ITLEFT=RTX(4,I)
                  IF(FLOAT(ITLEFT).LT.RTX(4,I)) ITLEFT=ITLEFT+1
                  IND=INDXB(IBUF,1000*LUN+100)
                  IF(IND.EQ.0) GOTO 95
                  IBUF(IND+5)=ITLEFT
               END IF
               GOTO 50
            ELSE IF(ITX(3,I).EQ.4) THEN
               IF(STLOOP) THEN
                  ITLEFT=ITX(4,I)
                  IND=INDXB(IBUF,1000*LUN+100)
                  IF(IND.EQ.0) GOTO 95
                  IBUF(IND+5)=ITLEFT
               END IF
               GOTO 50
            ELSE
               GOTO 90
            END IF
         END IF
         IF(CTEXT(1:6).EQ.'STOPMB') THEN
*           numeric information
            I=I+2
            IF(ITX(3,I).EQ.6) THEN
               IF(STLOOP) THEN
                  ITMEGB=RTX(4,I)
                  IF(FLOAT(ITMEGB).LT.RTX(4,I)) ITMEGB=ITMEGB+1
                  IND=INDXB(IBUF,1000*LUN+100)
                  IF(IND.EQ.0) GOTO 95
                  IBUF(IND+4)=ITMEGB
               END IF
               GOTO 50
            ELSE IF(ITX(3,I).EQ.4) THEN
               IF(STLOOP) THEN
                  ITMEGB=ITX(4,I)
                  IND=INDXB(IBUF,1000*LUN+100)
                  IF(IND.EQ.0) GOTO 95
                  IBUF(IND+4)=ITMEGB
               END IF
               GOTO 50
            ELSE
               GOTO 90
            END IF
         END IF
      ELSE
*     ------------------------------------------------------------------
*     case of REPORT and RESET
*     ------------------------------------------------------------------
*        otherwise compare other text
         IF(CTEXT(1:6).EQ.'REPORT') THEN
            IREPRT=+1
            GOTO 50
         ELSE IF(CTEXT(1:5).EQ.'RESET') THEN
            IRESET=+1
            GOTO 50
         END IF
      END IF
      GOTO 90
*     next item
   50 CONTINUE
      IF(I.GE.NTX) GOTO 80
      I=I+1
*     check for comma
      IF(ITX(3,I).EQ.0) GOTO 50
      IF(ITX(3,I).EQ.1) THEN
         IF(ITX(4,I).EQ.9) THEN
*           it's a comma
            IF(I+1.LT.NTX) I=I+1
         END IF
      END IF
      GOTO 11
*     finished
   80 CONTINUE
      IF(STLOOP) GOTO 100
*     ------------------------------------------------------------------
*     syntax OK, second loop to store data
*     ------------------------------------------------------------------
      STLOOP=.TRUE.
*     make sure that bank 20  is there
      IND=INDXB(IBUF,1000*LUN+20 )
      IF(IND.EQ.0) THEN
*        create bank for single steering information
         IND=INDXA(IBUF,1000*LUN+20 ,8)
         IF(IND.EQ.0) GOTO 95
         DO 82 J=1,5
   82    IBUF(IND+J)=0
*        set flag "RSELECT"
         IBUF(IND+8) = 2
      END IF
      NORMES='RSELECT parameter stored for '//DANAME
*     reset for keyword RESET
      IF(IREPRT.NE.0) IBUF(IND+1)=1
      IF(IRESET.EQ.0) GOTO 10
      DO 84 J=1,5
   84 IBUF(IND+J)=0
      IF(IREPRT.NE.0) IBUF(IND+1)=1
      DO 714 I=1000*LUN+21,IBNLIM
714   IND=INDXA(IBUF,I,-1)
      GOTO 10
*
*     ------------------------------------------------------------------
*     error - prepare error message in ERRMES
*     ------------------------------------------------------------------
*
   90 IEP=ITX(1,I)
      NB=LENB(ERRMES)
      NT=LEN (ERRMES)
      IF(NB.EQ.0) THEN
         ERRMES='Error at '
         NB=11
      ELSE IF(NB+11.LE.NT) THEN
         ERRMES(NB+1:NB+11)='; error at:'
         NB=NB+11
      ELSE
         GOTO 100
      END IF
      IF(NB.GE.NT) GOTO 100
      ERRMES(NB+1:NT)=STMT(IEP:MIN0(IEP+NT-NB,LEN(STMT)))
      GOTO 100
*
   95 ERRMES='No buffer space to store RSELECT parameters'
      GOTO 100
   93 ERRMES='Too many selections'
      GOTO 100
*
  100 RETURN
      END
