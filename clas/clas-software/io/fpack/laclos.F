*CMZ :  1.00/00 16/12/94  17.00.00  by  Pavel Binko
*-- Author :    Volker Blobel   16/12/94
      SUBROUTINE LACLOS
*
************************************************************************
*
*     CLOSE statement
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
      CHARACTER CTEXT*20, DANAME*16
      PARAMETER (LASTCL=1)
      CHARACTER*9 TEXTCL(LASTCL)
      INTEGER NUMBCL(LASTCL),IOPEN(IOPMAX)
      LOGICAL FIRST
      DATA TEXTCL/'UNIT'/
      DATA NUMERIC/1/
      DATA NUMBCL/-1/
*
      CALL TEXT32
      DO 10 I=1,IOPMAX
   10 IOPEN(I)=0
      DANAME=' '
      ERRMES=' '
      FIRST=.TRUE.
      IEPOS =0
      I=2
*
*     check = sign in I+1
   11 NEQ=0
      IF(I+1.LE.NTX) THEN
         IF(ITX(3,I+1).EQ.1.AND.ITX(4,I+1).EQ.6) THEN
            NEQ=1
            FIRST=.FALSE.
            IF(I+2.GT.NTX) THEN
*              error: no information after = sign
               IEPOS=ITX(1,I)
               GOTO 90
            END IF
         END IF
      END IF
      CTEXT=STMT(ITX(1,I):ITX(2,I))
      IF(NEQ.NE.0) THEN
*        for = sign compare title text
         DO 30 J=1,LASTCL
         IF(NUMBCL(J).GT.0) GOTO 30
         IF(TEXTCL(J).EQ.CTEXT) THEN
            IT=-NUMBCL(J)
            IF(IT.LE.NUMERIC) THEN
*              numeric information
               I=I+2
               IF(ITX(3,I).NE.4) THEN
                  IEPOS=ITX(1,I)
                  GOTO 90
               ELSE
                  IOPEN(IT)=ITX(4,I)
                  GOTO 50
               END IF
            ELSE
*              text information expected
               I=I+2
               IF(ITX(3,I).NE.3) THEN
*                 no text
                  IEPOS=ITX(1,I)
                  GOTO 90
               END IF
               CTEXT=STMT(ITX(1,I):ITX(2,I))
               DO 20 K=J+1,LASTCL
               IF(NUMBCL(K).LT.0) THEN
*                 no legal text found
                  IEPOS=ITX(1,I)
                  GOTO 90
               ELSE
                  IF(TEXTCL(K).EQ.CTEXT) THEN
                     IOPEN(IT)=NUMBCL(K)
                     GOTO 50
                  END IF
               END IF
   20          CONTINUE
*              no legal text found
               IEPOS=ITX(1,I)
               GOTO 90
            END IF
         END IF
   30    CONTINUE
      ELSE
*        otherwise compare other text
         DO 40 J=1,LASTCL
         IF(NUMBCL(J).LT.0) THEN
            IT=-NUMBCL(J)
         ELSE
            IF(TEXTCL(J).EQ.CTEXT) THEN
               IOPEN(IT)=NUMBCL(J)
               GOTO 50
            END IF
         END IF
   40    CONTINUE
         IF(FIRST) THEN
            DANAME=STMT(ITX(1,I):ITX(2,I))
            GOTO 50
         END IF
      END IF
      IEPOS=ITX(1,I)
      GOTO 90
*     next item
   50 IF(I.GE.NTX) GOTO 80
      I=I+1
*     check for comma
      IF(ITX(3,I).EQ.0) GOTO 50
      IF(ITX(3,I).EQ.1) THEN
         IF(ITX(4,I).EQ.9) THEN
*           it's a comma
            IF(I+1.LT.NTX) I=I+1
         END IF
      END IF
      FIRST=.FALSE.
      GOTO 11
*
*     finished
   80 CONTINUE
      LUN = IOPEN(1)
      CALL MKCLOS(LUN,DANAME)
*
   90 IF(IEPOS.NE.0) THEN
         IEP=ITX(1,I)
         NB=LENB(ERRMES)
         NT=LEN (ERRMES)
         IF(NB.EQ.0) THEN
            ERRMES='Error at:'
            NB=11
         ELSE IF(NB+11.LE.NT) THEN
            ERRMES(NB+1:NB+11)='; error at:'
            NB=NB+11
         ELSE
            GOTO 100
         END IF
         IF(NB.GE.NT) GOTO 100
         ERRMES(NB+1:NT)=STMT(IEP:MIN0(IEP+NT-NB,LEN(STMT)))
      END IF
*
  100 RETURN
      END
