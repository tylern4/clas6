*CMZ :  1.00/00 16/12/94  17.00.00  by  Pavel Binko
*-- Author :    Volker Blobel   16/12/94
      SUBROUTINE FCHACT(DANAME,ACTION,IER)
*
      CHARACTER*(*) DANAME, ACTION
*
************************************************************************
*
*     Change action parameter of FPACK file
*
*                        ______ ______
*            CALL FCHACT(DANAME,ACTION,IER)
*                                      ---
*     where  DANAME = symbolic file name of F-PACK file
*            ACTION = 'READ' or 'WRITE' or 'READWRITE'
*     returned:
*            IER    = 0 means no error, otherwise error
*
************************************************************************
*
      CHARACTER FINAME*256, HONAME*127
      CHARACTER TEXT*132, COMNT*80, ACT*10, STR*12
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
      INTEGER LUNPAR(ILPMAX)
*
      IER=1
*
*     test action
      ACT=ACTION
      CALL UCASE(ACT)
*
*     file inquire
      CALL FQFILE(DANAME,FINAME,HONAME,LUNPAR,JER)
      IF(JER.NE.0) GOTO 100
      TEXT=' OPEN                 ,UNIT=  ,ACCESS=SEQ    ,FORM=BINARY'//
     +     ',ACTION=READWRITE,RECL=         ,  FILE="'
*
*     insert symbolic file name
      TEXT(7:22)=DANAME
*
*     unit number
      CALL PNVI(LUNPAR(1),STR,JS)
      TEXT(29:30)=STR(1:2)
*
*     define access
      IF(LUNPAR(2).EQ.2) THEN
         TEXT(39:45)='DIR    '
      ELSE IF(LUNPAR(2).EQ.3) THEN
         TEXT(39:45)='KEYED  '
      ELSE IF(LUNPAR(2).EQ.4) THEN
         TEXT(39:45)='SPECIAL'
      ELSE IF(LUNPAR(2).EQ.5) THEN
         TEXT(39:45)='ORDERED'
      END IF
*
*     define format
      IF(LUNPAR(3).EQ.2) THEN
         TEXT(52:57)='TEXT  '
      END IF
*
*     define action
      TEXT(68:76)=ACT
*
*     record length
*     TEMPRARY FIX
      IF(LUNPAR(5).EQ.0) LUNPAR(5)=6232
      CALL PNVI(LUNPAR(5),STR,JS)
      TEXT(83:91)=STR(1:JS)
*
*     file name
      LL=LENB(FINAME)
      TEXT(101:100+LL)=FINAME(1:LL)
      TEXT(101+LL:101+LL)='"'
      KK=LENB(HONAME)
      IF(KK.GT.1) THEN
         TEXT(102+LL:106+LL)='HOST='
         TEXT(107+LL:107+LL+KK)=HONAME(1:KK)
         KL=107+LL+KK
      ELSE
         KL=101+LL
      END IF
      WRITE(6,213) TEXT(1:KL)
  213 FORMAT(1X,A)
*
*     close
      CALL FNCLOS(DANAME)
*
*     remove previous error message, if any
      CALL FERMES(COMNT,1)
      CALL FPRFLS
      CALL FPARM(TEXT(1:KL))
      CALL FPRFLS
*
*     check successful open
      CALL FERMES(COMNT,1)
      IF(COMNT.NE.' ') THEN
         WRITE(*,'(1X,A)') COMNT
         GOTO 100
      END IF
*
      IER=0
*
  100 RETURN
      END
