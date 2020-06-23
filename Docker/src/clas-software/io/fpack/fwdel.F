#ifdef HPUX
*CMZU:  1.00/00 16/12/94  17.00.00  by  Pavel Binko
*-- Author :    Volker Blobel   16/12/94
      SUBROUTINE FWDEL(DANAME,NAMKEY,NUM1,NUM2,IERR)
*
      CHARACTER*(*) DANAME,NAMKEY
*
************************************************************************
*
*     Delete logical record in keyed access file
*
************************************************************************
*
      CHARACTER CHA*4, KEY*20, NMKEY2*8, TITCOP*8
      INTEGER IDUM(12), IARR(2), IKEY(4)
*KEEP,FCOMMON.
*     IEEE format
      PARAMETER (LOCAL=1)
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
*KEEP,ARETOA.
      INTEGER ETOA(0:255)
*KEEP,DTETOA.
      DATA (ETOA(I),I=0,255) /
     +   0, 32, 32, 32, 32, 32, 32, 32, 32,  5, 32, 32, 32, 13, 32, 32,
     +  32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32,
     +  32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32,
     +  32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32,
     +  32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 46, 60, 40, 43,124,
     +  38, 32, 32, 32, 32, 32, 32, 32, 32, 32, 33, 36, 42, 41, 59, 94,
     +  45, 47, 32, 32, 32, 32, 32, 32, 32, 32, 32, 44, 37, 95, 62, 63,
     +  32, 32, 32, 32, 32, 32, 32, 32, 32, 96, 58, 35, 64, 39, 61, 34,
     +  32, 97, 98, 99,100,101,102,103,104,105, 32, 32, 32, 32, 32, 32,
     +  32,106,107,108,109,110,111,112,113,114, 32, 32, 32, 32, 32, 32,
     +  32,126,115,116,117,118,119,120,121,122, 32, 32, 32, 91, 32, 32,
     +  32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 93, 32, 32,
     + 123, 65, 66, 67, 68, 69, 70, 71, 72, 73, 32, 32, 32, 32, 32, 32,
     + 125, 74, 75, 76, 77, 78, 79, 80, 81, 82, 32, 32, 32, 32, 32, 32,
     +  92, 32, 83, 84, 85, 86, 87, 88, 89, 90, 32, 32, 32, 32, 32, 32,
     +  48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 32, 32, 32, 32, 32, 32 /
*KEND.
*
      IERR=1
*     save current W value LUNW, LW, restore later
      LWS  =LW
      LUNWS=LUNW
      CALL FWNAME(DANAME)
*     No delete for undefined unit ...
      IF(LW.EQ.0.OR.LUNW.EQ.0) GOTO 90
*     ... or access not equal keyed access
      IF(INFU(2,LW).EQ.5)THEN
         ISVLR = LR
         ISVLUNR = LUNR
         LR = LW
         LUNR = LUNW
         TITCOP=NAMKEY
         DO 11 I=1,2
           CHA=TITCOP(I*4-3:I*4)
*KEEP,ICHCHA.
*     get hollerith 4H from char*4
      ICH=IOR(ISHFT(ICHAR(CHA(1:1)),24),IOR(ISHFT(ICHAR(CHA(2:2)),16),
     +    IOR(ISHFT(ICHAR(CHA(3:3)), 8),ICHAR(          CHA(4:4)))))
*KEND.
           IARR(I)=ICH
   11    CONTINUE
*        save the old key
         CALL UCOPY(INFU(IRSTU+14,LR),IKEY,4)
         CALL FPKENC(IARR,INFU(IRSTU+14,LR))
         INFU(IRSTU+16,LR) = NUM1
         INFU(IRSTU+17,LR) = NUM2
         CALL FWKUPD(1,IERR)
*        reset the key
         CALL UCOPY(IKEY,INFU(IRSTU+14,LR),4)
*        path for read incorrect
         INFU(ISGEN+9,LR) = 1
         LR = ISVLR
         LUNR = ISVLUNR
         GOTO 90
      ELSE IF(INFU(2,LW).NE.3)THEN
*        not keyed access
         GOTO 90
      ENDIF
      NMKEY2=NAMKEY(1:MIN(8,LEN(NAMKEY)))
*
*     construct key
      NRC=0
   20 CALL TRAKEY('UC',KEY,IDUM,NMKEY2,NUM1,NUM2,NRC)
      IF(INFU(9,LUNIT(LUNW)).EQ.0) THEN
*        local file
*        local keyed access not yet implemented
         GOTO 90
      ELSE
*        remote file
         CALL KREADF(INFU(9,LUNIT(LUNW)),INFU(8,LUNIT(LUNW)),
     +               48,IDUM,12,1,KEY,IRET)
         IF(IRET.EQ.2) GOTO 90
         CALL KDELF(INFU(9,LUNIT(LUNW)),INFU(8,LUNIT(LUNW)),IRET)
      END IF
*     delete is done
      IERR=0
      NRC=NRC+1
      GOTO 20
   90 LW  =LWS
      LUNW=LUNWS
*
  100 RETURN
      END
#elif defined Linux
*CMZU:  1.00/00 16/12/94  17.00.00  by  Pavel Binko
*-- Author :    Volker Blobel   16/12/94
      SUBROUTINE FWDEL(DANAME,NAMKEY,NUM1,NUM2,IERR)
*
      CHARACTER*(*) DANAME,NAMKEY
*
************************************************************************
*
*     Delete logical record in keyed access file
*
************************************************************************
*
      CHARACTER CHA*4, KEY*20, NMKEY2*8, TITCOP*8
      INTEGER IDUM(12), IARR(2), IKEY(4)
*KEEP,FCOMMON.
*     DEC format (little-endian IEEE)
      PARAMETER (LOCAL=4)
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
*KEEP,ARETOA.
      INTEGER ETOA(0:255)
*KEEP,DTETOA.
      DATA (ETOA(I),I=0,255) /
     +   0, 32, 32, 32, 32, 32, 32, 32, 32,  5, 32, 32, 32, 13, 32, 32,
     +  32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32,
     +  32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32,
     +  32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32,
     +  32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 46, 60, 40, 43,124,
     +  38, 32, 32, 32, 32, 32, 32, 32, 32, 32, 33, 36, 42, 41, 59, 94,
     +  45, 47, 32, 32, 32, 32, 32, 32, 32, 32, 32, 44, 37, 95, 62, 63,
     +  32, 32, 32, 32, 32, 32, 32, 32, 32, 96, 58, 35, 64, 39, 61, 34,
     +  32, 97, 98, 99,100,101,102,103,104,105, 32, 32, 32, 32, 32, 32,
     +  32,106,107,108,109,110,111,112,113,114, 32, 32, 32, 32, 32, 32,
     +  32,126,115,116,117,118,119,120,121,122, 32, 32, 32, 91, 32, 32,
     +  32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 93, 32, 32,
     + 123, 65, 66, 67, 68, 69, 70, 71, 72, 73, 32, 32, 32, 32, 32, 32,
     + 125, 74, 75, 76, 77, 78, 79, 80, 81, 82, 32, 32, 32, 32, 32, 32,
     +  92, 32, 83, 84, 85, 86, 87, 88, 89, 90, 32, 32, 32, 32, 32, 32,
     +  48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 32, 32, 32, 32, 32, 32 /
*KEND.
*
      IERR=1
*     save current W value LUNW, LW, restore later
      LWS  =LW
      LUNWS=LUNW
      CALL FWNAME(DANAME)
*     No delete for undefined unit ...
      IF(LW.EQ.0.OR.LUNW.EQ.0) GOTO 90
*     ... or access not equal keyed access
      IF(INFU(2,LW).EQ.5)THEN
         ISVLR = LR
         ISVLUNR = LUNR
         LR = LW
         LUNR = LUNW
         TITCOP=NAMKEY
         DO 11 I=1,2
           CHA=TITCOP(I*4-3:I*4)
*KEEP,ICHCHA.
*     get hollerith 4H from char*4
      ICH=IOR(ISHFT(ICHAR(CHA(4:4)),24),IOR(ISHFT(ICHAR(CHA(3:3)),16),
     +    IOR(ISHFT(ICHAR(CHA(2:2)), 8),ICHAR(          CHA(1:1)))))
*KEND.
           IARR(I)=ICH
   11    CONTINUE
*        save the old key
         CALL UCOPY(INFU(IRSTU+14,LR),IKEY,4)
         CALL FPKENC(IARR,INFU(IRSTU+14,LR))
         INFU(IRSTU+16,LR) = NUM1
         INFU(IRSTU+17,LR) = NUM2
         CALL FWKUPD(1,IERR)
*        reset the key
         CALL UCOPY(IKEY,INFU(IRSTU+14,LR),4)
*        path for read incorrect
         INFU(ISGEN+9,LR) = 1
         LR = ISVLR
         LUNR = ISVLUNR
         GOTO 90
      ELSE IF(INFU(2,LW).NE.3)THEN
*        not keyed access
         GOTO 90
      ENDIF
      NMKEY2=NAMKEY(1:MIN(8,LEN(NAMKEY)))
*
*     construct key
      NRC=0
   20 CALL TRAKEY('UC',KEY,IDUM,NMKEY2,NUM1,NUM2,NRC)
      IF(INFU(9,LUNIT(LUNW)).EQ.0) THEN
*        local file
*        local keyed access impossible
         GOTO 90
      ELSE
*        remote file
         CALL KREADF(INFU(9,LUNIT(LUNW)),INFU(8,LUNIT(LUNW)),
     +               48,IDUM,12,1,KEY,IRET)
         IF(IRET.EQ.2) GOTO 90
         CALL KDELF(INFU(9,LUNIT(LUNW)),INFU(8,LUNIT(LUNW)),IRET)
      END IF
*     delete is done
      IERR=0
      NRC=NRC+1
      GOTO 20
   90 LW  =LWS
      LUNW=LUNWS
*
  100 RETURN
      END
#elif defined SunOS
*CMZU:  1.00/00 16/12/94  17.00.00  by  Pavel Binko
*-- Author :    Volker Blobel   16/12/94
      SUBROUTINE FWDEL(DANAME,NAMKEY,NUM1,NUM2,IERR)
*
      CHARACTER*(*) DANAME,NAMKEY
*
************************************************************************
*
*     Delete logical record in keyed access file
*
************************************************************************
*
      CHARACTER CHA*4, KEY*20, NMKEY2*8, TITCOP*8
      INTEGER IDUM(12), IARR(2), IKEY(4)
*KEEP,FCOMMON.
*     IEEE format
      PARAMETER (LOCAL=1)
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
*KEEP,ARETOA.
      INTEGER ETOA(0:255)
*KEEP,DTETOA.
      DATA (ETOA(I),I=0,255) /
     +   0, 32, 32, 32, 32, 32, 32, 32, 32,  5, 32, 32, 32, 13, 32, 32,
     +  32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32,
     +  32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32,
     +  32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32,
     +  32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 46, 60, 40, 43,124,
     +  38, 32, 32, 32, 32, 32, 32, 32, 32, 32, 33, 36, 42, 41, 59, 94,
     +  45, 47, 32, 32, 32, 32, 32, 32, 32, 32, 32, 44, 37, 95, 62, 63,
     +  32, 32, 32, 32, 32, 32, 32, 32, 32, 96, 58, 35, 64, 39, 61, 34,
     +  32, 97, 98, 99,100,101,102,103,104,105, 32, 32, 32, 32, 32, 32,
     +  32,106,107,108,109,110,111,112,113,114, 32, 32, 32, 32, 32, 32,
     +  32,126,115,116,117,118,119,120,121,122, 32, 32, 32, 91, 32, 32,
     +  32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 93, 32, 32,
     + 123, 65, 66, 67, 68, 69, 70, 71, 72, 73, 32, 32, 32, 32, 32, 32,
     + 125, 74, 75, 76, 77, 78, 79, 80, 81, 82, 32, 32, 32, 32, 32, 32,
     +  92, 32, 83, 84, 85, 86, 87, 88, 89, 90, 32, 32, 32, 32, 32, 32,
     +  48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 32, 32, 32, 32, 32, 32 /
*KEND.
*
      IERR=1
*     save current W value LUNW, LW, restore later
      LWS  =LW
      LUNWS=LUNW
      CALL FWNAME(DANAME)
*     No delete for undefined unit ...
      IF(LW.EQ.0.OR.LUNW.EQ.0) GOTO 90
*     ... or access not equal keyed access
      IF(INFU(2,LW).EQ.5)THEN
         ISVLR = LR
         ISVLUNR = LUNR
         LR = LW
         LUNR = LUNW
         TITCOP=NAMKEY
         DO 11 I=1,2
           CHA=TITCOP(I*4-3:I*4)
*KEEP,ICHCHA.
*     get hollerith 4H from char*4
      ICH=IOR(ISHFT(ICHAR(CHA(1:1)),24),IOR(ISHFT(ICHAR(CHA(2:2)),16),
     +    IOR(ISHFT(ICHAR(CHA(3:3)), 8),ICHAR(          CHA(4:4)))))
*KEND.
           IARR(I)=ICH
   11    CONTINUE
*        save the old key
         CALL UCOPY(INFU(IRSTU+14,LR),IKEY,4)
         CALL FPKENC(IARR,INFU(IRSTU+14,LR))
         INFU(IRSTU+16,LR) = NUM1
         INFU(IRSTU+17,LR) = NUM2
         CALL FWKUPD(1,IERR)
*        reset the key
         CALL UCOPY(IKEY,INFU(IRSTU+14,LR),4)
*        path for read incorrect
         INFU(ISGEN+9,LR) = 1
         LR = ISVLR
         LUNR = ISVLUNR
         GOTO 90
      ELSE IF(INFU(2,LW).NE.3)THEN
*        not keyed access
         GOTO 90
      ENDIF
      NMKEY2=NAMKEY(1:MIN(8,LEN(NAMKEY)))
*
*     construct key
      NRC=0
   20 CALL TRAKEY('UC',KEY,IDUM,NMKEY2,NUM1,NUM2,NRC)
      IF(INFU(9,LUNIT(LUNW)).EQ.0) THEN
*        local file
*        local keyed access impossible
         GOTO 90
      ELSE
*        remote file
         CALL KREADF(INFU(9,LUNIT(LUNW)),INFU(8,LUNIT(LUNW)),
     +               48,IDUM,12,1,KEY,IRET)
         IF(IRET.EQ.2) GOTO 90
         CALL KDELF(INFU(9,LUNIT(LUNW)),INFU(8,LUNIT(LUNW)),IRET)
      END IF
*     delete is done
      IERR=0
      NRC=NRC+1
      GOTO 20
   90 LW  =LWS
      LUNW=LUNWS
*
  100 RETURN
      END
#else
*CMZU:  1.00/00 16/12/94  17.00.00  by  Pavel Binko
*-- Author :    Volker Blobel   16/12/94
      SUBROUTINE FWDEL(DANAME,NAMKEY,NUM1,NUM2,IERR)
*
      CHARACTER*(*) DANAME,NAMKEY
*
************************************************************************
*
*     Delete logical record in keyed access file
*
************************************************************************
*
      CHARACTER CHA*4, KEY*20, NMKEY2*8, TITCOP*8
      INTEGER IDUM(12), IARR(2), IKEY(4)
*KEEP,FCOMMON.
*     IEEE format
      PARAMETER (LOCAL=1)
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
*KEEP,ARETOA.
      INTEGER ETOA(0:255)
*KEEP,DTETOA.
      DATA (ETOA(I),I=0,255) /
     +   0, 32, 32, 32, 32, 32, 32, 32, 32,  5, 32, 32, 32, 13, 32, 32,
     +  32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32,
     +  32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32,
     +  32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32,
     +  32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 46, 60, 40, 43,124,
     +  38, 32, 32, 32, 32, 32, 32, 32, 32, 32, 33, 36, 42, 41, 59, 94,
     +  45, 47, 32, 32, 32, 32, 32, 32, 32, 32, 32, 44, 37, 95, 62, 63,
     +  32, 32, 32, 32, 32, 32, 32, 32, 32, 96, 58, 35, 64, 39, 61, 34,
     +  32, 97, 98, 99,100,101,102,103,104,105, 32, 32, 32, 32, 32, 32,
     +  32,106,107,108,109,110,111,112,113,114, 32, 32, 32, 32, 32, 32,
     +  32,126,115,116,117,118,119,120,121,122, 32, 32, 32, 91, 32, 32,
     +  32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 93, 32, 32,
     + 123, 65, 66, 67, 68, 69, 70, 71, 72, 73, 32, 32, 32, 32, 32, 32,
     + 125, 74, 75, 76, 77, 78, 79, 80, 81, 82, 32, 32, 32, 32, 32, 32,
     +  92, 32, 83, 84, 85, 86, 87, 88, 89, 90, 32, 32, 32, 32, 32, 32,
     +  48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 32, 32, 32, 32, 32, 32 /
*KEND.
*
      IERR=1
*     save current W value LUNW, LW, restore later
      LWS  =LW
      LUNWS=LUNW
      CALL FWNAME(DANAME)
*     No delete for undefined unit ...
      IF(LW.EQ.0.OR.LUNW.EQ.0) GOTO 90
*     ... or access not equal keyed access
      IF(INFU(2,LW).EQ.5)THEN
         ISVLR = LR
         ISVLUNR = LUNR
         LR = LW
         LUNR = LUNW
         TITCOP=NAMKEY
         DO 11 I=1,2
           CHA=TITCOP(I*4-3:I*4)
*KEEP,ICHCHA.
*     get hollerith 4H from char*4
      ICH=IOR(ISHFT(ICHAR(CHA(1:1)),24),IOR(ISHFT(ICHAR(CHA(2:2)),16),
     +    IOR(ISHFT(ICHAR(CHA(3:3)), 8),ICHAR(          CHA(4:4)))))
*KEND.
           IARR(I)=ICH
   11    CONTINUE
*        save the old key
         CALL UCOPY(INFU(IRSTU+14,LR),IKEY,4)
         CALL FPKENC(IARR,INFU(IRSTU+14,LR))
         INFU(IRSTU+16,LR) = NUM1
         INFU(IRSTU+17,LR) = NUM2
         CALL FWKUPD(1,IERR)
*        reset the key
         CALL UCOPY(IKEY,INFU(IRSTU+14,LR),4)
*        path for read incorrect
         INFU(ISGEN+9,LR) = 1
         LR = ISVLR
         LUNR = ISVLUNR
         GOTO 90
      ELSE IF(INFU(2,LW).NE.3)THEN
*        not keyed access
         GOTO 90
      ENDIF
      NMKEY2=NAMKEY(1:MIN(8,LEN(NAMKEY)))
*
*     construct key
      NRC=0
   20 CALL TRAKEY('UC',KEY,IDUM,NMKEY2,NUM1,NUM2,NRC)
      IF(INFU(9,LUNIT(LUNW)).EQ.0) THEN
*        local file
*        local keyed access not yet implemented
         GOTO 90
      ELSE
*        remote file
         CALL KREADF(INFU(9,LUNIT(LUNW)),INFU(8,LUNIT(LUNW)),
     +               48,IDUM,12,1,KEY,IRET)
         IF(IRET.EQ.2) GOTO 90
         CALL KDELF(INFU(9,LUNIT(LUNW)),INFU(8,LUNIT(LUNW)),IRET)
      END IF
*     delete is done
      IERR=0
      NRC=NRC+1
      GOTO 20
   90 LW  =LWS
      LUNW=LUNWS
*
  100 RETURN
      END
#endif
