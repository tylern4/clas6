#ifdef HPUX
*CMZ :  1.00/16 24/03/96  17.10.31  by  Wiesiek Dlugosz
*CMZ :  1.00/15 09/03/96  14.08.37  by  Wiesiek Dlugosz
*CMZ :  1.00/14 02/03/96  12.22.20  by  Wiesiek Dlugosz
*CMZ :  1.00/13 20/02/96  17.35.13  by  Wiesiek Dlugosz
*CMZ :  1.00/12 10/02/96  19.47.46  by  Wiesiek Dlugosz
*CMZ :  1.00/09 02/11/95  10.03.39  by  Wiesiek Dlugosz
*CMZ :  1.00/05 06/08/95  17.45.46  by  Wiesiek Dlugosz
*CMZU:  1.00/02 07/02/95  10.43.26  by  Wiesiek Dlugosz
*CMZ :  1.00/00 16/12/94  17.00.00  by  Pavel Binko
*-- Author :    Pavel Binko   16/12/94
      SUBROUTINE FPCUST(CHFILE,HOST,MEDIA,IOPEN,IRC)
*
      CHARACTER*(*) CHFILE,HOST,MEDIA
*
************************************************************************
*
*     Customize IOPEN depending on environment using config tables
*
************************************************************************
*
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
*KEND.
*
      INTEGER IOPEN(IOPMAX)
*
      CHARACTER*8 GTHOST
*
      PARAMETER ( LGROUP=40, NGROUP=18 )
      CHARACTER*8 HOSTAB(LGROUP,NGROUP)
*
      PARAMETER ( NTABLE=6, NACTS=1 )
      CHARACTER*8 TABLE(3,NTABLE), ACTABL(2*NACTS,NTABLE)
      DATA TABLE
*       Local_host   Host_in_catlg    Mediatype
     1/  'HP-CLUST',   'HP-CLUST',     'DISK'
     2,  'HP-CLUST',   'HP-CLUST',     'TAPE'
     3,  'HP-CLUST',   'IBMMVS',       'DISK'
     4,  'HP-CLUST',   'IBMMVS',       'CART'
     5,  'HP-CLUST',   'SGI-H1',       'DISK'
     6,  'HP-CLUST',   'SGI-H1',       'AMPEX'
     +/
*
      DATA ACTABL
*        Action       Actual_Host
     1/  '-',          'LOCAL'
     2,  '?',          'LOCAL'
     3,  '-',          'DSYIBM'
     4,  'STAGE',      'DSYIBM'
     5,  '-',          'DICE2'
     6,  '?',          'DICE1'
     +/
*
      DATA HOSTAB
     1/'SGI-H1',  'DICE1','DICE2','DICE3','DICE4'
     +,           'REC06','Y4ME'
     +,           'CURACAO','RHODOS','BARBADOS','TORTUGA'
     +,           'MADEIRA','JERSEY','REUNION','SANTORIN'
     +,           'JAMAICA','KORSIKA','ARUBA','KORFU'
     +,           'TOBAGO',                                   20*' '
     2,'IBMMVS',  'DSYIBM','MVS',                             37*' '
     3,'RS6003',  'RWS03','FATMEN',                           37*' '
     4,'RS6001',  'RWS01','H1DO1',                            37*' '
     5,'VAXVMS',  'VAXVMS','VXDESY','VXDSYA','VXDSYB','VXDSYC'
     +,           'AXDSYA','AXDSYB','AXDSYD',                 31*' '
     6,'APOLLO',  'H110','H111','H112',                       36*' '
     7,'HP-CLUST','IPS101','IPS102','IPS103','IPS104','IPS105'
     +,           'IPS106','IPS107','IPS108','IPS109','IPS110'
     +,           'IPS111','IPS112','IPS113','IPS114','IPS115'
     +,           'IPS116','IPS117','IPS118','IPS119',        20*' '
     8,'MIPS',    'MIPS',                                     38*' '
     9,'SUN',     'SUN',                                      38*' '
     +,'ALLIANT', 'ALLIANT',                                  38*' '
     1,'MACII',   'MACII',                                    38*' '
     2,'DECS',    'DECS',                                     38*' '
     3,'IBMVM',   'IBMVM',                                    38*' '
     4,'SGI-HEID','DOC','DORA',                               37*' '
     5,'ACS',     'ACS',                                      38*' '
     6,'IBM-LYON','FRCPN11',                                  38*' '
     7,'HP-LYON', 'CCPNAHP','LPNHP3','POLHP2',                36*' '
     8,'UK',      'ATLAS',                                    38*' '
     +/
*
      CALL FQCOND('PRINT',IPFLAG)
*
      IRC = 0
      NACT = 0
*
*     get host name (where the program is running)
      CALL FPGTH(GTHOST,LMY)
      IF(LMY.LE.0) THEN
*        Unable to get host name
         IRC=1
         GOTO 999
      ENDIF
*
      CALL UCASE(GTHOST)
*
*     Find out which groups it belongs
      DO 20 I=1,NGROUP
         DO 10 J=1,LGROUP
   10    IF(GTHOST.EQ.HOSTAB(J,I)) GOTO 30
   20 CONTINUE
*     Not found My host in table HOSTAB
   30 KMY=I
*
      CALL UCASE(HOST)
*
*     find out the same about the host from catalog
      DO 50 I=1,NGROUP
         DO 40 J=1,LGROUP
   40    IF(HOST.EQ.HOSTAB(J,I)) GOTO 60
   50 CONTINUE
*     Not found catalogue host in table HOSTAB
   60 KHOST=I
*
      IF(KMY.GT.NGROUP .OR. KHOST.GT.NGROUP) THEN
         WRITE(*,*) '**************************************************'
         WRITE(*,*) '*                                                *'
         WRITE(*,*) '*   FPCUST. Your machine is not in the list.     *'
         WRITE(*,*) '*     You can use index files only localy.       *'
         WRITE(*,*) '*                                                *'
         WRITE(*,*) '*     Please, contact the FPACK librarian.       *'
         WRITE(*,*) '*                                                *'
         WRITE(*,*) '**************************************************'
         IF(GTHOST.EQ.HOST) THEN
*           default access
            HOST = 'LOCAL'
            IOPEN(24)=0
            IOPEN(15)=0
            NACT = NACT+1
            GOTO 999
         ELSE
            GOTO 75
         ENDIF
      ENDIF
*
      DO 70 IACT=1,NTABLE
         IF(HOSTAB(1,KMY)  .EQ.TABLE(1,IACT) .AND.
     +      HOSTAB(1,KHOST).EQ.TABLE(2,IACT) .AND.
     +      MEDIA.EQ.TABLE(3,IACT) ) THEN
            GOTO 80
         ENDIF
   70 CONTINUE
*
*     Entry not found in table TABLE
   75 IF(IPFLAG.NE.0) THEN
         WRITE(*,*) 'FPCUST. Access to the data not foreseen'
         WRITE(*,*) '        Your program runs on the : ',
     +        GTHOST(:LENB(GTHOST))
         WRITE(*,*) '        Your data resides on the : ',
     +        HOST(:LENB(HOST))
         WRITE(*,*) '               The media type is : ',
     +        MEDIA(:LENB(MEDIA))
      ENDIF
      IRC=1
      GOTO 999

*
***********************************************************************
*
      ENTRY FPCSTE(CHFILE,HOST,MEDIA,IOPEN,IRC)
*
***********************************************************************
*
 80   NACT = NACT+1
*     Set SHIFT flag
      IF(CHFILE(1:1).EQ.'[') THEN
         IF(HOSTAB(1,KMY).EQ.'SGI-H1' .AND. HOSTAB(1,KHOST).EQ.'SGI-H1'
     +         .AND. INDEX(CHFILE,']').NE.0) THEN
            IOPEN(17) = 1

        ELSEIF(HOSTAB(1,KMY).EQ.'RS6001'
     +     .AND.HOSTAB(1,KHOST).EQ. 'SGI-H1'
     +     .AND. INDEX(CHFILE,']').NE.0) THEN
            IOPEN(17) = 1

        ELSEIF(HOSTAB(1,KMY).EQ.'HP-LYON'
     +     .AND.HOSTAB(1,KHOST).EQ. 'HP-LYON'
     +     .AND. INDEX(CHFILE,']').NE.0) THEN
            IOPEN(17) = 1

         ELSEIF(HOSTAB(1,KMY).NE.'SGI-H1'
     +         .OR. ACTABL(2*NACT-1,IACT).EQ.'STAGE'
     +         .OR. ACTABL(2*NACT-1,IACT).EQ.'AMPEX') THEN
*           forget this file has SHIFT "origin"
            INDX = INDEX(CHFILE,']')
            IF( INDX.GT.0 ) THEN
               CHFILE = CHFILE(INDX+1:)
               IOPEN(17) = 0
            ENDIF
         ENDIF
      ENDIF
*
*     The actual HOST name is each even column of ACTABL.
      HOST=ACTABL(2*NACT,IACT)
*
      LX = LUNIT(IOPEN(1))
*
      IF(ACTABL(2*NACT-1,IACT).EQ.'STAGE') THEN
         IOPEN(24)=0
         IF(INFU(42,LX).EQ.4) THEN
            IOPEN(15)=4
         ELSE
            IOPEN(15)=3
         ENDIF
         IF(IPFLAG.GT.0) WRITE(*,*) '... by STAGE from ' ,
     +                               GTHOST(:LENB(GTHOST))
      ELSEIF(ACTABL(2*NACT-1,IACT).EQ.'AMPEX') THEN
         IOPEN(24)=1
         IF(INFU(42,LX).EQ.4) THEN
            IOPEN(15)=4
         ELSE
            IOPEN(15)=3
         ENDIF
         IF(IPFLAG.GT.0) WRITE(*,*) '... by STAGE AMPEX from the DICE1.'
      ELSEIF(ACTABL(2*NACT-1,IACT).EQ.'ACS') THEN
         IOPEN(24)=2
         IF(INFU(42,LX).EQ.4) THEN
            IOPEN(15)=4
         ELSE
            IOPEN(15)=3
         ENDIF
         IF(IPFLAG.GT.0) WRITE(*,*) '... by STAGE ACS from the ACS.'
      ELSEIF(ACTABL(2*NACT-1,IACT).EQ.'DEFER') THEN
         IOPEN(24)=0
         IOPEN(15)=2
         IF(IPFLAG.NE.0) WRITE(*,*) '... by DEFER from the IBM MVS.'
      ELSEIF(ACTABL(2*NACT-1,IACT).EQ.'-') THEN
         IOPEN(24)=0
         IOPEN(15)=0
         IF(IPFLAG.NE.0) THEN
            IF(HOST.EQ.'LOCAL') THEN
               WRITE(*,*) '... locally.'
            ELSE
               WRITE(*,*) '... by FPSERVER'
            ENDIF
         ENDIF
      ELSEIF(ACTABL(2*NACT-1,IACT).EQ.'?') THEN
         WRITE(*,*) 'FPCUST. Access to the data not defined'
         WRITE(*,*) '        Your program runs on the : ',
     +        GTHOST(:LENB(GTHOST))
         WRITE(*,*) '        Your data resides on the : ',
     +        HOST(:LENB(HOST))
         WRITE(*,*) '               The media type is : ',
     +        MEDIA(:LENB(MEDIA))
         IRC=1
         GOTO 999
      ENDIF
      INFU(41,LX) = IOPEN(24)
      INFU(12,LX) = IOPEN(15)
      WRITE(*,*) '----------------------------------------------'
*
  999 RETURN
      END
#elif defined Linux
*CMZ :  1.00/05 06/08/95  17.45.46  by  Wiesiek Dlugosz
*CMZU:  1.00/02 07/02/95  10.43.26  by  Wiesiek Dlugosz
*CMZ :  1.00/00 16/12/94  17.00.00  by  Pavel Binko
*-- Author :    Pavel Binko   16/12/94
      SUBROUTINE FPCUST(CHFILE,HOST,MEDIA,IOPEN,IRC)
*
      CHARACTER*(*) CHFILE,HOST,MEDIA
*
************************************************************************
*
*     Customize IOPEN depending on environment using config tables
*
************************************************************************
*
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
*KEND.
*
      INTEGER IOPEN(IOPMAX)
*
      CHARACTER*8 GTHOST
*
      PARAMETER ( LGROUP=20, NGROUP=15 )
      CHARACTER*8 HOSTAB(LGROUP,NGROUP)
*
      PARAMETER ( NTABLE=1, NACTS=1 )
      CHARACTER*8 TABLE(3,NTABLE), ACTABL(2*NACTS,NTABLE)
      DATA TABLE
*       Local_host   Host_in_catlg    Mediatype
     1/  'DECS',       'DECS',         'DISK'
     +/
*
      DATA ACTABL
*        Action       Actual_Host
     1/  '-',          'LOCAL'
     +/
*
      DATA HOSTAB
     1/'SGI-H1',  'DICE1','DICE2','REC06','Y4ME',             15*' '
     2,'IBMMVS',  'DSYIBM','MVS',                             17*' '
     3,'RS6003',  'RWS03','FATMEN',                           17*' '
     4,'RS6001',  'RWS01',                                    18*' '
     5,'VAXVMS',  'VAXVMS','VXDESY','VXDSYA','VXDSYB','VXDSYC'
     +,           'AXDSYA','AXDSYB','AXDSYD',                 11*' '
     6,'APOLLO',  'H110','H111','H112',                       16*' '
     7,'HP-CLUST','IPS101','IPS102','IPS103','IPS104','IPS105'
     +,           'IPS106','IPS107','IPS108','IPS109','IPS110'
     +,           'IPS111','IPS112','IPS113','IPS114','IPS115'
     +,           'IPS116','IPS117','IPS118','IPS119'
     8,'MIPS',    'MIPS',                                     18*' '
     9,'SUN',     'SUN',                                      18*' '
     +,'ALLIANT', 'ALLIANT',                                  18*' '
     1,'MACII',   'MACII',                                    18*' '
     2,'DECS',    'DECS',                                     18*' '
     3,'IBMVM',   'IBMVM',                                    18*' '
     4,'SGI-HEID','DOC','DORA',                               17*' '
     5,'ACS',     'ACS',                                      18*' '
     +/
*
      CALL FQCOND('PRINT',IPFLAG)
*
      IRC = 0
      NACT = 0
*
*     get host name (where the program is running)
      CALL FPGTH(GTHOST,LMY)
      IF(LMY.LE.0) THEN
*        Unable to get host name
         IRC=1
         GOTO 999
      ENDIF
*
      CALL UCASE(GTHOST)
*
*     Find out which groups it belongs
      DO 20 I=1,NGROUP
         DO 10 J=1,LGROUP
   10    IF(GTHOST.EQ.HOSTAB(J,I)) GOTO 30
   20 CONTINUE
*     Not found My host in table HOSTAB
   30 KMY=I
*
      CALL UCASE(HOST)
*
*     find out the same about the host from catalog
      DO 50 I=1,NGROUP
         DO 40 J=1,LGROUP
   40    IF(HOST.EQ.HOSTAB(J,I)) GOTO 60
   50 CONTINUE
*     Not found catalogue host in table HOSTAB
   60 KHOST=I
*
      IF(KMY.GT.NGROUP .OR. KHOST.GT.NGROUP) THEN
         WRITE(*,*) '**************************************************'
         WRITE(*,*) '*                                                *'
         WRITE(*,*) '*   FPCUST. Your machine is not in the list.     *'
         WRITE(*,*) '*     You can use index files only localy.       *'
         WRITE(*,*) '*                                                *'
         WRITE(*,*) '*     Please, contact the FPACK librarian.       *'
         WRITE(*,*) '*                                                *'
         WRITE(*,*) '**************************************************'
         IF(GTHOST.EQ.HOST) THEN
*           default access
            HOST = 'LOCAL'
            IOPEN(24)=0
            IOPEN(15)=0
            NACT = NACT+1
            GOTO 999
         ELSE
            GOTO 75
         ENDIF
      ENDIF
*
      DO 70 IACT=1,NTABLE
         IF(HOSTAB(1,KMY)  .EQ.TABLE(1,IACT) .AND.
     +      HOSTAB(1,KHOST).EQ.TABLE(2,IACT) .AND.
     +      MEDIA.EQ.TABLE(3,IACT) ) THEN
            GOTO 80
         ENDIF
   70 CONTINUE
*
*     Entry not found in table TABLE
   75 IF(IPFLAG.NE.0) THEN
         WRITE(*,*) 'FPCUST. Access to the data not foreseen'
         WRITE(*,*) '        Your program runs on the : ',
     +        GTHOST(:LENB(GTHOST))
         WRITE(*,*) '        Your data resides on the : ',
     +        HOST(:LENB(HOST))
         WRITE(*,*) '               The media type is : ',
     +        MEDIA(:LENB(MEDIA))
      ENDIF
      IRC=1
      GOTO 999

*
***********************************************************************
*
      ENTRY FPCSTE(CHFILE,HOST,MEDIA,IOPEN,IRC)
*
***********************************************************************
*
 80   NACT = NACT+1
*     Set SHIFT flag
      IF(CHFILE(1:1).EQ.'[') THEN
         IF(HOSTAB(1,KMY).EQ.'SGI-H1' .AND. HOSTAB(1,KHOST).EQ.'SGI-H1'
     +         .AND. INDEX(CHFILE,']').NE.0) THEN
            IOPEN(17) = 1
         ELSEIF(HOSTAB(1,KMY).NE.'SGI-H1'
     +         .OR. ACTABL(2*NACT-1,IACT).EQ.'STAGE'
     +         .OR. ACTABL(2*NACT-1,IACT).EQ.'AMPEX') THEN
*           forget this file has SHIFT "origin"
            INDX = INDEX(CHFILE,']')
            IF( INDX.GT.0 ) THEN
               CHFILE = CHFILE(INDX+1:)
               IOPEN(17) = 0
            ENDIF
         ENDIF
      ENDIF
*
*     The actual HOST name is each even column of ACTABL.
      HOST=ACTABL(2*NACT,IACT)
*
      LX = LUNIT(IOPEN(1))
*
      IF(ACTABL(2*NACT-1,IACT).EQ.'STAGE') THEN
         IOPEN(24)=0
         IF(INFU(42,LX).EQ.4) THEN
            IOPEN(15)=4
         ELSE
            IOPEN(15)=3
         ENDIF
         IF(IPFLAG.GT.0) WRITE(*,*) '... by STAGE from the IBM MVS.'
      ELSEIF(ACTABL(2*NACT-1,IACT).EQ.'AMPEX') THEN
         IOPEN(24)=1
         IF(INFU(42,LX).EQ.4) THEN
            IOPEN(15)=4
         ELSE
            IOPEN(15)=3
         ENDIF
         IF(IPFLAG.GT.0) WRITE(*,*) '... by STAGE AMPEX from the DICE1.'
      ELSEIF(ACTABL(2*NACT-1,IACT).EQ.'ACS') THEN
         IOPEN(24)=2
         IF(INFU(42,LX).EQ.4) THEN
            IOPEN(15)=4
         ELSE
            IOPEN(15)=3
         ENDIF
         IF(IPFLAG.GT.0) WRITE(*,*) '... by STAGE ACS from the ACS.'
      ELSEIF(ACTABL(2*NACT-1,IACT).EQ.'DEFER') THEN
         IOPEN(24)=0
         IOPEN(15)=2
         IF(IPFLAG.NE.0) WRITE(*,*) '... by DEFER from the IBM MVS.'
      ELSEIF(ACTABL(2*NACT-1,IACT).EQ.'-') THEN
         IOPEN(24)=0
         IOPEN(15)=0
         IF(IPFLAG.NE.0) THEN
            IF(HOST.EQ.'LOCAL') THEN
               WRITE(*,*) '... locally.'
            ELSE
               WRITE(*,*) '... by FPSERVER'
            ENDIF
         ENDIF
      ELSEIF(ACTABL(2*NACT-1,IACT).EQ.'?') THEN
         WRITE(*,*) 'FPCUST. Access to the data not defined'
         WRITE(*,*) '        Your program runs on the : ',
     +        GTHOST(:LENB(GTHOST))
         WRITE(*,*) '        Your data resides on the : ',
     +        HOST(:LENB(HOST))
         WRITE(*,*) '               The media type is : ',
     +        MEDIA(:LENB(MEDIA))
         IRC=1
         GOTO 999
      ENDIF
      INFU(41,LX) = IOPEN(24)
      INFU(12,LX) = IOPEN(15)
      WRITE(*,*) '----------------------------------------------'
*
  999 RETURN
      END
#elif defined SunOS
*CMZ :  1.00/16 24/03/96  17.10.31  by  Wiesiek Dlugosz
*CMZ :  1.00/15 09/03/96  14.08.37  by  Wiesiek Dlugosz
*CMZ :  1.00/14 02/03/96  12.22.20  by  Wiesiek Dlugosz
*CMZ :  1.00/13 20/02/96  17.35.13  by  Wiesiek Dlugosz
*CMZ :  1.00/12 10/02/96  19.47.46  by  Wiesiek Dlugosz
*CMZ :  1.00/09 02/11/95  10.03.39  by  Wiesiek Dlugosz
*CMZ :  1.00/05 06/08/95  17.45.46  by  Wiesiek Dlugosz
*CMZU:  1.00/02 07/02/95  10.43.26  by  Wiesiek Dlugosz
*CMZ :  1.00/00 16/12/94  17.00.00  by  Pavel Binko
*-- Author :    Pavel Binko   16/12/94
      SUBROUTINE FPCUST(CHFILE,HOST,MEDIA,IOPEN,IRC)
*
      CHARACTER*(*) CHFILE,HOST,MEDIA
*
************************************************************************
*
*     Customize IOPEN depending on environment using config tables
*
************************************************************************
*
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
*KEND.
*
      INTEGER IOPEN(IOPMAX)
*
      CHARACTER*8 GTHOST
*
      PARAMETER ( LGROUP=40, NGROUP=18 )
      CHARACTER*8 HOSTAB(LGROUP,NGROUP)
*
      PARAMETER ( NTABLE=1, NACTS=1 )
      CHARACTER*8 TABLE(3,NTABLE), ACTABL(2*NACTS,NTABLE)
      DATA TABLE
*       Local_host   Host_in_catlg    Mediatype
     1/  'SUN',        'SUN',          'DISK'
     +/
*
      DATA ACTABL
*        Action       Actual_Host
     1/  '-',          'LOCAL'
     +/
*
      DATA HOSTAB
     1/'SGI-H1',  'DICE1','DICE2','DICE3','DICE4'
     +,           'REC06','Y4ME'
     +,           'CURACAO','RHODOS','BARBADOS','TORTUGA'
     +,           'MADEIRA','JERSEY','REUNION','SANTORIN'
     +,           'JAMAICA','KORSIKA','ARUBA','KORFU'
     +,           'TOBAGO',                                   20*' '
     2,'IBMMVS',  'DSYIBM','MVS',                             37*' '
     3,'RS6003',  'RWS03','FATMEN',                           37*' '
     4,'RS6001',  'RWS01','H1DO1',                            37*' '
     5,'VAXVMS',  'VAXVMS','VXDESY','VXDSYA','VXDSYB','VXDSYC'
     +,           'AXDSYA','AXDSYB','AXDSYD',                 31*' '
     6,'APOLLO',  'H110','H111','H112',                       36*' '
     7,'HP-CLUST','IPS101','IPS102','IPS103','IPS104','IPS105'
     +,           'IPS106','IPS107','IPS108','IPS109','IPS110'
     +,           'IPS111','IPS112','IPS113','IPS114','IPS115'
     +,           'IPS116','IPS117','IPS118','IPS119',        20*' '
     8,'MIPS',    'MIPS',                                     38*' '
     9,'SUN',     'SUN',                                      38*' '
     +,'ALLIANT', 'ALLIANT',                                  38*' '
     1,'MACII',   'MACII',                                    38*' '
     2,'DECS',    'DECS',                                     38*' '
     3,'IBMVM',   'IBMVM',                                    38*' '
     4,'SGI-HEID','DOC','DORA',                               37*' '
     5,'ACS',     'ACS',                                      38*' '
     6,'IBM-LYON','FRCPN11',                                  38*' '
     7,'HP-LYON', 'CCPNAHP','LPNHP3','POLHP2',                36*' '
     8,'UK',      'ATLAS',                                    38*' '
     +/
*
      CALL FQCOND('PRINT',IPFLAG)
*
      IRC = 0
      NACT = 0
*
*     get host name (where the program is running)
      CALL FPGTH(GTHOST,LMY)
      IF(LMY.LE.0) THEN
*        Unable to get host name
         IRC=1
         GOTO 999
      ENDIF
*
      CALL UCASE(GTHOST)
*
*     Find out which groups it belongs
      DO 20 I=1,NGROUP
         DO 10 J=1,LGROUP
   10    IF(GTHOST.EQ.HOSTAB(J,I)) GOTO 30
   20 CONTINUE
*     Not found My host in table HOSTAB
   30 KMY=I
*
      CALL UCASE(HOST)
*
*     find out the same about the host from catalog
      DO 50 I=1,NGROUP
         DO 40 J=1,LGROUP
   40    IF(HOST.EQ.HOSTAB(J,I)) GOTO 60
   50 CONTINUE
*     Not found catalogue host in table HOSTAB
   60 KHOST=I
*
      IF(KMY.GT.NGROUP .OR. KHOST.GT.NGROUP) THEN
         WRITE(*,*) '**************************************************'
         WRITE(*,*) '*                                                *'
         WRITE(*,*) '*   FPCUST. Your machine is not in the list.     *'
         WRITE(*,*) '*     You can use index files only localy.       *'
         WRITE(*,*) '*                                                *'
         WRITE(*,*) '*     Please, contact the FPACK librarian.       *'
         WRITE(*,*) '*                                                *'
         WRITE(*,*) '**************************************************'
         IF(GTHOST.EQ.HOST) THEN
*           default access
            HOST = 'LOCAL'
            IOPEN(24)=0
            IOPEN(15)=0
            NACT = NACT+1
            GOTO 999
         ELSE
            GOTO 75
         ENDIF
      ENDIF
*
      DO 70 IACT=1,NTABLE
         IF(HOSTAB(1,KMY)  .EQ.TABLE(1,IACT) .AND.
     +      HOSTAB(1,KHOST).EQ.TABLE(2,IACT) .AND.
     +      MEDIA.EQ.TABLE(3,IACT) ) THEN
            GOTO 80
         ENDIF
   70 CONTINUE
*
*     Entry not found in table TABLE
   75 IF(IPFLAG.NE.0) THEN
         WRITE(*,*) 'FPCUST. Access to the data not foreseen'
         WRITE(*,*) '        Your program runs on the : ',
     +        GTHOST(:LENB(GTHOST))
         WRITE(*,*) '        Your data resides on the : ',
     +        HOST(:LENB(HOST))
         WRITE(*,*) '               The media type is : ',
     +        MEDIA(:LENB(MEDIA))
      ENDIF
      IRC=1
      GOTO 999
 
*
***********************************************************************
*
      ENTRY FPCSTE(CHFILE,HOST,MEDIA,IOPEN,IRC)
*
***********************************************************************
*
 80   NACT = NACT+1
*     Set SHIFT flag
      IF(CHFILE(1:1).EQ.'[') THEN
         IF(HOSTAB(1,KMY).EQ.'SGI-H1' .AND. HOSTAB(1,KHOST).EQ.'SGI-H1'
     +         .AND. INDEX(CHFILE,']').NE.0) THEN
            IOPEN(17) = 1
 
        ELSEIF(HOSTAB(1,KMY).EQ.'RS6001'
     +     .AND.HOSTAB(1,KHOST).EQ. 'SGI-H1'
     +     .AND. INDEX(CHFILE,']').NE.0) THEN
            IOPEN(17) = 1
 
        ELSEIF(HOSTAB(1,KMY).EQ.'HP-LYON'
     +     .AND.HOSTAB(1,KHOST).EQ. 'HP-LYON'
     +     .AND. INDEX(CHFILE,']').NE.0) THEN
            IOPEN(17) = 1
 
         ELSEIF(HOSTAB(1,KMY).NE.'SGI-H1'
     +         .OR. ACTABL(2*NACT-1,IACT).EQ.'STAGE'
     +         .OR. ACTABL(2*NACT-1,IACT).EQ.'AMPEX') THEN
*           forget this file has SHIFT "origin"
            INDX = INDEX(CHFILE,']')
            IF( INDX.GT.0 ) THEN
               CHFILE = CHFILE(INDX+1:)
               IOPEN(17) = 0
            ENDIF
         ENDIF
      ENDIF
*
*     The actual HOST name is each even column of ACTABL.
      HOST=ACTABL(2*NACT,IACT)
*
      LX = LUNIT(IOPEN(1))
*
      IF(ACTABL(2*NACT-1,IACT).EQ.'STAGE') THEN
         IOPEN(24)=0
         IF(INFU(42,LX).EQ.4) THEN
            IOPEN(15)=4
         ELSE
            IOPEN(15)=3
         ENDIF
         IF(IPFLAG.GT.0) WRITE(*,*) '... by STAGE from ' ,
     +                               GTHOST(:LENB(GTHOST))
      ELSEIF(ACTABL(2*NACT-1,IACT).EQ.'AMPEX') THEN
         IOPEN(24)=1
         IF(INFU(42,LX).EQ.4) THEN
            IOPEN(15)=4
         ELSE
            IOPEN(15)=3
         ENDIF
         IF(IPFLAG.GT.0) WRITE(*,*) '... by STAGE AMPEX from the DICE1.'
      ELSEIF(ACTABL(2*NACT-1,IACT).EQ.'ACS') THEN
         IOPEN(24)=2
         IF(INFU(42,LX).EQ.4) THEN
            IOPEN(15)=4
         ELSE
            IOPEN(15)=3
         ENDIF
         IF(IPFLAG.GT.0) WRITE(*,*) '... by STAGE ACS from the ACS.'
      ELSEIF(ACTABL(2*NACT-1,IACT).EQ.'DEFER') THEN
         IOPEN(24)=0
         IOPEN(15)=2
         IF(IPFLAG.NE.0) WRITE(*,*) '... by DEFER from the IBM MVS.'
      ELSEIF(ACTABL(2*NACT-1,IACT).EQ.'-') THEN
         IOPEN(24)=0
         IOPEN(15)=0
         IF(IPFLAG.NE.0) THEN
            IF(HOST.EQ.'LOCAL') THEN
               WRITE(*,*) '... locally.'
            ELSE
               WRITE(*,*) '... by FPSERVER'
            ENDIF
         ENDIF
      ELSEIF(ACTABL(2*NACT-1,IACT).EQ.'?') THEN
         WRITE(*,*) 'FPCUST. Access to the data not defined'
         WRITE(*,*) '        Your program runs on the : ',
     +        GTHOST(:LENB(GTHOST))
         WRITE(*,*) '        Your data resides on the : ',
     +        HOST(:LENB(HOST))
         WRITE(*,*) '               The media type is : ',
     +        MEDIA(:LENB(MEDIA))
         IRC=1
         GOTO 999
      ENDIF
      INFU(41,LX) = IOPEN(24)
      INFU(12,LX) = IOPEN(15)
      WRITE(*,*) '----------------------------------------------'
*
  999 RETURN
      END
#elif defined(IRIX)
*CMZ :  1.00/16 24/03/96  17.10.31  by  Wiesiek Dlugosz
*CMZ :  1.00/15 09/03/96  14.08.37  by  Wiesiek Dlugosz
*CMZ :  1.00/14 02/03/96  12.22.20  by  Wiesiek Dlugosz
*CMZ :  1.00/13 20/02/96  17.35.13  by  Wiesiek Dlugosz
*CMZ :  1.00/12 10/02/96  19.47.46  by  Wiesiek Dlugosz
*CMZ :  1.00/09 02/11/95  10.03.39  by  Wiesiek Dlugosz
*CMZ :  1.00/05 06/08/95  17.45.46  by  Wiesiek Dlugosz
*CMZU:  1.00/02 07/02/95  10.43.26  by  Wiesiek Dlugosz
*CMZ :  1.00/00 16/12/94  17.00.00  by  Pavel Binko
*-- Author :    Pavel Binko   16/12/94
      SUBROUTINE FPCUST(CHFILE,HOST,MEDIA,IOPEN,IRC)
*
      CHARACTER*(*) CHFILE,HOST,MEDIA
*
************************************************************************
*
*     Customize IOPEN depending on environment using config tables
*
************************************************************************
*
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
*KEND.
*
      INTEGER IOPEN(IOPMAX)
*
      CHARACTER*8 GTHOST
*
      PARAMETER ( LGROUP=40, NGROUP=18 )
      CHARACTER*8 HOSTAB(LGROUP,NGROUP)
*
      PARAMETER ( NTABLE=7, NACTS=1)
      CHARACTER*8 TABLE(3,NTABLE), ACTABL(2*NACTS,NTABLE)
      DATA TABLE
*       Local_host   Host_in_catlg    Mediatype
     1/  'SGI-H1',     'SGI-H1',       'DISK'
     2,  'SGI-H1',     'SGI-H1',       'AMPEX'
     3,  'SGI-H1',     'IBMMVS',       'DISK'
     4,  'SGI-H1',     'IBMMVS',       'CART'
     5,  'SGI-H1',     'ACS',          'CART'
     6,  'SGI-HEID',   'SGI-HEID',     'DISK'
     7,  'SGI-HEID',   'SGI-H1',       'DISK'
     +/
*
      DATA ACTABL
*        Action       Actual_Host
     1/  '-',          'LOCAL'
     2,  'AMPEX',      'LOCAL'
     3,  'STAGE',      'LOCAL'
     4,  'STAGE',      'LOCAL'
     5,  'ACS',        'LOCAL'
     6,  '-',          'LOCAL'
     7,  '-',          'DICE2'
     +/
*
      DATA HOSTAB
     1/'SGI-H1',  'DICE1','DICE2','DICE3','DICE4'
     +,           'REC06','Y4ME'
     +,           'CURACAO','RHODOS','BARBADOS','TORTUGA'
     +,           'MADEIRA','JERSEY','REUNION','SANTORIN'
     +,           'JAMAICA','KORSIKA','ARUBA','KORFU'
     +,           'TOBAGO',                                   20*' '
     2,'IBMMVS',  'DSYIBM','MVS',                             37*' '
     3,'RS6003',  'RWS03','FATMEN',                           37*' '
     4,'RS6001',  'RWS01','H1DO1',                            37*' '
     5,'VAXVMS',  'VAXVMS','VXDESY','VXDSYA','VXDSYB','VXDSYC'
     +,           'AXDSYA','AXDSYB','AXDSYD',                 31*' '
     6,'APOLLO',  'H110','H111','H112',                       36*' '
     7,'HP-CLUST','IPS101','IPS102','IPS103','IPS104','IPS105'
     +,           'IPS106','IPS107','IPS108','IPS109','IPS110'
     +,           'IPS111','IPS112','IPS113','IPS114','IPS115'
     +,           'IPS116','IPS117','IPS118','IPS119',        20*' '
     8,'MIPS',    'MIPS',                                     38*' '
     9,'SUN',     'SUN',                                      38*' '
     +,'ALLIANT', 'ALLIANT',                                  38*' '
     1,'MACII',   'MACII',                                    38*' '
     2,'DECS',    'DECS',                                     38*' '
     3,'IBMVM',   'IBMVM',                                    38*' '
     4,'SGI-HEID','DOC','DORA',                               37*' '
     5,'ACS',     'ACS',                                      38*' '
     6,'IBM-LYON','FRCPN11',                                  38*' '
     7,'HP-LYON', 'CCPNAHP','LPNHP3','POLHP2',                36*' '
     8,'UK',      'ATLAS',                                    38*' '
     +/
*
      CALL FQCOND('PRINT',IPFLAG)
*
      IRC = 0
      NACT = 0
*
*     get host name (where the program is running)
      CALL FPGTH(GTHOST,LMY)
      IF(LMY.LE.0) THEN
*        Unable to get host name
         IRC=1
         GOTO 999
      ENDIF
*
      CALL UCASE(GTHOST)
*
*     Find out which groups it belongs
      DO 20 I=1,NGROUP
         DO 10 J=1,LGROUP
   10    IF(GTHOST.EQ.HOSTAB(J,I)) GOTO 30
   20 CONTINUE
*     Not found My host in table HOSTAB
   30 KMY=I
*
      CALL UCASE(HOST)
*
*     find out the same about the host from catalog
      DO 50 I=1,NGROUP
         DO 40 J=1,LGROUP
   40    IF(HOST.EQ.HOSTAB(J,I)) GOTO 60
   50 CONTINUE
*     Not found catalogue host in table HOSTAB
   60 KHOST=I
*
      IF(KMY.GT.NGROUP .OR. KHOST.GT.NGROUP) THEN
         WRITE(*,*) '**************************************************'
         WRITE(*,*) '*                                                *'
         WRITE(*,*) '*   FPCUST. Your machine is not in the list.     *'
         WRITE(*,*) '*     You can use index files only localy.       *'
         WRITE(*,*) '*                                                *'
         WRITE(*,*) '*     Please, contact the FPACK librarian.       *'
         WRITE(*,*) '*                                                *'
         WRITE(*,*) '**************************************************'
         IF(GTHOST.EQ.HOST) THEN
*           default access
            HOST = 'LOCAL'
            IOPEN(24)=0
            IOPEN(15)=0
            NACT = NACT+1
            GOTO 999
         ELSE
            GOTO 75
         ENDIF
      ENDIF
*
      DO 70 IACT=1,NTABLE
         IF(HOSTAB(1,KMY)  .EQ.TABLE(1,IACT) .AND.
     +      HOSTAB(1,KHOST).EQ.TABLE(2,IACT) .AND.
     +      MEDIA.EQ.TABLE(3,IACT) ) THEN
            GOTO 80
         ENDIF
   70 CONTINUE
*
*     Entry not found in table TABLE
   75 IF(IPFLAG.NE.0) THEN
         WRITE(*,*) 'FPCUST. Access to the data not foreseen'
         WRITE(*,*) '        Your program runs on the : ',
     +        GTHOST(:LENB(GTHOST))
         WRITE(*,*) '        Your data resides on the : ',
     +        HOST(:LENB(HOST))
         WRITE(*,*) '               The media type is : ',
     +        MEDIA(:LENB(MEDIA))
      ENDIF
      IRC=1
      GOTO 999

*
***********************************************************************
*
      ENTRY FPCSTE(CHFILE,HOST,MEDIA,IOPEN,IRC)
*
***********************************************************************
*
 80   NACT = NACT+1
*     Set SHIFT flag
      IF(CHFILE(1:1).EQ.'[') THEN
         IF(HOSTAB(1,KMY).EQ.'SGI-H1' .AND. HOSTAB(1,KHOST).EQ.'SGI-H1'
     +         .AND. INDEX(CHFILE,']').NE.0) THEN
            IOPEN(17) = 1

        ELSEIF(HOSTAB(1,KMY).EQ.'RS6001'
     +     .AND.HOSTAB(1,KHOST).EQ. 'SGI-H1'
     +     .AND. INDEX(CHFILE,']').NE.0) THEN
            IOPEN(17) = 1

        ELSEIF(HOSTAB(1,KMY).EQ.'HP-LYON'
     +     .AND.HOSTAB(1,KHOST).EQ. 'HP-LYON'
     +     .AND. INDEX(CHFILE,']').NE.0) THEN
            IOPEN(17) = 1

         ELSEIF(HOSTAB(1,KMY).NE.'SGI-H1'
     +         .OR. ACTABL(2*NACT-1,IACT).EQ.'STAGE'
     +         .OR. ACTABL(2*NACT-1,IACT).EQ.'AMPEX') THEN
*           forget this file has SHIFT "origin"
            INDX = INDEX(CHFILE,']')
            IF( INDX.GT.0 ) THEN
               CHFILE = CHFILE(INDX+1:)
               IOPEN(17) = 0
            ENDIF
         ENDIF
      ENDIF
*
*     The actual HOST name is each even column of ACTABL.
      HOST=ACTABL(2*NACT,IACT)
*
      LX = LUNIT(IOPEN(1))
*
      IF(ACTABL(2*NACT-1,IACT).EQ.'STAGE') THEN
         IOPEN(24)=0
         IF(INFU(42,LX).EQ.4) THEN
            IOPEN(15)=4
         ELSE
            IOPEN(15)=3
         ENDIF
         IF(IPFLAG.GT.0) WRITE(*,*) '... by STAGE from ' ,
     +                               GTHOST(:LENB(GTHOST))
      ELSEIF(ACTABL(2*NACT-1,IACT).EQ.'AMPEX') THEN
         IOPEN(24)=1
         IF(INFU(42,LX).EQ.4) THEN
            IOPEN(15)=4
         ELSE
            IOPEN(15)=3
         ENDIF
         IF(IPFLAG.GT.0) WRITE(*,*) '... by STAGE AMPEX from the DICE1.'
      ELSEIF(ACTABL(2*NACT-1,IACT).EQ.'ACS') THEN
         IOPEN(24)=2
         IF(INFU(42,LX).EQ.4) THEN
            IOPEN(15)=4
         ELSE
            IOPEN(15)=3
         ENDIF
         IF(IPFLAG.GT.0) WRITE(*,*) '... by STAGE ACS from the ACS.'
      ELSEIF(ACTABL(2*NACT-1,IACT).EQ.'DEFER') THEN
         IOPEN(24)=0
         IOPEN(15)=2
         IF(IPFLAG.NE.0) WRITE(*,*) '... by DEFER from the IBM MVS.'
      ELSEIF(ACTABL(2*NACT-1,IACT).EQ.'-') THEN
         IOPEN(24)=0
         IOPEN(15)=0
         IF(IPFLAG.NE.0) THEN
            IF(HOST.EQ.'LOCAL') THEN
               WRITE(*,*) '... locally.'
            ELSE
               WRITE(*,*) '... by FPSERVER'
            ENDIF
         ENDIF
      ELSEIF(ACTABL(2*NACT-1,IACT).EQ.'?') THEN
         WRITE(*,*) 'FPCUST. Access to the data not defined'
         WRITE(*,*) '        Your program runs on the : ',
     +        GTHOST(:LENB(GTHOST))
         WRITE(*,*) '        Your data resides on the : ',
     +        HOST(:LENB(HOST))
         WRITE(*,*) '               The media type is : ',
     +        MEDIA(:LENB(MEDIA))
         IRC=1
         GOTO 999
      ENDIF
      INFU(41,LX) = IOPEN(24)
      INFU(12,LX) = IOPEN(15)
      WRITE(*,*) '----------------------------------------------'
*
  999 RETURN
      END

#else
*CMZ :  1.00/16 24/03/96  17.10.31  by  Wiesiek Dlugosz
*CMZ :  1.00/15 09/03/96  14.08.37  by  Wiesiek Dlugosz
*CMZ :  1.00/14 02/03/96  12.22.20  by  Wiesiek Dlugosz
*CMZ :  1.00/13 20/02/96  17.35.13  by  Wiesiek Dlugosz
*CMZ :  1.00/12 10/02/96  19.47.46  by  Wiesiek Dlugosz
*CMZ :  1.00/09 02/11/95  10.03.39  by  Wiesiek Dlugosz
*CMZ :  1.00/05 06/08/95  17.45.46  by  Wiesiek Dlugosz
*CMZU:  1.00/02 07/02/95  10.43.26  by  Wiesiek Dlugosz
*CMZ :  1.00/00 16/12/94  17.00.00  by  Pavel Binko
*-- Author :    Pavel Binko   16/12/94
      SUBROUTINE FPCUST(CHFILE,HOST,MEDIA,IOPEN,IRC)
*
      CHARACTER*(*) CHFILE,HOST,MEDIA
*
************************************************************************
*
*     Customize IOPEN depending on environment using config tables
*
************************************************************************
*
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
*KEND.
*
      INTEGER IOPEN(IOPMAX)
*
      CHARACTER*8 GTHOST
*
      PARAMETER ( LGROUP=40, NGROUP=18 )
      CHARACTER*8 HOSTAB(LGROUP,NGROUP)
*
      PARAMETER ( NTABLE=13, NACTS=1 )
      CHARACTER*8 TABLE(3,NTABLE), ACTABL(2*NACTS,NTABLE)
      DATA TABLE
*       Local_host   Host_in_catlg    Mediatype
     1/  'RS6003',     'RS6003',       'DISK'
     2,  'RS6003',     'RS6003',       'TAPE'
     3,  'RS6003',     'IBMMVS',       'DISK'
     4,  'RS6003',     'IBMMVS',       'CART'
     5,  'RS6003',     'SGI-H1',       'DISK'
     6,  'RS6003',     'SGI-H1',       'AMPEX'
*
     7,  'RS6001',     'RS6001',       'DISK'
     8,  'RS6001',     'RS6001',       'TAPE'
     9,  'RS6001',     'IBMMVS',       'DISK'
     +,  'RS6001',     'IBMMVS',       'CART'
     1,  'RS6001',     'SGI-H1',       'DISK'
     2,  'RS6001',     'SGI-H1',       'AMPEX'
     3,  'RS6001',     'ACS',          'CART'
     +/
*
      DATA ACTABL
*        Action       Actual_Host
     1/  '-',          'LOCAL'
     2,  '?',          'LOCAL'
     3,  '-',          'DSYIBM'
     4,  'STAGE',      'DSYIBM'
     5,  '-',          'DICE2'
     6,  '?',          'DICE1'
*
     7,  '-',          'LOCAL'
     8,  '?',          'LOCAL'
     9,  '-',          'DSYIBM'
     +,  'STAGE',      'DSYIBM'
     1,  '-',          'LOCAL'
     2,  'AMPEX',      'LOCAL'
     3,  'ACS',        'LOCAL'
     +/
*
      DATA HOSTAB
     1/'SGI-H1',  'DICE1','DICE2','DICE3','DICE4'
     +,           'REC06','Y4ME'
     +,           'CURACAO','RHODOS','BARBADOS','TORTUGA'
     +,           'MADEIRA','JERSEY','REUNION','SANTORIN'
     +,           'JAMAICA','KORSIKA','ARUBA','KORFU'
     +,           'TOBAGO',                                   20*' '
     2,'IBMMVS',  'DSYIBM','MVS',                             37*' '
     3,'RS6003',  'RWS03','FATMEN',                           37*' '
     4,'RS6001',  'RWS01','H1DO1',                            37*' '
     5,'VAXVMS',  'VAXVMS','VXDESY','VXDSYA','VXDSYB','VXDSYC'
     +,           'AXDSYA','AXDSYB','AXDSYD',                 31*' '
     6,'APOLLO',  'H110','H111','H112',                       36*' '
     7,'HP-CLUST','IPS101','IPS102','IPS103','IPS104','IPS105'
     +,           'IPS106','IPS107','IPS108','IPS109','IPS110'
     +,           'IPS111','IPS112','IPS113','IPS114','IPS115'
     +,           'IPS116','IPS117','IPS118','IPS119',        20*' '
     8,'MIPS',    'MIPS',                                     38*' '
     9,'SUN',     'SUN',                                      38*' '
     +,'ALLIANT', 'ALLIANT',                                  38*' '
     1,'MACII',   'MACII',                                    38*' '
     2,'DECS',    'DECS',                                     38*' '
     3,'IBMVM',   'IBMVM',                                    38*' '
     4,'SGI-HEID','DOC','DORA',                               37*' '
     5,'ACS',     'ACS',                                      38*' '
     6,'IBM-LYON','FRCPN11',                                  38*' '
     7,'HP-LYON', 'CCPNAHP','LPNHP3','POLHP2',                36*' '
     8,'UK',      'ATLAS',                                    38*' '
     +/
*
      CALL FQCOND('PRINT',IPFLAG)
*
      IRC = 0
      NACT = 0
*
*     get host name (where the program is running)
      CALL FPGTH(GTHOST,LMY)
      IF(LMY.LE.0) THEN
*        Unable to get host name
         IRC=1
         GOTO 999
      ENDIF
*
      CALL UCASE(GTHOST)
*
*     Find out which groups it belongs
      DO 20 I=1,NGROUP
         DO 10 J=1,LGROUP
   10    IF(GTHOST.EQ.HOSTAB(J,I)) GOTO 30
   20 CONTINUE
*     Not found My host in table HOSTAB
   30 KMY=I
*
      CALL UCASE(HOST)
*
*     find out the same about the host from catalog
      DO 50 I=1,NGROUP
         DO 40 J=1,LGROUP
   40    IF(HOST.EQ.HOSTAB(J,I)) GOTO 60
   50 CONTINUE
*     Not found catalogue host in table HOSTAB
   60 KHOST=I
*
      IF(KMY.GT.NGROUP .OR. KHOST.GT.NGROUP) THEN
         WRITE(*,*) '**************************************************'
         WRITE(*,*) '*                                                *'
         WRITE(*,*) '*   FPCUST. Your machine is not in the list.     *'
         WRITE(*,*) '*     You can use index files only localy.       *'
         WRITE(*,*) '*                                                *'
         WRITE(*,*) '*     Please, contact the FPACK librarian.       *'
         WRITE(*,*) '*                                                *'
         WRITE(*,*) '**************************************************'
         IF(GTHOST.EQ.HOST) THEN
*           default access
            HOST = 'LOCAL'
            IOPEN(24)=0
            IOPEN(15)=0
            NACT = NACT+1
            GOTO 999
         ELSE
            GOTO 75
         ENDIF
      ENDIF
*
      DO 70 IACT=1,NTABLE
         IF(HOSTAB(1,KMY)  .EQ.TABLE(1,IACT) .AND.
     +      HOSTAB(1,KHOST).EQ.TABLE(2,IACT) .AND.
     +      MEDIA.EQ.TABLE(3,IACT) ) THEN
            GOTO 80
         ENDIF
   70 CONTINUE
*
*     Entry not found in table TABLE
   75 IF(IPFLAG.NE.0) THEN
         WRITE(*,*) 'FPCUST. Access to the data not foreseen'
         WRITE(*,*) '        Your program runs on the : ',
     +        GTHOST(:LENB(GTHOST))
         WRITE(*,*) '        Your data resides on the : ',
     +        HOST(:LENB(HOST))
         WRITE(*,*) '               The media type is : ',
     +        MEDIA(:LENB(MEDIA))
      ENDIF
      IRC=1
      GOTO 999
 
*
***********************************************************************
*
      ENTRY FPCSTE(CHFILE,HOST,MEDIA,IOPEN,IRC)
*
***********************************************************************
*
 80   NACT = NACT+1
*     Set SHIFT flag
      IF(CHFILE(1:1).EQ.'[') THEN
         IF(HOSTAB(1,KMY).EQ.'SGI-H1' .AND. HOSTAB(1,KHOST).EQ.'SGI-H1'
     +         .AND. INDEX(CHFILE,']').NE.0) THEN
            IOPEN(17) = 1
 
        ELSEIF(HOSTAB(1,KMY).EQ.'RS6001'
     +     .AND.HOSTAB(1,KHOST).EQ. 'SGI-H1'
     +     .AND. INDEX(CHFILE,']').NE.0) THEN
            IOPEN(17) = 1
 
        ELSEIF(HOSTAB(1,KMY).EQ.'HP-LYON'
     +     .AND.HOSTAB(1,KHOST).EQ. 'HP-LYON'
     +     .AND. INDEX(CHFILE,']').NE.0) THEN
            IOPEN(17) = 1
 
         ELSEIF(HOSTAB(1,KMY).NE.'SGI-H1'
     +         .OR. ACTABL(2*NACT-1,IACT).EQ.'STAGE'
     +         .OR. ACTABL(2*NACT-1,IACT).EQ.'AMPEX') THEN
*           forget this file has SHIFT "origin"
            INDX = INDEX(CHFILE,']')
            IF( INDX.GT.0 ) THEN
               CHFILE = CHFILE(INDX+1:)
               IOPEN(17) = 0
            ENDIF
         ENDIF
      ENDIF
*
*     The actual HOST name is each even column of ACTABL.
      HOST=ACTABL(2*NACT,IACT)
*
      LX = LUNIT(IOPEN(1))
*
      IF(ACTABL(2*NACT-1,IACT).EQ.'STAGE') THEN
         IOPEN(24)=0
         IF(INFU(42,LX).EQ.4) THEN
            IOPEN(15)=4
         ELSE
            IOPEN(15)=3
         ENDIF
         IF(IPFLAG.GT.0) WRITE(*,*) '... by STAGE from ' ,
     +                               GTHOST(:LENB(GTHOST))
      ELSEIF(ACTABL(2*NACT-1,IACT).EQ.'AMPEX') THEN
         IOPEN(24)=1
         IF(INFU(42,LX).EQ.4) THEN
            IOPEN(15)=4
         ELSE
            IOPEN(15)=3
         ENDIF
         IF(IPFLAG.GT.0) WRITE(*,*) '... by STAGE AMPEX from the DICE1.'
      ELSEIF(ACTABL(2*NACT-1,IACT).EQ.'ACS') THEN
         IOPEN(24)=2
         IF(INFU(42,LX).EQ.4) THEN
            IOPEN(15)=4
         ELSE
            IOPEN(15)=3
         ENDIF
         IF(IPFLAG.GT.0) WRITE(*,*) '... by STAGE ACS from the ACS.'
      ELSEIF(ACTABL(2*NACT-1,IACT).EQ.'DEFER') THEN
         IOPEN(24)=0
         IOPEN(15)=2
         IF(IPFLAG.NE.0) WRITE(*,*) '... by DEFER from the IBM MVS.'
      ELSEIF(ACTABL(2*NACT-1,IACT).EQ.'-') THEN
         IOPEN(24)=0
         IOPEN(15)=0
         IF(IPFLAG.NE.0) THEN
            IF(HOST.EQ.'LOCAL') THEN
               WRITE(*,*) '... locally.'
            ELSE
               WRITE(*,*) '... by FPSERVER'
            ENDIF
         ENDIF
      ELSEIF(ACTABL(2*NACT-1,IACT).EQ.'?') THEN
         WRITE(*,*) 'FPCUST. Access to the data not defined'
         WRITE(*,*) '        Your program runs on the : ',
     +        GTHOST(:LENB(GTHOST))
         WRITE(*,*) '        Your data resides on the : ',
     +        HOST(:LENB(HOST))
         WRITE(*,*) '               The media type is : ',
     +        MEDIA(:LENB(MEDIA))
         IRC=1
         GOTO 999
      ENDIF
      INFU(41,LX) = IOPEN(24)
      INFU(12,LX) = IOPEN(15)
      WRITE(*,*) '----------------------------------------------'
*
  999 RETURN
      END
#endif
