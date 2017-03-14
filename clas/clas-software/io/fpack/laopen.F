#ifdef HPUX
*CMZ :  1.00/16 11/03/96  15.14.41  by  Unknown
*CMZU:  1.00/14 27/02/96  17.26.15  by  Dave Sankey at RAL
*CMZU:  1.00/13 31/08/95  15.38.16  by  Unknown
*CMZ :  1.00/00 16/12/94  17.00.00  by  Pavel Binko
*-- Author :    Volker Blobel   16/12/94
      SUBROUTINE LAOPEN
*
************************************************************************
*
*     OPEN STATEMENT
*
*     Open parameters
*
*      I   IOPEN(I)
*     --   ---------
*      1   unit number (UNIT)
*      2   physical record length (RECL)
*      3   number of records - primary space (NREC)
*      4   number of records - secondary space (NREC2)
*      5   blocking factor (BLFACTOR)
*
*      6   number of buffers (BUFNO)
*      7   not yet used
*      8   number of MB per file (SPLITMB)
*      9   action  ( 1=read   2=write  3=readwrite  4=modify )
*     10   status  ( 1=old    2=new    3=scratch )
*
*     11   access  ( 1=seq    2=dir    3=keyed   4=special  5=ordered )
*     12   form    ( 1=binary 2=text )
*     13   wordfmt ( 0=local  1=ieee   2=ibm     3=vax      4=dec )
*     14   recsep  ( 1=recsep, logical records separating )
*     15   noopen  ( 1=NOOPEN, open should NOT be done )
*                  ( 2=DEFER, DD card with DEFER parameter exists )
*                  ( 3=STAGE )  ( 4 = STAGE and KEEP in the shift pool )
*                  ( 5=UNIXTAPE = tapes on UNIX machines )
*
*     16   compact ( 1=compact=default  2=nocompact for export )
*     17   catalog ( 1=shift )
*     18   skipcorev ( 1=skip corrupted event at the end of file )
*     19   data file (son) unit number (DUNIT)
*     20   flag "FATMEN catalog used" (1=used)
*          flag "FATMEN must be updated in FICLOS" (2=update)
*     21   1=DSTAGE has been set
*     22   1=DDEFER has been set
*     23   number of events per file (SPLITEV)
*     24   remote tape server
*          ( 0 = IBM (dsyibm), 1 = AMPEX (dice1), 2 = ACS (dice2) )
*     25   REPAIR mode (ordered access)
*     26   NOSYNC flag (ordered access) prevents the call to FPSYNC
*     27   not yet used
*     28   not yet used
*     29   not yet used
*     30   not yet used
*
*     DANAME = data_name
*     GENAME = generic_name
*     FINAME = file_name
*     HONAME = host_name
*     VOLSER = volume_name ( only private cartridges on the IBMDESY)
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
      PARAMETER (LASTCL=60)
      CHARACTER CTEXT*20, DANAME*16
      CHARACTER FINAME*256, HONAME*127, HONAM2*127, MEDIA*8, VONAME*6
      CHARACTER*9 TEXTCL(LASTCL)
      INTEGER LISTI(ILIMAX)
      DATA LISTI/0,1,1,1,0/
      CHARACTER*2 STRLUN
*
      INTEGER NUMBCL(LASTCL), IOPEN(IOPMAX)
      LOGICAL FIRST
      DATA TEXTCL/'UNIT','RECL','NREC','NREC2','BLFACTOR','BUFNO',
     +            'NOTUSED','SPLITMB','DUNIT','SPLITEV',
     +            'ACTION','READ','WRITE','READWRITE','MODIFY',
     +            'STATUS','OLD','NEW','SCR',
     +            'ACCESS','SEQ','DIR','KEYED','SPECIAL','ORDERED',
     +            'FORM','BINARY','TEXT',
     +            'WORDFMT','WFLOCAL','WFIEEE','WFIBM','WFVAX','WFDEC',
     +            'RECSEP','RECSEP',
     +            'NOOPEN','NOOPEN','DEFER','STAGE','STAGEKEEP',
     +                     'UNIXTAPE',
     +            'COMPACT','COMP','NOCOMP',
     +            'CATALOG','SHIFT',
     +            'SKIPCOREV','SKIPCOREV',
     +            'DSTAGE','DSTAGE',
     +            'DDEFER','DDEFER',
     +            'AMPEX','AMPEX','ACS',
     +            'REPAIR','REPAIR',
     +            'NOSYNC','NOSYNC'/
*
      DATA NUMERIC/10/
      DATA NUMBCL/-1,-2,-3,-4,-5,-6,
     +            -7,-8,-19,-23,
     +            -9,1,2,3,4,
     +            -10,1,2,3,
     +            -11,1,2,3,4,5,
     +            -12,1,2,
     +            -13,0,1,2,3,4,
     +            -14,1,
     +            -15,1,2,3,4,5,
     +            -16,1,2,
     +            -17,1,
     +            -18,1,
     +            -21,1,
     +            -22,1,
     +            -24,1,2,
     +            -25,1,
     +            -26,1/
*
      CALL FQCOND('PRINT',IPFLAG)
*
      CALL TEXT32
*
      DO 10 I=1,IOPMAX
   10 IOPEN(I)=0
*
      DANAME = ' '
      FINAME = ' '
      HONAME = ' '
      MEDIA  = ' '
      VONAME = ' '
*
      FIRST=.TRUE.
      IEPOS =0
*
*     starting with I=2 (I=1 is OPEN or similar)
      I=2
      IFNIDX = -1
      IFNPOS = 0
      IVSIDX = -1
      IVSPOS = 0
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
      NC=ITX(2,I)-ITX(1,I)+1
*
*     special case of FILE = list of file_names
      IF(CTEXT.EQ.'FILE'.AND.NEQ.EQ.1) THEN
         I=I+2
         IF(ITX(3,I).NE.2.AND.ITX(3,I).NE.3) THEN
*           no text
            IEPOS=ITX(1,I)
            GOTO 90
         END IF
         IFNIDX = I
         IFNPOS = ITX(1,I)
         CALL FPFSTN(FINAME,STMT(1:ITX(2,I)),IFNPOS)
         GOTO 50
      END IF
*
*     special case of HOST = host_name
      IF(CTEXT.EQ.'HOST'.AND.NEQ.EQ.1) THEN
         I=I+2
         IF(ITX(3,I).NE.2.AND.ITX(3,I).NE.3) THEN
*           no text
            IEPOS=ITX(1,I)
            GOTO 90
         END IF
         HONAME=STMT(ITX(1,I):ITX(2,I))
         GOTO 50
      END IF
*
*     special case of VOLSER = list of serial_numbers of volumes
      IF(CTEXT.EQ.'VOLSER'.AND.NEQ.EQ.1) THEN
         I=I+2
         IF(ITX(3,I).NE.2.AND.ITX(3,I).NE.3) THEN
*           no text
            IEPOS=ITX(1,I)
            GOTO 90
         END IF
         IVSIDX = I
         IVSPOS = ITX(1,I)
         CALL FPFSTN(VONAME,STMT(1:ITX(2,I)),IVSPOS)
         GOTO 50
      END IF
*
      IF(NEQ.NE.0) THEN
*        for = sign compare title text
         DO 30 J=1,LASTCL
         IF(NUMBCL(J).GT.0) GOTO 30
         IF(TEXTCL(J).EQ.CTEXT) THEN
            IT=-NUMBCL(J)
            IF(J.LE.NUMERIC) THEN
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
   50 IF(I.GE.NTX) GOTO 60
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
*     end of the command string analysis
*
   60 CONTINUE
*
*     Reserve the unit number UNIT=x (or the first free unit number)
      LUN = IOPEN(1)
      CALL FUPARM(LUN,DANAME,LISTI)
      IOPEN(1) = LUN
      LX = LUNIT(LUN)
*
*     Save KEEP flag (can be used in index files or in FATMEN interface)
      IF(IOPEN(15).EQ.4) THEN
         INFU(42,LX)=IOPEN(15)
      ENDIF
*
*
*     check host_name
      HONAM2=HONAME
      CALL UCASE(HONAM2)
      IF(HONAM2.EQ.'LOCAL') HONAME=' '
*
*     check consistency of OPEN parameters and insert defaults
*
      IF(IOPEN(9).EQ.1) THEN
*        action = read
         IF(IOPEN(10).EQ.2.OR.IOPEN(10).EQ.3) THEN
            ERRMES='Illegal status NEW or SCR for READ'
            GOTO 90
         ELSE IF(FINAME.EQ.' ') THEN
            ERRMES='File_name must be given'
            GOTO 90
         END IF
*        default for READ is OLD
         IF(IOPEN(10).EQ.0) IOPEN(10)=1
      ELSE IF(IOPEN(9).EQ.2) THEN
*        action = write
         IF(IOPEN(10).EQ.3.) THEN
            ERRMES='Illegal status SCR for WRITE'
            GOTO 90
         ELSE IF(FINAME.EQ.' ') THEN
            ERRMES='File_name must be given'
            GOTO 90
         END IF
*        default for write is new
         IF(IOPEN(10).EQ.0) IOPEN(10)=2
      ELSE IF(IOPEN(9).EQ.3) THEN
*        action is readwrite
         IF(IOPEN(10).EQ.0) THEN
            IF(FINAME.EQ.' ') THEN
*              default for READWRITE is SCR (for file_name not given)
               IOPEN(10)=3
            ELSE
*              default for READWRITE is OLD (for file_name given)
               IOPEN(10)=1
            END IF
         END IF
         IF(IOPEN(10).NE.3) THEN
*           no SCR unit
            IF(FINAME.EQ.' ') THEN
               ERRMES='File_name must be given'
               GOTO 90
            END IF
         END IF
      ELSE IF(IOPEN(9).EQ.4) THEN
*        action is modify
         IF(IOPEN(10).EQ.2.OR.IOPEN(10).EQ.3) THEN
            ERRMES='Illegal status NEW or SCR for MODIFY'
            GOTO 90
         ELSE IF(FINAME.EQ.' ') THEN
            ERRMES='File_name must be given'
            GOTO 90
         END IF
*        default for MODIFY is OLD
         IF(IOPEN(10).EQ.0) IOPEN(10)=1
      ELSE
*        action not specified IOPEN(9) = 0
         IF(IOPEN(10).EQ.2) THEN
*           NEW
            IF(FINAME.EQ.' ') THEN
               ERRMES='File_name must be given'
               GOTO 90
            END IF
*           default for NEW is WRITE
            IOPEN(9)=2
         ELSE IF(IOPEN(10).EQ.3) THEN
*           SCR
*           default for SCR is READWRITE
            IOPEN(9)=3
         ELSE IF(FINAME.EQ.' ') THEN
            ERRMES='File_name must be given'
            GOTO 90
         END IF
      END IF
*
*     if action and status are not given, default is READ and OLD
      IF(IOPEN(9).EQ.0) IOPEN(9)=1
      IF(IOPEN(10).EQ.0) IOPEN(10)=1
*
      IF(IOPEN(9).NE.1.AND.IOPEN(2).EQ.0) THEN
         ERRMES='RECL must be given for WRITE, READWRITE or MODIFY'
         GOTO 90
      END IF
*
      IF(IOPEN(15).EQ.5.AND.IOPEN(2).EQ.0) THEN
         ERRMES='RECL must be given for UNIXTAPE'
         GOTO 90
      END IF
*
      IF(IOPEN(10).EQ.2.OR.IOPEN(10).EQ.3) THEN
*        NEW or SCR file
*        assume seq and binary if not given
         IF(IOPEN(11).EQ.0) IOPEN(11)=1
         IF(IOPEN(12).EQ.0) IOPEN(12)=1
         IF(IOPEN(12).EQ.1) THEN
*           binary form
            IF(IOPEN(2).EQ.0) IOPEN(2)=3600
            IF(IOPEN(3).EQ.0) IOPEN(3)=100
            IF(IOPEN(5).NE.0.AND.IOPEN(5).NE.1) THEN
               ERRMES='No blocking factor for binary files allowed'
               GOTO 90
            END IF
            IOPEN(5)=1
         ELSE IF(IOPEN(12).EQ.2) THEN
*           text form
*           default RECL is 80
            IF(IOPEN(2).EQ.0) IOPEN(2)=80
            IF(IOPEN(2).LT.80.OR.IOPEN(2).GT.132) THEN
               ERRMES='RECL value must be minimal 80 and maximal 132'
               GOTO 90
            END IF
            IF(IOPEN(3).EQ.0) IOPEN(3)=100
            IF(IOPEN(5).EQ.0) THEN
*              define blocking factor
               IOPEN(5)=3200/IOPEN(2)
               IF(IOPEN(5).EQ.0) IOPEN(5)=1
            END IF
         END IF
         IF(IOPEN(13).EQ.0) IOPEN(13)=LOCAL
      ELSE
*        OLD data set
         IOPEN(3)=0
         IOPEN(4)=0
         IOPEN(5)=0
*        assume seq and binary if no specified
         IF(IOPEN(11).EQ.0) IOPEN(11)=1
         IF(IOPEN(12).EQ.0) IOPEN(12)=1
      END IF
*     number of buffers default 2
      IF(IOPEN(6).EQ.0) IOPEN(6)=2
*
      IF(IOPEN(8).GT.0 .AND. IOPEN(23).GT.0) THEN
         ERRMES='SPLITMB and SPLITEV not allowed simultaneuosly'
         GOTO 90
      ENDIF
*
*     concatenate file_name with the suffix .<letter><digit><digit>
*               for SPLITMB  or SPLITEV only, not for UNIXTAPE
*
      IF(IOPEN(8).GT.0 .OR. IOPEN(23).GT.0) THEN
         IF(FINAME.EQ.' ') THEN
            IF(IOPEN(8).GT.0) THEN
               ERRMES='File_name must be given for parameter SPLITMB'
            ELSE
               ERRMES='File_name must be given for parameter SPLITEV'
            ENDIF
            GOTO 90
         END IF
         IF(IOPEN(9).EQ.1) THEN
            IF(IOPEN(8).GT.0) THEN
               ERRMES='Action READ not allowed for parameter SPLITMB'
            ELSE
               ERRMES='Action READ not allowed for parameter SPLITEV'
            ENDIF
            GOTO 90
         END IF
*
*        don't concatenate file_name with the suffix for UNIXTAPE
         IF(IOPEN(15).NE.5) THEN
            L = LENB(FINAME)
*           don't concatenate with .A00
*           if the suffix is .<letter><digit><digit> or .<C>7*<digit>
            IF( ( L.LE.LEN(FINAME)-4 ) .AND.
     +          ((L.LE.4) .OR. (FINAME(L-3:L-3).NE.'.') .OR.
     +                     (IFPWNM(FINAME(L-2:L)).LT.0)) .AND.
     +          ((L.LE.9) .OR. (FINAME(L-8:L-8).NE.'.') .OR.
     +                     (IFPWNM(FINAME(L-7:L)).LT.0)) )
     +             FINAME(L+1:) = '.A00'
         END IF
*
      END IF
*
      IF(IOPEN(11).EQ.3.)THEN
*        force record separation for keyed access
         IOPEN(14) = 1
      ENDIF
      IF(IOPEN(11).EQ.5.AND.IOPEN(12).EQ.2)THEN
         ERRMES = 'Text mode not allowed for ordered access'
         GOTO 90
      ENDIF
      IF(IOPEN(11).EQ.5.AND.IOPEN(2).NE.0.AND.IOPEN(2).LT.160)THEN
         ERRMES = 'RECL no less than 160 for ordered access'
         GOTO 90
      ENDIF
      IF(IOPEN(11).EQ.5.AND.IOPEN(8).NE.0)THEN
         ERRMES = 'SPLITMB not allowed for ordered access'
         GOTO 90
      ENDIF
      IF(IOPEN(11).EQ.5.AND.IOPEN(15).EQ.1)THEN
         ERRMES = 'NOOPEN not allowed for ordered access'
         GOTO 90
      ENDIF
      IF(IOPEN(11).EQ.5.AND.IOPEN(15).EQ.2)THEN
         ERRMES = 'DEFER not allowed for ordered access'
         GOTO 90
      ENDIF
      IF(IOPEN(11).EQ.5.AND.IOPEN(15).EQ.5)THEN
         ERRMES = 'UNIXTAPE not allowed for ordered access'
         GOTO 90
      ENDIF
      IF(IOPEN(11).EQ.5.AND.IOPEN(9).EQ.3)THEN
*        change READWRITE to MODIFY
         IOPEN(9) = 4
      ENDIF
      IF(IOPEN(25).NE.0.AND.(IOPEN(11).NE.5.OR.IOPEN(9).NE.4)) THEN
         ERRMES = 'REPAIR can be used with ORDERED and MODIFY'
         GOTO 90
      ENDIF
      IF(IOPEN(26).NE.0.AND.(IOPEN(11).NE.5.OR.IOPEN(9).NE.4)) THEN
         ERRMES = 'NOSYNC can be used with ORDERED and MODIFY'
         GOTO 90
      ENDIF
*
*     AMPEX and ACS make sense with STAGE or STAGEKEEP only
      IF(IOPEN(24).EQ.1 .AND. IOPEN(15).NE.3 .AND. IOPEN(15).NE.4) THEN
         ERRMES = 'AMPEX can be used only with STAGE or STAGEKEEP'
         GOTO 90
      ENDIF
      IF(IOPEN(24).EQ.2 .AND. IOPEN(15).NE.3 .AND. IOPEN(15).NE.4) THEN
         ERRMES = 'ACS can be used only with STAGE or STAGEKEEP'
         GOTO 90
      ENDIF
*
*     VOLSER can be used only with DEFER
      IF(VONAME.NE.' ' .AND. IOPEN(15).NE.2) THEN
         ERRMES = 'VOLSER can be used only with DEFER'
         GOTO 90
      ENDIF
*     Before MKOPEN it is a good idea to reserve DLUN for cartridge
*     datafile if necessary
      IF(IOPEN(19).NE.0) THEN
         LDANAM=LENB(DANAME)
         CALL PNVI(IOPEN(19),STRLUN,LSTR)
         LDANAM= MIN(LDANAM,7)
         CALL FUPARM(IOPEN(19)
     +,              DANAME(1:LDANAM)//' '//STRLUN(1:LSTR)//' DCART'
     +,              LISTI)
      ENDIF
*
      IF(IVSIDX.LE.0) THEN
         KPOS = IVSPOS-1
      ELSE
         KPOS = ITX(2,IVSIDX)
      ENDIF
*
      IF(IFNIDX.LE.0) THEN
         LPOS = IFNPOS-1
      ELSE
         LPOS = ITX(2,IFNIDX)
      ENDIF
*
      IF(IFNPOS.LE.LPOS .AND. IVSPOS.LE.KPOS) THEN
         CALL MKOPEN(IOPEN,FINAME,DANAME,HONAME,VONAME,
     +               STMT(IFNPOS:LPOS),STMT(IVSPOS:KPOS),IER)
      ELSEIF(IFNPOS.LE.LPOS) THEN
         CALL MKOPEN(IOPEN,FINAME,DANAME,HONAME,VONAME,
     +               STMT(IFNPOS:LPOS),' ',IER)
      ELSEIF(IVSPOS.LE.KPOS) THEN
         CALL MKOPEN(IOPEN,FINAME,DANAME,HONAME,VONAME,
     +               ' ',STMT(IVSPOS:KPOS),IER)
      ELSE
         CALL MKOPEN(IOPEN,FINAME,DANAME,HONAME,VONAME,
     +               ' ',' ',IER)
      ENDIF
*
      GOTO 100
*
   90 CALL FQCSET('OPENERROR',1)
*
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
*
  100 RETURN
      END
#elif defined Linux
*CMZ :  1.00/00 16/12/94  17.00.00  by  Pavel Binko
*-- Author :    Volker Blobel   16/12/94
      SUBROUTINE LAOPEN
*
************************************************************************
*
*     OPEN STATEMENT
*
*     Open parameters
*
*      I   IOPEN(I)
*     --   ---------
*      1   unit number (UNIT)
*      2   physical record length (RECL)
*      3   number of records - primary space (NREC)
*      4   number of records - secondary space (NREC2)
*      5   blocking factor (BLFACTOR)
*
*      6   number of buffers (BUFNO)
*      7   not yet used
*      8   number of MB per file (SPLITMB)
*      9   action  ( 1=read   2=write  3=readwrite  4=modify )
*     10   status  ( 1=old    2=new    3=scratch )
*
*     11   access  ( 1=seq    2=dir    3=keyed   4=special  5=ordered )
*     12   form    ( 1=binary 2=text )
*     13   wordfmt ( 0=local  1=ieee   2=ibm     3=vax      4=dec )
*     14   recsep  ( 1=recsep, logical records separating )
*     15   noopen  ( 1=NOOPEN, open should NOT be done )
*                  ( 2=DEFER, DD card with DEFER parameter exists )
*                  ( 3=STAGE )  ( 4 = STAGE and KEEP in the shift pool )
*                  ( 5=UNIXTAPE = tapes on UNIX machines )
*
*     16   compact ( 1=compact=default  2=nocompact for export )
*     17   catalog ( 1=shift )
*     18   skipcorev ( 1=skip corrupted event at the end of file )
*     19   data file (son) unit number (DUNIT)
*     20   flag "FATMEN catalog used" (1=used)
*          flag "FATMEN must be updated in FICLOS" (2=update)
*     21   1=DSTAGE has been set
*     22   1=DDEFER has been set
*     23   number of events per file (SPLITEV)
*     24   remote tape server
*          ( 0 = IBM (dsyibm), 1 = AMPEX (dice1), 2 = ACS (dice2) )
*     25   not yet used
*     26   not yet used
*     27   not yet used
*     28   not yet used
*     29   not yet used
*     30   not yet used
*
*     DANAME = data_name
*     GENAME = generic_name
*     FINAME = file_name
*     HONAME = host_name
*     VOLSER = volume_name ( only private cartridges on the IBMDESY)
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
      PARAMETER (LASTCL=56)
      CHARACTER CTEXT*20, DANAME*16
      CHARACTER FINAME*256, HONAME*127, HONAM2*127, MEDIA*8, VONAME*6
      CHARACTER*9 TEXTCL(LASTCL)
      CHARACTER IENVRN*6, FLUNIX*256
      INTEGER LISTI(ILIMAX)
      DATA LISTI/0,1,1,1,0/
      CHARACTER*2 STRLUN
*
      INTEGER NUMBCL(LASTCL), IOPEN(IOPMAX)
      LOGICAL FIRST
      DATA TEXTCL/'UNIT','RECL','NREC','NREC2','BLFACTOR','BUFNO',
     +            'NOTUSED','SPLITMB','DUNIT','SPLITEV',
     +            'ACTION','READ','WRITE','READWRITE','MODIFY',
     +            'STATUS','OLD','NEW','SCR',
     +            'ACCESS','SEQ','DIR','KEYED','SPECIAL','ORDERED',
     +            'FORM','BINARY','TEXT',
     +            'WORDFMT','WFLOCAL','WFIEEE','WFIBM','WFVAX','WFDEC',
     +            'RECSEP','RECSEP',
     +            'NOOPEN','NOOPEN','DEFER','STAGE','STAGEKEEP',
     +                     'UNIXTAPE',
     +            'COMPACT','COMP','NOCOMP',
     +            'CATALOG','SHIFT',
     +            'SKIPCOREV','SKIPCOREV',
     +            'DSTAGE','DSTAGE',
     +            'DDEFER','DDEFER',
     +            'AMPEX','AMPEX','ACS'/
*
      DATA NUMERIC/10/
      DATA NUMBCL/-1,-2,-3,-4,-5,-6,
     +            -7,-8,-19,-23,
     +            -9,1,2,3,4,
     +            -10,1,2,3,
     +            -11,1,2,3,4,5,
     +            -12,1,2,
     +            -13,0,1,2,3,4,
     +            -14,1,
     +            -15,1,2,3,4,5,
     +            -16,1,2,
     +            -17,1,
     +            -18,1,
     +            -21,1,
     +            -22,1,
     +            -24,1,2/
*
      CALL FQCOND('PRINT',IPFLAG)
*
      CALL TEXT32
*
      DO 10 I=1,IOPMAX
   10 IOPEN(I)=0
*
      DANAME = ' '
      FINAME = ' '
      HONAME = ' '
      MEDIA  = ' '
      VONAME = ' '
*
      FIRST=.TRUE.
      IEPOS =0
*
*     starting with I=2 (I=1 is OPEN or similar)
      I=2
      IFNIDX = -1
      IFNPOS = 0
      IVSIDX = -1
      IVSPOS = 0
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
      NC=ITX(2,I)-ITX(1,I)+1
*
*     special case of FILE = list of file_names
      IF(CTEXT.EQ.'FILE'.AND.NEQ.EQ.1) THEN
         I=I+2
         IF(ITX(3,I).NE.2.AND.ITX(3,I).NE.3) THEN
*           no text
            IEPOS=ITX(1,I)
            GOTO 90
         END IF
         IFNIDX = I
         IFNPOS = ITX(1,I)
         CALL FPFSTN(FINAME,STMT(1:ITX(2,I)),IFNPOS)
         GOTO 50
      END IF
*
*     special case of HOST = host_name
      IF(CTEXT.EQ.'HOST'.AND.NEQ.EQ.1) THEN
         I=I+2
         IF(ITX(3,I).NE.2.AND.ITX(3,I).NE.3) THEN
*           no text
            IEPOS=ITX(1,I)
            GOTO 90
         END IF
         HONAME=STMT(ITX(1,I):ITX(2,I))
         GOTO 50
      END IF
*
*     special case of VOLSER = list of serial_numbers of volumes
      IF(CTEXT.EQ.'VOLSER'.AND.NEQ.EQ.1) THEN
         I=I+2
         IF(ITX(3,I).NE.2.AND.ITX(3,I).NE.3) THEN
*           no text
            IEPOS=ITX(1,I)
            GOTO 90
         END IF
         IVSIDX = I
         IVSPOS = ITX(1,I)
         CALL FPFSTN(VONAME,STMT(1:ITX(2,I)),IVSPOS)
         GOTO 50
      END IF
*
      IF(NEQ.NE.0) THEN
*        for = sign compare title text
         DO 30 J=1,LASTCL
         IF(NUMBCL(J).GT.0) GOTO 30
         IF(TEXTCL(J).EQ.CTEXT) THEN
            IT=-NUMBCL(J)
            IF(J.LE.NUMERIC) THEN
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
   50 IF(I.GE.NTX) GOTO 60
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
*     end of the command string analysis
*
   60 CONTINUE
*
*     Reserve the unit number UNIT=x (or the first free unit number)
      LUN = IOPEN(1)
      CALL FUPARM(LUN,DANAME,LISTI)
      IOPEN(1) = LUN
      LX = LUNIT(LUN)
*
*     Save KEEP flag (can be used in index files or in FATMEN interface)
      IF(IOPEN(15).EQ.4) THEN
         INFU(42,LX)=IOPEN(15)
      ENDIF
*
*
*     check host_name
      HONAM2=HONAME
      CALL UCASE(HONAM2)
      IF(HONAM2.EQ.'LOCAL') HONAME=' '
*
*     check file_name
      IF(FINAME.EQ.' ') THEN
         IF(IOPEN(1).LT.10) THEN
         WRITE(IENVRN,'(''FORT'',I1)')IOPEN(1)
         ELSEIF(IOPEN(1).LT.100) THEN
         WRITE(IENVRN,'(''FORT'',I2)')IOPEN(1)
         ELSEIF(IOPEN(1).LT.1000) THEN
         WRITE(IENVRN,'(''FORT'',I3)')IOPEN(1)
       ELSE
           ERRMES='Unit number too high.'
           GOTO 90
       ENDIF
         CALL GETENV(IENVRN,FLUNIX)
         LNUNIX = LENB(FLUNIX)
         IF(LNUNIX.GT.0) THEN
            FINAME = FLUNIX(1:LNUNIX)
            WRITE(6,*) ' The following file will be opened '
            WRITE(6,*) FLUNIX(1:LNUNIX)
         END IF
      END IF
*     check consistency of OPEN parameters and insert defaults
*
      IF(IOPEN(9).EQ.1) THEN
*        action = read
         IF(IOPEN(10).EQ.2.OR.IOPEN(10).EQ.3) THEN
            ERRMES='Illegal status NEW or SCR for READ'
            GOTO 90
         ELSE IF(FINAME.EQ.' ') THEN
            ERRMES='File_name must be given'
            GOTO 90
         END IF
*        default for READ is OLD
         IF(IOPEN(10).EQ.0) IOPEN(10)=1
      ELSE IF(IOPEN(9).EQ.2) THEN
*        action = write
         IF(IOPEN(10).EQ.3.) THEN
            ERRMES='Illegal status SCR for WRITE'
            GOTO 90
         ELSE IF(FINAME.EQ.' ') THEN
            ERRMES='File_name must be given'
            GOTO 90
         END IF
*        default for write is new
         IF(IOPEN(10).EQ.0) IOPEN(10)=2
      ELSE IF(IOPEN(9).EQ.3) THEN
*        action is readwrite
         IF(IOPEN(10).EQ.0) THEN
            IF(FINAME.EQ.' ') THEN
*              default for READWRITE is SCR (for file_name not given)
               IOPEN(10)=3
            ELSE
*              default for READWRITE is OLD (for file_name given)
               IOPEN(10)=1
            END IF
         END IF
         IF(IOPEN(10).NE.3) THEN
*           no SCR unit
            IF(FINAME.EQ.' ') THEN
               ERRMES='File_name must be given'
               GOTO 90
            END IF
         END IF
      ELSE IF(IOPEN(9).EQ.4) THEN
*        action is modify
         IF(IOPEN(10).EQ.2.OR.IOPEN(10).EQ.3) THEN
            ERRMES='Illegal status NEW or SCR for MODIFY'
            GOTO 90
         ELSE IF(FINAME.EQ.' ') THEN
            ERRMES='File_name must be given'
            GOTO 90
         END IF
*        default for MODIFY is OLD
         IF(IOPEN(10).EQ.0) IOPEN(10)=1
      ELSE
*        action not specified IOPEN(9) = 0
         IF(IOPEN(10).EQ.2) THEN
*           NEW
            IF(FINAME.EQ.' ') THEN
               ERRMES='File_name must be given'
               GOTO 90
            END IF
*           default for NEW is WRITE
            IOPEN(9)=2
         ELSE IF(IOPEN(10).EQ.3) THEN
*           SCR
*           default for SCR is READWRITE
            IOPEN(9)=3
         ELSE IF(FINAME.EQ.' ') THEN
            ERRMES='File_name must be given'
            GOTO 90
         END IF
      END IF
*
*     if action and status are not given, default is READ and OLD
      IF(IOPEN(9).EQ.0) IOPEN(9)=1
      IF(IOPEN(10).EQ.0) IOPEN(10)=1
*
      IF(IOPEN(9).NE.1.AND.IOPEN(2).EQ.0) THEN
         ERRMES='RECL must be given for WRITE, READWRITE or MODIFY'
         GOTO 90
      END IF
*
      IF(IOPEN(15).EQ.5.AND.IOPEN(2).EQ.0) THEN
         ERRMES='RECL must be given for UNIXTAPE'
         GOTO 90
      END IF
*
      IF(IOPEN(10).EQ.2.OR.IOPEN(10).EQ.3) THEN
*        NEW or SCR file
*        assume seq and binary if not given
         IF(IOPEN(11).EQ.0) IOPEN(11)=1
         IF(IOPEN(12).EQ.0) IOPEN(12)=1
         IF(IOPEN(12).EQ.1) THEN
*           binary form
            IF(IOPEN(2).EQ.0) IOPEN(2)=3600
            IF(IOPEN(3).EQ.0) IOPEN(3)=100
            IF(IOPEN(5).NE.0.AND.IOPEN(5).NE.1) THEN
               ERRMES='No blocking factor for binary files allowed'
               GOTO 90
            END IF
            IOPEN(5)=1
         ELSE IF(IOPEN(12).EQ.2) THEN
*           text form
*           default RECL is 80
            IF(IOPEN(2).EQ.0) IOPEN(2)=80
            IF(IOPEN(2).LT.80.OR.IOPEN(2).GT.132) THEN
               ERRMES='RECL value must be minimal 80 and maximal 132'
               GOTO 90
            END IF
            IF(IOPEN(3).EQ.0) IOPEN(3)=100
            IF(IOPEN(5).EQ.0) THEN
*              define blocking factor
               IOPEN(5)=3200/IOPEN(2)
               IF(IOPEN(5).EQ.0) IOPEN(5)=1
            END IF
         END IF
         IF(IOPEN(13).EQ.0) IOPEN(13)=LOCAL
      ELSE
*        OLD data set
         IOPEN(3)=0
         IOPEN(4)=0
         IOPEN(5)=0
*        assume seq and binary if no specified
         IF(IOPEN(11).EQ.0) IOPEN(11)=1
         IF(IOPEN(12).EQ.0) IOPEN(12)=1
      END IF
*     number of buffers default 2
      IF(IOPEN(6).EQ.0) IOPEN(6)=2
*
      IF(IOPEN(8).GT.0 .AND. IOPEN(23).GT.0) THEN
         ERRMES='SPLITMB and SPLITEV not allowed simultaneuosly'
         GOTO 90
      ENDIF
*
*     concatenate file_name with the suffix .<letter><digit><digit>
*               for SPLITMB  or SPLITEV only, not for UNIXTAPE
*
      IF(IOPEN(8).GT.0 .OR. IOPEN(23).GT.0) THEN
         IF(FINAME.EQ.' ') THEN
            IF(IOPEN(8).GT.0) THEN
               ERRMES='File_name must be given for parameter SPLITMB'
            ELSE
               ERRMES='File_name must be given for parameter SPLITEV'
            ENDIF
            GOTO 90
         END IF
         IF(IOPEN(9).EQ.1) THEN
            IF(IOPEN(8).GT.0) THEN
               ERRMES='Action READ not allowed for parameter SPLITMB'
            ELSE
               ERRMES='Action READ not allowed for parameter SPLITEV'
            ENDIF
            GOTO 90
         END IF
*
*        don't concatenate file_name with the suffix for UNIXTAPE
         IF(IOPEN(15).NE.5) THEN
            L = LENB(FINAME)
*           don't concatenate with .A00
*           if the suffix is .<letter><digit><digit> or .<C>7*<digit>
            IF( ( L.LE.LEN(FINAME)-4 ) .AND.
     +          ((L.LE.4) .OR. (FINAME(L-3:L-3).NE.'.') .OR.
     +                     (IFPWNM(FINAME(L-2:L)).LT.0)) .AND.
     +          ((L.LE.9) .OR. (FINAME(L-8:L-8).NE.'.') .OR.
     +                     (IFPWNM(FINAME(L-7:L)).LT.0)) )
     +             FINAME(L+1:) = '.A00'
         END IF
*
      END IF
*
      IF(IOPEN(11).EQ.3.)THEN
*        force record separation for keyed access
         IOPEN(14) = 1
      ENDIF
      IF(IOPEN(11).EQ.5.AND.IOPEN(12).EQ.2)THEN
         ERRMES = 'Text mode not allowed for ordered access'
         GOTO 90
      ENDIF
      IF(IOPEN(11).EQ.5.AND.IOPEN(2).NE.0.AND.IOPEN(2).LT.160)THEN
         ERRMES = 'RECL no less than 160 for ordered access'
         GOTO 90
      ENDIF
      IF(IOPEN(11).EQ.5.AND.IOPEN(8).NE.0)THEN
         ERRMES = 'SPLITMB not allowed for ordered access'
         GOTO 90
      ENDIF
      IF(IOPEN(11).EQ.5.AND.IOPEN(15).EQ.1)THEN
         ERRMES = 'NOOPEN not allowed for ordered access'
         GOTO 90
      ENDIF
      IF(IOPEN(11).EQ.5.AND.IOPEN(15).EQ.2)THEN
         ERRMES = 'DEFER not allowed for ordered access'
         GOTO 90
      ENDIF
      IF(IOPEN(11).EQ.5.AND.IOPEN(15).EQ.5)THEN
         ERRMES = 'UNIXTAPE not allowed for ordered access'
         GOTO 90
      ENDIF
      IF(IOPEN(11).EQ.5.AND.IOPEN(9).EQ.3)THEN
*        change READWRITE to MODIFY
         IOPEN(9) = 4
      ENDIF
*
*     AMPEX and ACS make sense with STAGE or STAGEKEEP only
      IF(IOPEN(24).EQ.1 .AND. IOPEN(15).NE.3 .AND. IOPEN(15).NE.4) THEN
         ERRMES = 'AMPEX can be used only with STAGE or STAGEKEEP'
         GOTO 90
      ENDIF
      IF(IOPEN(24).EQ.2 .AND. IOPEN(15).NE.3 .AND. IOPEN(15).NE.4) THEN
         ERRMES = 'ACS can be used only with STAGE or STAGEKEEP'
         GOTO 90
      ENDIF
*
*     VOLSER can be used only with DEFER
      IF(VONAME.NE.' ' .AND. IOPEN(15).NE.2) THEN
         ERRMES = 'VOLSER can be used only with DEFER'
         GOTO 90
      ENDIF
*     Before MKOPEN it is a good idea to reserve DLUN for cartridge
*     datafile if necessary
      IF(IOPEN(19).NE.0) THEN
         LDANAM=LENB(DANAME)
         CALL PNVI(IOPEN(19),STRLUN,LSTR)
         LDANAM= MIN(LDANAM,7)
         CALL FUPARM(IOPEN(19)
     +,              DANAME(1:LDANAM)//' '//STRLUN(1:LSTR)//' DCART'
     +,              LISTI)
      ENDIF
*
      IF(IVSIDX.LE.0) THEN
         KPOS = IVSPOS-1
      ELSE
         KPOS = ITX(2,IVSIDX)
      ENDIF
*
      IF(IFNIDX.LE.0) THEN
         LPOS = IFNPOS-1
      ELSE
         LPOS = ITX(2,IFNIDX)
      ENDIF
*
      IF(IFNPOS.LE.LPOS .AND. IVSPOS.LE.KPOS) THEN
         CALL MKOPEN(IOPEN,FINAME,DANAME,HONAME,VONAME,
     +               STMT(IFNPOS:LPOS),STMT(IVSPOS:KPOS),IER)
      ELSEIF(IFNPOS.LE.LPOS) THEN
         CALL MKOPEN(IOPEN,FINAME,DANAME,HONAME,VONAME,
     +               STMT(IFNPOS:LPOS),' ',IER)
      ELSEIF(IVSPOS.LE.KPOS) THEN
         CALL MKOPEN(IOPEN,FINAME,DANAME,HONAME,VONAME,
     +               ' ',STMT(IVSPOS:KPOS),IER)
      ELSE
         CALL MKOPEN(IOPEN,FINAME,DANAME,HONAME,VONAME,
     +               ' ',' ',IER)
      ENDIF
*
      GOTO 100
*
   90 CALL FQCSET('OPENERROR',1)
*
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
*
  100 RETURN
      END
#elif defined SunOS
*CMZ :  1.00/16 11/03/96  15.14.41  by  Unknown
*CMZU:  1.00/14 27/02/96  17.26.15  by  Dave Sankey at RAL
*CMZU:  1.00/13 31/08/95  15.38.16  by  Unknown
*CMZ :  1.00/00 16/12/94  17.00.00  by  Pavel Binko
*-- Author :    Volker Blobel   16/12/94
      SUBROUTINE LAOPEN
*
************************************************************************
*
*     OPEN STATEMENT
*
*     Open parameters
*
*      I   IOPEN(I)
*     --   ---------
*      1   unit number (UNIT)
*      2   physical record length (RECL)
*      3   number of records - primary space (NREC)
*      4   number of records - secondary space (NREC2)
*      5   blocking factor (BLFACTOR)
*
*      6   number of buffers (BUFNO)
*      7   not yet used
*      8   number of MB per file (SPLITMB)
*      9   action  ( 1=read   2=write  3=readwrite  4=modify )
*     10   status  ( 1=old    2=new    3=scratch )
*
*     11   access  ( 1=seq    2=dir    3=keyed   4=special  5=ordered )
*     12   form    ( 1=binary 2=text )
*     13   wordfmt ( 0=local  1=ieee   2=ibm     3=vax      4=dec )
*     14   recsep  ( 1=recsep, logical records separating )
*     15   noopen  ( 1=NOOPEN, open should NOT be done )
*                  ( 2=DEFER, DD card with DEFER parameter exists )
*                  ( 3=STAGE )  ( 4 = STAGE and KEEP in the shift pool )
*                  ( 5=UNIXTAPE = tapes on UNIX machines )
*
*     16   compact ( 1=compact=default  2=nocompact for export )
*     17   catalog ( 1=shift )
*     18   skipcorev ( 1=skip corrupted event at the end of file )
*     19   data file (son) unit number (DUNIT)
*     20   flag "FATMEN catalog used" (1=used)
*          flag "FATMEN must be updated in FICLOS" (2=update)
*     21   1=DSTAGE has been set
*     22   1=DDEFER has been set
*     23   number of events per file (SPLITEV)
*     24   remote tape server
*          ( 0 = IBM (dsyibm), 1 = AMPEX (dice1), 2 = ACS (dice2) )
*     25   REPAIR mode (ordered access)
*     26   NOSYNC flag (ordered access) prevents the call to FPSYNC
*     27   not yet used
*     28   not yet used
*     29   not yet used
*     30   not yet used
*
*     DANAME = data_name
*     GENAME = generic_name
*     FINAME = file_name
*     HONAME = host_name
*     VOLSER = volume_name ( only private cartridges on the IBMDESY)
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
      PARAMETER (LASTCL=60)
      CHARACTER CTEXT*20, DANAME*16
      CHARACTER FINAME*256, HONAME*127, HONAM2*127, MEDIA*8, VONAME*6
      CHARACTER*9 TEXTCL(LASTCL)
      CHARACTER IENVRN*6, FLUNIX*256
      INTEGER LISTI(ILIMAX)
      DATA LISTI/0,1,1,1,0/
      CHARACTER*2 STRLUN
*
      INTEGER NUMBCL(LASTCL), IOPEN(IOPMAX)
      LOGICAL FIRST
      DATA TEXTCL/'UNIT','RECL','NREC','NREC2','BLFACTOR','BUFNO',
     +            'NOTUSED','SPLITMB','DUNIT','SPLITEV',
     +            'ACTION','READ','WRITE','READWRITE','MODIFY',
     +            'STATUS','OLD','NEW','SCR',
     +            'ACCESS','SEQ','DIR','KEYED','SPECIAL','ORDERED',
     +            'FORM','BINARY','TEXT',
     +            'WORDFMT','WFLOCAL','WFIEEE','WFIBM','WFVAX','WFDEC',
     +            'RECSEP','RECSEP',
     +            'NOOPEN','NOOPEN','DEFER','STAGE','STAGEKEEP',
     +                     'UNIXTAPE',
     +            'COMPACT','COMP','NOCOMP',
     +            'CATALOG','SHIFT',
     +            'SKIPCOREV','SKIPCOREV',
     +            'DSTAGE','DSTAGE',
     +            'DDEFER','DDEFER',
     +            'AMPEX','AMPEX','ACS',
     +            'REPAIR','REPAIR',
     +            'NOSYNC','NOSYNC'/
*
      DATA NUMERIC/10/
      DATA NUMBCL/-1,-2,-3,-4,-5,-6,
     +            -7,-8,-19,-23,
     +            -9,1,2,3,4,
     +            -10,1,2,3,
     +            -11,1,2,3,4,5,
     +            -12,1,2,
     +            -13,0,1,2,3,4,
     +            -14,1,
     +            -15,1,2,3,4,5,
     +            -16,1,2,
     +            -17,1,
     +            -18,1,
     +            -21,1,
     +            -22,1,
     +            -24,1,2,
     +            -25,1,
     +            -26,1/
*
      CALL FQCOND('PRINT',IPFLAG)
*
      CALL TEXT32
*
      DO 10 I=1,IOPMAX
   10 IOPEN(I)=0
*
      DANAME = ' '
      FINAME = ' '
      HONAME = ' '
      MEDIA  = ' '
      VONAME = ' '
*
      FIRST=.TRUE.
      IEPOS =0
*
*     starting with I=2 (I=1 is OPEN or similar)
      I=2
      IFNIDX = -1
      IFNPOS = 0
      IVSIDX = -1
      IVSPOS = 0
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
      NC=ITX(2,I)-ITX(1,I)+1
*
*     special case of FILE = list of file_names
      IF(CTEXT.EQ.'FILE'.AND.NEQ.EQ.1) THEN
         I=I+2
         IF(ITX(3,I).NE.2.AND.ITX(3,I).NE.3) THEN
*           no text
            IEPOS=ITX(1,I)
            GOTO 90
         END IF
         IFNIDX = I
         IFNPOS = ITX(1,I)
         CALL FPFSTN(FINAME,STMT(1:ITX(2,I)),IFNPOS)
         GOTO 50
      END IF
*
*     special case of HOST = host_name
      IF(CTEXT.EQ.'HOST'.AND.NEQ.EQ.1) THEN
         I=I+2
         IF(ITX(3,I).NE.2.AND.ITX(3,I).NE.3) THEN
*           no text
            IEPOS=ITX(1,I)
            GOTO 90
         END IF
         HONAME=STMT(ITX(1,I):ITX(2,I))
         GOTO 50
      END IF
*
*     special case of VOLSER = list of serial_numbers of volumes
      IF(CTEXT.EQ.'VOLSER'.AND.NEQ.EQ.1) THEN
         I=I+2
         IF(ITX(3,I).NE.2.AND.ITX(3,I).NE.3) THEN
*           no text
            IEPOS=ITX(1,I)
            GOTO 90
         END IF
         IVSIDX = I
         IVSPOS = ITX(1,I)
         CALL FPFSTN(VONAME,STMT(1:ITX(2,I)),IVSPOS)
         GOTO 50
      END IF
*
      IF(NEQ.NE.0) THEN
*        for = sign compare title text
         DO 30 J=1,LASTCL
         IF(NUMBCL(J).GT.0) GOTO 30
         IF(TEXTCL(J).EQ.CTEXT) THEN
            IT=-NUMBCL(J)
            IF(J.LE.NUMERIC) THEN
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
   50 IF(I.GE.NTX) GOTO 60
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
*     end of the command string analysis
*
   60 CONTINUE
*
*     Reserve the unit number UNIT=x (or the first free unit number)
      LUN = IOPEN(1)
      CALL FUPARM(LUN,DANAME,LISTI)
      IOPEN(1) = LUN
      LX = LUNIT(LUN)
*
*     Save KEEP flag (can be used in index files or in FATMEN interface)
      IF(IOPEN(15).EQ.4) THEN
         INFU(42,LX)=IOPEN(15)
      ENDIF
*
*
*     check host_name
      HONAM2=HONAME
      CALL UCASE(HONAM2)
      IF(HONAM2.EQ.'LOCAL') HONAME=' '
*
*     check file_name
      IF(FINAME.EQ.' ') THEN
         IF(IOPEN(1).LT.10) WRITE(IENVRN,'(''FORT0'',I1)')IOPEN(1)
         IF(IOPEN(1).GE.10) WRITE(IENVRN,'(''FORT'',I2)')IOPEN(1)
         CALL GETENV(IENVRN,FLUNIX)
         LNUNIX = LENB(FLUNIX)
         IF(LNUNIX.GT.0) THEN
            FINAME = FLUNIX(1:LNUNIX)
            WRITE(6,*) ' The following file will be opened '
            WRITE(6,*) FLUNIX(1:LNUNIX)
         END IF
      END IF
*     check consistency of OPEN parameters and insert defaults
*
      IF(IOPEN(9).EQ.1) THEN
*        action = read
         IF(IOPEN(10).EQ.2.OR.IOPEN(10).EQ.3) THEN
            ERRMES='Illegal status NEW or SCR for READ'
            GOTO 90
         ELSE IF(FINAME.EQ.' ') THEN
            ERRMES='File_name must be given'
            GOTO 90
         END IF
*        default for READ is OLD
         IF(IOPEN(10).EQ.0) IOPEN(10)=1
      ELSE IF(IOPEN(9).EQ.2) THEN
*        action = write
         IF(IOPEN(10).EQ.3.) THEN
            ERRMES='Illegal status SCR for WRITE'
            GOTO 90
         ELSE IF(FINAME.EQ.' ') THEN
            ERRMES='File_name must be given'
            GOTO 90
         END IF
*        default for write is new
         IF(IOPEN(10).EQ.0) IOPEN(10)=2
      ELSE IF(IOPEN(9).EQ.3) THEN
*        action is readwrite
         IF(IOPEN(10).EQ.0) THEN
            IF(FINAME.EQ.' ') THEN
*              default for READWRITE is SCR (for file_name not given)
               IOPEN(10)=3
            ELSE
*              default for READWRITE is OLD (for file_name given)
               IOPEN(10)=1
            END IF
         END IF
         IF(IOPEN(10).NE.3) THEN
*           no SCR unit
            IF(FINAME.EQ.' ') THEN
               ERRMES='File_name must be given'
               GOTO 90
            END IF
         END IF
      ELSE IF(IOPEN(9).EQ.4) THEN
*        action is modify
         IF(IOPEN(10).EQ.2.OR.IOPEN(10).EQ.3) THEN
            ERRMES='Illegal status NEW or SCR for MODIFY'
            GOTO 90
         ELSE IF(FINAME.EQ.' ') THEN
            ERRMES='File_name must be given'
            GOTO 90
         END IF
*        default for MODIFY is OLD
         IF(IOPEN(10).EQ.0) IOPEN(10)=1
      ELSE
*        action not specified IOPEN(9) = 0
         IF(IOPEN(10).EQ.2) THEN
*           NEW
            IF(FINAME.EQ.' ') THEN
               ERRMES='File_name must be given'
               GOTO 90
            END IF
*           default for NEW is WRITE
            IOPEN(9)=2
         ELSE IF(IOPEN(10).EQ.3) THEN
*           SCR
*           default for SCR is READWRITE
            IOPEN(9)=3
         ELSE IF(FINAME.EQ.' ') THEN
            ERRMES='File_name must be given'
            GOTO 90
         END IF
      END IF
*
*     if action and status are not given, default is READ and OLD
      IF(IOPEN(9).EQ.0) IOPEN(9)=1
      IF(IOPEN(10).EQ.0) IOPEN(10)=1
*
      IF(IOPEN(9).NE.1.AND.IOPEN(2).EQ.0) THEN
         ERRMES='RECL must be given for WRITE, READWRITE or MODIFY'
         GOTO 90
      END IF
*
      IF(IOPEN(15).EQ.5.AND.IOPEN(2).EQ.0) THEN
         ERRMES='RECL must be given for UNIXTAPE'
         GOTO 90
      END IF
*
      IF(IOPEN(10).EQ.2.OR.IOPEN(10).EQ.3) THEN
*        NEW or SCR file
*        assume seq and binary if not given
         IF(IOPEN(11).EQ.0) IOPEN(11)=1
         IF(IOPEN(12).EQ.0) IOPEN(12)=1
         IF(IOPEN(12).EQ.1) THEN
*           binary form
            IF(IOPEN(2).EQ.0) IOPEN(2)=3600
            IF(IOPEN(3).EQ.0) IOPEN(3)=100
            IF(IOPEN(5).NE.0.AND.IOPEN(5).NE.1) THEN
               ERRMES='No blocking factor for binary files allowed'
               GOTO 90
            END IF
            IOPEN(5)=1
         ELSE IF(IOPEN(12).EQ.2) THEN
*           text form
*           default RECL is 80
            IF(IOPEN(2).EQ.0) IOPEN(2)=80
            IF(IOPEN(2).LT.80.OR.IOPEN(2).GT.132) THEN
               ERRMES='RECL value must be minimal 80 and maximal 132'
               GOTO 90
            END IF
            IF(IOPEN(3).EQ.0) IOPEN(3)=100
            IF(IOPEN(5).EQ.0) THEN
*              define blocking factor
               IOPEN(5)=3200/IOPEN(2)
               IF(IOPEN(5).EQ.0) IOPEN(5)=1
            END IF
         END IF
         IF(IOPEN(13).EQ.0) IOPEN(13)=LOCAL
      ELSE
*        OLD data set
         IOPEN(3)=0
         IOPEN(4)=0
         IOPEN(5)=0
*        assume seq and binary if no specified
         IF(IOPEN(11).EQ.0) IOPEN(11)=1
         IF(IOPEN(12).EQ.0) IOPEN(12)=1
      END IF
*     number of buffers default 2
      IF(IOPEN(6).EQ.0) IOPEN(6)=2
*
      IF(IOPEN(8).GT.0 .AND. IOPEN(23).GT.0) THEN
         ERRMES='SPLITMB and SPLITEV not allowed simultaneuosly'
         GOTO 90
      ENDIF
*
*     concatenate file_name with the suffix .<letter><digit><digit>
*               for SPLITMB  or SPLITEV only, not for UNIXTAPE
*
      IF(IOPEN(8).GT.0 .OR. IOPEN(23).GT.0) THEN
         IF(FINAME.EQ.' ') THEN
            IF(IOPEN(8).GT.0) THEN
               ERRMES='File_name must be given for parameter SPLITMB'
            ELSE
               ERRMES='File_name must be given for parameter SPLITEV'
            ENDIF
            GOTO 90
         END IF
         IF(IOPEN(9).EQ.1) THEN
            IF(IOPEN(8).GT.0) THEN
               ERRMES='Action READ not allowed for parameter SPLITMB'
            ELSE
               ERRMES='Action READ not allowed for parameter SPLITEV'
            ENDIF
            GOTO 90
         END IF
*
*        don't concatenate file_name with the suffix for UNIXTAPE
         IF(IOPEN(15).NE.5) THEN
            L = LENB(FINAME)
*           don't concatenate with .A00
*           if the suffix is .<letter><digit><digit> or .<C>7*<digit>
            IF( ( L.LE.LEN(FINAME)-4 ) .AND.
     +          ((L.LE.4) .OR. (FINAME(L-3:L-3).NE.'.') .OR.
     +                     (IFPWNM(FINAME(L-2:L)).LT.0)) .AND.
     +          ((L.LE.9) .OR. (FINAME(L-8:L-8).NE.'.') .OR.
     +                     (IFPWNM(FINAME(L-7:L)).LT.0)) )
     +             FINAME(L+1:) = '.A00'
         END IF
*
      END IF
*
      IF(IOPEN(11).EQ.3.)THEN
*        force record separation for keyed access
         IOPEN(14) = 1
      ENDIF
      IF(IOPEN(11).EQ.5.AND.IOPEN(12).EQ.2)THEN
         ERRMES = 'Text mode not allowed for ordered access'
         GOTO 90
      ENDIF
      IF(IOPEN(11).EQ.5.AND.IOPEN(2).NE.0.AND.IOPEN(2).LT.160)THEN
         ERRMES = 'RECL no less than 160 for ordered access'
         GOTO 90
      ENDIF
      IF(IOPEN(11).EQ.5.AND.IOPEN(8).NE.0)THEN
         ERRMES = 'SPLITMB not allowed for ordered access'
         GOTO 90
      ENDIF
      IF(IOPEN(11).EQ.5.AND.IOPEN(15).EQ.1)THEN
         ERRMES = 'NOOPEN not allowed for ordered access'
         GOTO 90
      ENDIF
      IF(IOPEN(11).EQ.5.AND.IOPEN(15).EQ.2)THEN
         ERRMES = 'DEFER not allowed for ordered access'
         GOTO 90
      ENDIF
      IF(IOPEN(11).EQ.5.AND.IOPEN(15).EQ.5)THEN
         ERRMES = 'UNIXTAPE not allowed for ordered access'
         GOTO 90
      ENDIF
      IF(IOPEN(11).EQ.5.AND.IOPEN(9).EQ.3)THEN
*        change READWRITE to MODIFY
         IOPEN(9) = 4
      ENDIF
      IF(IOPEN(25).NE.0.AND.(IOPEN(11).NE.5.OR.IOPEN(9).NE.4)) THEN
         ERRMES = 'REPAIR can be used with ORDERED and MODIFY'
         GOTO 90
      ENDIF
      IF(IOPEN(26).NE.0.AND.(IOPEN(11).NE.5.OR.IOPEN(9).NE.4)) THEN
         ERRMES = 'NOSYNC can be used with ORDERED and MODIFY'
         GOTO 90
      ENDIF
*
*     AMPEX and ACS make sense with STAGE or STAGEKEEP only
      IF(IOPEN(24).EQ.1 .AND. IOPEN(15).NE.3 .AND. IOPEN(15).NE.4) THEN
         ERRMES = 'AMPEX can be used only with STAGE or STAGEKEEP'
         GOTO 90
      ENDIF
      IF(IOPEN(24).EQ.2 .AND. IOPEN(15).NE.3 .AND. IOPEN(15).NE.4) THEN
         ERRMES = 'ACS can be used only with STAGE or STAGEKEEP'
         GOTO 90
      ENDIF
*
*     VOLSER can be used only with DEFER
      IF(VONAME.NE.' ' .AND. IOPEN(15).NE.2) THEN
         ERRMES = 'VOLSER can be used only with DEFER'
         GOTO 90
      ENDIF
*     Before MKOPEN it is a good idea to reserve DLUN for cartridge
*     datafile if necessary
      IF(IOPEN(19).NE.0) THEN
         LDANAM=LENB(DANAME)
         CALL PNVI(IOPEN(19),STRLUN,LSTR)
         LDANAM= MIN(LDANAM,7)
         CALL FUPARM(IOPEN(19)
     +,              DANAME(1:LDANAM)//' '//STRLUN(1:LSTR)//' DCART'
     +,              LISTI)
      ENDIF
*
      IF(IVSIDX.LE.0) THEN
         KPOS = IVSPOS-1
      ELSE
         KPOS = ITX(2,IVSIDX)
      ENDIF
*
      IF(IFNIDX.LE.0) THEN
         LPOS = IFNPOS-1
      ELSE
         LPOS = ITX(2,IFNIDX)
      ENDIF
*
      IF(IFNPOS.LE.LPOS .AND. IVSPOS.LE.KPOS) THEN
         CALL MKOPEN(IOPEN,FINAME,DANAME,HONAME,VONAME,
     +               STMT(IFNPOS:LPOS),STMT(IVSPOS:KPOS),IER)
      ELSEIF(IFNPOS.LE.LPOS) THEN
         CALL MKOPEN(IOPEN,FINAME,DANAME,HONAME,VONAME,
     +               STMT(IFNPOS:LPOS),' ',IER)
      ELSEIF(IVSPOS.LE.KPOS) THEN
         CALL MKOPEN(IOPEN,FINAME,DANAME,HONAME,VONAME,
     +               ' ',STMT(IVSPOS:KPOS),IER)
      ELSE
         CALL MKOPEN(IOPEN,FINAME,DANAME,HONAME,VONAME,
     +               ' ',' ',IER)
      ENDIF
*
      GOTO 100
*
   90 CALL FQCSET('OPENERROR',1)
*
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
*
  100 RETURN
      END
#else
*CMZ :  1.00/16 11/03/96  15.14.41  by  Unknown
*CMZU:  1.00/14 27/02/96  17.26.15  by  Dave Sankey at RAL
*CMZU:  1.00/13 31/08/95  15.38.16  by  Unknown
*CMZ :  1.00/00 16/12/94  17.00.00  by  Pavel Binko
*-- Author :    Volker Blobel   16/12/94
      SUBROUTINE LAOPEN
*
************************************************************************
*
*     OPEN STATEMENT
*
*     Open parameters
*
*      I   IOPEN(I)
*     --   ---------
*      1   unit number (UNIT)
*      2   physical record length (RECL)
*      3   number of records - primary space (NREC)
*      4   number of records - secondary space (NREC2)
*      5   blocking factor (BLFACTOR)
*
*      6   number of buffers (BUFNO)
*      7   not yet used
*      8   number of MB per file (SPLITMB)
*      9   action  ( 1=read   2=write  3=readwrite  4=modify )
*     10   status  ( 1=old    2=new    3=scratch )
*
*     11   access  ( 1=seq    2=dir    3=keyed   4=special  5=ordered )
*     12   form    ( 1=binary 2=text )
*     13   wordfmt ( 0=local  1=ieee   2=ibm     3=vax      4=dec )
*     14   recsep  ( 1=recsep, logical records separating )
*     15   noopen  ( 1=NOOPEN, open should NOT be done )
*                  ( 2=DEFER, DD card with DEFER parameter exists )
*                  ( 3=STAGE )  ( 4 = STAGE and KEEP in the shift pool )
*                  ( 5=UNIXTAPE = tapes on UNIX machines )
*
*     16   compact ( 1=compact=default  2=nocompact for export )
*     17   catalog ( 1=shift )
*     18   skipcorev ( 1=skip corrupted event at the end of file )
*     19   data file (son) unit number (DUNIT)
*     20   flag "FATMEN catalog used" (1=used)
*          flag "FATMEN must be updated in FICLOS" (2=update)
*     21   1=DSTAGE has been set
*     22   1=DDEFER has been set
*     23   number of events per file (SPLITEV)
*     24   remote tape server
*          ( 0 = IBM (dsyibm), 1 = AMPEX (dice1), 2 = ACS (dice2) )
*     25   REPAIR mode (ordered access)
*     26   NOSYNC flag (ordered access) prevents the call to FPSYNC
*     27   not yet used
*     28   not yet used
*     29   not yet used
*     30   not yet used
*
*     DANAME = data_name
*     GENAME = generic_name
*     FINAME = file_name
*     HONAME = host_name
*     VOLSER = volume_name ( only private cartridges on the IBMDESY)
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
      PARAMETER (LASTCL=60)
      CHARACTER CTEXT*20, DANAME*16
      CHARACTER FINAME*256, HONAME*127, HONAM2*127, MEDIA*8, VONAME*6
      CHARACTER*9 TEXTCL(LASTCL)
      INTEGER LISTI(ILIMAX)
      DATA LISTI/0,1,1,1,0/
      CHARACTER*2 STRLUN
*
      INTEGER NUMBCL(LASTCL), IOPEN(IOPMAX)
      LOGICAL FIRST
      DATA TEXTCL/'UNIT','RECL','NREC','NREC2','BLFACTOR','BUFNO',
     +            'NOTUSED','SPLITMB','DUNIT','SPLITEV',
     +            'ACTION','READ','WRITE','READWRITE','MODIFY',
     +            'STATUS','OLD','NEW','SCR',
     +            'ACCESS','SEQ','DIR','KEYED','SPECIAL','ORDERED',
     +            'FORM','BINARY','TEXT',
     +            'WORDFMT','WFLOCAL','WFIEEE','WFIBM','WFVAX','WFDEC',
     +            'RECSEP','RECSEP',
     +            'NOOPEN','NOOPEN','DEFER','STAGE','STAGEKEEP',
     +                     'UNIXTAPE',
     +            'COMPACT','COMP','NOCOMP',
     +            'CATALOG','SHIFT',
     +            'SKIPCOREV','SKIPCOREV',
     +            'DSTAGE','DSTAGE',
     +            'DDEFER','DDEFER',
     +            'AMPEX','AMPEX','ACS',
     +            'REPAIR','REPAIR',
     +            'NOSYNC','NOSYNC'/
*
      DATA NUMERIC/10/
      DATA NUMBCL/-1,-2,-3,-4,-5,-6,
     +            -7,-8,-19,-23,
     +            -9,1,2,3,4,
     +            -10,1,2,3,
     +            -11,1,2,3,4,5,
     +            -12,1,2,
     +            -13,0,1,2,3,4,
     +            -14,1,
     +            -15,1,2,3,4,5,
     +            -16,1,2,
     +            -17,1,
     +            -18,1,
     +            -21,1,
     +            -22,1,
     +            -24,1,2,
     +            -25,1,
     +            -26,1/
*
      CALL FQCOND('PRINT',IPFLAG)
*
      CALL TEXT32
*
      DO 10 I=1,IOPMAX
   10 IOPEN(I)=0
*
      DANAME = ' '
      FINAME = ' '
      HONAME = ' '
      MEDIA  = ' '
      VONAME = ' '
*
      FIRST=.TRUE.
      IEPOS =0
*
*     starting with I=2 (I=1 is OPEN or similar)
      I=2
      IFNIDX = -1
      IFNPOS = 0
      IVSIDX = -1
      IVSPOS = 0
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
      NC=ITX(2,I)-ITX(1,I)+1
*
*     special case of FILE = list of file_names
      IF(CTEXT.EQ.'FILE'.AND.NEQ.EQ.1) THEN
         I=I+2
         IF(ITX(3,I).NE.2.AND.ITX(3,I).NE.3) THEN
*           no text
            IEPOS=ITX(1,I)
            GOTO 90
         END IF
         IFNIDX = I
         IFNPOS = ITX(1,I)
         CALL FPFSTN(FINAME,STMT(1:ITX(2,I)),IFNPOS)
         GOTO 50
      END IF
*
*     special case of HOST = host_name
      IF(CTEXT.EQ.'HOST'.AND.NEQ.EQ.1) THEN
         I=I+2
         IF(ITX(3,I).NE.2.AND.ITX(3,I).NE.3) THEN
*           no text
            IEPOS=ITX(1,I)
            GOTO 90
         END IF
         HONAME=STMT(ITX(1,I):ITX(2,I))
         GOTO 50
      END IF
*
*     special case of VOLSER = list of serial_numbers of volumes
      IF(CTEXT.EQ.'VOLSER'.AND.NEQ.EQ.1) THEN
         I=I+2
         IF(ITX(3,I).NE.2.AND.ITX(3,I).NE.3) THEN
*           no text
            IEPOS=ITX(1,I)
            GOTO 90
         END IF
         IVSIDX = I
         IVSPOS = ITX(1,I)
         CALL FPFSTN(VONAME,STMT(1:ITX(2,I)),IVSPOS)
         GOTO 50
      END IF
*
      IF(NEQ.NE.0) THEN
*        for = sign compare title text
         DO 30 J=1,LASTCL
         IF(NUMBCL(J).GT.0) GOTO 30
         IF(TEXTCL(J).EQ.CTEXT) THEN
            IT=-NUMBCL(J)
            IF(J.LE.NUMERIC) THEN
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
   50 IF(I.GE.NTX) GOTO 60
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
*     end of the command string analysis
*
   60 CONTINUE
*
*     Reserve the unit number UNIT=x (or the first free unit number)
      LUN = IOPEN(1)
      CALL FUPARM(LUN,DANAME,LISTI)
      IOPEN(1) = LUN
      LX = LUNIT(LUN)
*
*     Save KEEP flag (can be used in index files or in FATMEN interface)
      IF(IOPEN(15).EQ.4) THEN
         INFU(42,LX)=IOPEN(15)
      ENDIF
*
*
*     check host_name
      HONAM2=HONAME
      CALL UCASE(HONAM2)
      IF(HONAM2.EQ.'LOCAL') HONAME=' '
*
*     check consistency of OPEN parameters and insert defaults
*
      IF(IOPEN(9).EQ.1) THEN
*        action = read
         IF(IOPEN(10).EQ.2.OR.IOPEN(10).EQ.3) THEN
            ERRMES='Illegal status NEW or SCR for READ'
            GOTO 90
         ELSE IF(FINAME.EQ.' ') THEN
            ERRMES='File_name must be given'
            GOTO 90
         END IF
*        default for READ is OLD
         IF(IOPEN(10).EQ.0) IOPEN(10)=1
      ELSE IF(IOPEN(9).EQ.2) THEN
*        action = write
         IF(IOPEN(10).EQ.3.) THEN
            ERRMES='Illegal status SCR for WRITE'
            GOTO 90
         ELSE IF(FINAME.EQ.' ') THEN
            ERRMES='File_name must be given'
            GOTO 90
         END IF
*        default for write is new
         IF(IOPEN(10).EQ.0) IOPEN(10)=2
      ELSE IF(IOPEN(9).EQ.3) THEN
*        action is readwrite
         IF(IOPEN(10).EQ.0) THEN
            IF(FINAME.EQ.' ') THEN
*              default for READWRITE is SCR (for file_name not given)
               IOPEN(10)=3
            ELSE
*              default for READWRITE is OLD (for file_name given)
               IOPEN(10)=1
            END IF
         END IF
         IF(IOPEN(10).NE.3) THEN
*           no SCR unit
            IF(FINAME.EQ.' ') THEN
               ERRMES='File_name must be given'
               GOTO 90
            END IF
         END IF
      ELSE IF(IOPEN(9).EQ.4) THEN
*        action is modify
         IF(IOPEN(10).EQ.2.OR.IOPEN(10).EQ.3) THEN
            ERRMES='Illegal status NEW or SCR for MODIFY'
            GOTO 90
         ELSE IF(FINAME.EQ.' ') THEN
            ERRMES='File_name must be given'
            GOTO 90
         END IF
*        default for MODIFY is OLD
         IF(IOPEN(10).EQ.0) IOPEN(10)=1
      ELSE
*        action not specified IOPEN(9) = 0
         IF(IOPEN(10).EQ.2) THEN
*           NEW
            IF(FINAME.EQ.' ') THEN
               ERRMES='File_name must be given'
               GOTO 90
            END IF
*           default for NEW is WRITE
            IOPEN(9)=2
         ELSE IF(IOPEN(10).EQ.3) THEN
*           SCR
*           default for SCR is READWRITE
            IOPEN(9)=3
         ELSE IF(FINAME.EQ.' ') THEN
            ERRMES='File_name must be given'
            GOTO 90
         END IF
      END IF
*
*     if action and status are not given, default is READ and OLD
      IF(IOPEN(9).EQ.0) IOPEN(9)=1
      IF(IOPEN(10).EQ.0) IOPEN(10)=1
*
      IF(IOPEN(9).NE.1.AND.IOPEN(2).EQ.0) THEN
         ERRMES='RECL must be given for WRITE, READWRITE or MODIFY'
         GOTO 90
      END IF
*
      IF(IOPEN(15).EQ.5.AND.IOPEN(2).EQ.0) THEN
         ERRMES='RECL must be given for UNIXTAPE'
         GOTO 90
      END IF
*
      IF(IOPEN(10).EQ.2.OR.IOPEN(10).EQ.3) THEN
*        NEW or SCR file
*        assume seq and binary if not given
         IF(IOPEN(11).EQ.0) IOPEN(11)=1
         IF(IOPEN(12).EQ.0) IOPEN(12)=1
         IF(IOPEN(12).EQ.1) THEN
*           binary form
            IF(IOPEN(2).EQ.0) IOPEN(2)=3600
            IF(IOPEN(3).EQ.0) IOPEN(3)=100
            IF(IOPEN(5).NE.0.AND.IOPEN(5).NE.1) THEN
               ERRMES='No blocking factor for binary files allowed'
               GOTO 90
            END IF
            IOPEN(5)=1
         ELSE IF(IOPEN(12).EQ.2) THEN
*           text form
*           default RECL is 80
            IF(IOPEN(2).EQ.0) IOPEN(2)=80
            IF(IOPEN(2).LT.80.OR.IOPEN(2).GT.132) THEN
               ERRMES='RECL value must be minimal 80 and maximal 132'
               GOTO 90
            END IF
            IF(IOPEN(3).EQ.0) IOPEN(3)=100
            IF(IOPEN(5).EQ.0) THEN
*              define blocking factor
               IOPEN(5)=3200/IOPEN(2)
               IF(IOPEN(5).EQ.0) IOPEN(5)=1
            END IF
         END IF
         IF(IOPEN(13).EQ.0) IOPEN(13)=LOCAL
      ELSE
*        OLD data set
         IOPEN(3)=0
         IOPEN(4)=0
         IOPEN(5)=0
*        assume seq and binary if no specified
         IF(IOPEN(11).EQ.0) IOPEN(11)=1
         IF(IOPEN(12).EQ.0) IOPEN(12)=1
      END IF
*     number of buffers default 2
      IF(IOPEN(6).EQ.0) IOPEN(6)=2
*
      IF(IOPEN(8).GT.0 .AND. IOPEN(23).GT.0) THEN
         ERRMES='SPLITMB and SPLITEV not allowed simultaneuosly'
         GOTO 90
      ENDIF
*
*     concatenate file_name with the suffix .<letter><digit><digit>
*               for SPLITMB  or SPLITEV only, not for UNIXTAPE
*
      IF(IOPEN(8).GT.0 .OR. IOPEN(23).GT.0) THEN
         IF(FINAME.EQ.' ') THEN
            IF(IOPEN(8).GT.0) THEN
               ERRMES='File_name must be given for parameter SPLITMB'
            ELSE
               ERRMES='File_name must be given for parameter SPLITEV'
            ENDIF
            GOTO 90
         END IF
         IF(IOPEN(9).EQ.1) THEN
            IF(IOPEN(8).GT.0) THEN
               ERRMES='Action READ not allowed for parameter SPLITMB'
            ELSE
               ERRMES='Action READ not allowed for parameter SPLITEV'
            ENDIF
            GOTO 90
         END IF
*
*        don't concatenate file_name with the suffix for UNIXTAPE
         IF(IOPEN(15).NE.5) THEN
            L = LENB(FINAME)
*           don't concatenate with .A00
*           if the suffix is .<letter><digit><digit> or .<C>7*<digit>
            IF( ( L.LE.LEN(FINAME)-4 ) .AND.
     +          ((L.LE.4) .OR. (FINAME(L-3:L-3).NE.'.') .OR.
     +                     (IFPWNM(FINAME(L-2:L)).LT.0)) .AND.
     +          ((L.LE.9) .OR. (FINAME(L-8:L-8).NE.'.') .OR.
     +                     (IFPWNM(FINAME(L-7:L)).LT.0)) )
     +             FINAME(L+1:) = '.A00'
         END IF
*
      END IF
*
      IF(IOPEN(11).EQ.3.)THEN
*        force record separation for keyed access
         IOPEN(14) = 1
      ENDIF
      IF(IOPEN(11).EQ.5.AND.IOPEN(12).EQ.2)THEN
         ERRMES = 'Text mode not allowed for ordered access'
         GOTO 90
      ENDIF
      IF(IOPEN(11).EQ.5.AND.IOPEN(2).NE.0.AND.IOPEN(2).LT.160)THEN
         ERRMES = 'RECL no less than 160 for ordered access'
         GOTO 90
      ENDIF
      IF(IOPEN(11).EQ.5.AND.IOPEN(8).NE.0)THEN
         ERRMES = 'SPLITMB not allowed for ordered access'
         GOTO 90
      ENDIF
      IF(IOPEN(11).EQ.5.AND.IOPEN(15).EQ.1)THEN
         ERRMES = 'NOOPEN not allowed for ordered access'
         GOTO 90
      ENDIF
      IF(IOPEN(11).EQ.5.AND.IOPEN(15).EQ.2)THEN
         ERRMES = 'DEFER not allowed for ordered access'
         GOTO 90
      ENDIF
      IF(IOPEN(11).EQ.5.AND.IOPEN(15).EQ.5)THEN
         ERRMES = 'UNIXTAPE not allowed for ordered access'
         GOTO 90
      ENDIF
      IF(IOPEN(11).EQ.5.AND.IOPEN(9).EQ.3)THEN
*        change READWRITE to MODIFY
         IOPEN(9) = 4
      ENDIF
      IF(IOPEN(25).NE.0.AND.(IOPEN(11).NE.5.OR.IOPEN(9).NE.4)) THEN
         ERRMES = 'REPAIR can be used with ORDERED and MODIFY'
         GOTO 90
      ENDIF
      IF(IOPEN(26).NE.0.AND.(IOPEN(11).NE.5.OR.IOPEN(9).NE.4)) THEN
         ERRMES = 'NOSYNC can be used with ORDERED and MODIFY'
         GOTO 90
      ENDIF
*
*     AMPEX and ACS make sense with STAGE or STAGEKEEP only
      IF(IOPEN(24).EQ.1 .AND. IOPEN(15).NE.3 .AND. IOPEN(15).NE.4) THEN
         ERRMES = 'AMPEX can be used only with STAGE or STAGEKEEP'
         GOTO 90
      ENDIF
      IF(IOPEN(24).EQ.2 .AND. IOPEN(15).NE.3 .AND. IOPEN(15).NE.4) THEN
         ERRMES = 'ACS can be used only with STAGE or STAGEKEEP'
         GOTO 90
      ENDIF
*
*     VOLSER can be used only with DEFER
      IF(VONAME.NE.' ' .AND. IOPEN(15).NE.2) THEN
         ERRMES = 'VOLSER can be used only with DEFER'
         GOTO 90
      ENDIF
*     Before MKOPEN it is a good idea to reserve DLUN for cartridge
*     datafile if necessary
      IF(IOPEN(19).NE.0) THEN
         LDANAM=LENB(DANAME)
         CALL PNVI(IOPEN(19),STRLUN,LSTR)
         LDANAM= MIN(LDANAM,7)
         CALL FUPARM(IOPEN(19)
     +,              DANAME(1:LDANAM)//' '//STRLUN(1:LSTR)//' DCART'
     +,              LISTI)
      ENDIF
*
      IF(IVSIDX.LE.0) THEN
         KPOS = IVSPOS-1
      ELSE
         KPOS = ITX(2,IVSIDX)
      ENDIF
*
      IF(IFNIDX.LE.0) THEN
         LPOS = IFNPOS-1
      ELSE
         LPOS = ITX(2,IFNIDX)
      ENDIF
*
      IF(IFNPOS.LE.LPOS .AND. IVSPOS.LE.KPOS) THEN
         CALL MKOPEN(IOPEN,FINAME,DANAME,HONAME,VONAME,
     +               STMT(IFNPOS:LPOS),STMT(IVSPOS:KPOS),IER)
      ELSEIF(IFNPOS.LE.LPOS) THEN
         CALL MKOPEN(IOPEN,FINAME,DANAME,HONAME,VONAME,
     +               STMT(IFNPOS:LPOS),' ',IER)
      ELSEIF(IVSPOS.LE.KPOS) THEN
         CALL MKOPEN(IOPEN,FINAME,DANAME,HONAME,VONAME,
     +               ' ',STMT(IVSPOS:KPOS),IER)
      ELSE
         CALL MKOPEN(IOPEN,FINAME,DANAME,HONAME,VONAME,
     +               ' ',' ',IER)
      ENDIF
*
      GOTO 100
*
   90 CALL FQCSET('OPENERROR',1)
*
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
*
  100 RETURN
      END
#endif
