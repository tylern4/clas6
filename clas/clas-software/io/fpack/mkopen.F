#ifndef Linux
*CMZ :  1.00/16 24/03/96  17.44.53  by  Wiesiek Dlugosz
*CMZ :  1.00/14 02/03/96  12.01.39  by  Wiesiek Dlugosz
*CMZ :  1.00/13 20/02/96  16.18.09  by  Wiesiek Dlugosz
*CMZ :  1.00/12 11/02/96  18.25.41  by  Wiesiek Dlugosz
#endif /* not Linux */
*CMZ :  1.00/02 15/02/95  09.53.20  by  Unknown
*CMZ :  1.00/01 16/01/95  15.34.54  by  Unknown
*CMZ :  1.00/00 16/12/94  17.00.00  by  Pavel Binko
*-- Author :    Volker Blobel   16/12/94
      SUBROUTINE MKOPEN(IOPEN,FNS,DANAME,HONAME,VONAME,
     +                  ADDNAM,ADD2,IER)
*
      CHARACTER*(*) FNS, DANAME, HONAME, VONAME, ADDNAM, ADD2
*
************************************************************************
*
*     Make OPEN
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
*KEEP,FBUFFER.
*     buffer common
      PARAMETER (NBUF=100000)
      COMMON/FBUF/IBUF(NBUF)
      SAVE/FBUF/
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
*
      INTEGER IOPEN(IOPMAX), LISTI(ILIMAX)
*
      LOGICAL EXS, OPN
      CHARACTER*7 SEQ, DIR, KYD
      CHARACTER FN*256, FINAME*256, HONAM2*127
      CHARACTER STRERR*12, STRREC*12, STRBUF*12
      CHARACTER STA*8, FRM*11, ACT*9
      INTEGER JBUF(12), IOLAB(2)
*     flag "direct access needed"
      LOGICAL DIRACC
*     flag "entered via alternative entry"
      LOGICAL OSPL
      DATA LX/0/,LENBUF/0/
*
      OSPL = .FALSE.
*
      LUN = IOPEN(1)
*
*     save the flag STAGEKEEP for FUPARM
      INFU42 = 0
      IF(LUN.NE.0) THEN
         LX = LUNIT(LUN)
         IF(INFU(42,LX).NE.0) INFU42 = INFU(42,LX)
      ENDIF
*
*     parameters for FUPARM
      LISTI(1)=IOPEN(2)
      LISTI(2)=IOPEN(11)
      LISTI(3)=IOPEN(12)
      LISTI(4)=IOPEN(9)
      LISTI(5)=IOPEN(13)
      CALL FUPARM(LUN,DANAME,LISTU)
*
      IOPEN(1) = LUN
      LX = LUNIT(LUN)
*
*     restore the flag STAGEKEEP after FUPARM
      IF(INFU42.NE.0) INFU(42,LX) = INFU42
*
      IF( ADDNAM.NE.' ' .AND. IOPEN(15).NE.1) THEN
         IDXNAM = INDXA(IBUF,1000*LUN+6,(LENB(ADDNAM)+3)/4)
         IF(IDXNAM.LE.0)CALL LOGERROR('NO SPACE FOR ADD. NAMES')
         CALL TXTOHL(ADDNAM,IBUF(IDXNAM+1),IBUF(IDXNAM))
      END IF
*
*     VOLSER = volume_name ( only private cartridges on the IBMDESY)
      IF(VONAME.NE.' ') THEN
         ERRMES='Parameter VOLSER not implemented'
         GOTO 100
      END IF
*
*
*     Restore INFU from IOPEN
      CALL FPINFU(IOPEN,LX,FNS)
*
*     converting integer IOPEN(6) into character*2 STRBUF
      CALL PNVI(IOPEN(6),STRBUF,LENBUF)
*
      GOTO 20
*
************************************************************************
*
      ENTRY MKOSPL(IOPEN,FNS,DANAME,HONAME,VONAME,
     +             IER)
*
************************************************************************
*
      OSPL = .TRUE.
*
   20 FN = FNS
*
*     check host_name
      HONAM2=HONAME
      CALL UCASE(HONAM2)
      IF(HONAM2.EQ.'LOCAL') HONAME=' '
*
*     parameter NOOPEN specified
   21 IF(IOPEN(15).EQ.1) THEN
         NORMES='File connected to FPACK, opened must be by the user'
*        store data name, file name and host name
         FILNAM(LX) = FNS
         DATNAM(LX) = DANAME
         HOSNAM(LX) = HONAME
         DTFLNM(LX) = ' '
         GOTO 110
      END IF
*
      LUN=IOPEN(1)
      LX = LUNIT(LUN)
*     store data name, file name and host name
      FILNAM(LX) = FN
      DATNAM(LX) = DANAME
      HOSNAM(LX) = HONAME
      DTFLNM(LX) = FN
      DTFLHO(LX) = HONAME
*     set default for media type; below it may be changed
      DTFLMD(LX) = 'DISK'
      RLFLNM(LX) = FN
      RLFLHO(LX) = HONAME
*     set default for media type; below it may be changed
      RLFLMD(LX) = 'DISK'
*
      DTFLKB(LX) = 0
      DTFLPS(LX) = 0
      DTFLVS(LX) = ' '
      INFU(IRSTU+7,LX) = 0
      INFU(28,LX) = INFU(15,LX)
*
      FINAME = FN
      LENFIN = LENB(FINAME)
*
*     check file existence and file properties
*     (not for READWRITE, NOOPEN, DEFER, STAGE, UNIXTAPE, SHIFT)
*
      IF(HONAME.EQ.' '.AND.FINAME.NE.' '
     +   .AND.IOPEN(9).NE.3.AND.IOPEN(15).EQ.0.AND.IOPEN(17).EQ.0
     +   ) THEN
         INQUIRE(FILE=FINAME(1:LENFIN),EXIST=EXS,OPENED=OPN,
     +      SEQUENTIAL=SEQ,DIRECT=DIR,IOSTAT=IOS)
         IF(OPN) THEN
            ERRMES='File already opened'
            GOTO 100
         END IF
         IF(EXS) THEN
*           file existing, not allowed is NEW and SCR
            IF(IOPEN(10).EQ.2.OR.IOPEN(10).EQ.3) THEN
               ERRMES='File existing for status = NEW or SCR'
               GOTO 100
            END IF
         ELSE
*           file not existing, not allowed is OLD
            IF(IOPEN(10).EQ.1) THEN
               ERRMES='File not existing for status = OLD'
               GOTO 100
            END IF
            SEQ='YES'
            DIR='YES'
            KYD='YES'
         END IF
*
         IF((IOPEN(11).EQ.1.AND.SEQ.EQ.'NO').OR.
     +      (IOPEN(11).EQ.2.AND.DIR.EQ.'NO')
     +   ) THEN
            ERRMES='Requested access mode not possible'
            GOTO 100
         END IF
      END IF
*
*     form
      IF(IOPEN(12).EQ.1) THEN
         FRM='UNFORMATTED'
      ELSE IF(IOPEN(12).EQ.2) THEN
         FRM='FORMATTED'
      END IF
*
      IOP11 = IOPEN(11)
      IOP9 = IOPEN(9)
      IAGAIN = 0
      DIRACC = IOPEN(11).EQ.5 .OR. IOPEN(11).EQ.2
555   CONTINUE
      IF(DIRACC)THEN
*        ordered or direct access
         IF(IOPEN(9).NE.2.AND.IAGAIN.EQ.0
     +     )THEN
*           first open file as seq. to determine record length
            IOP11 = 1
         ELSE
*           open as direct
            IOP11 = 2
         END IF
         IF(IOPEN(9).NE.1 .AND.IOPEN(9).NE.2) THEN
*     convert any non-(read or write) mode to modify
            IOP9  = 4
         END IF
      END IF
*     status
      IF(IOPEN(10).EQ.1) THEN
         STA='OLD'
      ELSE IF(IOPEN(10).EQ.2) THEN
         STA='NEW'
      ELSE IF(IOPEN(10).EQ.3) THEN
         STA='SCRATCH'
      END IF
*     action
      IF(IOP9.EQ.1) THEN
         ACT='READ'
      ELSE IF(IOP9.EQ.2) THEN
         ACT='WRITE'
      ELSE IF(IOP9.EQ.3.OR.IOP9.EQ.4) THEN
         ACT='READWRITE'
      END IF
*
      IF(HONAME.NE.' ') THEN
         IF(IOPEN(12).EQ.2) THEN
            ERRMES='No network support for text files'
            GOTO 100
         END IF
         IF(FINAME.EQ.' ') THEN
            ERRMES='Remote file must be named'
            GOTO 100
         END IF
*
* ------ remote open --------------------------------------------
*
         IF(IOP9.EQ.4) ACT='MODIFY'
         NREC =IOPEN(3)
         NREC2=IOPEN(4)
         IF(NREC2.EQ.0) NREC2=MAX0(1,NREC/5)
         CALL FPOPEN(LUN,HONAME,FINAME,ACT,IOPEN(2),NREC,NREC2,IOS)
         IF(IOS.GT.0) THEN
            CALL PNVI(IOS,STRERR,LENERR)
            ERRMES='Remote STAGE error '//STRERR(1:LENERR)
            GOTO 100
         END IF
         IF(IOS.NE.0) THEN
            ERRMES='Remote OPEN failed'
            GOTO 100
         END IF
      ELSE
*
* ------ local open ---------------------------------------------
*
         IF(FINAME.NE.' ') THEN
*
*
            IF(IOP11.EQ.1.AND.IOPEN(15).EQ.2) THEN
*              for SEQ files and DEFER parameter
               ERRMES='Parameter DEFER not implemented'
               GOTO 100
            END IF
*
            IF((IOP11.EQ.1
     +             ) .AND. (IOPEN(15).EQ.3.OR.IOPEN(15).EQ.4)) THEN
*              for SEQ (and maybe DIR) files and STAGE parameter
               ERRMES='Parameter STAGE not implemented'
               GOTO 100
            END IF
*
            IF(IOP11.EQ.1.AND.IOPEN(15).EQ.5) THEN
*              for SEQ files and UNIXTAPE parameter
               CALL FPSOPE(LUN,FINAME(1:LENFIN),ACT(1:LENB(ACT)),IOS)
               GOTO 90
            END IF
*
            IF(IOP11.EQ.1.AND.IOPEN(15).EQ.0) THEN
* --------- sequential access------------------------------------
               IF(IOPEN(12).EQ.1) THEN
*                 open binary file from C
                  CALL FPSOPE(LUN,FINAME(1:LENFIN),
     +                       ACT(1:LENB(ACT)),IOS)
               ELSE
                  OPEN(LUN,FILE=FINAME(1:LENFIN),FORM=FRM,
     +                 ACCESS='SEQUENTIAL',
     +                 STATUS=STA,
     +                 IOSTAT=IOS)
               END IF
            ELSE IF(IOP11.EQ.2) THEN
* --------- direct access ---------------------------------------
               IF(IOPEN(12).EQ.1) THEN
*                 open binary file from C
                  IF( ACT.EQ.'READWRITE' ) THEN
                     CALL FPSOPE(LUN,FINAME(1:LENFIN),
     +                          'UPDATE',        IOS)
                  ELSE
                     CALL FPSOPE(LUN,FINAME(1:LENFIN),
     +                          ACT(1:LENB(ACT)),IOS)
                  ENDIF
               ELSE
                  OPEN(LUN,FILE=FINAME(1:LENFIN),FORM=FRM,
     +                 ACCESS='DIRECT',RECL=IOPEN(2),
     +                 STATUS=STA,
     +                 IOSTAT=IOS)
               END IF
            ELSE IF(IOP11.EQ.3) THEN
* --------- keyed access-----------------------------------------
               ERRMES='Keyed access not implemented for this machine'
               GOTO 100
            ELSE IF(IOP11.EQ.4) THEN
* --------- special access---------------------------------------
               NORMES='File connected to FPACK, '//
     +                'opened must be by the user'
               GOTO 110
            END IF
         ELSE
*           no file name specified
*           set the flag for SCR file
*           such files are always opened from Fortran
            INFU(8,LX)=-1
            IF(IOP11.EQ.1) THEN
* --------- sequential access------------------------------------
               OPEN(LUN,FORM=FRM,
     +              ACCESS='SEQUENTIAL',
     +              STATUS=STA,
     +              IOSTAT=IOS)
            ELSE IF(IOP11.EQ.2) THEN
* --------- direct access ---------------------------------------
               OPEN(LUN,FORM=FRM,
     +              ACCESS='DIRECT',
     +              RECL=IOPEN(2),
     +              STATUS=STA,
     +              IOSTAT=IOS)
            ELSE IF(IOP11.EQ.3) THEN
* --------- keyed access-----------------------------------------
            ELSE IF(IOP11.EQ.4) THEN
* --------- special access---------------------------------------
            END IF
         END IF
      END IF
   90 CONTINUE
      IF(IOS.EQ.0) THEN
         NORMES='File opened'
      ELSE
         CALL PNVI(IOS,STRERR,LENERR)
         ERRMES='File OPEN with error code IOS= '//STRERR(1:LENERR)
         GOTO 100
      END IF
      IF(DIRACC.AND.IOP11.EQ.1) THEN
         LUNRS=LUNR
         LRSAV = LR
*        seq read from direct file
         CALL FRUNIT(LUN)
         CALL FRSEQ(LUNR,1,12,JBUF,IEND)
         INFU(14,LR) = 0
         IF(IEND.EQ.0)THEN
            CALL FSWAP(JBUF,12)
            IOPEN(2) = JBUF(1)*4
         ELSE IF(IOPEN(2).EQ.0)THEN
*           default value for RECL
            IOPEN(2) = 23400
         END IF
         IAGAIN = 1
         CALL FICLOS(LUNR)
*        restore
         LUNR=LUNRS
         LR  =LRSAV
         GOTO 555
      END IF
      INFU(2,LX) = IOPEN(11)
      INFU(1,LX) = IOPEN(2)
      IF(IOPEN(11).EQ.5)THEN
         LUNWS=LUNW
         LWSAV=LW
         LUNRS=LUNR
         LRSAV=LR
         INFU(IRSTU+12,LX) = 2
         IF(INFU(1,LX).LE.0) CALL LOGERROR('RECL not defined')
         INDR = INDXA(IBUF,1000*LUN+1,INFU(1,LX)/4)
         IF( INDR.LE.0 ) CALL LOGERROR('Out of space 1')
         IBUF(INDR+1) = INFU(1,LX)/4
         IBUF(INDR+2) = 2
         IF( IOPEN(9).NE.1 ) THEN
            INDW = INDXA(IBUF,1000*LUN+2,INFU(1,LX)/4)
            IF( INDW.LE.0 ) CALL LOGERROR('Out of space 2')
            IBUF(INDW+1) = INFU(1,LX)/4
            IBUF(INDW+2) = 2
         ENDIF
         INDR = INDXA(IBUF,1000*LUN+9+1,INFU(1,LX)/4)
         IF( INDR.LE.0 ) CALL LOGERROR('Out of space 3')
         IBUF(INDR+1) = INFU(1,LX)/4
         IBUF(INDR+2) = 2
*        only one level
         INFU(25,LX) = 1
*        initial counter
         IBUF(INDR+8) = 2
         IBUF(INDR+9) = 2
*        the home block number
         IBUF(INDR+11) = 1
         IBUF(INDR+14)= 10
         IBUF(INDR+22) = 1
*        current position - 0
         IBUF(INDR+15)= 0
*        this block points to data blocks
         IBUF(INDR+17) = 1
*        the alt home block number
         IBUF(INDR+19) = 2
*        2 blocks in file
         IBUF(INDR+18) = 2
         LR = LX
         LW = LX
         LUNR = LUN
         LUNW = LUN
         IF(IOPEN(9).EQ.2)THEN
*     write
            CALL FPWIDS(IBUF(INDR+1),1)
*     write the alternative block also
            CALL FPWIDS(IBUF(INDR+1),1)
*     close new ordered file because it has been open for write
            CALL FICLOS(LUN)
*     next open will be for modify access & ...
            IOPEN(9)=4
            INFU(4,LX)=IOPEN(9)
*     ... status old
            IOPEN(10)=1
            INFU(16,LX)=IOPEN(10)
*     restore all things as before
            LUNW=LUNWS
            LW  =LWSAV
            LUNR=LUNRS
            LR  =LRSAV
*     go to open again ...
            GOTO 555
         ELSE
#ifndef Linux
*     if REPAIR flag set and MODIFY, repair the database
            IF(IOPEN(25).NE.0.AND.IOPEN(9).NE.1) THEN
               CALL FRPAIR(DANAME,IRC)
               IF(IRC.NE.0) THEN
                  PRINT *,'REPAIR failed and file is locked possibly'
                  GOTO 100
               ENDIF
            ENDIF
#endif /* not Linux */
*           read top index record
            INFU(27,LR) = 1
            CALL FRRECX(LUNR,IBUF(INDR),IBUF(INDR+1),IER)
            IF( IER.NE.0 )
     +           CALL LOGERROR('MKOPEN. Corrupted ordered file 1')
*           save label of ordered file: 'FPINDEXA'
            IOLAB(1)=IBUF(INDR+6)
            IOLAB(2)=IBUF(INDR+7)
*           check if index structure is updated right now
            IF( IBUF(INDR+8).EQ.1 .AND. IBUF(INDR+9).EQ.1 ) THEN
*              read the alternative top record
               INFU(27,LR) = 2
               CALL FRRECX(LUNR,IBUF(INDR),IBUF(INDR+1),IER)
               IF( IER.NE.0 )
     +              CALL LOGERROR('MKOPEN. Corrupted ordered file 2')
               IF( IOPEN(9).NE.1 ) THEN
                  N1 = IBUF(INDR+8)
                  N2 = IBUF(INDR+9)
                  IBUF(INDR+8) = 1
                  IBUF(INDR+9) = 1
                  IBUF(INDR+11) = 1
                  IBUF(INDR+19) = 2
                  CALL FPWIDS(IBUF(INDR+1),0)
                  IBUF(INDR+8) = N1
                  IBUF(INDR+9) = N2
                  IBUF(INDR+11) = 1
                  IBUF(INDR+19) = 2
                  CALL FPWIDS(IBUF(INDR+1),0)
               ENDIF
            ENDIF
            IF( IOPEN(9).NE.1 ) THEN
               INFU(30,LW) = IBUF(INDR+18)
               ITMP27      = INFU(27,LR)
               INFU(27,LR) = INFU(30,LW)
*              read last block to output buffer if not 'rec.sep.'
               CALL FRRECX(LUNR,IBUF(INDW),IBUF(INDW+1),IER)
               IF( IER.NE.0 )
     +              CALL LOGERROR('MKOPEN. Corrupted ordered file 3')
               INFU(27,LR) = ITMP27
               IF(IOLAB(1).EQ.IBUF(INDW+6) .AND.
     +            IOLAB(2).EQ.IBUF(INDW+7)) THEN
*                 last block is an index block
*                 so that the next data record will start
*                 from begining of next physical record
                  INFU(26,LW)=INFU(30,LW)+1
                  IBUF(INDW+2)=2
               ELSE
                  INFU(26,LW)=INFU(30,LW)
                  CALL FRCONV(IBUF(INDW+1),LOCAL,ICER)
                  IP = 2
 997              IPNEXT = (IP+11)+IBUF(INDW+IP+11)
                  IF(IPNEXT.LT.IBUF(INDW+2)) THEN
                     IP = IPNEXT
                     GOTO 997
                  ENDIF
                  JP = IP + 11
                  NJ = IBUF(INDW+JP+1)
                  KP = JP + NJ
                  INFU(IWSTU+4,LW) = 0
                  INFU(IWSTU+5,LW) = 0
                  INFU(IWSTU+8,LW) = IP
                  INFU(IWSTU+9,LW) = JP
                  INFU(IWSTU+10,LW)= KP
#ifndef Linux
               ENDIF
*              synchronize a file's in-memory state with that on the disk
*              this is the default behaviour ...
               INFU(ISGEN+12,LW) = 1
               IF(IOPEN(26).EQ.1) THEN
*                 NOSYNC flag was requested
                  INFU(ISGEN+12,LW) = 0
#endif /* not Linux */
               ENDIF
            ENDIF
            CALL FPSRCH(1,IFOUND)
         END IF
*
*        restore
         LUNW=LUNWS
         LW  =LWSAV
         LUNR=LUNRS
         LR  =LRSAV
      END IF
*
      CALL FQCOND('PRINT',IPFLAG)
      IF(IPFLAG.NE.0) THEN
         LFN = LENB(FN)
         CALL FPHIDE(FN,LFN)
         IF( INFU(15,LX).EQ.2 ) THEN
            WRITE(6,*) ' OPEN> File opened:',FN(1:LFN),' (SHIFT)'
         ELSE
            WRITE(6,*) ' OPEN> File opened:',FN(1:LFN)
            IF(INFU(31,LX).EQ.1 ) THEN
               WRITE(6,*) '       Volume serial number:',VONAME
            END IF
         END IF
      END IF
*
110   IER = 0
      GOTO 200
*
100   CALL FQCSET('OPENERROR',2)
*
      CALL FQCOND('PRINT',IPFLAG)
      IF(IPFLAG.NE.0) THEN
         LFN = LENB(FN)
         CALL FPHIDE(FN,LFN)
         WRITE(6,*) ' OPEN> ',ERRMES
         WRITE(6,*) '    File : ',FN(1:LFN)
      END IF
      ERRMES = 'OPEN failed'
c     CALL FPNXTN(LUN,FN)
c     IF( FN.NE.' ') GOTO 21
      IF( .NOT.OSPL ) THEN
         LUNBK(LX) = 0
         LUNIT(LUN) = 0
         DATNAM(LX) = ' '
         IBLA = IFRSTD(DANAME,JBLA)
      END IF
      IER = 1
*
200   RETURN
      END
