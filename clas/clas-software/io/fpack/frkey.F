#ifndef Linux
*CMZ :  1.00/16 24/03/96  15.20.03  by  Wiesiek Dlugosz
#endif /* not Linux */
*CMZU:  1.00/02 10/02/95  15.59.14  by  Wiesiek Dlugosz
*CMZ :  1.00/00 16/12/94  17.00.00  by  Pavel Binko
*-- Author :    Volker Blobel   16/12/94
      SUBROUTINE FRKEY(NAMKEY,NUM1,NUM2,NDATWD,IER)
*
      CHARACTER*(*) NAMKEY
*
************************************************************************
*
*     Read and return next key block (segment code 0 or 1)
*
*     IER =  0   no error
*         = -1   end-of-data
*         >  0   error code
*                = 777   incomplete logical record
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
*KEND.
      CHARACTER CHA*4, NAMEC*8, WNAME*8, ERRSTR*72, FMT*80
      INTEGER IHL(96), IDXINF(8), IFMTA(2)
      CHARACTER*512 TMPBUF
      LOGICAL RESOK,START
      DATA START /.TRUE./
      SAVE RESOK,START
*
      IF(START) THEN
         START=.FALSE.
         CALL FTCTOI('A',IFMTA,NFMTA)
      ENDIF
*
      IF(LR.EQ.0) THEN
         CALL FPWMSG(99,'FRKEY','File not opened')
         GOTO 99
      ENDIF
*
      IF(INFU(IRSTU+2,LR).NE.0 .OR. INFU(IRSTU+3,LR).GT.0) THEN
         WRITE(ERRSTR,7777) INFU(IRKEY+4,LR),INFU(IRKEY+5,LR),
     +                 INFU(IRKEY+6,LR),INFU(IRKEY+7,LR)
 7777    FORMAT(1X,'*** Incomplete logical record "',2A4,2I12,'"')
C        WRITE(*,*) ERRSTR
         IF(INFU(32,LR).EQ.1) THEN
            WRITE(*,*) '*** To be continued ... ***'
            INFU(IRSTU+2,LR) = 0
            INFU(IRSTU+3,LR) = 0
         ELSE
            WRITE(*,*) '*** Stop reading    ... ***'
            CALL FPWMSG(99,'FRKEY',ERRSTR)
            GOTO 99
         END IF
      END IF
*
   1  IER = 0
      RESOK = .FALSE.
*
      IF( INFU(10,LR).GT.0 )
     +   CALL LOGERROR('FRKEY called for data file without index file')
*
      LX = INFU(7,LR)
*     Block up for a while switching on datafile in FRDAT
      INFU(7,LR) = 0
      IF(LX.NE.0) THEN
*        this file is an index file
         IF( IDXNEW ) THEN
*           New format of index file
            CALL FRDAT(NR,IDXINF,4)
            IF(NR.EQ.4) GOTO 10
         ELSE
*           Old format of index file
            CALL FRDAT(NR,IDXINF,7)
            IF(NR.EQ.7) GOTO 10
         ENDIF
9        CONTINUE
         IF(LX.GT.0) CALL FUCLOX(LUNBK(LX))
         DTFLNM(LR) = FILNAM(LR)
         DTFLHO(LR) = HOSNAM(LR)
         INFU(28,LR) = INFU(15,LR)
         IF(INFU(12,LR).EQ.1 .OR. INFU(12,LR).EQ.5) DTFLNM(LR) = ' '
         GOTO 1
 10      IF( IDXNEW ) THEN
            IF(IDXINF(1).EQ.-2) THEN
*              Run name changed ...
*              convert to local
               CALL FCNVR(INFU(5,LR),LOCAL,IDXINF(2),2,0,IFMTA,NFMTA)
*              read next ... (FRDAT may call FRKEX, using key info!!!)
               CALL FRDAT(NR,IDXINF(5),4)
               IF(NR.NE.4) GOTO 91
*              fill out run name,
               INFU(IRKEY+4,LR) = IDXINF(2)
               INFU(IRKEY+5,LR) = IDXINF(3)
*              ... & run number,
               INFU(IRKEY+6,LR) = IDXINF(4)
*              ... copy event number
               INFU(IRKEY+7,LR) = IDXINF(5)
*              ... classword
               INFU(IRKEY+9,LR) = IDXINF(6)
*              ... address
               DTFLKB(LR)       = IDXINF(7)
               DTFLPS(LR)       = IDXINF(8)
*              ... save "new" run name & number for future
               DTCRNM(1,LR)     = INFU(IRKEY+4,LR)
               DTCRNM(2,LR)     = INFU(IRKEY+5,LR)
               DTCRRN(LR)       = INFU(IRKEY+6,LR)
            ELSEIF(IDXINF(1).EQ.-1) THEN
*              Run number changed
*              First read address ( 2 words )
               CALL FRDAT(NR,IDXINF(5),2)
               IF(NR.NE.2) GOTO 91
*              ... copy "old" run name
               INFU(IRKEY+4,LR) = DTCRNM(1,LR)
               INFU(IRKEY+5,LR) = DTCRNM(2,LR)
*              ... then copy new run number
               INFU(IRKEY+6,LR) = IDXINF(2)
*              ... event number
               INFU(IRKEY+7,LR) = IDXINF(3)
*              ... classword
               INFU(IRKEY+9,LR) = IDXINF(4)
*              ... copy address
               DTFLKB(LR)       = IDXINF(5)
               DTFLPS(LR)       = IDXINF(6)
*              save "new" run number
               DTCRRN(LR)       = INFU(IRKEY+6,LR)
            ELSE
*              run name & run number haven't been changed
*              run NaMe
               INFU(IRKEY+4,LR) = DTCRNM(1,LR)
               INFU(IRKEY+5,LR) = DTCRNM(2,LR)
*              Run Number
               INFU(IRKEY+6,LR) = DTCRRN(LR)
*              Event Number
               INFU(IRKEY+7,LR) = IDXINF(1)
*              Classword
               INFU(IRKEY+9,LR) = IDXINF(2)
*              Event address into data file
               DTFLKB(LR)       = IDXINF(3)
               DTFLPS(LR)       = IDXINF(4)
            ENDIF
         ELSE
*           "old" index file format
*           convert to local ( run name )
            CALL FCNVR(INFU(5,LR),LOCAL,IDXINF(1),2,0,IFMTA,NFMTA)
            INFU(IRKEY+4,LR) = IDXINF(1)
            INFU(IRKEY+5,LR) = IDXINF(2)
            INFU(IRKEY+6,LR) = IDXINF(3)
            INFU(IRKEY+7,LR) = IDXINF(4)
            INFU(IRKEY+9,LR) = IDXINF(5)
            DTFLKB(LR)       = IDXINF(6)
            DTFLPS(LR)       = IDXINF(7)
         ENDIF
*     Permit a switching on datafile in FRDAT
         INFU(7,LR) = LX
         IF(LX.GT.0) THEN
            INFU(27,LX)      = DTFLKB(LR)
            INFU(IRSTU+8,LX) = DTFLPS(LR)
            INFU(IRSTU+9,LX) = 0
            INFU(IRSTU+3,LX) = 0
*           flag SKIPCOREV is expanded from index to data files
            IF(INFU(32,LX).NE.INFU(32,LR)) INFU(32,LX) = INFU(32,LR)
         ENDIF
         INFU(IRSTA+6,LR) = INFU(IRSTA+6,LR)+1
      ELSE IF( INFU(25,LR).GT.0 ) THEN
*        this is an ORDERED file with some index level(s) in memory
         IF( INFU(ISGEN+9,LR).EQ.1 ) THEN
            CALL FPFIND(1,IFOUND)
            INFU(ISGEN+9,LR) = 0
#ifndef Linux
         ELSE IF( INFU(ISGEN+8,LR).LE.0 ) THEN
#else /* Linux */
         ELSE IF( INFU(ISGEN+8,LR).NE.1 ) THEN
#endif /* Linux */
*           file is not locked, so we have to check
*           whether it was changed
            CALL FPCKTP(IRES)
            IF( IRES.NE.0 ) THEN
*              file was changed
*              forget the data record
               INFU(29,LR) = 0
*              find the position in top level block
               CALL FPSRCH(1,IFOUND)
            ENDIF
         ENDIF
         LEVEL = INFU(25,LR)
         INDR = INDXB(IBUF,1000*LUNR+9+LEVEL)
         IF( IBUF(INDR+22).LT.7 ) THEN
*           level index is empty
            IF( LEVEL.NE.1 ) CALL LOGERROR('Empty index')
            IER = -1
            GOTO 100
         ENDIF
         IF( IBUF(INDR+17).GT.1 ) THEN
*           there are unread index levels
*           build the index path
            CALL FPDOWN(1,IFOUND)
         ENDIF
         GOTO 551
      ELSE
         CALL FRKEX(WNAME,NUM1,NUM2,NDATWD,IER)
         DTFLKB(LR) = INFU(IRSTU+7,LR)
         DTFLPS(LR) = INFU(IRSTU+8,LR)
         NAMKEY = WNAME
         RESOK = .TRUE.
*
         IF( IER.EQ.-1 ) THEN
*     OPEN the next file if any
            CALL MKNXOP(IER)
            IF(IER.EQ.0) GOTO 01
            IF(IER.NE.0) GOTO 99
         ENDIF
*
         IF( IER.NE.0 ) GOTO 100
*
         IF( (WNAME.EQ.'FPINDEXK' .OR. WNAME.EQ.'FPIDXNEW')
     +             .AND.INFU(ISGEN+10,LR).EQ.0 ) THEN
            INFU(IRSTA+6,LR) = INFU(IRSTA+6,LR)-1
*     Save old value of flag "suppress data block selections"
            ISV24 = INFU(24,LR)
*     and suppress block selection
            INFU(24,LR) = 1
*     check what kind of index file do we deal with, old or new?
            IF(WNAME.EQ.'FPIDXNEW') THEN
*     This is a new index file format.
               IDXNEW=.TRUE.
*     Filename, hostname, mediatype and, optionally, volume name
*     of (IBM) tape are keeping together, separated by ';'.
               CALL FRHDR(WNAME,ISHFLG,ILRECL,NROW,FMT,NCH,IER)
               IF( IER.NE.0 )      GOTO 77
               INFU(IRSTA+5,LR) = INFU(IRSTA+5,LR)-1
               IF(WNAME.NE.'FPINDEX0' .OR. NROW.NE.6) GOTO 77
*     Read filename into integer array
               CALL FRDAT(NR,IHL,96)
               IF( NR.EQ.0 )       GOTO 77
               INFU(28,LR)=ISHFLG
               INFU(34,LR)=ILRECL
               TMPBUF = ' '
               CALL HLTOTX(IHL,NR,TMPBUF,NCH)
               I1 = INDEX(TMPBUF, ';')
               IF( I1 .EQ. 0 )         GOTO 77
               DTFLNM(LR)=TMPBUF(1:I1-1)
               RLFLNM(LR)=TMPBUF(1:I1-1)
               I2 = INDEX(TMPBUF(I1+1:),';')+I1
               IF( I2.EQ.I1 .OR. I2.EQ.I1+1 )   GOTO 77
               DTFLHO(LR)=TMPBUF(I1+1:I2-1)
               RLFLHO(LR)=TMPBUF(I1+1:I2-1)
               I3 = INDEX(TMPBUF(I2+1:),';')+I2
               IF( I3.EQ.I2 .OR. I3.EQ.I2+1 )   GOTO 77
               DTFLMD(LR)=TMPBUF(I2+1:I3-1)
               RLFLMD(LR)=TMPBUF(I2+1:I3-1)
c$$$               I4 = INDEX(TMPBUF(I3+1:),';')+I3
c$$$               DTFLVS(LR) = ' '
c$$$               IF( I4.NE.I3 .AND. I4.NE.I3+1 )
c$$$     +              DTFLVS(LR)=TMPBUF(I3+1:I4-1)
*
               CALL FRHDR(WNAME,NUMDB,NCOL,NROW,FMT,NCH,IER)
               IF( IER.NE.0 )       GOTO 77
               INFU(IRSTA+5,LR) = INFU(IRSTA+5,LR)-1
               IF(WNAME.NE.'FPINDEX1' .OR. NUMDB.NE.7 .OR. NCOL.NE.8
     +              .OR. NROW.NE.9)    GOTO 77
            ELSE
*     Old index file format encountered.
               IDXNEW=.FALSE.
               CALL FRHDR(WNAME,ISHFLG,NCOL,NROW,FMT,NCH,IER)
               IF( IER.NE.0 )      GOTO 77
               INFU(IRSTA+5,LR) = INFU(IRSTA+5,LR)-1
               IF(WNAME.NE.'FPINDEX0' .OR. NCOL.NE.5
     +                                .OR. NROW.NE.6) GOTO 77
*     Read filename into integer array
               CALL FRDAT(NR,IHL,96)
               IF( NR.EQ.0 )       GOTO 77
               INFU(28,LR)=ISHFLG
               DTFLNM(LR) = ' '
               CALL HLTOTX(IHL,NR,DTFLNM(LR),NCH)
*     Read the next bank (FPINDEXH and/or FPINDEXV are optional
*     and FPINDEX1 is obligatory!)
 30            CALL FRHDR(WNAME,NUMDB,NCOL,NROW,FMT,NCH,IER)
               IF( IER.NE.0 )       GOTO 77
               INFU(IRSTA+5,LR) = INFU(IRSTA+5,LR)-1
*     check that bank have correct markers (7,8,9)
               IF(NUMDB.NE.7.OR.NCOL.NE.8.OR.NROW.NE.9) GOTO 77
               IF(WNAME.EQ.'FPINDEXH')THEN
                  CALL FRDAT(NR,IHL,32)
                  IF( NR.EQ.0 )        GOTO 77
                  DTFLHO(LR) = ' '
*     convert to character
                  CALL HLTOTX(IHL,NR,DTFLHO(LR),NCH)
*     this was not FPINDEX1 bank, but it must be! Try again ...
                  GOTO 30
               ENDIF
               IF(WNAME.EQ.'FPINDEXV')THEN
                  CALL FRDAT(NR,IHL,3)
                  IF( NR.NE.3 )        GOTO 77
                  DTFLVS(LR) = ' '
                  CALL HLTOTX(IHL,2,DTFLVS(LR),NCH)
*     get record length for datafile on cartridge
                  INFU(34,LR)=IHL(3)
*     Try again to read FPINDEX1 bank
                  GOTO 30
               ENDIF
               IF(WNAME.NE.'FPINDEX1') GOTO 77
            ENDIF
*     Restore old value of flag "suppress data block selection"
            INFU(24,LR) = ISV24
            INFU(7,LR)  = -1
            GOTO 1
*     checking failed
 77         IER = 1
            WRITE(6,*) 'ERROR> Wrong or corrupted index file'
            CALL FPWMSG(IER,'FRKEY','Wrong or corrupted index file')
            GOTO 99
         ELSE IF( WNAME.EQ.'FPINDEXA'.AND.INFU(ISGEN+10,LR).EQ.0 ) THEN
*           the file is ORDERED !
            INFU(2,LR) = 5
            INFU(25,LR) = 1
*           create the buffer for top level index
            INDR = INDXA(IBUF,1000*LUNR+9+1,INFU(1,LR)/4)
            IF( INDR.EQ.0 ) CALL LOGERROR('No space for index')
            INDR1 = INDXB(IBUF,1000*LUNR+1)
*           and fill it
            CALL UCOPY(IBUF(INDR1+1),IBUF(INDR+1),IBUF(INDR1))
            IF( IBUF(INDR1+8).EQ.1.AND.IBUF(INDR1+9).EQ.1 ) THEN
*              "unfinished"
               GOTO 1
            ENDIF
*           find the position in top level block
            CALL FPSRCH(1,IFOUND)
*           build the index path
            CALL FPDOWN(1,IFOUND)
            GOTO 551
#ifndef Linux
         ELSE
            IDXNEW=.TRUE.
#endif /* not Linux */
         ENDIF
      ENDIF
      GOTO 552
*
*     move to the next record in ordered file
551   CONTINUE
         LEVEL = INFU(25,LR)
*        find the index(level) block
         INDR = INDXB(IBUF,1000*LUNR+9+LEVEL)
*        get the current position in it
         NPOS = IBUF(INDR+15)
         JP = 13
*        data position
         KP = JP + IBUF(INDR+JP+1)
*        suppose it is inside the block
         IOUT = 0
*        move the position
#ifndef Linux
*        If this is a leaf level & position was requested ... keep pos.
         IF(IBUF(INDR+17).EQ.1 .AND.
     +      (INFU(IRSTU+12,LR).EQ.2.OR.INFU(IRSTU+12,LR).EQ.-2)) THEN
*           ... do nothing
         ELSEIF( INFU(IRSTU+12,LR).GE.0 ) THEN
*           ... direction to the right
#else /* Linux */
         IF( INFU(IRSTU+12,LR).GE.0 ) THEN
#endif /* Linux */
            NPOS = NPOS+7
#ifdef Linux
            IF( NPOS.GE.IBUF(INDR+22) ) IOUT = 1
#endif /* Linux */
         ELSE
#ifndef Linux
*           ... direction to the left
#endif /* not Linux */
            NPOS = NPOS-7
#ifndef Linux
         ENDIF
         IF( NPOS.GE.IBUF(INDR+22) ) IOUT = 1
#endif /* not Linux */
         IF( NPOS.LT.0 ) IOUT = 1
#ifdef Linux
         ENDIF
#endif /* Linux */
         IF( IOUT.NE.0 ) THEN
*           the new position is outside the block
            IF( LEVEL.EQ.1 ) THEN
*              we got the end of file
               IER = -1
               GOTO 100
            ELSE
*              goto the upper level
               INFU(25,LR) = LEVEL - 1
               GOTO 551
            ENDIF
         ENDIF
         IBUF(INDR+15) = NPOS
         IF( IBUF(INDR+17).GT.1 ) THEN
*           index block refers to index
            CALL FPDOWN(1,IFOUND)
            GOTO 551
         ENDIF
*     clear "error in logical record" flag
      INFU(IRSTU+3,LR) = 0
*     data segment header position in index block
      JP = 13
*     data position
      KP = JP + IBUF(INDR+JP+1)
*     uncode character part of the key
      CALL FPKUNC(IBUF(INDR+KP+NPOS+1),INFU(IRKEY+4,LR),NAMKEY)
*     copy other key subfields
      INFU(IRSTU+14,LR)= IBUF(INDR+KP+NPOS+1)
      INFU(IRSTU+15,LR)= IBUF(INDR+KP+NPOS+2)
      NUM1             = IBUF(INDR+KP+NPOS+3)
      INFU(IRKEY+6,LR) = NUM1
      INFU(IRSTU+16,LR)= NUM1
      NUM2             = IBUF(INDR+KP+NPOS+4)
      INFU(IRKEY+7,LR) = NUM2
      INFU(IRSTU+17,LR)= NUM2
      NDATWD           = IBUF(INDR+KP+NPOS+5)
      INFU(IRKEY+9,LR) = NDATWD
      DTFLKB(LR)       = IBUF(INDR+KP+NPOS+6)
      INFU(27,LR)      = DTFLKB(LR)
      DTFLPS(LR)       = IBUF(INDR+KP+NPOS+7)
#ifndef Linux
      INFU(IRSTU+21,LR)= 0
#endif /* not Linux */
*     set direction flag
      IF( INFU(IRSTU+12,LR).GE.0 ) THEN
         INFU(IRSTU+12,LR) = 1
      ELSE
         INFU(IRSTU+12,LR) = -1
      ENDIF
      IF( DTFLKB(LR).NE.INFU(29,LR) ) THEN
*        we have to reread the data record
         INFU(IRSTU+8,LR) = 0
         INFU(29,LR) = 0
      ELSE
*        data record is ok
*        it is converted already
         INFU(IRSTU+1,LR) = 1
*        set the correct postion
         INFU(IRSTU+8,LR) = DTFLPS(LR)
         INFU(IRSTU+9,LR) = 0
      ENDIF
      RESOK = .TRUE.
*
552   CONTINUE
*
*     record select
      CALL FSELCT(1,LUNBK(LR),IDEC)
      IF(IDEC.EQ.1) GOTO 01
      IF(IDEC.EQ.2) GOTO 99
*
      IF(.NOT.RESOK) THEN
*     convert key name into character string
         ICH=INFU(IRKEY+4,LR)
*KEEP,CHAICH.
*     get char*4 from hollerith 4H
#ifndef Linux
      CHA=CHAR(IAND(ISHFT(ICH,-24),255))//CHAR(IAND(ISHFT(ICH,-16),255))
     +  //CHAR(IAND(ISHFT(ICH,- 8),255))//CHAR(IAND      (ICH     ,255))
#else /* Linux */
      CHA=CHAR(IAND      (ICH     ,255))//CHAR(IAND(ISHFT(ICH,- 8),255))
     +  //CHAR(IAND(ISHFT(ICH,-16),255))//CHAR(IAND(ISHFT(ICH,-24),255))
#endif /* Linux */
*KEND.
         NAMEC(1:4)=CHA
         ICH=INFU(IRKEY+5,LR)
*KEEP,CHAICH.
*     get char*4 from hollerith 4H
#ifndef Linux
      CHA=CHAR(IAND(ISHFT(ICH,-24),255))//CHAR(IAND(ISHFT(ICH,-16),255))
     +  //CHAR(IAND(ISHFT(ICH,- 8),255))//CHAR(IAND      (ICH     ,255))
#else /* Linux */
      CHA=CHAR(IAND      (ICH     ,255))//CHAR(IAND(ISHFT(ICH,- 8),255))
     +  //CHAR(IAND(ISHFT(ICH,-16),255))//CHAR(IAND(ISHFT(ICH,-24),255))
#endif /* Linux */
*KEND.
         NAMEC(5:8)=CHA
*        move record key to arguments
         NAMKEY=NAMEC
         NUM1  =INFU(IRKEY+6,LR)
         NUM2  =INFU(IRKEY+7,LR)
         NDATWD=INFU(IRKEY+9,LR)
      ENDIF
*     set flag "key is just read"
      INFU(IRSTU+4,LR) = 1
*     increment number of logical records read (after selection)
      INFU(IRSTA+3,LR) = INFU(IRSTA+3,LR)+1
      GOTO 100
*
*     index file corrupted
 91   IER = 91
      CALL FPWMSG(IER,'FRKEY','Index file element truncated')
      GOTO 100
*
*     end-of-file
   99 IER = -1
*
  100 RETURN
      END
