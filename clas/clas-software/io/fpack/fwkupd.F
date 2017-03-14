#ifndef Linux
*CMZ :  1.00/16 11/03/96  21.23.10  by  Unknown
#endif /* not Linux */
*CMZU:  1.00/04 20/02/95  11.01.34  by  Unknown
*CMZU:  1.00/01 10/01/95  11.35.46  by  Unknown
*CMZ :  1.00/00 16/12/94  17.00.00  by  Pavel Binko
*-- Author :    Ruten Gurin   16/12/94
      SUBROUTINE FWKUPD(IDEL,IERR)
*
************************************************************************
*
*     Delete/Insert/Update the key for ordered access
*     Called in FWEND if IDEL=0 & in FWDEL if IDEL!=0
*
************************************************************************
*
#ifndef Linux
      PARAMETER (NW5 = 5)
#endif /* not Linux */
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
#ifndef Linux
      CHARACTER CHA*4
      INTEGER KEY2(7),KEY1(7),KEY0(7),ISVK(4),ISVW(NLDIM-IWSTU)
      INTEGER IFREE(2)
      LOGICAL START, SAVFLG
      DATA    START/.TRUE./,SAVFLG/.TRUE./
*
      IF(START) THEN
         CHA='#-FR'
*KEEP,ICHCHA.
*     get hollerith 4H from char*4
      ICH=IOR(ISHFT(ICHAR(CHA(1:1)),24),IOR(ISHFT(ICHAR(CHA(2:2)),16),
     +    IOR(ISHFT(ICHAR(CHA(3:3)), 8),ICHAR(          CHA(4:4)))))
*KEND.
         IFREE(1) = ICH
         CHA='EE-#'
*KEEP,ICHCHA.
*     get hollerith 4H from char*4
      ICH=IOR(ISHFT(ICHAR(CHA(1:1)),24),IOR(ISHFT(ICHAR(CHA(2:2)),16),
     +    IOR(ISHFT(ICHAR(CHA(3:3)), 8),ICHAR(          CHA(4:4)))))
*KEND.
         IFREE(2) = ICH
         START = .FALSE.
      ENDIF
      IERR = 0
*     save LR,LUNR
      ISVLR = LR
      ISVLUN = LUNR
*     reset them
      LR = LW
      LUNR = LUNW
*
      IF(IDEL.EQ.1.AND.INFU(ISGEN+8,LW).EQ.0) THEN
         NTRY = 0
10       CALL FPLOCK(' ',IRC)
         NTRY = NTRY+1
         IF(IRC.NE.0.AND.NTRY.LT.100) GOTO 10
         IF(IRC.NE.0) GOTO 299
      ENDIF
#else /* Linux */
      INTEGER KEY1(7),KEY0(7),ISVK(4),ISVW(NLDIM-IWSTU)
#endif /* Linux */
*
#ifndef Linux
      IF(INFU(ISGEN+8,LW).EQ.0) CALL LOGERROR('FWKUPD. Missing lock')
#else /* Linux */
      IERR = 0
*     check the write buffer
#endif /* Linux */
      INDW = INDXB(IBUF,1000*LUNW+2)
      IF( INDW.GT.0.AND.IBUF(INDW+2).GT.2 ) THEN
#ifdef Linux
*        save info for out record
         CALL UCOPY(INFU(IWSTU+1,LW),ISVW,NLDIM-IWSTU)
         ISV26 = INFU(26,LW)
#endif /* Linux */
*        create an additional buffer
         IWRK1 = INDXB(IBUF,1000*LUNW+3)
         IF( IWRK1.LE.0 ) THEN
            IWRK1 = INDXA(IBUF,1000*LUNW+3,INFU(1,LW)/4)
            IF( IWRK1.LE.0 ) CALL LOGERROR('Out of space in FWKUPD')
            INDW = INDXB(IBUF,1000*LUNW+2)
         ENDIF
#ifndef Linux
*        save info for out record
         CALL UCOPY(INFU(IWSTU+1,LW),ISVW,NLDIM-IWSTU)
*        Extract pointers
         IF(INFU(ISGEN+11,LW).NE.0) THEN
            INFU(26,LW) = INFU(IWSTU+17,LW)
            IP = INFU(IWSTU+18,LW)
            JP = INFU(IWSTU+19,LW)
            KP = INFU(IWSTU+20,LW)
            NALL = IP+11+IBUF(INDW+IP+11)-NW5
            NUSED = JP+IBUF(INDW+JP+1)+IBUF(INDW+JP+9)
         ELSE
            IP = INFU(IWSTU+8,LW)
            JP = INFU(IWSTU+9,LW)
            KP = INFU(IWSTU+10,LW)
            NALL = IP+11+IBUF(INDW+IP+11)-NW5
            NUSED = IBUF(INDW+2)
            IBUF(INDW+2) = NALL+NW5
            IBUF(INDW+NALL+3) = 0
            IBUF(INDW+NALL+4) = 0
         ENDIF
*        Lock the segment
         IBUF(INDW+NALL+5) = 0
*      PRINT *,'*+*+*+* FWKUPD  BLK#',INFU(26,LW),' *+*+*+*+*+*+*+*'
*      PRINT '(1X,''IP='',I4,1X,''JP='',I4,1X,''KP='',I4)',IP,JP,KP
*      PRINT '(1X,''NUSED='',I4,1X,''NALL='',I4)',NUSED,NALL
*      PRINT *,' '
*   if the space is available, mark the unused rest as free space.
         NFREE = NALL - NUSED
         IF(NFREE.GT.0) IBUF(INDW+NUSED+1)=NFREE
         IF(NFREE.GT.1) IBUF(INDW+NUSED+2)=IFREE(1)
         IF(NFREE.GT.2) IBUF(INDW+NUSED+3)=IFREE(2)
         IF(INFU(ISGEN+11,LW).NE.0) THEN
            SAVFLG = .FALSE.
            ISV26 = INFU(ISGEN+11,LW)
            IF(ISV26.EQ.INFU(IWSTU+17,LW)) SAVFLG = .TRUE.
            INFU(ISGEN+11,LW) = 0
         ELSE
            ISV26 = INFU(26,LW)
            SAVFLG = .TRUE.
         ENDIF
         IF(SAVFLG) THEN
*           Save filled part of output buffer
            CALL UCOPY(IBUF(INDW+1),IBUF(IWRK1+1),IBUF(INDW+2))
         ENDIF
 
*        Now we can flush the buffer
*        convert word format ...
         JFM=INFU(5,LW)
*        ... if output ne local and inhibit flag not set
         IF(JFM.NE.LOCAL.AND.INFU(ISGEN+4,LW).EQ.0) THEN
             CALL FRCONV(IBUF(INDW+1),JFM,ICER)
         END IF
*        Flush the output buffer
*      print *,'FWKUPD.INFU(26,LW),INFU(30,LW)',infu(26,lw),infu(30,lw)
         CALL FWREC(LUNW,IBUF(INDW+1),IBUF(INDW+1),IER)
#else /* Linux */
         CALL UCOPY(IBUF(INDW+1),IBUF(IWRK1+1),INFU(1,LW)/4)
#endif /* Linux */
      ELSE
         ISV26 = -1
      ENDIF
*
#ifdef Linux
*     save LR,LUNR
      ISVLR = LR
      ISVLUN = LUNR
*     reset them
      LR = LW
      LUNR = LUNW
*
#endif /* Linux */
      IF( IDEL.EQ.0 ) THEN
*        save current read key
         CALL UCOPY(INFU(IRSTU+14,LR),ISVK,4)
*        fill the key
         CALL FPKENC(INFU(IWKEY+4,LW),KEY1)
         KEY1(3) = INFU(IWKEY+6,LW)
         KEY1(4) = INFU(IWKEY+7,LW)
         KEY1(5) = INFU(IWKEY+9,LW)
         KEY1(6) = INFU(IWSTU+15,LW)
         KEY1(7) = INFU(IWSTU+16,LW)
#ifndef Linux
         IBLK    = INFU(IWSTU+15,LW)
         IOFF    = INFU(IWSTU+16,LW)
#endif /* not Linux */
*        move key1 to read key
         CALL UCOPY(KEY1,INFU(IRSTU+14,LR),4)
#ifndef Linux
*        PRINT *,'REQUESTED KEY'
*        PRINT '(1X,2A4,5I9)',(KEY1(j),j=1,7)
*        PRINT *,' '
      ELSE
*        DELETE mode
         CALL UCOPY(INFU(IRSTU+14,LR),KEY2(1),4)
         KEY2(5) = 0
         KEY2(6) = 0
         KEY2(7) = 0
#endif /* not Linux */
      ENDIF
*
*     build the index path
      CALL FPFIND(0,IFOUND)
*     no key0 at the moment
      KEY0(6) = 0
#ifndef Linux
*      PRINT *,'FWKUPD. IFOUND(FPFIND)=',IFOUND
#endif /* not Linux */
*
*     if the key was not found ...
      IF(IFOUND.EQ.0) THEN
         IF(IDEL.NE.0) THEN
*     ... nothing to delete
            IERR = 1
            GOTO 200
         ENDIF
*     ... append record to end
#ifdef Linux
         GOTO 253
      ENDIF
*
*+++++++++++++++ try to 'update in place' +++++++++++++++++++
      IF(IDEL.EQ.0 .AND. ISV26.GT.0) THEN
*     check that output buffer is not empty
         IF(INFU(IWSTU+8,LW).LE.2) GOTO 253
         IPNEW = INFU(IWSTU+8,LW)
         JPNEW = INFU(IWSTU+9,LW)
*     check correctness of pointers
         IF(INFU(IWSTU+10,LW).NE.(JPNEW+IBUF(IWRK1+JPNEW)) .OR.
     +        INFU(IWSTU+10,LW).NE.IBUF(IWRK1+2)) THEN
            GOTO 253
         ENDIF
*     does the logical record fit in this block as a whole?
*     - check record and data segment codes ...
         IF(IBUF(INDW+IPNEW+10).NE.0 .OR. IBUF(INDW+JPNEW+7).NE.0)
     +        GOTO 253
*     Previous version of record exists
*     (the index path was built and the key was found)
         LEVEL = INFU(25,LR)
*     find the buffer
         IF( LEVEL.LE.0 ) CALL LOGERROR('FWKUPD 03')
         INDRX = INDXB(IBUF,1000*LUNR+9+LEVEL)
         IF( INDRX.LE.0 ) CALL LOGERROR('FWK 102')
         JPX = 13
*     pointer to index information
         KPX = JPX + IBUF(INDRX+JPX+1)
*     wanted key position
         NPOS  = IBUF(INDRX+15)
*     physical record number with previous (old) version
         IRCN  = IBUF(INDRX+KPX+NPOS+6)
*     is it the same record number as a current one?
         IF(IRCN.NE.ISV26) THEN
*     read wanted block from disk into output buffer
            ISV27 = INFU(27,LR)
            INFU(27,LR) = IRCN
            CALL FRRECX(LUNR,IBUF(INDW+1),IBUF(INDW+1),IRC)
            INFU(27,LR)=ISV27
            IF(IRC.NE.0) GOTO 252
            CALL FRCONV(IBUF(INDW+1),LOCAL,IER)
            IF(IER.NE.0) GOTO 252
         ENDIF
         IPOLD = IBUF(INDRX+KPX+NPOS+7)
         JPOLD = IPOLD+11
*     Is previous version as a whole inside of this block?
         IF(IBUF(INDW+IPOLD+10).NE.0) GOTO 252
         LL = IBUF(INDW+JPOLD)
         IF(IBUF(IWRK1+JPNEW) .NE. LL) GOTO 252
*     Previous and new versions have the same lenghts, so copy
         DO 88 J=1,LL
 88      IBUF(INDW+JPOLD+J) = IBUF(IWRK1+JPNEW+J)
*
         JFM = INFU(5,LW)
         IF(JFM.NE.LOCAL .AND. INFU(ISGEN+4,LW).EQ.0)
     +        CALL FRCONV(IBUF(INDW+1),JFM,ICER)
*
         IF(INFU(ISGEN+8,LW).EQ.0) THEN
            CALL FPLOCK(' ',IRC)
            INFU(ISGEN+8,LW)=1
#endif /* Linux */
      ENDIF
#ifdef Linux
*     flush modified buffer to disk
         INFU(26,LW)=IRCN
         CALL FWREC(LUNW,IBUF(INDW+1),IBUF(INDW+1),IER)
         INFU(26,LW)=ISV26
         IF(IER.NE.0)  GOTO 252
         IF(ISV26.NE.IRCN) THEN
*     restore output buffer copying before last log. rec.
            CALL UCOPY(IBUF(IWRK1+1),IBUF(INDW+1),IPNEW)
         ENDIF
*     update counter of used words in this block
         IBUF(INDW+2) = IPNEW
*     find IP,JP,KP pointers of previous logical record in buffer
         IP=0
         IPNEXT=2
 120     IF(IPNEXT.GE.IBUF(INDW+2)) GOTO 130
         IP = IPNEXT
         IPNEXT=IP+11 + IBUF(INDW+IP+11)
         GOTO 120
 130     IF(IP.EQ.0) IP=2
*     It is not necessary to update the key info in index blocks
         INFU(IWSTU+8,LW) =IP
         INFU(IWSTU+9,LW) =IP+11
         INFU(IWSTU+10,LW)=IBUF(INDW+2)
         INFU(IWSTU+15,LW)=ISV26
         INFU(IWSTU+16,LW)=IP
         INFU(IWSTU+7,LW) =INFU(IWSTU+7,LW) - 1
*     go to exit
         GOTO 100
*
*     restore buffer contents
 252     CALL UCOPY(IBUF(IWRK1+1),IBUF(INDW+1),IBUF(IWRK1+1))
      ENDIF
*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
 253  CONTINUE
*     flush the output buffer if necessary
      IF(ISV26.GT.0) CALL FPFLSH(IERR)
#endif /* Linux */
*
*     mode for delete: 1 - delete, 0 - update
      IDELMD = 1
*     now delete/insert/update the key
      LEVEL = INFU(25,LR)
551   CONTINUE
*        find the buffer
         IF( LEVEL.LE.0 ) CALL LOGERROR('FWKUPD 03')
         INDR = INDXB(IBUF,1000*LUNR+9+LEVEL)
         IF( INDR.LE.0 ) CALL LOGERROR('FWK 02')
         JP = 13
*        pointer to index information
         KP = JP + IBUF(INDR+JP+1)
*        index element size (words)
         IELSZ = 7
*        number of words
         NW = IBUF(INDR+22)
*        current position
         NPOS = IBUF(INDR+15)
*        number of words in the whole buffer
#ifndef Linux
         NLIM = IBUF(INDR+1) - NW5
#else /* Linux */
         NLIM = IBUF(INDR)
#endif /* Linux */
         IF( NW.LT.IELSZ ) THEN
            NW = 0
            NPOS = 0
         ENDIF
         IF( KEY0(6).NE.0 ) THEN
            CALL UCOPY(KEY0,IBUF(INDR+KP+NPOS+1),IELSZ)
            IF( NW.EQ.0 ) NW = IELSZ
            NPOS = NPOS + IELSZ
            KEY0(6) = 0
*           new key must be inserted
            IFOUND = 0
         ENDIF
         IF( IDEL.NE.0 ) THEN
#ifndef Linux
*      PRINT *,'FWKUPD. DEL. KEY'
#endif /* not Linux */
            IF( IDELMD.EQ.1 ) THEN
#ifndef Linux
*      PRINT *,'DELETED KEY'
*      PRINT '(1X,2A4,5I9)',(IBUF(INDR+KP+NPOS+j),j=1,7)
               IF(KEY2(6).EQ.0) THEN
                  KEY2(6) = IBUF(INDR+KP+NPOS+6)
                  KEY2(7) = IBUF(INDR+KP+NPOS+7)
               ENDIF
#endif /* not Linux */
               DO 55427 JJ=INDR+KP+NPOS+1,INDR+KP+NW-IELSZ
                  IBUF(JJ) = IBUF(JJ+IELSZ)
55427          CONTINUE
               NW = NW - IELSZ
               IF( NW.EQ.0 ) THEN
                  IBUF(INDR+22) = 1
                  IF( LEVEL.NE.1 ) THEN
                     LEVEL = LEVEL - 1
                     INFU(25,LR) = LEVEL
                     GOTO 551
                  ENDIF
               ELSE
                  IDELMD = 0
                  IBUF(INDR+22) = NW
               ENDIF
            ELSE
*              we replace the current key
               CALL UCOPY(KEY1,IBUF(INDR+KP+NPOS+1),IELSZ)
            ENDIF
         ELSE IF( IFOUND.EQ.1 ) THEN
#ifndef Linux
*           PRINT *,'+++++++++ FWKUPD. REPLACED KEY ++++++++'
*           PRINT '(1X,2A4,5I9)',(IBUF(INDR+KP+NPOS+j),j=1,7)
*           PRINT *,'+++++++++++++++++++++++++++++++++++++++'
#endif /* not Linux */
*           we replace the current key
            CALL UCOPY(KEY1,IBUF(INDR+KP+NPOS+1),IELSZ)
         ELSE IF( KP+NW+IELSZ .LE. NLIM ) THEN
#ifndef Linux
*           PRINT *,'FWKUPD. NEW KEY - ENOUGH ROOM. LEVEL=', LEVEL
#endif /* not Linux */
*           enough place for new key
            NW = NW+IELSZ
            IBUF(INDR+22) = NW
            IF(NW.GT.IELSZ) THEN
*              shift the keys
               DO 55422 JJ=INDR+KP+NW-IELSZ,INDR+KP+NPOS+1,-1
                  IBUF(JJ+IELSZ) = IBUF(JJ)
55422          CONTINUE
            ENDIF
            CALL UCOPY(KEY1,IBUF(INDR+KP+NPOS+1),IELSZ)
         ELSE
#ifndef Linux
*           PRINT *,'FWKUPD. NEW KEY - NO ROOM. LEVEL=',LEVEL
#endif /* not Linux */
*           not enough space to insert the key
*           we have to split the block
*           use the free buffer from the next level
            IWRK2 = INDXB(IBUF,1000*LUNW+9+LEVEL+1)
            IF( IWRK2.LE.0 ) THEN
               IWRK2 = INDXA(IBUF,1000*LUNW+9+LEVEL+1,INFU(1,LW)/4)
               IF( IWRK2.LE.0 ) CALL LOGERROR('Out of sp in FWKUPD')
*              INDR was changed maybe
               INDR = INDXB(IBUF,1000*LUNR+9+LEVEL)
#ifndef Linux
               INDW = INDXB(IBUF,1000*LUNW+2)
#endif /* not Linux */
            ENDIF
*           copy the header part of the record
            CALL UCOPY(IBUF(INDR+1),IBUF(IWRK2+1),KP)
*           we will split the record, putting NW1 words
*           to the left block, and NW-NW1+IELSZ - to the right.
            NENT = NW/IELSZ
            NW1 = IELSZ*((2*NENT)/3)
            IF( NW1.EQ.0 ) NW1 = IELSZ
            NW2 = NW-NW1+IELSZ
            IF( NW1.LE.NPOS ) THEN
               CALL UCOPY(IBUF(INDR+KP+NW1+1),IBUF(IWRK2+KP+1),
     +                          NPOS-NW1)
               CALL UCOPY(KEY1,IBUF(IWRK2+KP+NPOS-NW1+1),IELSZ)
               CALL UCOPY(IBUF(INDR+KP+NPOS+1),
     +                         IBUF(IWRK2+KP+NPOS-NW1+IELSZ+1),
     +                         NW-NPOS)
            ELSE
               CALL UCOPY(IBUF(INDR+KP+NW1-IELSZ+1),
     +                         IBUF(IWRK2+KP+1),
     +                         NW2)
               DO 55421 JJ=INDR+KP+NW1-IELSZ,INDR+KP+NPOS+1,-1
                  IBUF(JJ+IELSZ) = IBUF(JJ)
55421          CONTINUE
               CALL UCOPY(KEY1,IBUF(INDR+KP+NPOS+1),IELSZ)
            ENDIF
#ifndef Linux
*           set new sizes and refill key0,key1 - maximum items
#else /* Linux */
*           set new sizes and refill key0,key1
#endif /* Linux */
            IBUF(INDR+22) = NW1
            IBUF(IWRK2+22) = NW2
            CALL UCOPY(IBUF(INDR+KP+NW1-IELSZ+1),KEY0,4)
            CALL UCOPY(IBUF(IWRK2+KP+NW2-IELSZ+1),KEY1,4)
            KEY0(7) = 2
            KEY1(7) = 2
*     set the first and alternative rec. numbers
*     for the second part
*     (these are the new index blocks invisible for other users yet)
            IBUF(IWRK2+11) = INFU(30,LW)+1
            IBUF(IWRK2+19) = INFU(30,LW)+2
            KEY1(6) = IBUF(IWRK2+11)
*           write the second part
            CALL FPWIDS(IBUF(IWRK2+1),1)
*           write the alternative version
            CALL FPWIDS(IBUF(IWRK2+1),1)
            IF( LEVEL.EQ.1 ) THEN
*              we splitted the top block
*              so we write the first part to new place
#ifndef Linux
*              PRINT *,'FWKUPD.SPLIT. 26,30',INFU(26,LW),INFU(30,LW)
#endif /* not Linux */
               IBUF(INDR+11) = INFU(30,LW)+1
               IBUF(INDR+19) = INFU(30,LW)+2
               KEY0(6) = IBUF(INDR+11)
*              write the first part and automatic swap
               CALL FPWIDS(IBUF(INDR+1),1)
*              write the alternative version & swap
               CALL FPWIDS(IBUF(INDR+1),1)
*              Refill the top block
*              The block is empty
               IBUF(INDR+22) = 1
*              current position - 0
               IBUF(INDR+15)= 0
*              this block points to other index blocks
               IBUF(INDR+17) = 2
            ELSE
*              rewrite always to the alternative version
               KEY0(6) = IBUF(INDR+19)
*              swap versions
               IBUF(INDR+19) = IBUF(INDR+11)
               IBUF(INDR+11) = KEY0(6)
*              rewrite the block & no swap
               CALL FPWIDS(IBUF(INDR+1),0)
               LEVEL = LEVEL - 1
            ENDIF
            GOTO 551
         ENDIF
         IF( LEVEL.NE.1 ) THEN
*           rewrite always to the alternative version of index block
*           reset the key1
            CALL UCOPY(IBUF(INDR+KP+NW-IELSZ+1),KEY1,IELSZ)
            KEY1(6) = IBUF(INDR+19)
            KEY1(7) = 2
*           Swap versions of index block before FPWIDS
            IBUF(INDR+19) = IBUF(INDR+11)
            IBUF(INDR+11) = KEY1(6)
*           rewrite the buffer & no swap
            CALL FPWIDS(IBUF(INDR+1),0)
*           and go up
            LEVEL = LEVEL - 1
            IFOUND = 1
            GOTO 551
         ENDIF
*     update the counter
      INDR = INDXB(IBUF,1000*LUNW+9+1)
      N1 = IBUF(INDR+8)
      N2 = IBUF(INDR+9)
      N2 = N2 + 1
      IF( N2.LE.0 ) THEN
         N2 = 2
         N1 = N1 + 1
         IF( N1.LE.0 ) N1 = 2
      ENDIF
      IBUF(INDR+8) = N1
      IBUF(INDR+9) = N2
      IBUF(INDR+18) = INFU(30,LW)
*     we write the top block three times:
*        first to record 2
*           if it fails we still have the correct block in 1
*        then to record 1 with "unfinished" flag
*           if it's done only partially, then
*             either the old content is Ok,
*             or "unfinished" flag is written
*        then again to record 1 to clean up the "unfinished" flag
*     set the phys record numbers
      IBUF(INDR+11) = 2
      IBUF(INDR+19) = 1
      CALL FPWIDS(IBUF(INDR+1),1)
#ifndef Linux
      IF(IDEL.NE.0) GOTO 4000
*
*     Now we can unlock data record, i.e. set correct version# &
*     correct link from 'old' parts of data record to new one
*
      IWSAV = INDXB(IBUF,1000*LUNW+888)
      IF(IWSAV.LE.0) THEN
         IWSAV = INDXA(IBUF,1000*LUNW+888,INFU(1,LW)/4)
         IF(IWSAV.LE.0)
     +         CALL LOGERROR('FWKUPD.No space for +888 buffer')
         INDR = INDXB(IBUF,1000*LUNW+9+1)
         INDW = INDXB(IBUF,1000*LUNW+2)
      ENDIF
      ISV27 = INFU(27,LW)
3434  CONTINUE
*      PRINT '(1X,''FWKUPD.'',2A4,5I10)',
*     +  (INFU(IRSTU+14+(J-1),LR),J=1,5),IBLK,IOFF
      IF(IBLK.EQ.0 .AND. IOFF.EQ.0) GOTO 3444
         INFU(27,LW) = IBLK
         IF(IBLK.GT.INFU(30,LW))
     +      CALL LOGERROR('FWKUPD.3434. Incorrect blk#')
         IF(IBLK.EQ.ISV26) THEN
            CALL UCOPY(IBUF(IWRK1+1),IBUF(IWSAV+1),IBUF(IWRK1+2))
         ELSE
            CALL FRRECX(LUNW,IBUF(IWSAV),IBUF(IWSAV+1),IEND)
            IF(IEND.NE.0)
     +         CALL LOGERROR('FWKUPD. Ordered file corrupted')
            CALL FSWAP(IBUF(IWSAV+1),0)
            CALL FRCONV(IBUF(IWSAV+1),LOCAL,ICER)
         ENDIF
         IP = IOFF
         IPNXT = IP+11+IBUF(IWSAV+IP+11)-NW5
*
         IBUF(IWSAV+IPNXT+1) = INFU(IWSTU+22,LW)
         IBUF(IWSAV+IPNXT+2) = INFU(IWSTU+23,LW)
         IF(IBLK.EQ.INFU(IWSTU+17,LW)
     +           .AND.IOFF.EQ.INFU(IWSTU+18,LW)) THEN
            IF(IBUF(IWSAV+IPNXT+3).EQ.0) THEN
               IBUF(IWSAV+IPNXT+3) = INFU(IWSTU+24,LW)
               IBUF(IWSAV+IPNXT+4) = INFU(IWSTU+25,LW)
            ENDIF
         ENDIF
*        unlock data extent
         IBUF(IWSAV+IPNXT+5) = INFU(IWSTU+21,LW)
         ISEGCD = MOD(IBUF(IWSAV+IP+10),16)
         IBUF(IWSAV+IP+10) = ISEGCD+16*INFU(IWSTU+21,LW)
*         PRINT '(1X,''FWKUPD. VERSION#='',I7)',INFU(IWSTU+21,LW)
         NXTBLK = IBUF(IWSAV+IPNXT+3)
         NXTOFF = IBUF(IWSAV+IPNXT+4)
*
         IF(IBLK.EQ.ISV26) THEN
            CALL UCOPY(IBUF(IWSAV+1),IBUF(IWRK1+1),IBUF(IWSAV+2))
         ENDIF
*        Convert back if it's necessary and 'inhibit' flag is off
         IF(INFU(5,LW).NE.LOCAL.AND.INFU(ISGEN+4,LW).EQ.0) THEN
            CALL FRCONV(IBUF(IWSAV+1),INFU(5,LW),ICER)
         ENDIF
*        Write to disk
         INFU(26,LW) = IBLK
         CALL FWREC(LUNW,IBUF(IWSAV+1),IBUF(IWSAV+1),IEND)
         IF(IEND.NE.0) CALL LOGERROR('FWKUPD. File corrupted. L3434')
         IBLK = NXTBLK
         IOFF = NXTOFF
      GOTO 3434
3444  INFU(27,LW) = ISV27
*      IWSAV = INDXA(IBUF,1000*LUNW+888,-1)
4000  CONTINUE
*      PRINT *,'FWKUPD. LABEL 4000,+26,+30',INFU(26,LW),INFU(30,LW)
*
#endif /* not Linux */
*     set "unfinished" values
      IBUF(INDR+8) = 1
      IBUF(INDR+9) = 1
      CALL FPWIDS(IBUF(INDR+1),1)
*     reset the values
      IBUF(INDR+8) = N1
      IBUF(INDR+9) = N2
      IBUF(INDR+11) = 1
      IBUF(INDR+19) = 2
      CALL FPWIDS(IBUF(INDR+1),0)
*
      IF( ISV26.EQ.INFU(30,LW) ) THEN
*        restore the output buffer
         INDW = INDXB(IBUF,1000*LUNW+2)
         IWRK1 = INDXB(IBUF,1000*LUNW+3)
#ifndef Linux
         CALL UCOPY(IBUF(IWRK1+1),IBUF(INDW+1),IBUF(IWRK1+2))
#else /* Linux */
         CALL UCOPY(IBUF(IWRK1+1),IBUF(INDW+1),INFU(1,LW)/4)
#endif /* Linux */
         CALL UCOPY(ISVW,INFU(IWSTU+1,LW),NLDIM-IWSTU)
      ENDIF
#ifndef Linux
*
      IF(IDEL.NE.0 .AND. KEY2(6).NE.0) THEN
*        We have just deleted KEY2 from index directory, but the data
*        record is still kept in the file. In order to skip 'deleted'
*        data record during the 'repair' procedure, we have to mark up
*        it (& alternative version as well) as being free.
         IWSAV = INDXB(IBUF,1000*LUNW+888)
         IF(IWSAV.LE.0) THEN
            IWSAV = INDXA(IBUF,1000*LUNW+888,INFU(1,LW)/4)
            IF(IWSAV.LE.0)
     +         CALL LOGERROR('FWKUPD.No space for +888 buffer')
            INDR = INDXB(IBUF,1000*LUNW+9+1)
            INDW = INDXB(IBUF,1000*LUNW+2)
         ENDIF
         IBLK  = KEY2(6)
         IOFF  = KEY2(7)
         ISV27 = INFU(27,LW)
         IALT  = 0
4434     CONTINUE
*      PRINT '(1X,''FWKUPD.'',2A4,6I10)',
*     +  (INFU(IRSTU+14+(J-1),LR),J=1,4),IBLK,IOFF,IALT,INFU(30,LW)
         IF(IBLK.EQ.0 .AND. IOFF.EQ.0) GOTO 4444
         INFU(27,LW) = IBLK
         IF(IBLK.GT.INFU(30,LW))
     +      CALL LOGERROR('FWKUPD.4434. Incorrect blk#')
         IF(IBLK.EQ.ISV26) THEN
            CALL UCOPY(IBUF(IWRK1+1),IBUF(IWSAV+1),IBUF(IWRK1+2))
         ELSE
            CALL FRRECX(LUNW,IBUF(IWSAV),IBUF(IWSAV+1),IEND)
            IF(IEND.NE.0)
     +         CALL LOGERROR('FWKUPD. Ordered file corrupted')
            CALL FSWAP(IBUF(IWSAV+1),0)
            CALL FRCONV(IBUF(IWSAV+1),LOCAL,ICER)
         ENDIF
         IP = IOFF
         NALL  = IP+11+IBUF(IWSAV+IP+11)
         NTAIL = NALL - NW5
*      PRINT '(1X,''FWKUPD. ALT'',2I8,'' NEXT'',2I8,'' VERS#'',2I8)',
*     +   (IBUF(IWSAV+NTAIL+J),J=1,5),ISV26
*        Mark segment as being free
         CHA = '#Del'
*KEEP,ICHCHA.
*     get hollerith 4H from char*4
      ICH=IOR(ISHFT(ICHAR(CHA(1:1)),24),IOR(ISHFT(ICHAR(CHA(2:2)),16),
     +    IOR(ISHFT(ICHAR(CHA(3:3)), 8),ICHAR(          CHA(4:4)))))
*KEND.
         IBUF(IWSAV+IP+4) = ICH
         CHA = 'eted'
*KEEP,ICHCHA.
*     get hollerith 4H from char*4
      ICH=IOR(ISHFT(ICHAR(CHA(1:1)),24),IOR(ISHFT(ICHAR(CHA(2:2)),16),
     +    IOR(ISHFT(ICHAR(CHA(3:3)), 8),ICHAR(          CHA(4:4)))))
*KEND.
         IBUF(IWSAV+IP+5) = ICH
*        Get next address
         NXTBLK = IBUF(IWSAV+NTAIL+3)
         NXTOFF = IBUF(IWSAV+NTAIL+4)
         IF(NXTBLK.EQ.0.AND.IALT.EQ.0) THEN
*      PRINT *,'********************'
*      PRINT *,'BEFORE IALT',IALT,' NXTBLK',NXTBLK,' NXTOFF',NXTOFF
            NXTBLK = IBUF(IWSAV+NTAIL+1)
            NXTOFF = IBUF(IWSAV+NTAIL+2)
            IALT = IALT+1
*      PRINT *,'NTAIL+1-2',IBUF(IWSAV+NTAIL+1),IBUF(IWSAV+NTAIL+2)
*      PRINT *,'AFTER IALT',IALT,' NXTBLK',NXTBLK,' NXTOFF',NXTOFF
*      PRINT *,'********************'
         ENDIF
*
         IF(IBLK.EQ.ISV26) THEN
            CALL UCOPY(IBUF(IWSAV+1),IBUF(IWRK1+1),IBUF(IWSAV+2))
         ENDIF
*
*        Put it back on disk ...
*        Convert ...
         IF(INFU(5,LW).NE.LOCAL.AND.INFU(ISGEN+4,LW).EQ.0) THEN
            CALL FRCONV(IBUF(IWSAV+1),INFU(5,LW),ICER)
         ENDIF
*        ... and write down
         INFU(26,LW) = IBLK
         CALL FWREC(LUNW,IBUF(IWSAV+1),IBUF(IWSAV+1),IEND)
         IF(IEND.NE.0) CALL LOGERROR('FWKUPD. File corrupted. L3434')
         IBLK = NXTBLK
         IOFF = NXTOFF
         GOTO 4434
4444     INFU(27,LW) = ISV27
      ENDIF
*
#endif /* not Linux */
 100  CONTINUE
#ifdef Linux
*     unlock the record
      CALL FPUNLK(' ',IRET)
      INFU(ISGEN+8,LW) = 0
#endif /* Linux */
*     restore
      IF( IDEL.EQ.0 ) THEN
         CALL UCOPY(ISVK,INFU(IRSTU+14,LW),4)
      ENDIF
#ifndef Linux
*
#endif /* not Linux */
      INFU(IWSTU+4,LW) = 0
#ifndef Linux
*
#endif /* not Linux */
      IF(ISV26.EQ.INFU(30,LW)) THEN
         INFU(26,LW) =INFU(30,LW)
      ELSE
         INFU(26,LW) =INFU(30,LW)+1
      ENDIF
 200  CONTINUE
#ifndef Linux
*
*     unlock the record
      CALL FPUNLK(' ',IRET)
#endif /* not Linux */
*     set flag 'Reread data record'
      INFU(29,LR)=0
*     path for read incorrect
      INFU(ISGEN+9,LR) = 1
      LR = ISVLR
      LUNR = ISVLUN
#ifndef Linux
      RETURN
#endif /* not Linux */
*
#ifndef Linux
 299  PRINT *,' '
      PRINT *,'**************  STOP MESSAGE ***********************'
      PRINT *,'I was trying to lock for you the ordered file many'
      PRINT *,'times without success. It is likely another unknown '
      PRINT *,'writing process has locked it before you and keeps'
      PRINT *,'locking it for a long time. I need to stop your job'
      PRINT *,'to avoid destroying of the ordered file''s structure.'
      PRINT *,'Try again a bit later. Sorry for inconveniences,'
      PRINT *,' '
      PRINT *,'                  FWKUPD subr., FPACK'
      PRINT *,'****************************************************'
      STOP 777
#else /* Linux */
      RETURN
#endif /* Linux */
      END
