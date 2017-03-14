#ifndef Linux
*CMZ :  1.00/16 11/03/96  21.25.36  by  Unknown
*-- Author :    Sergey Esenov   04/03/96
      SUBROUTINE FRPAIR(DANAME,IER)
*
******* REPAIR ORDERED FILE WITH CORRUPTED INDEX STRUCTURE *************
*
*     Lookup through the ordered file skipping index blocks and
*     build new index tree using if possible the space occupied
*     by old index blocks.
*
************************************************************************
*
      CHARACTER*(*) DANAME
      PARAMETER (NW5 = 5)
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
*KEND.
      CHARACTER CHA*4
      INTEGER  IBLK,IOFF,KEY(7)
      INTEGER  IFREE(2),IDEL(2),IIND(2)
      LOGICAL  IS01,IS03,ILLG,START
      DATA     START/.TRUE./
      SAVE     START
*
*     statement functions
         NEXTIP(IP)=IP+IBUF(INR+IP+11)+11
         NEXTJP(JP)=JP+IBUF(INR+JP+ 1)+IBUF(INR+JP+ 9)
         ILLG(I)   =I.LT.0.OR.I.GT.3
         IS01(I)   =I.EQ.0.OR.I.EQ.1
         IS03(I)   =I.EQ.0.OR.I.EQ.3
*
      LWS   = LW
      LUNWS = LUNW
      CALL FWNAME(DANAME)
      IF(LR.EQ.0.OR.LW.EQ.0.OR.LUNR.EQ.0.OR.LUNW.EQ.0) THEN
         IER = 1
         GOTO 999
      ENDIF
      IF(START) THEN
         START=.FALSE.
         CHA  ='#Del'
*KEEP,ICHCHA.
*     get hollerith 4H from char*4
      ICH=IOR(ISHFT(ICHAR(CHA(1:1)),24),IOR(ISHFT(ICHAR(CHA(2:2)),16),
     +    IOR(ISHFT(ICHAR(CHA(3:3)), 8),ICHAR(          CHA(4:4)))))
*KEND.
         IDEL(1) = ICH
         CHA  ='eted'
*KEEP,ICHCHA.
*     get hollerith 4H from char*4
      ICH=IOR(ISHFT(ICHAR(CHA(1:1)),24),IOR(ISHFT(ICHAR(CHA(2:2)),16),
     +    IOR(ISHFT(ICHAR(CHA(3:3)), 8),ICHAR(          CHA(4:4)))))
*KEND.
         IDEL(2) = ICH
         CHA  = '#-FR'
*KEEP,ICHCHA.
*     get hollerith 4H from char*4
      ICH=IOR(ISHFT(ICHAR(CHA(1:1)),24),IOR(ISHFT(ICHAR(CHA(2:2)),16),
     +    IOR(ISHFT(ICHAR(CHA(3:3)), 8),ICHAR(          CHA(4:4)))))
*KEND.
         IFREE(1)= ICH
         CHA  = 'EE-#'
*KEEP,ICHCHA.
*     get hollerith 4H from char*4
      ICH=IOR(ISHFT(ICHAR(CHA(1:1)),24),IOR(ISHFT(ICHAR(CHA(2:2)),16),
     +    IOR(ISHFT(ICHAR(CHA(3:3)), 8),ICHAR(          CHA(4:4)))))
*KEND.
         IFREE(2)= ICH
         CHA  = 'FPIN'
*KEEP,ICHCHA.
*     get hollerith 4H from char*4
      ICH=IOR(ISHFT(ICHAR(CHA(1:1)),24),IOR(ISHFT(ICHAR(CHA(2:2)),16),
     +    IOR(ISHFT(ICHAR(CHA(3:3)), 8),ICHAR(          CHA(4:4)))))
*KEND.
         IIND(1) = ICH
         CHA  = 'DEXA'
*KEEP,ICHCHA.
*     get hollerith 4H from char*4
      ICH=IOR(ISHFT(ICHAR(CHA(1:1)),24),IOR(ISHFT(ICHAR(CHA(2:2)),16),
     +    IOR(ISHFT(ICHAR(CHA(3:3)), 8),ICHAR(          CHA(4:4)))))
*KEND.
         IIND(2) = ICH
      ENDIF
*
      LRSAV  = LR
      LNRSAV = LUNR
      LR     = LW
      LUNR   = LUNW
      ISV26  = INFU(26,LW)
      ISV27  = INFU(27,LR)	
*
      PRINT *,' '
      PRINT *,'Step 1. Check the file size'
      PRINT *,'---------------------------'
      PRINT *,' '
*
*     Get READ & WRITE buffers
      INR   = INDXB(IBUF,1000*LUNW+1)
      IF(INR.LE.0) THEN
         INR = INDXA(IBUF,1000*LUNW+1,INFU(1,LW)/4)
         IF(INR.LE.0) CALL LOGERROR('FRPAIR. No space '//
     +       'for READ buffer.')
         IBUF(INR+1) = IBUF(INR)
         IBUF(INR+2) = 2
      ENDIF
      INW   = INDXB(IBUF,1000*LUNW+2)
      IF(INW.LE.0) THEN
         INW = INDXA(IBUF,1000*LUNW+2,INFU(1,LW)/4)
         IF(INW.LE.0) CALL LOGERROR('FRPAIR. No space '//
     +            'for WRITE buffer.')
         IBUF(INW+1) = IBUF(INW)
         IBUF(INW+2) = 2
      ENDIF
*
*     Set exclusive lock ...
      NTRY = 0
01    CALL FPLOCK(' ',IRET)
      NTRY = NTRY + 1
      IF(NTRY.LT.100.AND.IRET.NE.0) GOTO 01
      IF(IRET.NE.0) THEN
         PRINT *,' '
         PRINT *,'FRPAIR. Unable to lock the file. Try later again'
         PRINT *,' '
         IER = 2
         GOTO 200
      ENDIF
*
*     Read the 1-st or the 2-nd block and extract the size of the file
*     ----------------------------------------------------------------
      IFSZ = 0
      IBLK = 1
10    NTRY = 0
20    INFU(27,LR) = IBLK
      CALL FRRECX(LUNR,IBUF(INR),IBUF(INR+1),IRC)
      IF(IRC.LT.0) THEN
*        EOF encountered ...
         PRINT *,' '
         PRINT '(1X,''FRPAIR. When attempting to read the block#'',I2'
     +      // ','' EOF condition encountered ... so'')',IBLK
         PRINT *,'FRPAIR. File is empty ... bye-bye!'
         IER  = -1
         GOTO 190
      ELSEIF(IRC.GT.0) THEN
*        Read error of physical block ...
         NTRY = NTRY + 1
         IF(NTRY.LT.10) GOTO 20
         PRINT '(1X,''Cannot read the INDEX block#'',I2,' //
     +      ''' after 10 attempts! Error code'',I5)',IRC
         PRINT *,'FRPAIR. ... skip this block.'
         IBLK = IBLK + 1
         IF(IBLK.LE.2) GOTO 10
         IFSZ = 0
      ELSE
*        Convert the buffer
         CALL FRCONV(IBUF(INR+1),LOCAL,ICER)
         IFSZ = IBUF(INR+18)
*        Is it really the index block?
         IF(IBUF(INR+6).NE.IIND(1).OR.IBUF(INR+7).NE.IIND(2).OR.
     +      IBUF(INR+15).NE.IIND(1).OR.IBUF(INR+16).NE.IIND(2)) THEN
            IFSZ=0
            IBLK = IBLK + 1
            IF(IBLK.LE.2) GOTO 10
         ENDIF
      ENDIF
      IF(IFSZ.EQ.0) THEN
         PRINT *,'FRPAIR. Top level''s index blocks corrupted.'
         PRINT *,'FRPAIR. Find out the actual file size ...'
         IBLK = 3
30       INFU(27,LR) = IBLK
         CALL FRRECX(LUNR,IBUF(INR),IBUF(INR+1),IRC)
         IF(IRC.LT.0) GOTO 40
         IBLK = IBLK+1
         GOTO 30
*        This is the actual size of the file (in blocks)
40       IFSZ = IBLK-1
      ENDIF
*
      PRINT *,'The file size is',IFSZ,' blocks.'
      PRINT *,'--------  End of step 1 --------'
*
      INFU(30,LW) = IFSZ
*
      PRINT *,' '
      PRINT *,'Step 2. Build the new index tree ...'
      PRINT *,'------------------------------------'
      PRINT *,' '
*     get index buffer
      INDX   = INDXB(IBUF,1000*LUNW+9+1)
      IF(INDX.EQ.0) THEN
         INDX = INDXA(IBUF,1000*LUNW+9+1,INFU(1,LW)/4)
         IF(INDX.LE.0) CALL LOGERROR('FRPAIR. No space '//
     +         'for index buffer.')
         IBUF(INDX+1) = IBUF(INDX)
      ENDIF
*     get working buffer ...
      INWRK  = INDXB(IBUF,1000*LUNW+3)
      IF(INWRK.LE.0) THEN
         INWRK = INDXA(IBUF,1000*LUNW+3,INFU(1,LW)/4)
         IF(INWRK.LE.0) CALL LOGERROR('FRPAIR. No space '//
     +          'for working buffer.')
         IBUF(INWRK+1)=IBUF(INWRK)
         IBUF(INWRK+2)=2
      ENDIF
*
*     save old sync flag
      ISV12 = INFU(ISGEN+12,LW)
      INFU(ISGEN+12,LW) = 1
*
*     Initialise top level index block
      IBUF(INDX+1) = INFU(1,LR)/4
      IBUF(INDX+2) = IBUF(INDX+1)
*     only one level
      INFU(25,LW) = 1
*     initial counter; set "unfinished" flag
      IBUF(INDX+8) = 1
      IBUF(INDX+9) = 1
*     the home block number
      IBUF(INDX+11) = 1
      IBUF(INDX+14) = 10
      IBUF(INDX+22) = 1
*     current position - 0
      IBUF(INDX+15)= 0
*     this block points to data blocks
      IBUF(INDX+17) = 1
*     the alt home block number
      IBUF(INDX+19) = 2
*     the number of blocks in the file
      IBUF(INDX+18) = INFU(30,LW)
*     write
      CALL FPWIDS(IBUF(INDX+1),1)
*     write the alternative block also
      CALL FPWIDS(IBUF(INDX+1),1)
*
*     Switch off sync flag (for speeding)
      INFU(ISGEN+12,LW) = 0
*
*
*     Number of error blocks
      NERR   = 0
*     Number of data records
      NDTREC = 0
*     Number of index blocks
      NIXBLK = 2
*     Total number of blocks
      NALBLK = 2
*
      IBLK   = 2
50    IBLK   = IBLK + 1
      IOFF   = 2
      INFU(27,LR) = IBLK
      CALL FRRECX(LUNR,IBUF(INR),IBUF(INR+1),IRC)
      IF(IRC.LT.0) GOTO 100
      NALBLK = NALBLK+1
      IF(IRC.GT.0) THEN
         PRINT '(1X,''FRPAIR. Block #'',I8,''Read error #'',I5)',
     +      IBLKIRC
         PRINT *,'FRPAIR. ... skip this block.'
         NERR = NERR+1
         GOTO 50
      ENDIF
*     Convert the buffer
      CALL FSWAP(IBUF(INR+1),0)
      CALL FRCONV(IBUF(INR+1),LOCAL,ICER)
*
*     Loop inside physical block
60    IP = IOFF
*
*     If end-of-information reached in this block ...
      IF(IP.GE.IBUF(INR+2)) GOTO 50
*
*     Fill the key
      DO 70 I=1,4
70    KEY(I) = IBUF(INR+IP+4+(I-1))
      KEY(5) = IBUF(INR+IP+9)
      KEY(6) = IBLK
      KEY(7) = IOFF
*
*      PRINT *,'NEW EVENT ....++++++++++++++++++++++++++++'
*      PRINT '(1X,2I6,1X,2A4,5I7)',IBLK,IOFF,(KEY(J),J=1,7)
*
      IOFF = NEXTIP(IP)
*
*     Check key name ...
*     ... if index block ... skip block
      IF(KEY(1).EQ.IIND(1).AND.KEY(2).EQ.IIND(2)) THEN
         NIXBLK = NIXBLK+1
*         print *,'**** index ****> skip'
         GOTO 50
      ENDIF
*
*     ... if deleted or free logical record ... skip it
      IF((KEY(1).EQ.IDEL(1).AND.KEY(2).EQ.IDEL(2)) .OR.
     +   (KEY(1).EQ.IFREE(1).AND.KEY(2).EQ.IFREE(2))) THEN
*         PRINT *,'IDEL OR IFREE'
         GOTO 60
      ENDIF
*
*     Check the record segment header
      ISGC   = MOD(IBUF(INR+IP+10),16)
      IF(ILLG(ISGC)) THEN
         PRINT *,' '
         PRINT
     +      '(1X,''FRPAIR. BLK#'',I8,'' OFF'',I6,''KEY '',2A4,2I10)'
     +      ,IBLK,IP,(KEY(I),I=1,4)
         PRINT '(1X,''FRPAIR. Illegal record segment code'',I3)',ISGC
         PRINT *,'FRPAIR. ... skip this segment'
         GOTO 60
      ENDIF
*
*     If this is not the start segment of logical record ... skip
      IF(.NOT.(IS01(ISGC))) THEN
*        PRINT *,' THIS SEGMENT IS ',ISGC
        GOTO 60
      ENDIF
*
*     get pointer to the tail control words
      NTAIL = IOFF - NW5
*     Check the versions of the first segment ...
*     ... in the beginning of segment
      IVERS1 = IBUF(INR+IP+10)/16
*     ... and in the end
      IVERS2 = IBUF(INR+NTAIL+5)
*     if version#'s are not equal it's "unfinished" segment ...
      IF(IVERS1.EQ.0.OR.IVERS2.EQ.0.OR.IVERS1.NE.IVERS2) THEN
         PRINT '(1X,''FRPAIR. The data record "'',2A4,2I12,''" '''//
     +     ',''is skipped'',/,10X,''because of version control:'''//
     +     ','' top version'',I7,'' bottom version'',I7)',
     +     (KEY(J),J=1,4),IVERS1,IVERS2
         GOTO 60
      ENDIF
*     Save version# of the 1-st segment ...
      IVERS = IVERS1
*
      NXTBLK = IBUF(INR+NTAIL+3)
      NXTOFF = IBUF(INR+NTAIL+4)
*
80    CONTINUE
*      print *,'CONT:NXTBLK,NXTOFF',NXTBLK,NXTOFF
      IF(NXTBLK.EQ.0.AND.NXTOFF.EQ.0) GOTO 90
      IF(NXTBLK.GT.INFU(30,LW).OR.NXTBLK.LE.2) THEN
*         PRINT *,'CONT. NXTBLK,INFU(30,LW)',NXTBLK,INFU(30,LW)
         GOTO 60
      ENDIF
      INFU(27,LR)=NXTBLK
      CALL FRRECX(LUNR,IBUF(INWRK),IBUF(INWRK+1),IRC)
*     Skip checking in case of READ errors
      IF(IRC.NE.0) GOTO 80
      CALL FSWAP(IBUF(INWRK+1),0)
      CALL FRCONV(IBUF(INWRK+1),LOCAL,ICER)
      IF(IBUF(INWRK+1).NE.INFU(1,LR)/4) GOTO 60
      IF(IBUF(INWRK+2).GT.IBUF(INWRK+1).AND.IBUF(INWRK+2).LT.2) THEN
*      PRINT *,'CONT. BUF+1,BUF+2',IBUF(INWRK+1),IBUF(INWRK+2)
           GOTO 60
      ENDIF
      IPN = NXTOFF
***      PRINT '(1X,''CONT:'',2A4,2I6,1x,z8)',
***     + (IBUF(INWRK+IPN+J),J=4,7),IBUF(INWRK+IPN+10)
      IV1 = IBUF(INWRK+IPN+10)/16
      NTAILN = IPN+11+IBUF(INWRK+IPN+11)
      IF(NTAILN.GT.IBUF(INWRK+2)) THEN
***      PRINT *,'CONT. NTAILN,IBUF(INWRK+2)',NTAILN,IBUF(INWRK+2)
         GOTO 60
      ENDIF
      NTAILN = NTAILN - NW5
      IV2 = IBUF(INWRK+NTAILN+5)
*     if version# of followed segments <> version# of 1-st segment
*        .... "unfinished" flag?
      IF(IV1.NE.IVERS.OR.IV2.NE.IVERS) THEN
***      PRINT *,'VERSION CONTROL OF CONT IVERS,IV1,IV2',IVERS,IV1,IV2
         GOTO 60
      ENDIF
      ISEGN  = MOD(IBUF(INWRK+IPN+10),16)
*     Check if this is the last segment ...
      IF(IS03(ISEGN)) GOTO 90
      NXTBLK = IBUF(INWRK+NTAILN+3)
      NXTOFF = IBUF(INWRK+NTAILN+4)
      GOTO 80
*
*     At this point logical record looks being correct one
*
90    CONTINUE
***      PRINT *,'CONT. OK'
*
*     Check if we have already added this entry to index directory
*     Copy and encoding the key ...
      CALL UCOPY(KEY,INFU(IRSTU+14,LR),4)
      CALL FPKENC(KEY,INFU(IRSTU+14,LR))
*     ... and searching ...
      CALL FPFIND(0,IFOUND)
***      PRINT '(1X,''IFOUND='',I2)',IFOUND
*
*     if this is a first such log. record ...
      IF(IFOUND.EQ.0) GOTO 95
*
*     The logical record with the same key exists AND has been tested
*     We need to check what version is the last
*
      LEVEL = INFU(25,LR)
      INX   = INDXB(IBUF,1000*LUNR+9+LEVEL)
      IF(INX.LE.0) CALL LOGERROR('FRPAIR. Index_buffer not found. 2')
      IF(IBUF(INX+17).GT.1) CALL LOGERROR('FRPAIR. MUST be leaf lev')
      JPX  = 13
      KPX  = JPX + IBUF(INX+JPX+1)
      NPOS = IBUF(INX+15)
*     Check the pointers to alt. log. rec.
      IALTBLK = IBUF(INX+KPX+NPOS+6)
      IALTOFF = IBUF(INX+KPX+NPOS+7)
*
*     If something wrong about registered version ...register this one
      IF(IALTBLK.GT.INFU(30,LW).OR.IALTBLK.LE.2) GOTO 95
      INFU(27,LR)=IALTBLK
      CALL FRRECX(LUNR,IBUF(INWRK),IBUF(INWRK+1),IRC)
      IF(IRC.NE.0) GOTO 95
      CALL FSWAP(IBUF(INWRK+1),0)
      CALL FRCONV(IBUF(INWRK+1),LOCAL,ICER)
      IF(IBUF(INWRK+1).NE.INFU(1,LR)/4) GOTO 95
      IF(IBUF(INWRK+2).LT.2.OR.IBUF(INWRK+2).GT.IBUF(INWRK+1))
     +    GOTO 95
      IF(IALTOFF.LT.2.OR.IALTOFF.GT.IBUF(INWRK+2)) GOTO 95
      IPA = IALTOFF
*     Check the alt. key ...
      IF(IBUF(INWRK+IPA+4).NE.KEY(1).OR.
     +    IBUF(INWRK+IPA+5).NE.KEY(2).OR.
     +     IBUF(INWRK+IPA+6).NE.KEY(3).OR.
     +      IBUF(INWRK+IPA+7).NE.KEY(4)) GOTO 95
*
      IV = IBUF(INWRK+IPA+10)/16
*     If registered version > this one ...
      IF(IV.EQ.1.AND.IVERS.EQ.1000000.OR.IV.GT.IVERS) THEN
***      PRINT *,'ALT. IV,IVERS',IV,IVERS
         GOTO 60
      ENDIF
*
*     Register this entry in index directory ...
95    CALL FWKIDX(KEY,IRC)
      NDTREC = NDTREC+1
*     Go to the next segment
      GOTO 60
*
100   CONTINUE
      INDX = INDXB(IBUF,1000*LUNW+9+1)
      IF(INDX.EQ.0) CALL LOGERROR('FRPAIR. TOP BUFFER NOT FOUND')
*     initial counter
      IBUF(INDX+8) = 2
      IBUF(INDX+9) = 3
      IBUF(INDX+18)= INFU(30,LW)
*     set the phys record numbers
      IBUF(INDX+11) = 2
      IBUF(INDX+19) = 1
      CALL FPWIDS(IBUF(INDX+1),1)
*     Set sync flag
      INFU(ISGEN+12,LW)=1
      CALL FPWIDS(IBUF(INDX+1),1)
*
*     Restore sync flag
      INFU(ISGEN+12,LW) = ISV12
*
*     unlock the record
190   CALL FPUNLK(' ',IRET)
*
      PRINT *,' '
      PRINT *,'************** INDEX REPAIR SUMMARY **************'
      PRINT *,'*'
      PRINT '(1X,''* Total number of blocks read ........'',I9)',NALBLK
      PRINT '(1X,''* Number of index blocks read ........'',I9)',NIXBLK
      PRINT '(1X,''* Number of error blocks .............'',I9)',NERR
      PRINT '(1X,''* Number of data records read ........'',I9)',NDTREC
      PRINT *,'*'
      PRINT *,'**************************************************'
      PRINT *,' '
*
200   INFU(27,LR) = ISV27
      INFU(26,LW) = ISV26
*     Rebuild the path
      INFU(ISGEN+9,LR) = 1
*     Reread data record
      INFU(29,LR) = 0
      LR          = LRSAV
      LUNR        = LNRSAV
*
999   LW   = LWS
      LUNW = LUNWS
      RETURN
      END
#else /* Linux */
      subroutine dummy_frpair
      implicit none
      return
      end
#endif /* Linux */
