#ifndef Linux
*CMZ :  1.00/16 08/03/96  21.57.56  by  Unknown
*-- Author :    Sergey Esenov   26/02/96
      SUBROUTINE FWDATO(NR,IARRAY)
      INTEGER IARRAY(*)
      PARAMETER (NW5 = 5)
      INTEGER IFMTR(3),IFREE(2)
      CHARACTER FREE*8, CHA*4
      LOGICAL START
      DATA START/.TRUE./
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
*KEEP,DORIGL.
*     text for origin flag, used for text IO
      CHARACTER*8 ORIGL(15)
      DATA ORIGL/'BOS     ','LOOK    ','        ','        ','        ',
     +           '        ','        ','        ','        ','        ',
     +           '        ','        ','        ','        ','        '/
*KEND.
      CHARACTER*72 ERRMSG
*
*     First initialize some useful arrays ...
      IF(START) THEN
         CALL FTCTOI('3I,2A,5I',IFMTR,NFMTR)
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
         START=.FALSE.
      END IF
*
*     Do several common checks ...
*     If there is nothing to write ... exit
      IF(NR.LE.0) GOTO 999
      IER=0
*     If I/O pointers not defined ... error
      IF(LW.EQ.0 .OR. LUNW.EQ.0) THEN
         ERRMSG='Write unit undefined'
         GOTO 910
      ENDIF
*     If this is not an ordered access ... exit
      IF(INFU(25,LW).LE.0) GOTO 999
*     check definition of record segment header
      IF(INFU(IWSTU+4,LW).EQ.0)  GOTO 910
*     check definition of data segment header
      IF(INFU(IWSTU+5,LW).EQ.0)  GOTO 910
*
*     Now initialize the following variables
*     IS = number of already copied data words
      IS = 0
*     Current record segment code
      ISGC=0
*     Current data segment code
      JSGC=0
*     Number of data words already written
      JP8=0
*     Flag: had we just overwritten the old data record extents
*     =0 - no, =1 - yes
      INPLACE = 0
*
*     What we have to do - wheither overwrite old extents belonging
*     this logical record or write (append) new data to the end of file.
*     Check the flag which handles this stuff ... (It was set up in FWKEY)
*
100   IF(INFU(ISGEN+11,LW).GT.0) GOTO 200
************************* 'APPEND' MODE ***********************
*     get pointer within buffer
      IP=INFU(IWSTU+ 8,LW)
      JP=INFU(IWSTU+ 9,LW)
      KP=INFU(IWSTU+10,LW)
*      PRINT *,'FWDATO.APPEND MODE-> BK,IP8,JP9,KP10',
*     +   INFU(26,LW),IP,JP,KP
*      IF(JP.NE.0.AND.KP.NE.0) THEN
*         IF(IBUF(INW+2).EQ.KP)
*     + PRINT *,'FWDATO. BLOCK',INFU(26,LW),' NOT flushed yet'
*         IF(IBUF(INW+2).EQ.IP+11+IBUF(INW+IP+11))
*     + PRINT *,'FWDATO. BLOCK',INFU(26,LW),' already flushed'
*      ENDIF
*     get the pointer to the buffer bank (...+2 for output)
      INW = INDXB(IBUF,1000*LUNW+2)
      IF(INW.EQ.0) THEN
*        Not found. Create one ...
         INW=INDXA(IBUF,1000*LUNW+2,INFU(1,LW)/4)
         IF(INW.EQ.0) CALL LOGERROR(
     +       'FWDATO. No free space for output buffer')
*        first two words of the buffer are defined
         IBUF(INW+1)=IBUF(INW)
         IBUF(INW+2)=2
         IP          =0
      ENDIF
*     Maximum number of words available in buffer
      NALL  = IBUF(INW+1) - NW5
      NUSED = IBUF(INW+2)
*      print *,'NALL,NUSED',NALL,NUSED
*
      IF(INFU(IWSTU+4,LW).EQ.1.OR.INPLACE.EQ.1) THEN
*        record segment header not yet added to buffer - set added flag
         INFU(IWSTU+4,LW)=2
*        update the position counter
         INFU(IWSTU+7,LW)=INFU(IWSTU+7,LW)+1
*        reset order flag
         INFU(IWKEY+8,LW)=0
*                RHS +  DSH       + 1 data word at least
         IF(NUSED+11+INFU(IWHDR+1,LW)+1.LE.NALL) THEN
*           update the pointer to the record-segment header IP
            IF(IP.EQ.0) THEN
               IP=2
*              reset the position counter
               INFU(IWSTU+7,LW)=1
            ELSE
               IP=(IP+11)+IBUF(INW+IP+11)
            END IF
*           The next 2 variables (...+15,+16) are used in the
*           'leaf' index item, i.e. we keep there the disk address
*           of the beginning of the data record
            IF(INPLACE.EQ.0) THEN
               INFU(IWSTU+15,LW) = INFU(26,LW)
               INFU(IWSTU+16,LW) = IP
            ELSE
               INFU(IWSTU+24,LW) = INFU(26,LW)
               INFU(IWSTU+25,LW) = IP
            ENDIF
            INPLACE = 0
            GOTO 120
         ENDIF
*        No space in this block
*        Previous log.rec. flushed already by FWKUPD
         IF(INFU(30,LW).GT.INFU(26,LW)) THEN
               INFU(26,LW) = INFU(30,LW)+1
         ELSE
               INFU(26,LW) = INFU(26,LW)+1
         ENDIF
         IF(INPLACE.EQ.0) THEN
            INFU(IWSTU+15,LW) = INFU(26,LW)
            INFU(IWSTU+16,LW) = 2
         ELSE
            INFU(IWSTU+24,LW) = INFU(26,LW)
            INFU(IWSTU+25,LW) = 2
         ENDIF
         INPLACE = 0
         GOTO 111
      ELSE
*        record segment header already added
         IF(INFU(IWSTU+5,LW).EQ.1) THEN
*           data segment header not yet added - set added flag
            INFU(IWSTU+5,LW)=2
*           new data segment header. Is the space available?
            IF(NUSED+INFU(IWHDR+1,LW)+1.LE.NALL) THEN
*              update the data segment header JP
               JP=JP+IBUF(INW+JP+1)+IBUF(INW+JP+9)
               GOTO 130
            ELSE
*              no space in current record - update record segment code
               IF(IBUF(INW+IP+10).LE.1)
     +            IBUF(INW+IP+10)=IBUF(INW+IP+10)+1
               ISGC=IBUF(INW+IP+10)
            END IF
         ELSE
*           data segment header already added
            IF(NUSED+1.LE.NALL) GOTO 140
            JP8=IBUF(INW+JP+8)+IBUF(INW+JP+9)
*           update record segment code
            IF(IBUF(INW+IP+10).LE.1)
     +           IBUF(INW+IP+10)=IBUF(INW+IP+10)+1
            ISGC=IBUF(INW+IP+10)
*           update segment code
            IF(IBUF(INW+JP+7).LE.1)
     +           IBUF(INW+JP+7)=IBUF(INW+JP+7)+1
            JSGC=IBUF(INW+JP+7)
*      PRINT '(1x,''FWDATO.APPEND.TOWRITE.BK,IP,JP,KP'',4I8)',
*     + INFU(26,LW),IP,JP,KP
         END IF
      END IF
*
*     write the record and reset the buffer
  110 CONTINUE
      NUSED = IBUF(INW+2)
      IBUF(INW+2) = IP + 11 + IBUF(INW+IP+11)
*      PRINT *,'Bef write NUSED,IBUF(INW+2)',NUSED,IBUF(INW+2)
*     Address of alternative version of log. record
      IBUF(INW+NUSED+1)=INFU(IWSTU+22,LW)
      IBUF(INW+NUSED+2)=INFU(IWSTU+23,LW)
*     Address of next extent of this log. record
      IBUF(INW+NUSED+3)=INFU(26,LW)+1
      IBUF(INW+NUSED+4)=2
*     ... & version# = 0 - 'segment is locked for reading processes'
      IBUF(INW+NUSED+5)=0
*
*     convert word format ...
      JFM=INFU(5,LW)
*     ... if output ne local and inhibit flag not set
      IF(JFM.NE.LOCAL.AND.INFU(ISGEN+4,LW).EQ.0) THEN
         CALL FRCONV(IBUF(INW+1),JFM,ICER)
      END IF
      CALL FWREC(LUNW,IBUF(INW+1),IBUF(INW+1),IEND)
*     increase order flag (not only for keyed access)
  111 INFU(IWKEY+8,LW)=INFU(IWKEY+8,LW)+1
*     increase the record counter
      INFU(IWSTA+1,LW)=INFU(IWSTA+1,LW)+1
*     reset the position counter
      INFU(IWSTU+7,LW)=1
*     reset the pointer to the record segment header IP
      IBUF(INW+2)=2
      IP=2
      JP=0
      KP=0
*
*     add the record segment header (update recordnumber code before)
  120 INFU(IWKEY+3,LW)=100*INFU(IWSTU+15,LW)+INFU(IWSTU+7,LW)-1
*     Copy the KEY Info to output buffer
      DO 122 I=1,10
  122 IBUF(INW+IP+I) =INFU(IWKEY+I,LW)
*     modify header format flag, if inhibit convert flag set
      IF(INFU(ISGEN+4,LW).NE.0) THEN
         JFM=INFU(5,LW)
         IBUF(INW+IP+2)=16*JFM+MOD(IBUF(INW+IP+2),16)
      END IF
*
*     convert the event name, if necessary
      JFRMT=INFU(5,LW)
      IF(JFRMT.EQ.0) JFRMT=LOCAL
      IF(JFRMT.EQ.2) JFRMT=1
      CALL FCNVR(LOCAL,JFRMT,IBUF(INW+IP+1),10,0,IFMTR,NFMTR)
*
      INFU(IWSTU+4,LW)=2
*     restore previous segment code
      IBUF(INW+IP+10)=ISGC
*     reset the counter for the words following
      IBUF(INW+IP+11)=NW5
*     reset the pointer to the data segment header JP
      JP=IP+11
*
*     add the data segment header
  130 DO 132 I=1,INFU(IWHDR+1,LW)
  132 IBUF(INW+JP+I)=INFU(IWHDR+I,LW)
      INFU(IWSTU+5,LW)=2
*     restore previous segment code
      IBUF(INW+JP+7)=JSGC
*     insert the p-counter and reset the counter for the words following
      IBUF(INW+JP+8)=JP8
      IBUF(INW+JP+9)=0
*     update length information in record
      IBUF(INW+IP+11)=IBUF(INW+IP+11)+INFU(IWHDR+1,LW)
*     reset the pointer to the data
      KP=JP+IBUF(INW+JP+1)
      NUSED = IP+11+IBUF(INW+IP+11)-NW5
*     copy as many data as possible
  140 NCOP=MIN0(NR-IS,NALL-NUSED)
      CALL UCOPY(IARRAY(IS+1),IBUF(INW+KP+1),NCOP)
*     update length information in record
      IBUF(INW+IP+11)=IBUF(INW+IP+11)+NCOP
      IBUF(INW+JP+ 9)=IBUF(INW+JP+ 9)+NCOP
      IBUF(INW+2)    =IP+11+IBUF(INW+IP+11)-NW5
*     update local pointer/counter
      IS=IS+NCOP
      KP=KP+NCOP
      IF(IS.LT.NR) THEN
*        data are splitted - save number of previous words
         JP8=IBUF(INW+JP+8)+IBUF(INW+JP+9)
*        update record segment code
         IF(IBUF(INW+IP+10).LE.1) IBUF(INW+IP+10)=IBUF(INW+IP+10)+1
         ISGC=IBUF(INW+IP+10)
*        update data segment code
         IF(IBUF(INW+JP+ 7).LE.1) IBUF(INW+JP+ 7)=IBUF(INW+JP+ 7)+1
         JSGC=IBUF(INW+JP+7)
         GOTO 110
      END IF
*
*     data transfer finished
*     save pointer
      INFU(IWSTU+ 8,LW)=IP
      INFU(IWSTU+ 9,LW)=JP
      INFU(IWSTU+10,LW)=KP
*
*      PRINT '(1X,''FWDATO.APPEND.NOWRITE.EXIT.BK,IP8,JP9,KP10'',6I8)',
*     + INFU(26,LW),IP,JP,KP,IBUF(INW+2),11+IP+IBUF(INW+IP+11)
      GOTO 999
*
********************'UPDATE IN PLACE' MODE *********************
200   CONTINUE
      IP=INFU(IWSTU+18,LW)
      JP=INFU(IWSTU+19,LW)
      KP=INFU(IWSTU+20,LW)
*      PRINT *,'FWDATO. UPDATE MODE-> BK17,IP18,JP19,KP20',
*     +   INFU(IWSTU+17,LW),IP,JP,KP
      INW = INDXB(IBUF,1000*LUNW+2)
      IF(INW.LE.0) CALL LOGERROR('FWDATO. +2-buffer not found')
*
*     We have to overwrite the space occupied with old version
      NALL = IP+11+IBUF(INW+IP+11)-NW5
*
      IF(KP.GT.0) THEN
         NUSED = KP
      ELSEIF(JP.GT.0) THEN
         NUSED = JP+IBUF(INW+JP+1)
      ELSE
         NUSED = IP+11
      ENDIF
*      PRINT '(1X,''FWDATO. IWST+4,NUSED ='',2I8)',
*     +     INFU(IWSTU+4,LW),NUSED
*
      IF(INFU(IWSTU+4,LW).EQ.1) THEN
         INFU(IWSTU+4,LW)  = 2
*        Clear segment code from version#
         IBUF(INW+IP+10)  = MOD(IBUF(INW+IP+10),16)
*        The space for record seg. header, data seg. header &
*        at least, one data word must be available.
         IF(NUSED+INFU(IWHDR+1,LW)+1.LE.NALL) GOTO 220
*        No space. Write to the end of file ...
*        First restore buffer ...
         IWRK1 = INDXB(IBUF,1000*LUNW+3)
         IF(IWRK1.LE.0) CALL LOGERROR('FWDATO.WRITE buffer not found')
         CALL UCOPY(IBUF(IWRK1+1),IBUF(INW+1),IBUF(IWRK1+2))
*        ... and control variables
         INFU(26,LW) = INFU(ISGEN+11,LW)
         INFU(ISGEN+11,LW) = 0
         INFU(IWSTU+17,LW) = 0
         INFU(IWSTU+18,LW) = 0
         GOTO 100
      ELSE
*        record segment header already added
         IF(INFU(IWSTU+5,LW).EQ.1) THEN
*           data segment header not yet added - set added flag
            INFU(IWSTU+5,LW)=2
*           new data segment header. Is the space available?
            IF(NUSED+INFU(IWHDR+1,LW)+1.LE.NALL) THEN
*              update the data segment header JP
               JP=JP+IBUF(INW+JP+1)+IBUF(INW+JP+9)
               GOTO 230
            ENDIF
*           no space in current record - update record segment code
            IF(IBUF(INW+IP+10).LE.1)
     +            IBUF(INW+IP+10)=IBUF(INW+IP+10)+1
            IF(IBUF(INW+IP+10).GT.2)  IBUF(INW+IP+10)=2
            ISGC=IBUF(INW+IP+10)
         ELSE
*           data segment header already added
            IF(NUSED+1.LE.NALL) GOTO 240
            JP8=IBUF(INW+JP+8)+IBUF(INW+JP+9)
*           update record segment code
            IF(IBUF(INW+IP+10).LE.1)
     +           IBUF(INW+IP+10)=IBUF(INW+IP+10)+1
            IF(IBUF(INW+IP+10).GT.2) IBUF(INW+IP+10)=2
            ISGC=IBUF(INW+IP+10)
*           update segment code
            IF(IBUF(INW+JP+7).LE.1)
     +           IBUF(INW+JP+7)=IBUF(INW+JP+7)+1
            JSGC=IBUF(INW+JP+7)
         END IF
      END IF
*
*     write the record and reset the buffer
300   CONTINUE
*     Try to write number of remaining words & label "#-FREE-#"
      NFREE = NALL - NUSED
      IF(NFREE.GT.0) IBUF(INW+NUSED+1) = NFREE
      IF(NFREE.GT.1) IBUF(INW+NUSED+2) = IFREE(1)
      IF(NFREE.GT.2) IBUF(INW+NUSED+3) = IFREE(2)
*     Set reference to alternative data record
      IBUF(INW+NALL+1) = INFU(IWSTU+22,LW)
      IBUF(INW+NALL+2) = INFU(IWSTU+23,LW)
*     Lock this segment for reading processes.
      IBUF(INW+NALL+5) = 0
*
*     Check if it's the last extent
      NXTBLK = IBUF(INW+NALL+3)
      NXTOFF = IBUF(INW+NALL+4)
*      PRINT '(1X,''FWDATO. NXTBLK='',I5,1X,''NXTOFF='',I5)',
*     +  NXTBLK,NXTOFF
*      PRINT '(1X,''FWDATO. IWSTU+17:20'',4I8)',
*     +  (INFU(IWSTU+J,LW),J=17,20)
*
*     if this block is in memory ...
      IF(INFU(IWSTU+17,LW).EQ.NXTBLK) GOTO 400
*
*      IF(NXTBLK.EQ.0.AND.INFU(IWSTU+17,LW).EQ.INFU(ISGEN+11,LW)) THEN
*         INFU(26,LW) = INFU(ISGEN+11,LW)
*         INFU(ISGEN+11,LW) = 0
*        we will keep the reference to new part of data record here
*         INFU(IWSTU+24,LW) = INFU(26,LW)
*         INFU(IWSTU+25,LW) = INFU(IWSTU+8,LW)
*
*         INPLACE = 1
*      PRINT '(1X,''FWDATO. UPDATE. GO APPEND. IP,JP,KP'',3I8)',
*     + IP,JP,KP
*         GOTO 100
*      ENDIF
*
*     Flush the buffer
*     ----------------
*     convert word format ...
      JFM=INFU(5,LW)
*     ... if output ne local and inhibit flag not set
      IF(JFM.NE.LOCAL.AND.INFU(ISGEN+4,LW).EQ.0) THEN
          CALL FRCONV(IBUF(INW+1),JFM,ICER)
      END IF
      ISV26 = INFU(26,LW)
      INFU(26,LW) = INFU(IWSTU+17,LW)
      CALL FWREC(LUNW,IBUF(INW+1),IBUF(INW+1),IEND)
      INFU(26,LW) = ISV26
*     increase order flag (not only for keyed access)
      INFU(IWKEY+8,LW)=INFU(IWKEY+8,LW)+1
*     increase the record counter
      INFU(IWSTA+1,LW)=INFU(IWSTA+1,LW)+1
*     reset the position counter
      INFU(IWSTU+7,LW)=1
*
320   CONTINUE
      IF(NXTBLK.EQ.0) THEN
         INFU(26,LW) = INFU(ISGEN+11,LW)
         INFU(ISGEN+11,LW) = 0
         INFU(IWSTU+24,LW) = INFU(26,LW)
         INFU(IWSTU+25,LW) = INFU(IWSTU+8,LW)
         INPLACE = 1
*        Now we have to read last block of file to buffer
         IWRK1 = INDXB(IBUF,1000*LUNW+3)
         IF(IWRK1.LE.0) CALL LOGERROR('FWDATO. +3 buffer not found')
*        Copy old contents (saved in FWKEY) to +2 buffer
         CALL UCOPY(IBUF(IWRK1+1),IBUF(INW+1),IBUF(IWRK1+2))
*         PRINT *,'UCOPY IBUF(IWRK1+2)',IBUF(IWRK1+2)
         GOTO 100
      ENDIF
*
*     It's not the last extent.
*     Read the block containing next extent
350   ISVLR  = LR
      ISVLNR = LUNR
      LR     = LW
      LUNR   = LUNW
      ISV27  = INFU(27,LR)
      INFU(27,LR) = NXTBLK
      CALL FRRECX(LUNR,IBUF(INW),IBUF(INW+1),IER)
      INFU(27,LR) = ISV27
      LR          = ISVLR
      LUNR        = ISVLNR
      IF(IER.NE.0)  CALL LOGERROR('FWDATO. Corrupted ordered file.')
      CALL FSWAP(IBUF(INW+1),0)
      CALL FRCONV(IBUF(INW+1),LOCAL,ICER)
*
400   INFU(IWSTU+17,LW) = NXTBLK
      INFU(IWSTU+18,LW) = NXTOFF
      INFU(IWSTU+19,LW) = 0
      INFU(IWSTU+20,LW) = 0
      IP = NXTOFF
      JP = 0
      KP = 0
      NALL = IP+11+IBUF(INW+IP+11)
      IF(IBUF(INW+IP+11).LT.0.OR.
     +   NALL.GT.IBUF(INW+2).OR.
     +   NALL.GT.IBUF(INW+1)) THEN
*        dirty small gap?
         NXTBLK = NXTBLK+1
         NXTOFF = 2
         GOTO 350
      ENDIF
      NALL = NALL - NW5
      IF(IP+11+INFU(IWHDR+1,LW)+1.GT.NALL) THEN
*        no room for new data
         NXTBLK = IBUF(INW+NALL+3)
         NXTOFF = IBUF(INW+NALL+4)
         GOTO 320
      ENDIF
      NUSED = IP+11
*
*     add the record segment header (update recordnumber code before)
  220 INFU(IWKEY+3,LW)=100*INFU(IWSTU+17,LW)+INFU(IWSTU+7,LW)-1
*     Copy the KEY Info to output buffer
      DO 222 I=1,10
  222 IBUF(INW+IP+I) =INFU(IWKEY+I,LW)
*     modify header format flag, if inhibit convert flag set
      IF(INFU(ISGEN+4,LW).NE.0) THEN
         JFM=INFU(5,LW)
         IBUF(INW+IP+2)=16*JFM+MOD(IBUF(INW+IP+2),16)
      END IF
*
*     convert the event name, if necessary
      JFRMT=INFU(5,LW)
      IF(JFRMT.EQ.0) JFRMT=LOCAL
      IF(JFRMT.EQ.2) JFRMT=1
      CALL FCNVR(LOCAL,JFRMT,IBUF(INW+IP+1),10,0,IFMTR,NFMTR)
*
      INFU(IWSTU+4,LW)=2
*     restore previous segment code
      IBUF(INW+IP+10)=ISGC
*     reset the pointer to the data segment header JP
      JP=IP+11
*
*     add the data segment header
  230 DO 232 I=1,INFU(IWHDR+1,LW)
  232 IBUF(INW+JP+I)=INFU(IWHDR+I,LW)
      INFU(IWSTU+5,LW)=2
*     restore previous segment code
      IBUF(INW+JP+7)=JSGC
*     insert the p-counter and reset the counter for the words following
      IBUF(INW+JP+8)=JP8
      IBUF(INW+JP+9)=0
      NUSED =NUSED+INFU(IWHDR+1,LW)
*     reset the pointer to the data
      KP=JP+IBUF(INW+JP+1)
*     copy as many data as possible
  240 NCOP=MIN0(NR-IS,NALL-KP)
      CALL UCOPY(IARRAY(IS+1),IBUF(INW+KP+1),NCOP)
      NUSED = NUSED + NCOP
      IBUF(INW+JP+ 9)=IBUF(INW+JP+ 9)+NCOP
*      PRINT '(1X,''FWDATO. JP+8,JP+9'',2I8)',(IBUF(INW+JP+J),J=8,9)
*     update local pointer/counter
      IS=IS+NCOP
      KP=KP+NCOP
      IF(IS.LT.NR) THEN
*        data are splitted - save number of previous words
         JP8=IBUF(INW+JP+8)+IBUF(INW+JP+9)
*        update record segment code
         IF(IBUF(INW+IP+10).LE.1) IBUF(INW+IP+10)=IBUF(INW+IP+10)+1
         ISGC=IBUF(INW+IP+10)
*        update data segment code
         IF(IBUF(INW+JP+ 7).LE.1) IBUF(INW+JP+ 7)=IBUF(INW+JP+ 7)+1
         JSGC=IBUF(INW+JP+7)
*      PRINT '(1X,''FWDATO. UPDATE. GO WRITE. IP,JP,KP'',3I8)',
*     + IP,JP,KP
         GOTO 300
      END IF
*
*     data transfer finished - save pointer
      INFU(IWSTU+18,LW)=IP
      INFU(IWSTU+19,LW)=JP
      INFU(IWSTU+20,LW)=KP
*      PRINT '(1X,''FWDATO.UPDATE.NOWRITE.EXIT.BK,IP,JP,KP'',4I8)',
*     +  INFU(IWSTU+17,LW),IP,JP,KP
*
      GOTO 999
*
*     error conditions
  910 IER=IER+1
CCC   CALL FPWMSG(IER,'FWDATO',ERRMSG)
*
  999 CONTINUE
      END
#else /* Linux */
      subroutine dummy_fwdato
      implicit none
      return
      end
#endif /* Linux */
