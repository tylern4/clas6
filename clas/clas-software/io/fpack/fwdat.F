#ifndef Linux
*CMZU:  1.00/16 26/02/96  11.53.10  by  Sergey Esenov
*CMZ :  1.00/13 21/02/96  12.13.06  by  Wiesiek Dlugosz
*CMZ :  1.00/09 02/11/95  01.14.05  by  Wiesiek Dlugosz
#endif /* not Linux */
*CMZ :  1.00/07 01/09/95  11.46.57  by  Wiesiek Dlugosz
*CMZ :  1.00/05 06/08/95  18.03.22  by  Wiesiek Dlugosz
*CMZ :  1.00/00 16/12/94  17.00.00  by  Pavel Binko
*-- Author :    Volker Blobel   16/12/94
      SUBROUTINE FWDAT(NR,IARRAY)
*
      INTEGER IARRAY(*)
*
************************************************************************
*
*     Add data array ARRAY(1) ... ARRAY(NR) to the buffer and
*     write eventually
*
************************************************************************
*
#ifndef Linux
      INTEGER IFMTR(3),IREPKEY
      INTEGER IKNME(2)
      CHARACTER*8 CKNME
#else /* Linux */
      INTEGER IFMTR(3)
#endif /* Linux */
      CHARACTER*100 CHFMT, NAMDAT*8, CHA*4
      INTEGER KEY1(7),KEY0(7),ISVK(4)
      LOGICAL START
      DATA START/.TRUE./
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
*KEEP,DORIGL.
*     text for origin flag, used for text IO
      CHARACTER*8 ORIGL(15)
      DATA ORIGL/'BOS     ','LOOK    ','        ','        ','        ',
     +           '        ','        ','        ','        ','        ',
     +           '        ','        ','        ','        ','        '/
*KEND.
      CHARACTER*72 ERRMSG
*
      IF(START) THEN
         CALL FTCTOI('3I,2A,5I',IFMTR,NFMTR)
         START=.FALSE.
      END IF
*
*     general checks - start with check of arguments
      IF(NR.LE.0) GOTO 100
*     reset error flag
      IER=0
*     unit undefined
      IF(LW.EQ.0) THEN
         ERRMSG='Write unit undefined'
         GOTO 91
      ENDIF
      IF( INFU(7,LW).NE.0 ) THEN
         ERRMSG=' '
         GOTO 91
      ENDIF
#ifndef Linux
*     Ordered access
      IF(INFU(25,LW).GT.0) THEN
         CALL FWDATO(NR,IARRAY)
         GOTO 100
      ENDIF
#endif /* not Linux */
*     check definition of record segment header
      IF(INFU(IWSTU+4,LW).EQ.0) THEN
         ERRMSG='Record segment header undefined'
         GOTO 91
      ENDIF
*     check definition of data segment header
      IF(INFU(IWSTU+5,LW).EQ.0) THEN
         ERRMSG='Data segment header undefined'
         GOTO 91
      ENDIF
*
*     IS = number of already copied data words
      IS = 0
*
      IF(INFU(3,LW).EQ.2) GOTO 60
*
*     code for binary output *****************
*
*     get pointer within buffer
      ISGC=0
      JSGC=0
      IP=INFU(IWSTU+ 8,LW)
      JP=INFU(IWSTU+ 9,LW)
      KP=INFU(IWSTU+10,LW)
      JP8=0
*
*     buffer bank (...+2 for output)
      INDW=INDXB(IBUF,1000*LUNW+2)
      IF(INDW.EQ.0) THEN
         INDW=INDXA(IBUF,1000*LUNW+2,INFU(1,LW)/4)
         IF(INDW.EQ.0) THEN
            ERRMSG='No free storage for output buffer'
            GOTO 91
         ENDIF
*        first two words of the buffer are defined
         IBUF(INDW+1)=IBUF(INDW)
         IBUF(INDW+2)=2
         IP          =0
      END IF
#ifndef Linux
*
#else /* Linux */
***************************************************
*  here we overwrite data of an ordered access
*  LW file opened with the MODIFY option
***************************************************
       IF(INFU(2,LW).EQ.5.AND.INFU(4,LW).EQ.4) THEN
*  find whether a particular key is present in the
*  LW file. To do this you have to use variables called
*  LR  and LUNR.
           ISV26W = INFU(26,LW)
           ISV27W = INFU(27,LW)
           ISV26R = INFU(26,LR)
           ISV27R = INFU(27,LR)
           ISVLR = LR
           ISVLUNR = LUNR
           LR =LW
           LUNR = LUNW
           CALL UCOPY(INFU(IRSTU+14,LR),ISVK,4)
           CALL FPKENC(INFU(IWKEY+4,LW),KEY1)
           KEY1(3) = INFU(IWKEY+6,LW)
           KEY1(4) = INFU(IWKEY+7,LW)
           KEY1(5) = INFU(IWKEY+9,LW)
           KEY1(6) = INFU(IWSTU+15,LW)
           KEY1(7) = INFU(IWSTU+16,LW)
           CALL UCOPY(KEY1,INFU(IRSTU+14,LR),4)
           CALL FPFIND(0,IFOUND)
           KEY0(6) = 0
           IF(IFOUND.EQ.1) THEN
*  once key is found, first prepare a work buffer IWRK1
              IWRK1 = INDXB(IBUF,1000*LUNW+3)
              IF( IWRK1.LE.0 ) THEN
                IWRK1 = INDXA(IBUF, 1000*LUNW+3, INFU(1,LW)/4)
                IF( IWRK1.LE.0 )
     +                  CALL LOGERROR('Out of space in FWDAT')
              ENDIF
*  then find the position of the relevant data in the LW file
*  NPOS is the key position inside the index block
*  IRCN is the record number containing the relevan t data
              LEVEL = INFU(25,LW)
              IF (LEVEL.LE.0)
     +               CALL LOGERROR('No level structure found:FWDAT')
              INDRX = INDXB(IBUF, 1000*LUNW+9+LEVEL)
              IF(INDRX.LE.0 )
     +               CALL LOGERROR('No index buffer found:FWDAT')
              JPX = 13
              KPX = JPX + IBUF(INDRX+JPX+1)
              NPOS = IBUF(INDRX+15)
              IRCN = IBUF(INDRX+KPX+NPOS+6)
              JPOLD = IBUF(INDRX+KPX+NPOS+7)
*  now copy the IRCN record into the IWRK1 buffer
              INFU(27,LW) = IRCN
              CALL FRRECX(LUNW, INFU(1,LW)/4, IBUF(IWRK1+1), IEND)
              CALL FRCONV(IBUF(IWRK1+1),LOCAL,IEND)
*  check whether the size of the new data (NR) equals the
*  size of the data already in LW and, if necessary, check
*  the size in the following records
              NROLD = IBUF(IWRK1+JPOLD+11+9)
              IF( NROLD.NE.NR )THEN
                IF( IBUF(IWRK1+JPOLD+11+7).EQ.0 )GOTO 230
190             CONTINUE
              CALL FRRECX(LUNW, INFU(1,LW)/4, IBUF(IWRK1+1), IEND)
              CALL FRCONV(IBUF(IWRK1+1),LOCAL,IEND)
                NROLD = NROLD + IBUF(IWRK1+2+11+9)
                IF( IBUF(IWRK1+2+11+7).NE.3) GOTO 190
              ENDIF
*  once the size of the replacement array equals the size of
*  the data, write the new data into the file
              IF (NROLD.EQ.NR) THEN
                 IF(INFU(ISGEN+8,LW).EQ.0)THEN
                       CALL FPLOCK(' ',IEND)
                       INFU(ISGEN+8,LW)=1
                  ENDIF
                 NCOPY=0
                 INFU(27,LW) = IRCN
                 INFU(26,LW) = IRCN
              CALL FRRECX(LUNW, INFU(1,LW)/4, IBUF(IWRK1+1), IEND)
              CALL FRCONV(IBUF(IWRK1+1),LOCAL,IEND)
                 NOFF = IBUF(IWRK1+JPOLD+11+1)
200              CONTINUE
                 NCOPY1 = IBUF(IWRK1+JPOLD+11+9)
                 DO 210 JJ=1,NCOPY1
                   IBUF(IWRK1+JPOLD+11+NOFF+JJ) = IARRAY(NCOPY+JJ)
210              CONTINUE
                 JFM = INFU(5,LW)
                 IF(JFM.NE.LOCAL . AND. INFU(ISGEN+4,LW).EQ.0)
     +                CALL FRCONV(IBUF(IWRK1+2),JFM,IEND)
                 CALL FWREC(LUNW, INFU(1,LW)/4, IBUF(IWRK1+1), IEND)
                 NCOPY = NCOPY + NCOPY1
                 IF(NCOPY .EQ. NR) THEN
                    CALL FPUNLK(' ',IRET)
                    INFU(ISGEN+8,LW) = 0
                    LR = ISVLR
                    LUNR = ISVLUNR
                    INFU(26,LW) = ISV26W
                    INFU(27,LW) = ISV27W
                    INFU(26,LR) = ISV26R
                    INFU(27,LR) = ISV27R
                    INFU(IWSTU+4,LW) = 0
                    INFU(29,LW) = 0
                    INFU(ISGEN+9,LW) = 1
                    CALL UCOPY(ISVK,INFU(IRSTU+14,LW),4)
                    GOTO 100
                 ENDIF
                 JPOLD = 2
                 CALL FRRECX(LUNW, INFU(1,LW)/4, IBUF(IWRK1+1), IEND)
                 CALL FRCONV(IBUF(IWRK1+1),LOCAL,IEND)
                 GOTO 200
             ENDIF
           ENDIF
230   CONTINUE
      LR = ISVLR
      LUNR = ISVLUNR
      INFU(26,LW) = ISV26W
      INFU(27,LW) = ISV27W
      INFU(26,LR) = ISV26R
      INFU(27,LR) = ISV27R
      CALL UCOPY(ISVK,INFU(IRSTU+14,LW),4)
***********************************************
240   CONTINUE
      ENDIF

#endif /* Linux */
      IF(INFU(IWSTU+4,LW).EQ.1) THEN
#ifndef Linux
*        record segment header not yet added to buffer - set added flag
#else /* Linux */
*        record segment header not yet added - set added flag
#endif /* Linux */
         INFU(IWSTU+4,LW)=2
*        update the position counter
         INFU(IWSTU+7,LW)=INFU(IWSTU+7,LW)+1
*        reset order flag
         INFU(IWKEY+8,LW)=0
         IF(IBUF(INDW+2)+11+INFU(IWHDR+1,LW)+1.LE.IBUF(INDW+1)) THEN
*           update the pointer to the record-segment header IP
            IF(IP.EQ.0) THEN
               IP=2
*              reset the position counter
               INFU(IWSTU+7,LW)=1
            ELSE
               IP=(IP+11)+IBUF(INDW+IP+11)
            END IF
            INFU(IWSTU+15,LW) = INFU(26,LW)
            INFU(IWSTU+16,LW) = IP
            GOTO 20
         ELSE
            INFU(IWSTU+15,LW) = INFU(26,LW) + 1
            INFU(IWSTU+16,LW) = 2
         END IF
      ELSE
*        record segment header already added
         IF(INFU(IWSTU+5,LW).EQ.1) THEN
*           data segment header not yet added - set added flag
            INFU(IWSTU+5,LW)=2
*           new data segment header
            IF(IBUF(INDW+2)+INFU(IWHDR+1,LW)+1.LE.IBUF(INDW+1)) THEN
*              update the data segment header JP
               JP=JP+IBUF(INDW+JP+1)+IBUF(INDW+JP+9)
               GOTO 30
            ELSE
*              no space in current record - update record segment code
           IF(IBUF(INDW+IP+10).LE.1) IBUF(INDW+IP+10)=IBUF(INDW+IP+10)+1
            ISGC=IBUF(INDW+IP+10)
            END IF
         ELSE
*           data segment header already added
            IF(IBUF(INDW+2)+1.LE.IBUF(INDW+1)) GOTO 40
            JP8=IBUF(INDW+JP+8)+IBUF(INDW+JP+9)
*           update record segment code
           IF(IBUF(INDW+IP+10).LE.1) IBUF(INDW+IP+10)=IBUF(INDW+IP+10)+1
            ISGC=IBUF(INDW+IP+10)
*           update segment code
            IF(IBUF(INDW+JP+7).LE.1) IBUF(INDW+JP+7)=IBUF(INDW+JP+7)+1
            JSGC=IBUF(INDW+JP+7)
         END IF
      END IF
*
*     write the record and reset the buffer
   10 CONTINUE
*     convert word format ...
      JFM=INFU(5,LW)
*     ... if output ne local and inhibit flag not set
      IF(JFM.NE.LOCAL.AND.INFU(ISGEN+4,LW).EQ.0) THEN
         CALL FRCONV(IBUF(INDW+1),JFM,ICER)
      END IF
*
      IF(INFU(2,LW).EQ.3 .AND. INFU(IWKEY+8,LW).EQ.0) THEN
*        if keyed access and first phys record of logical rec
*        then lock the file
         CALL FPLOCK(' ',IRET)
      END IF
      CALL FWREC(LUNW,IBUF(INDW+1),IBUF(INDW+1),IEND)
*     increase order flag (not only for keyed access)
      INFU(IWKEY+8,LW)=INFU(IWKEY+8,LW)+1
*     increase the record counter
      INFU(IWSTA+1,LW)=INFU(IWSTA+1,LW)+1
*     reset the position counter
      INFU(IWSTU+7,LW)=1
*     reset the pointer to the record segment header IP
      IBUF(INDW+2)=2
      IP=2
*
*     add the record segment header (update recordnumber code before)
   20 INFU(IWKEY+3,LW)=100*(INFU(IWSTA+1,LW)+1)+(INFU(IWSTU+7,LW)-1)
      DO 22 I=1,10
   22 IBUF(INDW+IP+I) =INFU(IWKEY+I,LW)
*     modify header format flag, if inhibit convert flag set
      IF(INFU(ISGEN+4,LW).NE.0) THEN
         JFM=INFU(5,LW)
         IBUF(INDW+IP+2)=16*JFM+MOD(IBUF(INDW+IP+2),16)
      END IF
*
*     convert the event name, if necessary
      JFRMT=INFU(5,LW)
      IF(JFRMT.EQ.0) JFRMT=LOCAL
      IF(JFRMT.EQ.2) JFRMT=1
      CALL FCNVR(LOCAL,JFRMT,IBUF(INDW+IP+1),10,0,IFMTR,NFMTR)
*
      INFU(IWSTU+4,LW)=2
*     restore previous segment code
      IBUF(INDW+IP+10)=ISGC
*     reset the counter for the words following
      IBUF(INDW+IP+11)=0
*     update length information in record
      IBUF(INDW+2)=IBUF(INDW+2)+11
*     reset the pointer to the data segment header JP
      JP=IP+11
*
*     add the data segment header
   30 DO 32 I=1,INFU(IWHDR+1,LW)
   32 IBUF(INDW+JP+I)=INFU(IWHDR+I,LW)
      INFU(IWSTU+5,LW)=2
*     restore previous segment code
      IBUF(INDW+JP+7)=JSGC
*     insert the p-counter and reset the counter for the words following
      IBUF(INDW+JP+8)=JP8
      IBUF(INDW+JP+9)=0
*     update length information in record
      IBUF(INDW+2)    =IBUF(INDW+2)    +INFU(IWHDR+1,LW)
      IBUF(INDW+IP+11)=IBUF(INDW+IP+11)+INFU(IWHDR+1,LW)
*     reset the pointer to the data
      KP=JP+IBUF(INDW+JP+1)
*     copy as many data as possible
   40 NCOP=MIN0(NR-IS,IBUF(INDW+1)-IBUF(INDW+2))
      CALL UCOPY(IARRAY(IS+1),IBUF(INDW+KP+1),NCOP)
*     update length information in record
      IBUF(INDW+2)    =IBUF(INDW+2)    +NCOP
      IBUF(INDW+IP+11)=IBUF(INDW+IP+11)+NCOP
      IBUF(INDW+JP+ 9)=IBUF(INDW+JP+ 9)+NCOP
*     update local pointer/counter
      IS=IS+NCOP
      KP=KP+NCOP
*     goto write if data are left
      IF(IS.LT.NR) THEN
*        data are splitted - save number of previous words
         JP8=IBUF(INDW+JP+8)+IBUF(INDW+JP+9)
*        update record segment code
         IF(IBUF(INDW+IP+10).LE.1) IBUF(INDW+IP+10)=IBUF(INDW+IP+10)+1
         ISGC=IBUF(INDW+IP+10)
*        update data segment code
         IF(IBUF(INDW+JP+ 7).LE.1) IBUF(INDW+JP+ 7)=IBUF(INDW+JP+ 7)+1
         JSGC=IBUF(INDW+JP+7)
         GOTO 10
      END IF
*     data transfer finished - save pointer
      INFU(IWSTU+ 8,LW)=IP
      INFU(IWSTU+ 9,LW)=JP
      INFU(IWSTU+10,LW)=KP
      GOTO 100
*
*     code for text output *******************
*
   60 IF(INFU(IWSTU+4,LW).EQ.1) THEN
*        record header: RECORD name [nr1   nr2 ndatwd]
         INFU(IWSTU+4,LW)=2
         CALL TEXTD('RECORD   ')
         ISAVE=INFU(IWKEY+8,LW)
         INFU(IWKEY+8,LW)=INFU(IWKEY+9,LW)
         NTKEY=5
         CALL FTCTOI('2C,3I',IFMT,NFMT)
         IF(INFU(IWKEY+8,LW).EQ.0) THEN
            NTKEY=4
            IF(INFU(IWKEY+7,LW).EQ.0) THEN
               NTKEY=3
               IF(INFU(IWKEY+6,LW).EQ.0) NTKEY=2
            END IF
         END IF
         CALL TEXTO(INFU(IWKEY+4,LW),NTKEY,IFMT,NFMT,0)
         IORG=MOD(INFU(IWKEY+2,LW),16)
         IF(IORG.EQ.0) THEN
            CALL TEXTC('  ! start of F-record')
         ELSE
            CALL TEXTC(ORIGL(IORG)//'  ! start of F-record')
         END IF
         CALL TEXTD(' ')
         INFU(IWKEY+8,LW)=ISAVE
      END IF
      IF(INFU(IWSTU+5,LW).EQ.1) THEN
*        data header: name numdb  nr ncol  FMT= ...
         INFU(IWSTU+5,LW)=2
         NTHDR=5
c        IF(INFU(IWHDR+5,LW).GE.0.AND.INFU(IWHDR+5,LW).LE.1) THEN
c           NTHDR=3
c           IF(INFU(IWHDR+4,LW).EQ.0) NTHDR=2
c        END IF
         CALL FTCTOI('2C,3I',IFMT,NFMT)
         CALL TEXTO(INFU(IWHDR+2,LW),NTHDR,IFMT,NFMT,0)
         IF(INFU(IWHDR+1,LW).GT.9) THEN
            CALL FTITOC(IFMTB,NFMTB,CHFMT,NCH)
            CALL TEXTD('   FMT='//CHFMT(1:NCH))
         END IF
         ICH=INFU(IWHDR+2,LW)
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
         NAMDAT(1:4)=CHA
         ICH=INFU(IWHDR+3,LW)
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
         NAMDAT(5:8)=CHA
         CALL TEXTC('  ! '//NAMDAT)
         CALL TEXTD(' ')
      END IF
*     data
      NDATA=NR
      CALL TEXTO(IARRAY,NR,IFMTB,NFMTB,0)
      CALL TEXTD(' ')
      GOTO 100
*     error conditions
   91 IER=IER+1
CCC   CALL FPWMSG(IER,'FWDAT',ERRMSG)
*
  100 CONTINUE
      END
#ifndef Linux
 
 
 
 
#endif /* not Linux */
