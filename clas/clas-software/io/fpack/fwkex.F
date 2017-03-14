#ifdef HPUX
*CMZU:  1.00/13 30/08/95  16.49.58  by  Unknown
*CMZ :  1.00/00 16/12/94  17.00.00  by  Pavel Binko
*-- Author :    Volker Blobel   16/12/94
      SUBROUTINE FWKEX(NAMKEY,NUM1,NUM2,NDATWD)
*
      CHARACTER*(*) NAMKEY
*
************************************************************************
*
*     Key definition for write
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
*KEND.
      CHARACTER NAMEC*8, CHA*4, STREXT*12
      CHARACTER DANAME*16, FINAME*256, FINAM2*256, HONAME*127, VONAME*6
      CHARACTER CHEXT*26
      DATA CHEXT / 'ABCDEFGHIJKLMNOPQRSTUVWXYZ' /
      INTEGER IOPEN(IOPMAX), LUNPAR(ILPMAX)
      PARAMETER (IBYORD=1+256*(2+256*(3+256*4)))
      CHARACTER*72 ERRMSG
*
      IF(LW.EQ.0) GOTO 100
      IF(INFU(7,LW).NE.0)GOTO 100
*
*     test on SPLITMB  & SPLITEV parameters
*     if user defines the both parameters, SPLITEV is not treated!!!
      IF(INFU(21,LW).NE.0) THEN
*        SPLITMB active
         F1MB = 1000000.0
*        number of Mbytes written
         FFL1=(INFU(IWSTA+2,LW)/F1MB)*INFU(1,LW)
*        limit from SPLITMB
         FFL2=INFU(21,LW)*(INFU(22,LW)-INFU(23,LW)+1)
      ELSEIF(INFU(39,LW).NE.0) THEN
*        SPLITEV active
*        number of logical records (events) written
         FFL1=INFU(IWSTA+3,LW)
*        limit from SPLITEV
         FFL2=INFU(39,LW)*(INFU(22,LW)-INFU(23,LW)+1)
      ELSE
         GOTO 50
      END IF
*
      IF(FFL1.GE.FFL2) THEN
*        increase number of extents
         INFU(22,LW)=INFU(22,LW)+1
         IF( (INFU(22,LW).EQ.2600) .OR.
     +       (INFU(22,LW).EQ.(INFU(23,LW)/100000+1)*100000) ) THEN
            WRITE(ERRMSG,'(A,I5)')
     +        'Too many extents on the unit',LUNBK(LW)
            GOTO 98
         END IF
*
*        generate the new file name
         III=IFRST(DANAME,-LUNBK(LW))
         IF(III.EQ.0) GOTO 100
         CALL FQFILE(DANAME,FINAME,HONAME,LUNPAR,IERR)
         IF(IERR.NE.0) THEN
            LNAME=LENB(DANAME)
            IF(LNAME.EQ.0) THEN
               LNAME=3
               DANAME='???'
            ENDIF
            ERRMSG='Wrong dataname '//DANAME(1:LNAME)
            GOTO 98
         END IF
*
*     don't append the suffix for UNIXTAPE
         IF(INFU(12,LW).NE.5) THEN
            IF(INFU(22,LW).LT.2600) THEN
*              suffix .<letter><digit><digit>
               IEXT1=INFU(22,LW)/100+1
               IEXT2=MOD(INFU(22,LW),100)
               FINAM2=FINAME(1:LENB(FINAME)-3)
               CALL PNVI(IEXT2,STREXT,LENEXT)
               IF(LENEXT.EQ.1) THEN
                  FINAME=FINAM2(1:LENB(FINAM2))
     +                         //CHEXT(IEXT1:IEXT1)//'0'//STREXT
               ELSE
                  FINAME=FINAM2(1:LENB(FINAM2))
     +                         //CHEXT(IEXT1:IEXT1)//STREXT
               END IF
            ELSE
*              suffix <C>7*<digit> (i.e. C9400199)
               FINAM2=FINAME(1:LENB(FINAME)-7)
               CALL PNVI(INFU(22,LW),STREXT,LENEXT)
               FINAME=FINAM2(1:LENB(FINAM2))//STREXT(2:LENEXT)
            END IF
         END IF
c        FILNAM(LW)=FINAME
*
*        Restore IOPEN from INFU
         CALL FPIOPEN(IOPEN,LW)
*
*        close the file without deleting its status
         CALL FICLOS(LUNBK(LW))
*
*        Clear the current record number fields before next OPEN
         INFU(14,LW)      = 0
         INFU(IWSTU+3,LW) = 0
*
*        Here we use the following rule: 1 file to the 1 cartridge
*        check 'private cartridge' flag and if OK ...
         IF(INFU(31,LW).EQ.1) THEN
*           check whether the next serial_number name exists.
            IF(INDXB(IBUF,1000*LUNW+7).LE.0) THEN
               WRITE(6,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
               WRITE(6,*) '!                                       !'
               WRITE(6,*) '!   E R R O R  :                        !'
               WRITE(6,*) '!                                       !'
               WRITE(6,*) '!   Not enough volume serial numbers    !'
               WRITE(6,*) '!      given in the OPEN command        !'
               WRITE(6,*) '!                                       !'
               WRITE(6,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
               GOTO 99
            END IF
*           get the next volume name
            CALL FPNXTV(LUNW,VONAME)
         ELSE
            VONAME=' '
         END IF
*        open the next file with old status
         CALL MKOSPL(IOPEN,FINAME,DANAME,HONAME,VONAME,
     +              IER)
         IF( IER.NE.0 ) THEN
            WRITE(ERRMSG,'(A,I5)') 'OPEN failed. Return code',IER
            GOTO 98
         END IF
      END IF
*
   50 CONTINUE
*     general status of unit
      IF(INFU(2,LW).EQ.3.OR.INFU(2,LW).EQ.5.OR.
     +   INFU(ISGEN+1,LW).EQ.0.OR.INFU(ISGEN+1,LW).EQ.5) THEN
*        keyed or ordered access or
*        status is undef or rewind - switch to writing
         INFU(ISGEN+1,LW)=3
         INFU(IWSTU+2,LW)=0
         INFU(IWSTU+3,LW)=0
*        drop bank too
      ELSE IF(INFU(ISGEN+1,LW).EQ.2) THEN
*        status is end-of-file at reading - error
         ERRMSG='Invalid status "EOF reading" when writing'
         GOTO 98
      ELSE IF(INFU(ISGEN+1,LW).EQ.4) THEN
*        status is end-file written - error
         ERRMSG='Invalid status "EOF written" when writing'
         GOTO 98
      ELSE IF(INFU(ISGEN+1,LW).EQ.1) THEN
*        status is read
         ERRMSG='Invalid status "READ" when writing'
         GOTO 98
      END IF
*
*     store record header
*
      INFU(IWKEY+1,LW)=IBYORD
*     word format
      INFU(IWKEY+2,LW)=LOCAL*16+INFU(ISGEN+2,LW)
*     record number*100 + position index
      INFU(IWKEY+3,LW)=0
      NAMEC=NAMKEY
*     first part of character key ...
      CHA=NAMEC(1:4)
*KEEP,ICHCHA.
*     get hollerith 4H from char*4
      ICH=IOR(ISHFT(ICHAR(CHA(1:1)),24),IOR(ISHFT(ICHAR(CHA(2:2)),16),
     +    IOR(ISHFT(ICHAR(CHA(3:3)), 8),ICHAR(          CHA(4:4)))))
*KEND.
      INFU(IWKEY+4,LW)=ICH
*     ... and second part
      CHA=NAMEC(5:8)
*KEEP,ICHCHA.
*     get hollerith 4H from char*4
      ICH=IOR(ISHFT(ICHAR(CHA(1:1)),24),IOR(ISHFT(ICHAR(CHA(2:2)),16),
     +    IOR(ISHFT(ICHAR(CHA(3:3)), 8),ICHAR(          CHA(4:4)))))
*KEND.
      INFU(IWKEY+5,LW)=ICH
      INFU(IWKEY+6,LW)=NUM1
      INFU(IWKEY+7,LW)=NUM2
      INFU(IWKEY+8,LW)=0
      INFU(IWKEY+9,LW)=NDATWD
      INFU(IWKEY+10,LW)=0
*     take information from FWDIR... calls, if any
      IF(INFU(IWSTU+11,LW).NE.0) THEN
         INFU(IWSTU+11,LW)=0
         IF(INFU(IWSTU+13,LW).NE.0) THEN
*           reset class word (and this flag)
            INFU(IWKEY+ 9,LW)=0
            INFU(IWSTU+13,LW)=0
         END IF
*        logical OR of classwords
         INFU(IWKEY+ 9,LW)=IOR(INFU(IWKEY+9,LW),INFU(IWSTU+12,LW))
         INFU(IWSTU+12,LW)=0
      END IF
*
*     set status: key defined, hdr undefined, no error
      INFU(IWSTU+4,LW)=1
      INFU(IWSTU+5,LW)=0
      INFU(IWSTU+6,LW)=0
*
*     select
      CALL FSELCT(3,LUNBK(LW),IDEC)
      IF(IDEC.NE.0) THEN
         INFU(IWSTU+4,LW)=0
         GOTO 100
      END IF
*
*     increase number of logical records written
      INFU(IWSTA+3,LW)=INFU(IWSTA+3,LW)+1
*
      GOTO 100
*
*     error
   98 WRITE(*,*) ERRMSG
      CALL FPWMSG(98,'FWKEY',ERRMSG)
   99 WRITE(6,*) ' Reading of the next logical record stopped'
      IF(LR.GT.0) THEN
*        Force stop of reading of the next logical record,
*        if writing is not more possible.
         INFU(ISGEN+1,LR)=2
      END IF
*
      INFU(IWSTU+4,LW)=0
      INFU(IWSTU+5,LW)=0
C     INFU(IWSTU+6,LW)=1
*
  100 CONTINUE
      END
#elif defined Linux
*CMZ :  1.00/00 16/12/94  17.00.00  by  Pavel Binko
*-- Author :    Volker Blobel   16/12/94
      SUBROUTINE FWKEX(NAMKEY,NUM1,NUM2,NDATWD)
*
      CHARACTER*(*) NAMKEY
*
************************************************************************
*
*     Key definition for write
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
*KEND.
      CHARACTER NAMEC*8, CHA*4, STREXT*12
      CHARACTER DANAME*16, FINAME*256, FINAM2*256, HONAME*127, VONAME*6
      CHARACTER CHEXT*26
      DATA CHEXT / 'ABCDEFGHIJKLMNOPQRSTUVWXYZ' /
      INTEGER IOPEN(IOPMAX), LUNPAR(ILPMAX)
      PARAMETER (IBYORD=1+256*(2+256*(3+256*4)))
      CHARACTER*72 ERRMSG
*
      IF(LW.EQ.0) GOTO 100
      IF(INFU(7,LW).NE.0)GOTO 100
*
*     test on SPLITMB  & SPLITEV parameters
*     if user defines the both parameters, SPLITEV is not treated!!!
      IF(INFU(21,LW).NE.0) THEN
*        SPLITMB active
         F1MB = 1000000.0
*        number of Mbytes written
         FFL1=(INFU(IWSTA+2,LW)/F1MB)*INFU(1,LW)
*        limit from SPLITMB
         FFL2=INFU(21,LW)*(INFU(22,LW)-INFU(23,LW)+1)
      ELSEIF(INFU(39,LW).NE.0) THEN
*        SPLITEV active
*        number of logical records (events) written
         FFL1=INFU(IWSTA+3,LW)
*        limit from SPLITEV
         FFL2=INFU(39,LW)*(INFU(22,LW)-INFU(23,LW)+1)
      ELSE
         GOTO 50
      END IF
*
      IF(FFL1.GE.FFL2) THEN
*        increase number of extents
         INFU(22,LW)=INFU(22,LW)+1
         IF( (INFU(22,LW).EQ.2600) .OR.
     +       (INFU(22,LW).EQ.(INFU(23,LW)/100000+1)*100000) ) THEN
            WRITE(ERRMSG,'(A,I5)')
     +        'Too many extents on the unit',LUNBK(LW)
            GOTO 98
         END IF
*
*        generate the new file name
         III=IFRST(DANAME,-LUNBK(LW))
         IF(III.EQ.0) GOTO 100
         CALL FQFILE(DANAME,FINAME,HONAME,LUNPAR,IERR)
         IF(IERR.NE.0) THEN
            LNAME=LENB(DANAME)
            IF(LNAME.EQ.0) THEN
               LNAME=3
               DANAME='???'
            ENDIF
            ERRMSG='Wrong dataname '//DANAME(1:LNAME)
            GOTO 98
         END IF
*
*     don't append the suffix for UNIXTAPE
         IF(INFU(12,LW).NE.5) THEN
            IF(INFU(22,LW).LT.2600) THEN
*              suffix .<letter><digit><digit>
               IEXT1=INFU(22,LW)/100+1
               IEXT2=MOD(INFU(22,LW),100)
               FINAM2=FINAME(1:LENB(FINAME)-3)
               CALL PNVI(IEXT2,STREXT,LENEXT)
               IF(LENEXT.EQ.1) THEN
                  FINAME=FINAM2(1:LENB(FINAM2))
     +                         //CHEXT(IEXT1:IEXT1)//'0'//STREXT
               ELSE
                  FINAME=FINAM2(1:LENB(FINAM2))
     +                         //CHEXT(IEXT1:IEXT1)//STREXT
               END IF
            ELSE
*              suffix <C>7*<digit> (i.e. C9400199)
               FINAM2=FINAME(1:LENB(FINAME)-7)
               CALL PNVI(INFU(22,LW),STREXT,LENEXT)
               FINAME=FINAM2(1:LENB(FINAM2))//STREXT(2:LENEXT)
            END IF
         END IF
c        FILNAM(LW)=FINAME
*
*        Restore IOPEN from INFU
         CALL FPIOPEN(IOPEN,LW)
*
*        close the file without deleting its status
         CALL FICLOS(LUNBK(LW))
*
*        Clear the current record number fields before next OPEN
         INFU(14,LW)      = 0
         INFU(IWSTU+3,LW) = 0
*
*        Here we use the following rule: 1 file to the 1 cartridge
*        check 'private cartridge' flag and if OK ...
         IF(INFU(31,LW).EQ.1) THEN
*           check whether the next serial_number name exists.
            IF(INDXB(IBUF,1000*LUNW+7).LE.0) THEN
               WRITE(6,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
               WRITE(6,*) '!                                       !'
               WRITE(6,*) '!   E R R O R  :                        !'
               WRITE(6,*) '!                                       !'
               WRITE(6,*) '!   Not enough volume serial numbers    !'
               WRITE(6,*) '!      given in the OPEN command        !'
               WRITE(6,*) '!                                       !'
               WRITE(6,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
               GOTO 99
            END IF
*           get the next volume name
            CALL FPNXTV(LUNW,VONAME)
         ELSE
            VONAME=' '
         END IF
*        open the next file with old status
         CALL MKOSPL(IOPEN,FINAME,DANAME,HONAME,VONAME,
     +              IER)
         IF( IER.NE.0 ) THEN
            WRITE(ERRMSG,'(A,I5)') 'OPEN failed. Return code',IER
            GOTO 98
         END IF
      END IF
*
   50 CONTINUE
*     general status of unit
      IF(INFU(2,LW).EQ.3.OR.INFU(2,LW).EQ.5.OR.
     +   INFU(ISGEN+1,LW).EQ.0.OR.INFU(ISGEN+1,LW).EQ.5) THEN
*        keyed or ordered access or
*        status is undef or rewind - switch to writing
         INFU(ISGEN+1,LW)=3
         INFU(IWSTU+2,LW)=0
         INFU(IWSTU+3,LW)=0
*        drop bank too
      ELSE IF(INFU(ISGEN+1,LW).EQ.2) THEN
*        status is end-of-file at reading - error
         ERRMSG='Invalid status "EOF reading" when writing'
         GOTO 98
      ELSE IF(INFU(ISGEN+1,LW).EQ.4) THEN
*        status is end-file written - error
         ERRMSG='Invalid status "EOF written" when writing'
         GOTO 98
      ELSE IF(INFU(ISGEN+1,LW).EQ.1) THEN
*        status is read
         ERRMSG='Invalid status "READ" when writing'
         GOTO 98
      END IF
*
*     store record header
*
      INFU(IWKEY+1,LW)=IBYORD
*     word format
      INFU(IWKEY+2,LW)=LOCAL*16+INFU(ISGEN+2,LW)
*     record number*100 + position index
      INFU(IWKEY+3,LW)=0
      NAMEC=NAMKEY
*     first part of character key ...
      CHA=NAMEC(1:4)
*KEEP,ICHCHA.
*     get hollerith 4H from char*4
      ICH=IOR(ISHFT(ICHAR(CHA(4:4)),24),IOR(ISHFT(ICHAR(CHA(3:3)),16),
     +    IOR(ISHFT(ICHAR(CHA(2:2)), 8),ICHAR(          CHA(1:1)))))
*KEND.
      INFU(IWKEY+4,LW)=ICH
*     ... and second part
      CHA=NAMEC(5:8)
*KEEP,ICHCHA.
*     get hollerith 4H from char*4
      ICH=IOR(ISHFT(ICHAR(CHA(4:4)),24),IOR(ISHFT(ICHAR(CHA(3:3)),16),
     +    IOR(ISHFT(ICHAR(CHA(2:2)), 8),ICHAR(          CHA(1:1)))))
*KEND.
      INFU(IWKEY+5,LW)=ICH
      INFU(IWKEY+6,LW)=NUM1
      INFU(IWKEY+7,LW)=NUM2
      INFU(IWKEY+8,LW)=0
      INFU(IWKEY+9,LW)=NDATWD
      INFU(IWKEY+10,LW)=0
*     take information from FWDIR... calls, if any
      IF(INFU(IWSTU+11,LW).NE.0) THEN
         INFU(IWSTU+11,LW)=0
         IF(INFU(IWSTU+13,LW).NE.0) THEN
*           reset class word (and this flag)
            INFU(IWKEY+ 9,LW)=0
            INFU(IWSTU+13,LW)=0
         END IF
*        logical OR of classwords
         INFU(IWKEY+ 9,LW)=IOR(INFU(IWKEY+9,LW),INFU(IWSTU+12,LW))
         INFU(IWSTU+12,LW)=0
      END IF
*
*     set status: key defined, hdr undefined, no error
      INFU(IWSTU+4,LW)=1
      INFU(IWSTU+5,LW)=0
      INFU(IWSTU+6,LW)=0
*
*     select
      CALL FSELCT(3,LUNBK(LW),IDEC)
      IF(IDEC.NE.0) THEN
         INFU(IWSTU+4,LW)=0
         GOTO 100
      END IF
*
*     increase number of logical records written
      INFU(IWSTA+3,LW)=INFU(IWSTA+3,LW)+1
*
      GOTO 100
*
*     error
   98 WRITE(*,*) ERRMSG
      CALL FPWMSG(98,'FWKEY',ERRMSG)
   99 WRITE(6,*) ' Reading of the next logical record stopped'
      IF(LR.GT.0) THEN
*        Force stop of reading of the next logical record,
*        if writing is not more possible.
         INFU(ISGEN+1,LR)=2
      END IF
*
      INFU(IWSTU+4,LW)=0
      INFU(IWSTU+5,LW)=0
C     INFU(IWSTU+6,LW)=1
*
  100 CONTINUE
      END
#elif defined SunOS
*CMZU:  1.00/13 30/08/95  16.49.58  by  Unknown
*CMZ :  1.00/00 16/12/94  17.00.00  by  Pavel Binko
*-- Author :    Volker Blobel   16/12/94
      SUBROUTINE FWKEX(NAMKEY,NUM1,NUM2,NDATWD)
*
      CHARACTER*(*) NAMKEY
*
************************************************************************
*
*     Key definition for write
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
*KEND.
      CHARACTER NAMEC*8, CHA*4, STREXT*12
      CHARACTER DANAME*16, FINAME*256, FINAM2*256, HONAME*127, VONAME*6
      CHARACTER CHEXT*26
      DATA CHEXT / 'ABCDEFGHIJKLMNOPQRSTUVWXYZ' /
      INTEGER IOPEN(IOPMAX), LUNPAR(ILPMAX)
      PARAMETER (IBYORD=1+256*(2+256*(3+256*4)))
      CHARACTER*72 ERRMSG
*
      IF(LW.EQ.0) GOTO 100
      IF(INFU(7,LW).NE.0)GOTO 100
*
*     test on SPLITMB  & SPLITEV parameters
*     if user defines the both parameters, SPLITEV is not treated!!!
      IF(INFU(21,LW).NE.0) THEN
*        SPLITMB active
         F1MB = 1000000.0
*        number of Mbytes written
         FFL1=(INFU(IWSTA+2,LW)/F1MB)*INFU(1,LW)
*        limit from SPLITMB
         FFL2=INFU(21,LW)*(INFU(22,LW)-INFU(23,LW)+1)
      ELSEIF(INFU(39,LW).NE.0) THEN
*        SPLITEV active
*        number of logical records (events) written
         FFL1=INFU(IWSTA+3,LW)
*        limit from SPLITEV
         FFL2=INFU(39,LW)*(INFU(22,LW)-INFU(23,LW)+1)
      ELSE
         GOTO 50
      END IF
*
      IF(FFL1.GE.FFL2) THEN
*        increase number of extents
         INFU(22,LW)=INFU(22,LW)+1
         IF( (INFU(22,LW).EQ.2600) .OR.
     +       (INFU(22,LW).EQ.(INFU(23,LW)/100000+1)*100000) ) THEN
            WRITE(ERRMSG,'(A,I5)')
     +        'Too many extents on the unit',LUNBK(LW)
            GOTO 98
         END IF
*
*        generate the new file name
         III=IFRST(DANAME,-LUNBK(LW))
         IF(III.EQ.0) GOTO 100
         CALL FQFILE(DANAME,FINAME,HONAME,LUNPAR,IERR)
         IF(IERR.NE.0) THEN
            LNAME=LENB(DANAME)
            IF(LNAME.EQ.0) THEN
               LNAME=3
               DANAME='???'
            ENDIF
            ERRMSG='Wrong dataname '//DANAME(1:LNAME)
            GOTO 98
         END IF
*
*     don't append the suffix for UNIXTAPE
         IF(INFU(12,LW).NE.5) THEN
            IF(INFU(22,LW).LT.2600) THEN
*              suffix .<letter><digit><digit>
               IEXT1=INFU(22,LW)/100+1
               IEXT2=MOD(INFU(22,LW),100)
               FINAM2=FINAME(1:LENB(FINAME)-3)
               CALL PNVI(IEXT2,STREXT,LENEXT)
               IF(LENEXT.EQ.1) THEN
                  FINAME=FINAM2(1:LENB(FINAM2))
     +                         //CHEXT(IEXT1:IEXT1)//'0'//STREXT
               ELSE
                  FINAME=FINAM2(1:LENB(FINAM2))
     +                         //CHEXT(IEXT1:IEXT1)//STREXT
               END IF
            ELSE
*              suffix <C>7*<digit> (i.e. C9400199)
               FINAM2=FINAME(1:LENB(FINAME)-7)
               CALL PNVI(INFU(22,LW),STREXT,LENEXT)
               FINAME=FINAM2(1:LENB(FINAM2))//STREXT(2:LENEXT)
            END IF
         END IF
c        FILNAM(LW)=FINAME
*
*        Restore IOPEN from INFU
         CALL FPIOPEN(IOPEN,LW)
*
*        close the file without deleting its status
         CALL FICLOS(LUNBK(LW))
*
*        Clear the current record number fields before next OPEN
         INFU(14,LW)      = 0
         INFU(IWSTU+3,LW) = 0
*
*        Here we use the following rule: 1 file to the 1 cartridge
*        check 'private cartridge' flag and if OK ...
         IF(INFU(31,LW).EQ.1) THEN
*           check whether the next serial_number name exists.
            IF(INDXB(IBUF,1000*LUNW+7).LE.0) THEN
               WRITE(6,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
               WRITE(6,*) '!                                       !'
               WRITE(6,*) '!   E R R O R  :                        !'
               WRITE(6,*) '!                                       !'
               WRITE(6,*) '!   Not enough volume serial numbers    !'
               WRITE(6,*) '!      given in the OPEN command        !'
               WRITE(6,*) '!                                       !'
               WRITE(6,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
               GOTO 99
            END IF
*           get the next volume name
            CALL FPNXTV(LUNW,VONAME)
         ELSE
            VONAME=' '
         END IF
*        open the next file with old status
         CALL MKOSPL(IOPEN,FINAME,DANAME,HONAME,VONAME,
     +              IER)
         IF( IER.NE.0 ) THEN
            WRITE(ERRMSG,'(A,I5)') 'OPEN failed. Return code',IER
            GOTO 98
         END IF
      END IF
*
   50 CONTINUE
*     general status of unit
      IF(INFU(2,LW).EQ.3.OR.INFU(2,LW).EQ.5.OR.
     +   INFU(ISGEN+1,LW).EQ.0.OR.INFU(ISGEN+1,LW).EQ.5) THEN
*        keyed or ordered access or
*        status is undef or rewind - switch to writing
         INFU(ISGEN+1,LW)=3
         INFU(IWSTU+2,LW)=0
         INFU(IWSTU+3,LW)=0
*        drop bank too
      ELSE IF(INFU(ISGEN+1,LW).EQ.2) THEN
*        status is end-of-file at reading - error
         ERRMSG='Invalid status "EOF reading" when writing'
         GOTO 98
      ELSE IF(INFU(ISGEN+1,LW).EQ.4) THEN
*        status is end-file written - error
         ERRMSG='Invalid status "EOF written" when writing'
         GOTO 98
      ELSE IF(INFU(ISGEN+1,LW).EQ.1) THEN
*        status is read
         ERRMSG='Invalid status "READ" when writing'
         GOTO 98
      END IF
*
*     store record header
*
      INFU(IWKEY+1,LW)=IBYORD
*     word format
      INFU(IWKEY+2,LW)=LOCAL*16+INFU(ISGEN+2,LW)
*     record number*100 + position index
      INFU(IWKEY+3,LW)=0
      NAMEC=NAMKEY
*     first part of character key ...
      CHA=NAMEC(1:4)
*KEEP,ICHCHA.
*     get hollerith 4H from char*4
      ICH=IOR(ISHFT(ICHAR(CHA(1:1)),24),IOR(ISHFT(ICHAR(CHA(2:2)),16),
     +    IOR(ISHFT(ICHAR(CHA(3:3)), 8),ICHAR(          CHA(4:4)))))
*KEND.
      INFU(IWKEY+4,LW)=ICH
*     ... and second part
      CHA=NAMEC(5:8)
*KEEP,ICHCHA.
*     get hollerith 4H from char*4
      ICH=IOR(ISHFT(ICHAR(CHA(1:1)),24),IOR(ISHFT(ICHAR(CHA(2:2)),16),
     +    IOR(ISHFT(ICHAR(CHA(3:3)), 8),ICHAR(          CHA(4:4)))))
*KEND.
      INFU(IWKEY+5,LW)=ICH
      INFU(IWKEY+6,LW)=NUM1
      INFU(IWKEY+7,LW)=NUM2
      INFU(IWKEY+8,LW)=0
      INFU(IWKEY+9,LW)=NDATWD
      INFU(IWKEY+10,LW)=0
*     take information from FWDIR... calls, if any
      IF(INFU(IWSTU+11,LW).NE.0) THEN
         INFU(IWSTU+11,LW)=0
         IF(INFU(IWSTU+13,LW).NE.0) THEN
*           reset class word (and this flag)
            INFU(IWKEY+ 9,LW)=0
            INFU(IWSTU+13,LW)=0
         END IF
*        logical OR of classwords
         INFU(IWKEY+ 9,LW)=IOR(INFU(IWKEY+9,LW),INFU(IWSTU+12,LW))
         INFU(IWSTU+12,LW)=0
      END IF
*
*     set status: key defined, hdr undefined, no error
      INFU(IWSTU+4,LW)=1
      INFU(IWSTU+5,LW)=0
      INFU(IWSTU+6,LW)=0
*
*     select
      CALL FSELCT(3,LUNBK(LW),IDEC)
      IF(IDEC.NE.0) THEN
         INFU(IWSTU+4,LW)=0
         GOTO 100
      END IF
*
*     increase number of logical records written
      INFU(IWSTA+3,LW)=INFU(IWSTA+3,LW)+1
*
      GOTO 100
*
*     error
   98 WRITE(*,*) ERRMSG
      CALL FPWMSG(98,'FWKEY',ERRMSG)
   99 WRITE(6,*) ' Reading of the next logical record stopped'
      IF(LR.GT.0) THEN
*        Force stop of reading of the next logical record,
*        if writing is not more possible.
         INFU(ISGEN+1,LR)=2
      END IF
*
      INFU(IWSTU+4,LW)=0
      INFU(IWSTU+5,LW)=0
C     INFU(IWSTU+6,LW)=1
*
  100 CONTINUE
      END
#else
*CMZU:  1.00/13 30/08/95  16.49.58  by  Unknown
*CMZ :  1.00/00 16/12/94  17.00.00  by  Pavel Binko
*-- Author :    Volker Blobel   16/12/94
      SUBROUTINE FWKEX(NAMKEY,NUM1,NUM2,NDATWD)
*
      CHARACTER*(*) NAMKEY
*
************************************************************************
*
*     Key definition for write
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
*KEND.
      CHARACTER NAMEC*8, CHA*4, STREXT*12
      CHARACTER DANAME*16, FINAME*256, FINAM2*256, HONAME*127, VONAME*6
      CHARACTER CHEXT*26
      DATA CHEXT / 'ABCDEFGHIJKLMNOPQRSTUVWXYZ' /
      INTEGER IOPEN(IOPMAX), LUNPAR(ILPMAX)
      PARAMETER (IBYORD=1+256*(2+256*(3+256*4)))
      CHARACTER*72 ERRMSG
*
      IF(LW.EQ.0) GOTO 100
      IF(INFU(7,LW).NE.0)GOTO 100
*
*     test on SPLITMB  & SPLITEV parameters
*     if user defines the both parameters, SPLITEV is not treated!!!
      IF(INFU(21,LW).NE.0) THEN
*        SPLITMB active
         F1MB = 1000000.0
*        number of Mbytes written
         FFL1=(INFU(IWSTA+2,LW)/F1MB)*INFU(1,LW)
*        limit from SPLITMB
         FFL2=INFU(21,LW)*(INFU(22,LW)-INFU(23,LW)+1)
      ELSEIF(INFU(39,LW).NE.0) THEN
*        SPLITEV active
*        number of logical records (events) written
         FFL1=INFU(IWSTA+3,LW)
*        limit from SPLITEV
         FFL2=INFU(39,LW)*(INFU(22,LW)-INFU(23,LW)+1)
      ELSE
         GOTO 50
      END IF
*
      IF(FFL1.GE.FFL2) THEN
*        increase number of extents
         INFU(22,LW)=INFU(22,LW)+1
         IF( (INFU(22,LW).EQ.2600) .OR.
     +       (INFU(22,LW).EQ.(INFU(23,LW)/100000+1)*100000) ) THEN
            WRITE(ERRMSG,'(A,I5)')
     +        'Too many extents on the unit',LUNBK(LW)
            GOTO 98
         END IF
*
*        generate the new file name
         III=IFRST(DANAME,-LUNBK(LW))
         IF(III.EQ.0) GOTO 100
         CALL FQFILE(DANAME,FINAME,HONAME,LUNPAR,IERR)
         IF(IERR.NE.0) THEN
            LNAME=LENB(DANAME)
            IF(LNAME.EQ.0) THEN
               LNAME=3
               DANAME='???'
            ENDIF
            ERRMSG='Wrong dataname '//DANAME(1:LNAME)
            GOTO 98
         END IF
*
*     don't append the suffix for UNIXTAPE
         IF(INFU(12,LW).NE.5) THEN
            IF(INFU(22,LW).LT.2600) THEN
*              suffix .<letter><digit><digit>
               IEXT1=INFU(22,LW)/100+1
               IEXT2=MOD(INFU(22,LW),100)
               FINAM2=FINAME(1:LENB(FINAME)-3)
               CALL PNVI(IEXT2,STREXT,LENEXT)
               IF(LENEXT.EQ.1) THEN
                  FINAME=FINAM2(1:LENB(FINAM2))
     +                         //CHEXT(IEXT1:IEXT1)//'0'//STREXT
               ELSE
                  FINAME=FINAM2(1:LENB(FINAM2))
     +                         //CHEXT(IEXT1:IEXT1)//STREXT
               END IF
            ELSE
*              suffix <C>7*<digit> (i.e. C9400199)
               FINAM2=FINAME(1:LENB(FINAME)-7)
               CALL PNVI(INFU(22,LW),STREXT,LENEXT)
               FINAME=FINAM2(1:LENB(FINAM2))//STREXT(2:LENEXT)
            END IF
         END IF
c        FILNAM(LW)=FINAME
*
*        Restore IOPEN from INFU
         CALL FPIOPEN(IOPEN,LW)
*
*        close the file without deleting its status
         CALL FICLOS(LUNBK(LW))
*
*        Clear the current record number fields before next OPEN
         INFU(14,LW)      = 0
         INFU(IWSTU+3,LW) = 0
*
*        Here we use the following rule: 1 file to the 1 cartridge
*        check 'private cartridge' flag and if OK ...
         IF(INFU(31,LW).EQ.1) THEN
*           check whether the next serial_number name exists.
            IF(INDXB(IBUF,1000*LUNW+7).LE.0) THEN
               WRITE(6,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
               WRITE(6,*) '!                                       !'
               WRITE(6,*) '!   E R R O R  :                        !'
               WRITE(6,*) '!                                       !'
               WRITE(6,*) '!   Not enough volume serial numbers    !'
               WRITE(6,*) '!      given in the OPEN command        !'
               WRITE(6,*) '!                                       !'
               WRITE(6,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
               GOTO 99
            END IF
*           get the next volume name
            CALL FPNXTV(LUNW,VONAME)
         ELSE
            VONAME=' '
         END IF
*        open the next file with old status
         CALL MKOSPL(IOPEN,FINAME,DANAME,HONAME,VONAME,
     +              IER)
         IF( IER.NE.0 ) THEN
            WRITE(ERRMSG,'(A,I5)') 'OPEN failed. Return code',IER
            GOTO 98
         END IF
      END IF
*
   50 CONTINUE
*     general status of unit
      IF(INFU(2,LW).EQ.3.OR.INFU(2,LW).EQ.5.OR.
     +   INFU(ISGEN+1,LW).EQ.0.OR.INFU(ISGEN+1,LW).EQ.5) THEN
*        keyed or ordered access or
*        status is undef or rewind - switch to writing
         INFU(ISGEN+1,LW)=3
         INFU(IWSTU+2,LW)=0
         INFU(IWSTU+3,LW)=0
*        drop bank too
      ELSE IF(INFU(ISGEN+1,LW).EQ.2) THEN
*        status is end-of-file at reading - error
         ERRMSG='Invalid status "EOF reading" when writing'
         GOTO 98
      ELSE IF(INFU(ISGEN+1,LW).EQ.4) THEN
*        status is end-file written - error
         ERRMSG='Invalid status "EOF written" when writing'
         GOTO 98
      ELSE IF(INFU(ISGEN+1,LW).EQ.1) THEN
*        status is read
         ERRMSG='Invalid status "READ" when writing'
         GOTO 98
      END IF
*
*     store record header
*
      INFU(IWKEY+1,LW)=IBYORD
*     word format
      INFU(IWKEY+2,LW)=LOCAL*16+INFU(ISGEN+2,LW)
*     record number*100 + position index
      INFU(IWKEY+3,LW)=0
      NAMEC=NAMKEY
*     first part of character key ...
      CHA=NAMEC(1:4)
*KEEP,ICHCHA.
*     get hollerith 4H from char*4
      ICH=IOR(ISHFT(ICHAR(CHA(1:1)),24),IOR(ISHFT(ICHAR(CHA(2:2)),16),
     +    IOR(ISHFT(ICHAR(CHA(3:3)), 8),ICHAR(          CHA(4:4)))))
*KEND.
      INFU(IWKEY+4,LW)=ICH
*     ... and second part
      CHA=NAMEC(5:8)
*KEEP,ICHCHA.
*     get hollerith 4H from char*4
      ICH=IOR(ISHFT(ICHAR(CHA(1:1)),24),IOR(ISHFT(ICHAR(CHA(2:2)),16),
     +    IOR(ISHFT(ICHAR(CHA(3:3)), 8),ICHAR(          CHA(4:4)))))
*KEND.
      INFU(IWKEY+5,LW)=ICH
      INFU(IWKEY+6,LW)=NUM1
      INFU(IWKEY+7,LW)=NUM2
      INFU(IWKEY+8,LW)=0
      INFU(IWKEY+9,LW)=NDATWD
      INFU(IWKEY+10,LW)=0
*     take information from FWDIR... calls, if any
      IF(INFU(IWSTU+11,LW).NE.0) THEN
         INFU(IWSTU+11,LW)=0
         IF(INFU(IWSTU+13,LW).NE.0) THEN
*           reset class word (and this flag)
            INFU(IWKEY+ 9,LW)=0
            INFU(IWSTU+13,LW)=0
         END IF
*        logical OR of classwords
         INFU(IWKEY+ 9,LW)=IOR(INFU(IWKEY+9,LW),INFU(IWSTU+12,LW))
         INFU(IWSTU+12,LW)=0
      END IF
*
*     set status: key defined, hdr undefined, no error
      INFU(IWSTU+4,LW)=1
      INFU(IWSTU+5,LW)=0
      INFU(IWSTU+6,LW)=0
*
*     select
      CALL FSELCT(3,LUNBK(LW),IDEC)
      IF(IDEC.NE.0) THEN
         INFU(IWSTU+4,LW)=0
         GOTO 100
      END IF
*
*     increase number of logical records written
      INFU(IWSTA+3,LW)=INFU(IWSTA+3,LW)+1
*
      GOTO 100
*
*     error
   98 WRITE(*,*) ERRMSG
      CALL FPWMSG(98,'FWKEY',ERRMSG)
   99 WRITE(6,*) ' Reading of the next logical record stopped'
      IF(LR.GT.0) THEN
*        Force stop of reading of the next logical record,
*        if writing is not more possible.
         INFU(ISGEN+1,LR)=2
      END IF
*
      INFU(IWSTU+4,LW)=0
      INFU(IWSTU+5,LW)=0
C     INFU(IWSTU+6,LW)=1
*
  100 CONTINUE
      END
#endif
