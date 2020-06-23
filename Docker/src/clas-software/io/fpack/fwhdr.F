*CMZ :  1.00/00 16/12/94  17.00.00  by  Pavel Binko
*-- Author :    Volker Blobel   16/12/94
      SUBROUTINE FWHDR(NAMDAT,NUM,NCOL,NROW,FORMAT)
*
      CHARACTER*(*) NAMDAT, FORMAT
*
************************************************************************
*
*     Define data header for write
*
************************************************************************
*
      CHARACTER NAMEC*8, FORMA2*3, CHA*4
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
*
*     ignore unit 0 or undefined unit
      IF(LW.EQ.0) GOTO 100
*     test whether it's an index file
      IF( INFU(7,LW).NE.0 ) GOTO 100
      IF(INFU(IWSTU+4,LW).EQ.0) THEN
CCC      CALL FPWMSG(1,'FWHDR','Record segment header undefined')
         GOTO 90
      ENDIF
      IF(INFU(3,LW).EQ.1) THEN
*        binary
         IF(INFU(IWSTU+5,LW).EQ.2) THEN
*           previous data segment header was added to buffer - update
            JP=INFU(IWSTU+9,LW)
            INDW = INDXB(IBUF,1000*LUNW+2)
            IF(INDW.LE.0) THEN
               CALL FPWMSG(1,'FWHDR','Output buffer not available')
               CALL LOGERROR('OUTPUT BUFFER LOST')
            ENDIF
            IF(IBUF(INDW+JP+7).NE.0) IBUF(INDW+JP+7)=3
         END IF
      END IF
      IF(MOD(INFU(IWSTU+2,LW),2).EQ.1) THEN
*        (bit 1 set) BOS record - use information from common
         INFU(IWHDR+2,LW)=NAMEB1
         INFU(IWHDR+3,LW)=NAMEB2
*        copy hollerith information
         DO 10 I=1,NFMTB
   10    INFU(IWHDR+9+I,LW)=IFMTB(I)
         INFU(IWHDR+1,LW)=9+NFMTB
      ELSE
         INFU(IWHDR+1,LW)=9
*        general call - data from argument
         NAMEC=NAMDAT
*        first part of array name ...
         CHA=NAMEC(1:4)
*KEEP,ICHCHA.
*     get hollerith 4H from char*4
#ifndef Linux
      ICH=IOR(ISHFT(ICHAR(CHA(1:1)),24),IOR(ISHFT(ICHAR(CHA(2:2)),16),
     +    IOR(ISHFT(ICHAR(CHA(3:3)), 8),ICHAR(          CHA(4:4)))))
#else /* Linux */
      ICH=IOR(ISHFT(ICHAR(CHA(4:4)),24),IOR(ISHFT(ICHAR(CHA(3:3)),16),
     +    IOR(ISHFT(ICHAR(CHA(2:2)), 8),ICHAR(          CHA(1:1)))))
#endif /* Linux */
*KEND.
         INFU(IWHDR+2,LW)=ICH
*        ... and second part
         CHA=NAMEC(5:8)
*KEEP,ICHCHA.
*     get hollerith 4H from char*4
#ifndef Linux
      ICH=IOR(ISHFT(ICHAR(CHA(1:1)),24),IOR(ISHFT(ICHAR(CHA(2:2)),16),
     +    IOR(ISHFT(ICHAR(CHA(3:3)), 8),ICHAR(          CHA(4:4)))))
#else /* Linux */
      ICH=IOR(ISHFT(ICHAR(CHA(4:4)),24),IOR(ISHFT(ICHAR(CHA(3:3)),16),
     +    IOR(ISHFT(ICHAR(CHA(2:2)), 8),ICHAR(          CHA(1:1)))))
#endif /* Linux */
*KEND.
         INFU(IWHDR+3,LW)=ICH
*        general treatment of format information
         IF(FORMAT.EQ.' ') THEN
            FORMA2='B32'
            NF=3
*           general case - format as character string
            DO 20 I=1,(NF+3)/4
            CHA=FORMA2(4*I-3:MIN0(NF,4*I))
*KEEP,ICHCHA.
*     get hollerith 4H from char*4
#ifndef Linux
      ICH=IOR(ISHFT(ICHAR(CHA(1:1)),24),IOR(ISHFT(ICHAR(CHA(2:2)),16),
     +    IOR(ISHFT(ICHAR(CHA(3:3)), 8),ICHAR(          CHA(4:4)))))
#else /* Linux */
      ICH=IOR(ISHFT(ICHAR(CHA(4:4)),24),IOR(ISHFT(ICHAR(CHA(3:3)),16),
     +    IOR(ISHFT(ICHAR(CHA(2:2)), 8),ICHAR(          CHA(1:1)))))
#endif /* Linux */
*KEND.
   20       INFU(IWHDR+9+I,LW)=ICH
            INFU(IWHDR+1,LW)=INFU(IWHDR+1,LW)+(NF+3)/4
            CALL FTCTOI(FORMA2,IFMTB,NFMTB)
            INFU(IWSTA+6,LW)=INFU(IWSTA+6,LW)+1
            IF(INFU(IWSTA+6,LW).LE.10) THEN
               WRITE(*,*) ' ********* WARNING FROM FWHDR ***********'
               WRITE(*,*) ' *  Data block ',NAMEC,' without format  *'
               WRITE(*,*) ' ****************************************'
               CALL FPWMSG(1,'FWHDR',
     +                'Data block '//NAMEC//' without format')
            END IF
         ELSE
*           general case - format as character string
            NF=LENB(FORMAT)
            DO 22 I=1,(NF+3)/4
            CHA=FORMAT(4*I-3:MIN0(NF,4*I))
*KEEP,ICHCHA.
*     get hollerith 4H from char*4
#ifndef Linux
      ICH=IOR(ISHFT(ICHAR(CHA(1:1)),24),IOR(ISHFT(ICHAR(CHA(2:2)),16),
     +    IOR(ISHFT(ICHAR(CHA(3:3)), 8),ICHAR(          CHA(4:4)))))
#else /* Linux */
      ICH=IOR(ISHFT(ICHAR(CHA(4:4)),24),IOR(ISHFT(ICHAR(CHA(3:3)),16),
     +    IOR(ISHFT(ICHAR(CHA(2:2)), 8),ICHAR(          CHA(1:1)))))
#endif /* Linux */
*KEND.
   22       INFU(IWHDR+9+I,LW)=ICH
            INFU(IWHDR+1,LW)=INFU(IWHDR+1,LW)+(NF+3)/4
            CALL FTCTOI(FORMAT,IFMTB,NFMTB)
         END IF
      END IF
*
      INFU(IWHDR+4,LW)=NUM
      INFU(IWHDR+5,LW)=NCOL
      INFU(IWHDR+6,LW)=NROW
*
*     set hdr defined
      INFU(IWSTU+5,LW)=1
*
*     select
      CALL FSELCT(4,LUNBK(LW),IDEC)
      IF(IDEC.NE.0) THEN
         INFU(IWSTU+5,LW)=0
         GOTO 100
      END IF
*
*     number of data blocks written
      INFU(IWSTA+5,LW)=INFU(IWSTA+5,LW)+1
*
      GOTO 100
*
*     error 1: key undefined
   90 INFU(IWSTU+6,LW)=2*(INFU(IWSTU+6,LW)/2)+1
*
  100 CONTINUE
      END
