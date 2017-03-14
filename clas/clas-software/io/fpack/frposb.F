*CMZ :  1.00/00 16/12/94  17.00.00  by  Pavel Binko
*-- Author :    Volker Blobel   16/12/94
      SUBROUTINE FRPOSB(TITLE,NR1,NR2,IERR)
*
      CHARACTER*(*) TITLE
*
************************************************************************
*
*     Set direction backwards for reading keyed access file
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
      CHARACTER TITCOP*8, CHA*4, NAMKEY*8
      INTEGER IARR(2)
*KEEP,ARETOA.
      INTEGER ETOA(0:255)
*KEEP,DTETOA.
      DATA (ETOA(I),I=0,255) /
     +   0, 32, 32, 32, 32, 32, 32, 32, 32,  5, 32, 32, 32, 13, 32, 32,
     +  32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32,
     +  32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32,
     +  32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32,
     +  32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 46, 60, 40, 43,124,
     +  38, 32, 32, 32, 32, 32, 32, 32, 32, 32, 33, 36, 42, 41, 59, 94,
     +  45, 47, 32, 32, 32, 32, 32, 32, 32, 32, 32, 44, 37, 95, 62, 63,
     +  32, 32, 32, 32, 32, 32, 32, 32, 32, 96, 58, 35, 64, 39, 61, 34,
     +  32, 97, 98, 99,100,101,102,103,104,105, 32, 32, 32, 32, 32, 32,
     +  32,106,107,108,109,110,111,112,113,114, 32, 32, 32, 32, 32, 32,
     +  32,126,115,116,117,118,119,120,121,122, 32, 32, 32, 91, 32, 32,
     +  32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 93, 32, 32,
     + 123, 65, 66, 67, 68, 69, 70, 71, 72, 73, 32, 32, 32, 32, 32, 32,
     + 125, 74, 75, 76, 77, 78, 79, 80, 81, 82, 32, 32, 32, 32, 32, 32,
     +  92, 32, 83, 84, 85, 86, 87, 88, 89, 90, 32, 32, 32, 32, 32, 32,
     +  48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 32, 32, 32, 32, 32, 32 /
*KEND.
*
      IERR=1
      IF(LR.EQ.0.OR.LUNR.EQ.0) GOTO 100
*
*     dummy read in case of first access to the file
      IF(INDXB(IBUF,1000*LUNR+1).EQ.0) THEN
         CALL FRKEY(NAMKEY,NUM1,NUM2,NDATW,IER)
      ENDIF
*
*     only for keyed and ordered access
      IF(INFU(2,LR).NE.5.AND.INFU(2,LR).NE.3) GOTO 100
      IERR=0
*
*     store requested key for backward reading
      INFU(IRSTU+11,LR)= 1
      INFU(IRSTU+12,LR)=-2
      INFU(IRSTU+13,LR)= 0
*
      IF(TITLE.EQ.'MAXIMKEY') THEN
         INFU(IRSTU+14,LR)=-1
         INFU(IRSTU+15,LR)=-1
         INFU(IRSTU+16,LR)=-1
         INFU(IRSTU+17,LR)=-1
      ELSE IF( INFU(25,LR).NE.0 ) THEN
         IF(TITLE.EQ.'MINIMKEY') THEN
            INFU(IRSTU+14,LR)=0
            INFU(IRSTU+15,LR)=0
            INFU(IRSTU+16,LR)=0
            INFU(IRSTU+17,LR)=0
         ELSE
            TITCOP=TITLE
            I = 1
            CHA=TITCOP(1:4)
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
            IARR(1) = ICH
            CHA=TITCOP(5:8)
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
            IARR(2) = ICH
            CALL FPKENC(IARR,INFU(IRSTU+14,LR))
            INFU(IRSTU+16,LR)=NR1
            INFU(IRSTU+17,LR)=NR2
         ENDIF
      ELSE
         TITCOP=TITLE
         DO 10 I=1,2
         CHA=TITCOP(I*4-3:I*4)
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
   10    INFU(IRSTU+13+I,LR)=ICH
         INFU(IRSTU+16,LR)=NR1
         INFU(IRSTU+17,LR)=NR2
      END IF
*
      INFU(IRSTU+18,LR)=0
      IF( INFU(25,LR).NE.0 ) THEN
         CALL FPFIND(1,IFOUND)
         INFU(ISGEN+9,LR) = 0
      ENDIF
*
*     reset flag key just read
      INFU(IRSTU+4,LR)=0
*
*     reset flag reading end-of-file to reading
      INFU(ISGEN+1,LR)=1
*
  100 RETURN
      END
