*CMZ :  1.00/00 16/12/94  17.00.00  by  Pavel Binko
*-- Author :    Volker Blobel   16/12/94
      SUBROUTINE FNDDIR(LX,NAMKEY,NUM1,NUM2,NDATWD,IMODE)
*
      CHARACTER*(*) NAMKEY
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
      CHARACTER*8 NMKEY2,NMKEY3
      CHARACTER*4 CHA
      INTEGER IARR(2)
      LOGICAL FOUND
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
      LUN = LUNBK(LX)
      IF(LUN.LE.0.OR.LUN.GT.99)CALL LOGERROR('BAD LUN')
      NMKEY2=NAMKEY(1:MIN(8,LEN(NAMKEY)))
      DO 1 I=1,2
      CHA=NMKEY2(I*4-3:I*4)
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
      IARR(I)=ICH
    1 CONTINUE
      FOUND = .FALSE.
*     Let's find DIR bank
      IDXD = INDXB(IBUF,1000*LUN+4)
      IF(IDXD.LE.0)CALL LOGERROR('DIR BANK MISSING')
      N = IBUF(IDXD)/6
      IF( 6*N .NE. IBUF(IDXD) ) CALL LOGERROR('Error in FNDDIR')
      ILOW = 1
5     CONTINUE
 
         J = ILOW
         IF( N.EQ.0 ) GOTO 6
         N1 = N/2
         N2 = N-N1-1
         J = ILOW + 6*N1
         ICH = IBUF(IDXD+J)
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
         NMKEY3(1:4) = CHA
         ICH = IBUF(IDXD+J+1)
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
         NMKEY3(5:8) = CHA
         IF(NMKEY3.LT.NMKEY2) GOTO 7
         IF(NMKEY3.GT.NMKEY2) GOTO 8
         IF(IBUF(IDXD+J+2).LT.NUM1) GOTO 7
         IF(IBUF(IDXD+J+2).GT.NUM1) GOTO 8
         IF(IBUF(IDXD+J+3).LT.NUM2) GOTO 7
         IF(IBUF(IDXD+J+3).GT.NUM2) GOTO 8
         FOUND = .TRUE.
         GOTO 6
 
 7       CONTINUE
*        Let's try upper part
         ILOW = J+6
         N = N2
         GOTO 5
 
 8       CONTINUE
         IF( N1.EQ.0 ) GOTO 6
*        Let's try lower part
         N = N1
         GOTO 5
 
 6    CONTINUE
      IF(IMODE.EQ.0)THEN
*        find request
         IF(FOUND)THEN
            IMODE = J
         ELSE
            IMODE = -J
         ENDIF
         GOTO 100
      ENDIF
      IF(FOUND)THEN
*        Let's find FAT bank
         IDXF = INDXB(IBUF,1000*LUN+5)
         IF(IDXF.LE.0)CALL LOGERROR('FAT BANK MISSING')
         K = IBUF(IDXD+J+5)
         IF(K.GT.0)THEN
            IREP = -2
         ELSE
            IREP = -1
            K = -K
         ENDIF
 771     CONTINUE
         IF(K.NE.0) THEN
            KK = IBUF(IDXF+K)
            IF(KK.LT.0)CALL LOGERROR('FAT CORRUPTED')
            IBUF(IDXF+K) = IREP
            IF( IREP.EQ.-1 .AND. K.LT.INFU(29,LX) )INFU(29,LX) = K
            K = KK
            GOTO 771
         ENDIF
         IBUF(IDXD+J+5) = 0
      ENDIF
      IF(IMODE.EQ.2)THEN
*        delete request
         IF(FOUND)THEN
            IMODE = J
            DO 10 I=J+6,IBUF(IDXD)
               IBUF(IDXD+I-6) = IBUF(IDXD+I)
 10         CONTINUE
            IBUF(IDXD) = IBUF(IDXD) - 6
         ELSE
            IMODE = -1
         ENDIF
      ELSE
         IMODE = J
         IF(.NOT.FOUND)THEN
*           Let's expand DIR bank
            N = IBUF(IDXD)+6
            IF( N.GT.IBUF(IDXD-3) )
     +         IDXD = INDXE(IBUF,1000*LUN+4,N+600)
            IF(IDXD.LE.0)CALL LOGERROR('DIR EXPANSION FAILED')
            IBUF(IDXD) = N
            DO 20 I=IBUF(IDXD),J+6,-1
               IBUF(IDXD+I) = IBUF(IDXD+I-6)
 20         CONTINUE
            IBUF(IDXD+J  ) = IARR(1)
            IBUF(IDXD+J+1) = IARR(2)
            IBUF(IDXD+J+2) = NUM1
            IBUF(IDXD+J+3) = NUM2
            IBUF(IDXD+J+4) = NDATWD
            IBUF(IDXD+J+5) = 0
         ENDIF
      END IF
*
  100 RETURN
      END
