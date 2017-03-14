#ifndef Linux
*CMZU:  1.00/16 27/02/96  16.27.15  by  Unknown
*-- Author :    Sergey Esenov   26/02/96
      SUBROUTINE FDTKEY(NAMKEY,NUM1,NUM2,IBLK,IOFF,IFOUND)
**********************************************************
*     Find DaTa address using the KEY (ORDERED ACCESS)
*     CALLED from FWKEY
*     Input parameters: KEY -> (NAMKEY,NUM1,NUM2)
*     Output parameters: IBLK - block number
*                        IOFF - offset in that block
*                        IFOUND = 1 key found
*                               = 0 key not found
**********************************************************
      CHARACTER*(*) NAMKEY
      CHARACTER NAMEC*8, CHA*4
      INTEGER   KEY(4),KEY1(4)
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
*
      IFOUND = 0
      IBLK   = 0
      IOFF   = 0
      IF(LR.EQ.0.OR.LW.EQ.0.OR.LUNR.EQ.0.OR.LUNW.EQ.0) GOTO 300
      ISVLR  = LR
      ISVLNR = LUNR
      LR     = LW
      LUNR   = LUNW
      IF(INFU(25,LR).EQ.0) GOTO 290
*     First get pointer to working buffer
      INDX = INDXB(IBUF,1000*LUNW+3)
      IF(INDX.LE.0) THEN
         INDX = INDXA(IBUF,1000*LUNW+3,INFU(1,LW)/4)
         IF(INDX.LE.0) GOTO 290
      ENDIF
*
*     Prepare the input key
      NAMEC = NAMKEY
      CHA = NAMEC(1:4)
*KEEP,ICHCHA.
*     get hollerith 4H from char*4
      ICH=IOR(ISHFT(ICHAR(CHA(1:1)),24),IOR(ISHFT(ICHAR(CHA(2:2)),16),
     +    IOR(ISHFT(ICHAR(CHA(3:3)), 8),ICHAR(          CHA(4:4)))))
*KEND.
      KEY(1) = ICH
      CHA = NAMEC(5:8)
*KEEP,ICHCHA.
*     get hollerith 4H from char*4
      ICH=IOR(ISHFT(ICHAR(CHA(1:1)),24),IOR(ISHFT(ICHAR(CHA(2:2)),16),
     +    IOR(ISHFT(ICHAR(CHA(3:3)), 8),ICHAR(          CHA(4:4)))))
*KEND.
      KEY(2) = ICH
      KEY1(3) = NUM1
      KEY1(4) = NUM2
      CALL FPKENC(KEY,KEY1)
*      PRINT *,'REQUESTED KEY'
*      PRINT '(1X,2A4,2I9)',(KEY1(J),J=1,4)
      ISV27 = INFU(27,LR)
      INFU(27,LR) = 1
10    CALL FRRECX(LUNR,IBUF(INDX),IBUF(INDX+1),IER)
*     Bad record ...
      IF(IER.NE.0) GOTO 200
*     find key position in this block
      NW = IBUF(INDX+22)
*     If index block is empty ...
      IF(NW.LT.7) GOTO 200
      JP=13
      KP = JP + IBUF(INDX+JP+1)
      NPOS = -7
20    CONTINUE
      NPOS = NPOS+7
      IF( NPOS.EQ.NW ) THEN
*        requested key is greater than the last key
         IF( IBUF(INDX+17).GT.1 ) THEN
*           we keep NPOS inside the block
            NPOS = NPOS-7
            INFU(27,LR) = IBUF(INDX+KP+NPOS+6)
*           Go down to leaves
            GOTO 10
         ENDIF
         GOTO 200
      ENDIF
*     compare the keys
*      PRINT '(1X,2A4,2I9)',(IBUF(INDX+KP+NPOS+J),J=1,4)
      DO 30 I=1,4
         N2 = KEY1(I)
*        zero is the smallest possible key
         N1 = IBUF(INDX+KP+NPOS+I)
*        the order is: 0 ... maxint minint ... -1
         IF( N1.NE.N2 ) THEN
            IF( N2.EQ.0 ) GOTO 100
            IF( N2.GT.0 .AND. N1.LT.0 ) GOTO 100
            IF( N1.GT.0 .AND. N2.LT.0 ) GOTO 20
            IF( N2.LT.N1 ) GOTO 100
            GOTO 20
         ENDIF
 30   CONTINUE
      INFU(27,LR) = IBUF(INDX+KP+NPOS+6)
*     If it's not a leaf's level, go down by reference
      IF(IBUF(INDX+17).GT.1) GOTO 10
*     KEY found
      IFOUND = 1
      IBLK   = IBUF(INDX+KP+NPOS+6)
      IOFF   = IBUF(INDX+KP+NPOS+7)
      GOTO 200
*     Key not found yet
 100  INFU(27,LR) = IBUF(INDX+KP+NPOS+6)
      IF( IBUF(INDX+17).GT.1 ) GOTO 10
200   INFU(27,LR) = ISV27
290   LR   = ISVLR
      LUNR = ISVLNR
300   CONTINUE
      RETURN
      END
#else /* Linux */
      subroutine dummy_fdtkey
      implicit none
      return
      end
#endif /* Linux */
