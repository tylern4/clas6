#ifndef Linux
*CMZU:  1.00/16 27/02/96  16.29.09  by  Unknown
*-- Author :    Sergey Esenov   26/02/96
      SUBROUTINE FFPREV(IBLK,IOFF,IVERS,JBLK,JOFF)
*     Extract version# of current record and disk address of
*     alternative version of data logical record.
*
*     Input parameters:  IBLK - block# of current version
*                        IOFF - offset from beginning of that block
*
*     Output parameters: IVERS - version# of current version
*                        JBLK - block# of alternative version
*                        JOFF - offset
*     Called from FWKEY
*
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
*     Function statements ...
      NEXTIP(IP)=IP+IBUF(INDR+IP+11)+11
*
      JBLK   = 0
      JOFF   = 0
      IVERS  = 0
*
      IF(LR.EQ.0.OR.LW.EQ.0.OR.LUNR.EQ.0.OR.LUNW.EQ.0) GO TO 100
      ISVLR  = LR
      ISVLNR = LUNR
      LR     = LW
      LUNR   = LUNW
      ISV27  = INFU(27,LR)
*
*     If it's not an ordered file ...
      IF(INFU(25,LW).EQ.0) GOTO 90
*     Get pointer to buffer
      INDW = INDXB(IBUF,1000*LUNW+3)
      IF(INDW.LE.0) THEN
         INDR = INDXA(IBUF,1000*LUNW+3,INFU(1,LR)/4)
         IF(INDW.LE.0) CALL LOGERROR('FFPREV. No space')
      ENDIF
*     Read block to buffer and convert to LOCAL format
      INFU(27,LR) = IBLK
      CALL FRRECX(LUNW,IBUF(INDW),IBUF(INDW+1),IRC)
      IF(IRC.NE.0) CALL LOGERROR('FFPREV. READ ERROR')
      CALL FSWAP(IBUF(INDR+1),0)
      CALL FRCONV(IBUF(INDR+1),LOCAL,ICER)
*     Find next logical record
      IPNEXT = IOFF+11+IBUF(INDW+IOFF+11) - NW5
*     5 words:
*     the fifth word - version#
      IVERS  = IBUF(INDW+IPNEXT+5)
*     the first and second  - address of alternative version
      JBLK = IBUF(INDW+IPNEXT+1)
      JOFF = IBUF(INDW+IPNEXT+2)
90    INFU(27,LR) = ISV27
      LR   = ISVLR
      LUNR = ISVLNR
100   CONTINUE
      RETURN
      END
#else /* Linux */
      subroutine dummy_ffprev
      implicit none
      return
      end
#endif /* Linux */
