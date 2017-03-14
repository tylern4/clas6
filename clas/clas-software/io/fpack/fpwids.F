#ifndef Linux
*CMZ :  1.00/16 10/03/96  21.40.34  by  Unknown
#endif /* not Linux */
*CMZU:  1.00/02 10/02/95  13.11.44  by  Unknown
*CMZ :  1.00/00 16/12/94  17.00.00  by  Pavel Binko
*-- Author :    Ruten Gurin   16/12/94
      SUBROUTINE FPWIDS(IARR,ISWAP)
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
      PARAMETER (NW5 = 5)
      PARAMETER (IBYORD=1+256*(2+256*(3+256*4)))
      CHARACTER CHA*4
      INTEGER   INAME(3)
      LOGICAL   START
      DATA      START/.TRUE./
#endif /* not Linux */
*
*     write the index block
*     The content of the block (words):
*        8,9      - update counter (top level block only)
*        11       - physical record number for that block
*        14       - size (in words) for data seg. header
*        17       - if <= 1 - the bottom index block
*        18       - number of used blocks in file (top level block only)
*        19       - alt. physical record number for that block
*        22       - number of words in index(placed after data seg. h.)
*
      INTEGER IARR(*)
#ifndef Linux
*
      IF(START) THEN
         START = .FALSE.
         CHA='FPIN'
*KEEP,ICHCHA.
*     get hollerith 4H from char*4
      ICH=IOR(ISHFT(ICHAR(CHA(1:1)),24),IOR(ISHFT(ICHAR(CHA(2:2)),16),
     +    IOR(ISHFT(ICHAR(CHA(3:3)), 8),ICHAR(          CHA(4:4)))))
*KEND.
         INAME(1) = ICH
         CHA='DEXA'
*KEEP,ICHCHA.
*     get hollerith 4H from char*4
      ICH=IOR(ISHFT(ICHAR(CHA(1:1)),24),IOR(ISHFT(ICHAR(CHA(2:2)),16),
     +    IOR(ISHFT(ICHAR(CHA(3:3)), 8),ICHAR(          CHA(4:4)))))
*KEND.
         INAME(2) = ICH
         CHA='I   '
*KEEP,ICHCHA.
*     get hollerith 4H from char*4
      ICH=IOR(ISHFT(ICHAR(CHA(1:1)),24),IOR(ISHFT(ICHAR(CHA(2:2)),16),
     +    IOR(ISHFT(ICHAR(CHA(3:3)), 8),ICHAR(          CHA(4:4)))))
*KEND.
         INAME(3) = ICH
      ENDIF
*
      IF(LW.EQ.0.OR.LUNW.EQ.0)
     +   CALL LOGERROR('FPWIDS. Unit not defined')
      INDW = INDXB(IBUF,1000*LUNW+2)
      NALL = IBUF(INDW+1)
      CALL VZERO(IBUF(INDW+1),NALL)
#endif /* not Linux */
      NREC = IARR(11)
#ifndef Linux
*
      IBUF(INDW+ 1) = NALL
      IBUF(INDW+ 2) = NALL
*     RECORD SEGMENT HEADER
      IBUF(INDW+ 3) = IBYORD
      IBUF(INDW+ 4) = LOCAL*16+INFU(ISGEN+2,LW)
      IBUF(INDW+ 5) = NREC*100
      IBUF(INDW+ 6) = INAME(1)
      IBUF(INDW+ 7) = INAME(2)
      IBUF(INDW+ 8) = IARR(8)
      IBUF(INDW+ 9) = IARR(9)
      IBUF(INDW+10) = 0
      IBUF(INDW+11) = NREC
      IBUF(INDW+12) = 0
      IBUF(INDW+13) = NALL-13
*     DATA SEGMENT HEADER
      IBUF(INDW+14) = 10
      IBUF(INDW+15) = INAME(1)
      IBUF(INDW+16) = INAME(2)
      IBUF(INDW+17) = IARR(17)
      IBUF(INDW+18) = IARR(18)
      IBUF(INDW+19) = IARR(19)
      IBUF(INDW+20) = 0
      IBUF(INDW+21) = 0
      IBUF(INDW+22) = IARR(22)
      IBUF(INDW+23) = INAME(3)
*     DATA (INDEXES)
      JP = 13
      KP = JP + IBUF(INDW+JP+1)
      CALL UCOPY(IARR(KP+1),IBUF(INDW+KP+1),IARR(22))
      IBUF(INDW+NALL)=IARR(9)
*     debug
*      NWORDS = IARR(22)
*      IELSZ  = 7
*      PRINT *,'---------------- FPWIDS ------------------'
*      PRINT '(1X,''COUNTER='',2I10,1X,''TYPE='',I2,1X,''NUSBLK='',2I9)',
*     + IARR(8),IARR(9),IARR(17),IARR(18),IARR(22)
*      PRINT '(1X,''NItems='',I4,2X,''IBLK='',I6,2X,''IALT='',I6)',
*     + NWORDS/IELSZ,IARR(11),IARR(19)
*      PRINT 1000
*1000  FORMAT(2X,'RECNAME',6X,'NUM1',5X,'NUM2',4X,'CLASS',
*     +5X,'BLK#',3X,'OFFSET')
*      DO I = 1,NWORDS-1,IELSZ
*         PRINT '(2X,2A4,1X,5I9)',(IBUF(INDW+KP+I+J-1),J=1,IELSZ)
*      ENDDO
*      PRINT *,'--------------- END OF FPWIDS --------------'
*
#endif /* not Linux */
      INFU(26,LW) = NREC
#ifndef Linux
*************************************************************
*      CALL FWKEX('FPINDEXA',IARR(8),IARR(9),IARR(11))
*      CALL FWHDR('FPINDEXA',IARR(17),IARR(18),IARR(19),'I')
*      CALL FWDAT(IARR(22),IARR(KP+1))
*      CALL FWENDX(IERR)
*************************************************************
*     convert word format ...
      JFM=INFU(5,LW)
*        ... if output ne local and inhibit flag not set
      IF(JFM.NE.LOCAL.AND.INFU(ISGEN+4,LW).EQ.0) THEN
         CALL FRCONV(IBUF(INDW+1),JFM,ICER)
      END IF
*
      CALL FWREC(LUNW,IBUF(INDW+1),IBUF(INDW+1),IEND)
*
      IF(INFU(ISGEN+12,LW).EQ.1.AND.IARR(11).EQ.1) CALL FPSYNC(IRC)
#else /* Linux */
      CALL FWKEX('FPINDEXA',IARR(8),IARR(9),IARR(11))
      CALL FWHDR('FPINDEXA',IARR(17),IARR(18),IARR(19),'I')
      JP = 13
      KP = JP + IARR(JP+1)
      CALL FWDAT(IARR(22),IARR(KP+1))
      CALL FWENDX(IERR)
#endif /* Linux */
      IF(ISWAP.NE.0) THEN
*     swap the record numbers
         IARR(11) = IARR(19)
         IARR(19) = NREC
      ENDIF
*     clear the output buffer
#ifdef Linux
      INDW = INDXB(IBUF,1000*LUNW+2)
#endif /* Linux */
      IBUF(INDW+2) = 2
      INFU(IWSTU+8,LW) = 0
      INFU(IWSTU+4,LW) = 0
      END
