#ifndef Linux
*CMZ :  1.00/16 10/03/96  15.14.59  by  Unknown
*-- Author :    Sergey Esenov   04/03/96
      SUBROUTINE FWKIDX(KEY,IERR)
*
************************************************************************
*
*     Insert/Update the key for ordered access
*     Prepared from FWKUPD (Author: Ruten Gurin)
************************************************************************
*
      PARAMETER (NW5 = 5)
      INTEGER  KEY(7),KEY1(7)
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
      INTEGER KEY0(7),ISVK(4)
      INTEGER IFREE(2)
      INTEGER NEWBLK
      SAVE    NEWBLK
*
      IERR = 0
*     save LR,LUNR
      ISVLR = LR
      ISVLUN = LUNR
*     reset them
      LR = LW
      LUNR = LUNW
*
      CALL UCOPY(INFU(IRSTU+14,LR),ISVK,4)
*     fill the key
      CALL FPKENC(KEY,KEY1)
      KEY1(3) = KEY(3)
      KEY1(4) = KEY(4)
      KEY1(5) = KEY(5)
      KEY1(6) = KEY(6)
      KEY1(7) = KEY(7)
*
      IBLK    = KEY(6)
      IOFF    = KEY(7)
*     move key1 to read key
      CALL UCOPY(KEY1,INFU(IRSTU+14,LR),4)
***      PRINT *,'REQUESTED KEY'
***      PRINT '(1X,2A4,5I9)',(KEY1(j),j=1,7)
***      PRINT *,' '
*
*     build the index path
      CALL FPFIND(0,IFOUND)
*     no key0 at the moment
      KEY0(6) = 0
***      PRINT *,'FWKIDX. IFOUND(FPFIND)=',IFOUND
*
*
      LEVEL = INFU(25,LR)
551   CONTINUE
*        find the buffer
         IF( LEVEL.LE.0 ) CALL LOGERROR('FWKUPD 03')
         INR = INDXB(IBUF,1000*LUNR+9+LEVEL)
         IF( INR.LE.0 ) CALL LOGERROR('FWK 02')
         JP = 13
*        pointer to index information
         KP = JP + IBUF(INR+JP+1)
*        index element size (words)
         IELSZ = 7
*        number of words
         NW = IBUF(INR+22)
*        current position
         NPOS = IBUF(INR+15)
*        number of words in the whole buffer
         NLIM = IBUF(INR+1) - NW5
         IF( NW.LT.IELSZ ) THEN
            NW = 0
            NPOS = 0
         ENDIF
         IF( KEY0(6).NE.0 ) THEN
            CALL UCOPY(KEY0,IBUF(INR+KP+NPOS+1),IELSZ)
            IF( NW.EQ.0 ) NW = IELSZ
            NPOS = NPOS + IELSZ
            KEY0(6) = 0
*           new key must be inserted
            IFOUND = 0
         ENDIF
         IF( IFOUND.EQ.1 ) THEN
*           we replace the current key
            CALL UCOPY(KEY1,IBUF(INR+KP+NPOS+1),IELSZ)
         ELSE IF( KP+NW+IELSZ .LE. NLIM ) THEN
*           PRINT *,'FWKUPD. NEW KEY - ENOUGH ROOM. LEVEL=', LEVEL
*           enough place for new key
            NW = NW+IELSZ
            IBUF(INR+22) = NW
            IF(NW.GT.IELSZ) THEN
*              shift the keys
               DO 55422 JJ=INR+KP+NW-IELSZ,INR+KP+NPOS+1,-1
                  IBUF(JJ+IELSZ) = IBUF(JJ)
55422          CONTINUE
            ENDIF
            CALL UCOPY(KEY1,IBUF(INR+KP+NPOS+1),IELSZ)
         ELSE
*           not enough space to insert the key
*           we have to split the block
*           use the free buffer from the next level
            IWRK2 = INDXB(IBUF,1000*LUNW+9+LEVEL+1)
            IF( IWRK2.LE.0 ) THEN
               IWRK2 = INDXA(IBUF,1000*LUNW+9+LEVEL+1,INFU(1,LW)/4)
               IF( IWRK2.LE.0 ) CALL LOGERROR('Out of sp in FWKUPD')
*              INR was changed maybe
               INR = INDXB(IBUF,1000*LUNR+9+LEVEL)
            ENDIF
*           copy the header part of the record
            CALL UCOPY(IBUF(INR+1),IBUF(IWRK2+1),KP)
*           we will split the record, putting NW1 words
*           to the left block, and NW-NW1+IELSZ - to the right.
            NENT = NW/IELSZ
            NW1 = IELSZ*((2*NENT)/3)
            IF( NW1.EQ.0 ) NW1 = IELSZ
            NW2 = NW-NW1+IELSZ
            IF( NW1.LE.NPOS ) THEN
               CALL UCOPY(IBUF(INR+KP+NW1+1),IBUF(IWRK2+KP+1),
     +                          NPOS-NW1)
               CALL UCOPY(KEY1,IBUF(IWRK2+KP+NPOS-NW1+1),IELSZ)
               CALL UCOPY(IBUF(INR+KP+NPOS+1),
     +                         IBUF(IWRK2+KP+NPOS-NW1+IELSZ+1),
     +                         NW-NPOS)
            ELSE
               CALL UCOPY(IBUF(INR+KP+NW1-IELSZ+1),
     +                         IBUF(IWRK2+KP+1),
     +                         NW2)
               DO 55421 JJ=INR+KP+NW1-IELSZ,INR+KP+NPOS+1,-1
                  IBUF(JJ+IELSZ) = IBUF(JJ)
55421          CONTINUE
               CALL UCOPY(KEY1,IBUF(INR+KP+NPOS+1),IELSZ)
            ENDIF
*           set new sizes and refill key0,key1 - maximum items
            IBUF(INR+22) = NW1
            IBUF(IWRK2+22) = NW2
            CALL UCOPY(IBUF(INR+KP+NW1-IELSZ+1),KEY0,4)
            CALL UCOPY(IBUF(IWRK2+KP+NW2-IELSZ+1),KEY1,4)
            KEY0(7) = 2
            KEY1(7) = 2
*     set the first and alternative rec. numbers
*     for the second part
*
            IBUF(IWRK2+11) = IDXCHK(INFU(30,LW)+1)
            IBUF(IWRK2+19) = IDXCHK(INFU(30,LW)+2)
            KEY1(6) = IBUF(IWRK2+11)
*           write the second part
            CALL FPWIDS(IBUF(IWRK2+1),1)
*           write the alternative version
            CALL FPWIDS(IBUF(IWRK2+1),1)
            IF( LEVEL.EQ.1 ) THEN
*              we splitted the top block
*              so we write the first part to new place
               IBUF(INR+11) = IDXCHK(INFU(30,LW)+1)
               IBUF(INR+19) = IDXCHK(INFU(30,LW)+2)
               KEY0(6) = IBUF(INR+11)
*              write the first part and automatic swap
               CALL FPWIDS(IBUF(INR+1),1)
*              write the alternative version & swap
               CALL FPWIDS(IBUF(INR+1),1)
*              Refill the top block
*              The block is empty
               IBUF(INR+22) = 1
*              current position - 0
               IBUF(INR+15)= 0
*              this block points to other index blocks
               IBUF(INR+17) = 2
            ELSE
*              rewrite always to the alternative version
               KEY0(6) = IBUF(INR+19)
*              swap versions
               IBUF(INR+19) = IBUF(INR+11)
               IBUF(INR+11) = KEY0(6)
*              rewrite the block & no swap
               CALL FPWIDS(IBUF(INR+1),0)
               LEVEL = LEVEL - 1
            ENDIF
            GOTO 551
         ENDIF
         IF( LEVEL.NE.1 ) THEN
*           rewrite always to the alternative version of index block
*           reset the key1
            CALL UCOPY(IBUF(INR+KP+NW-IELSZ+1),KEY1,IELSZ)
            KEY1(6) = IBUF(INR+19)
            KEY1(7) = 2
*           Swap versions of index block before FPWIDS
            IBUF(INR+19) = IBUF(INR+11)
            IBUF(INR+11) = KEY1(6)
*           rewrite the buffer & no swap
            CALL FPWIDS(IBUF(INR+1),0)
*           and go up
            LEVEL = LEVEL - 1
            IFOUND = 1
            GOTO 551
         ENDIF
*     update the counter
      INR = INDXB(IBUF,1000*LUNW+9+1)
*
*     set "unfinished" values
      IBUF(INR+8) = 1
      IBUF(INR+9) = 1
      IBUF(INR+18) = INFU(30,LW)
*     set the phys record numbers
      IBUF(INR+11) = 2
      IBUF(INR+19) = 1
      CALL FPWIDS(IBUF(INR+1),1)
      CALL FPWIDS(IBUF(INR+1),1)
*
      INFU(ISGEN+9,LW) = 1
 100  CONTINUE
*     restore
      CALL UCOPY(ISVK,INFU(IRSTU+14,LW),4)
*
 200  CONTINUE
      LR = ISVLR
      LUNR = ISVLUN
      RETURN
      END
#else /* Linux */
      subroutine dummy_fwkidx
      implicit none
      return
      end
#endif /* Linux */
