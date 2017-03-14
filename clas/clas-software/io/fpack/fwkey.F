#ifndef Linux
*CMZ :  1.00/16 13/03/96  18.45.57  by  Unknown
#endif /* not Linux */
*CMZ :  1.00/00 16/12/94  17.00.00  by  Pavel Binko
*-- Author :    Volker Blobel   16/12/94
      SUBROUTINE FWKEY(NAMKEY,NUM1,NUM2,NDATWD)
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
      CHARACTER NAMEC*8,CHA*4
      INTEGER*4 ISVR14(4)
      INTEGER   IIND(2)
      LOGICAL   START
      DATA      START/.TRUE./
      SAVE      START,IIND
*
      IF(START) THEN
         START = .FALSE.
         CHA   = 'FPIN'
*KEEP,ICHCHA.
*     get hollerith 4H from char*4
      ICH=IOR(ISHFT(ICHAR(CHA(1:1)),24),IOR(ISHFT(ICHAR(CHA(2:2)),16),
     +    IOR(ISHFT(ICHAR(CHA(3:3)), 8),ICHAR(          CHA(4:4)))))
*KEND.
         IIND(1) = ICH
         CHA   = 'DEXA'
*KEEP,ICHCHA.
*     get hollerith 4H from char*4
      ICH=IOR(ISHFT(ICHAR(CHA(1:1)),24),IOR(ISHFT(ICHAR(CHA(2:2)),16),
     +    IOR(ISHFT(ICHAR(CHA(3:3)), 8),ICHAR(          CHA(4:4)))))
*KEND.
         IIND(2) = ICH
      ENDIF
#endif /* not Linux */
*
      IF(LW.EQ.0) GOTO 100
      IF(INFU(7,LW).NE.0) GOTO 100
#ifndef Linux
*      PRINT *,'----------------- FWKEY ---------------'
*      PRINT *,NAMKEY,' ',NUM1,NUM2,NDATWD
#endif /* not Linux */
*
      IF(INFU(IWSTU+4,LW).EQ.2) THEN
         CALL FWEND(IERR)
      END IF
#ifndef Linux
*
      INFU(ISGEN+11,LW) = 0
      DO 20 K=14,25
20    INFU(IWSTU+K,LW)=0
*
#endif /* not Linux */
      IF( INFU(25,LW).GT.0 ) THEN
#ifndef Linux
*      print *,'FWKEY. INFU(25,LW)=',INFU(25,LW)
#endif /* not Linux */
*        this is an ORDERED file with some index level(s) in memory
#ifndef Linux
         NTRY = 0
30       CALL FPLOCK(' ',IRET)
         NTRY = NTRY + 1
         IF(NTRY.LT.100.AND.IRET.NE.0) GOTO 30
         IF(IRET.NE.0) GOTO 91
#else /* Linux */
         IF( INFU(ISGEN+8,LW).EQ.1 ) CALL LOGERROR('Unlock missing')
         CALL FPLOCK(' ',IRET)
         INFU(ISGEN+8,LW) = 1
#endif /* Linux */
*        we have to check whether it was changed
         ISVLR = LR
         ISVLUNR = LUNR
         LR = LW
         LUNR = LUNW
#ifndef Linux
*        Check if file was changed by concurrent writing process ...
*      PRINT *,'FWKEY. 1.INFU(30,LW)=',INFU(30,LW)
*
* This CALL to FPCKTP (check tree path) is now obsolete. We do its job
* in the lines below.
*
#endif /* not Linux */
         CALL FPCKTP(IRES)
#ifdef Linux
         LR = ISVLR
         LUNR = ISVLUNR
#endif /* Linux */
         IF( IRES.NE.0 ) THEN
#ifndef Linux
*      PRINT *,'FWKEY. IRES(FPCKTP)=',IRES
*           ISV27 = INFU(27,LR)
*           INX = INDXB(IBUF,1000*LUNR+9+1)
*           IF(INX.LE.0) CALL LOGERROR('FWKEY.NOT ORDER')
*           INFU(27,LR) = 1
*           CALL FRRECX(LUNR,IBUF(INX),IBUF(INX+1),IRC)
*           INFU(27,LR) = ISV27
*           IF(IRC.LT.0.OR.IRC.GT.0) GOTO 98
*           CALL FSWAP(IBUF(INX+1),0)
*           CALL FRCONV(IBUF(INX+1),LOCAL,ICER)
*           INFU(30,LW) = IBUF(INX+18)
*         file was changed!?
*         forget the out record ....
#else /* Linux */
*           file was changed
*           forget the out record
#endif /* Linux */
            INDW = INDXB(IBUF,1000*LUNW+2)
            IF( INDW.GT.0 ) IBUF(INDW+2) = 2
            INFU(IWSTU+8,LW) = 0
            INFU(IWSTU+4,LW) = 0
#ifndef Linux
*
*           Only the top level block (in memory) is reliable
            INFU(25,LR) = 1
*           Rebuild PATH
            CALL FPFIND(0,IFOUND)
*      PRINT *,'FWKEY. IFOUND=',IFOUND,' INFU(30)=',INFU(30,LW)
*
*         try to restore the new last segment from the end of file ...
*           INFU(30,LW) - new last block in file
*           Read last block of file
            ISV27 = INFU(27,LR)
            INFU(27,LR) = INFU(30,LW)
            CALL FRRECX(LUNR,IBUF(INDW),IBUF(INDW+1),IRC)
            INFU(27,LR) = ISV27
            IF(IRC.LT.0) CALL LOGERROR('FWKEY. Wrong INFU(30,LW)')
            IF(IRC.GT.0) GOTO 98
            CALL FSWAP(IBUF(INDW+1),0)
            CALL FRCONV(IBUF(INDW+1),LOCAL,ICER)
*
*           if this is the index block or the block is full ...
            IF(IBUF(INDW+6).EQ.IIND(1).AND.
     +          IBUF(INDW+7).EQ.IIND(2) .OR.
     +           IBUF(INDW+1).EQ.IBUF(INDW+2)) THEN
*               ...  write to the next block
               IBUF(INDW+2) = 2
               INFU(26,LW)  = INFU(30,LW)+1
            ELSE
*              ... last block is not full yet ...
*              ... find the last segment
               IP = 2
40             IPN = IP + 11 + IBUF(INDW+IP+11)
               IF(IPN.EQ.IBUF(INDW+2)) GOTO 50
               IP = IPN
               GOTO 40
50             INFU(IWSTU+8,LW) = IP
               INFU(26,LW) = INFU(30,LW)
#endif /* not Linux */
            ENDIF
#ifndef Linux
*
#endif /* not Linux */
         ENDIF
#ifndef Linux
*---------------------------------------------------------------------
*   INFU(ISGEN +...,LW)    ORDERED ACCESS
*                11     - 'update in place' (use +3-buffer)
*
*   INFU(IWSTU +...,LW)
*                15     - blk# of first extent of new version
*                16     - offset -"-
*                17     - current block# when overwriting old segment
*                18     - IP -"-
*                19     - JP -"-
*                20     - KP -"-
*                21     - version# of a new version of logical record
*                22     - blk# of alternative version
*                23     - offset -"-
*                24     - blk# of first new segment
*                25     - offset -"-
*---------------------------------------------------------------------
*        Initial version# for new log. record
         INFU(IWSTU+21,LW) = 1
*        Find the (NAMKEY,NUM1,NUM2) key in index directory
         CALL FDTKEY(NAMKEY,NUM1,NUM2,IBLK,IOFF,IFOUND)
*      PRINT *,'FWKEY. IBLK,IOFF,IFOUND',IBLK,IOFF,IFOUND
         IF(IFOUND.EQ.1) THEN
*           KEY found in index directory & index item is referenced to
            INFU(IWSTU+22,LW) = IBLK
            INFU(IWSTU+23,LW) = IOFF
*           Check if a previous (alternative) version of this log. record
*           exists. (Now it's not accessible through the indexes and
*           invisible for reading processes).
            CALL FFPREV(IBLK,IOFF,IVERS,JBLK,JOFF)
*      PRINT *,'FWKEY. FFPREV->IBLK,IOFF,IVERS,JBLK,JOFF',
*     +IBLK,IOFF,IVERS,JBLK,JOFF
*           IVERS - current version# of active data record
*           IVERS = 0 -
*           Prepare new IVERS for new version
            IF(IVERS.GT.1000000) THEN
               IVERS = 1
            ELSE
               IVERS = IVERS+1
            ENDIF
*           save new IVERS for future
            INFU(IWSTU+21,LW) = IVERS
            IF(JBLK.NE.0.AND.JOFF.NE.0) THEN
*              'old' extents exist, so we will overwrite old contents
               INDW = INDXB(IBUF,1000*LUNW+2)
               IF(INDW.LE.0) GOTO 99
*              We will use +2 write buffer therefore ...
*              ... save block# of end-of-file
               INFU(ISGEN+11,LW) = INFU(26,LW)
*              save address of first extent
               INFU(IWSTU+15,LW) = JBLK
               INFU(IWSTU+16,LW) = JOFF
*              set address of current extent. This address will be changed
*              when going from current extent to next one.
               INFU(IWSTU+17,LW) = JBLK
               INFU(IWSTU+18,LW) = JOFF
               IWRK1 = INDXB(IBUF,1000*LUNW+3)
               IF(IWRK1.LE.0) THEN
                  IWRK1 = INDXA(IBUF,1000*LUNW+3,INFU(1,LW)/4)
                  IF(IWRK1.LE.0) GOTO 97
               ENDIF
*              if buffer already contains what we wanted ...
               IF(INFU(IWSTU+17,LW).EQ.INFU(ISGEN+11,LW)) THEN
*                 ... do nothing
               ELSE
*                 save old contents
                  CALL UCOPY(IBUF(INDW+1),IBUF(IWRK1+1),IBUF(INDW+2))
*                 ... load wanted block to buffer
                  ISV27 = INFU(27,LR)
                  INFU(27,LR) = INFU(IWSTU+17,LW)
                  CALL FRRECX(LUNR,IBUF(INDW),IBUF(INDW+1),IER)
                  INFU(27,LR) = ISV27
                  IF(IER.NE.0) GOTO 98
                  CALL FSWAP(IBUF(INDW+1),0)
                  CALL FRCONV(IBUF(INDW+1),LOCAL,ICER)
               ENDIF
            ENDIF
         ENDIF
         LR = ISVLR
         LUNR = ISVLUNR
      ENDIF
*
#endif /* not Linux */
      CALL FWKEX(NAMKEY,NUM1,NUM2,NDATWD)
#ifndef Linux
      GOTO 100
*
  91  PRINT *,' '
      PRINT *,'**************  STOP MESSAGE ***********************'
      PRINT *,'I was trying to lock for you the ordered file many'
      PRINT *,'times without success. It is likely another unknown'
      PRINT *,'writing process has locked it before you and keeps'
      PRINT *,'locking it for a long time. I need to stop your job'
      PRINT *,'to avoid destroying of the ordered file''s structure.'
      PRINT *,'Try again a bit later. Sorry for inconveniences,'
      PRINT *,' '
      PRINT *,'                  FWKEY subr., FPACK'
      PRINT *,'****************************************************'
      STOP 777
*
  97  CALL LOGERROR('FWKEY. No space for +3 buffer')
  98  CALL LOGERROR('FWKEY. Ordered file corrupted')
  99  CALL LOGERROR('FWKEY. Write buffer (+2) not found')
#endif /* not Linux */
*
#ifndef Linux
  100 CONTINUE
*      PRINT *,'INFU(IWSTU+...,LW)->'
*      DO 1000 I=1,25,5
*1000      PRINT *,(INFU(IWSTU+I+(J-1),LW),J=1,5)
*      PRINT *,'-------------FWKEY END -----------------'
      RETURN
#else /* Linux */
  100 RETURN
#endif /* Linux */
      END
