*CMZ :  1.00/00 16/12/94  17.00.00  by  Pavel Binko
*-- Author :    Volker Blobel   16/12/94
      SUBROUTINE FCOPY(DAIN,DAOUT,NREC,IFLG)
*
      CHARACTER*(*) DAIN,DAOUT
*
************************************************************************
*
*     Copy NREC logical records from DAIN into DAOUT.
*     If NREC=0, the whole file DAIN will be copied into DAOUT.
*     Returned flag IFLG counts the number of errors.
*
************************************************************************
*
      PARAMETER (NDIM=4000)
      INTEGER DATA(NDIM)
*
      CHARACTER NAMKEY*8, NAMDAT*8, FORMAT*180, OUFILE*60, HOST*40
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
      INTEGER LUNPAR(ILPMAX)
*
      NN=0
      CALL FQCOND('PRINT',IPFLAG)
      IF(IPFLAG.GE.2) THEN
         WRITE(*,101) DAIN,DAOUT,NREC
  101    FORMAT('0CALL FCOPY(',A16,20X,'! input  file '/
     +                     12X,A16,20X,'! output file '/
     +                     12X,I16,20X,'! number of records'/
     +                22X,'IFLAG)',20X,'! return flag')
      END IF
*
      IFLG=0
      NUMREC = NREC
      IF(NUMREC.EQ.0) NUMREC = 99999999
*
*     save read and write units ...
      LRSAVE=LR
      LWSAVE=LW
*
*     ... and define new units
      CALL FRNAME(DAIN)
      CALL FWNAME(DAOUT)
      IF( LR.EQ.0 .OR. LW.EQ.0 ) THEN
         IF(IPFLAG.GE.0) THEN
            WRITE(*,*) ' *******************************************'
            WRITE(*,*) ' '
            IF(LR.EQ.0)
     +         WRITE(*,*) ' FCOPY. Error: Input file not opened'
            IF(LW.EQ.0)
     +         WRITE(*,*) ' FCOPY. Error: Output file not opened'
            WRITE(*,*) ' '
            WRITE(*,*) ' *******************************************'
         END IF
         GOTO 80
      END IF
*
* check for illegal writing to database file
      CALL FQFILE(DAOUT,OUFILE,HOST,LUNPAR,IERFQ)
      CALL UCASE(OUFILE)
      IF( INDEX(OUFILE,'BASE').NE.0 ) THEN
         WRITE(*,*) ' *******************************************'
         WRITE(*,*) ' '
         WRITE(*,*) ' to avoid possible database clashes FCOPY',
     +              ' does NOT write to any file with name "*BASE*"'
         WRITE(*,*) ' '
         WRITE(*,*) ' *******************************************'
         GOTO 80
      END IF
*
      IND1 = INDXB(IBUF,1000*LUNBK(LR)+100)
      IND2 = INDXB(IBUF,1000*LUNBK(LW)+100)
      IF(NREC.LT.0.AND.INFU(2,LR).NE.5.AND.INFU(2,LW).NE.5
     +   .AND.IND1.EQ.0.AND.IND2.EQ.0 ) THEN
         CALL FCOP11(NN,IFLG)
         GOTO 80
      END IF
*
      DO 60 IREC=1,NUMREC
      IF(MOD(IREC,3).EQ.0) THEN
         CALL FQCOND('TIME',ITFLAG)
         IF(ITFLAG.NE.0) THEN
            IF(IPFLAG.GE.0) THEN
               WRITE(*,*) ' -------------------------------------------'
               WRITE(*,*) ' '
               WRITE(*,*) ' FCOPY. Stop - time limit reached'
               WRITE(*,*) ' '
               WRITE(*,*) ' -------------------------------------------'
            END IF
            GOTO 70
         END IF
      END IF
*
*     start of copy loop -----------------------------------
*
*     key
   10 CALL FRKEY2(NAMKEY,NUM1,NUM2,NDATWD,IRER)
      IF(IRER.LT.0) THEN
         IF(IPFLAG.GE.2) WRITE(*,104) NN
  104    FORMAT(10X,'End_of_file on input after',I7,' Records'/)
         GOTO 70
      END IF
      IF(IRER.GT.0) GOTO 65
      NN=NN+1
      CALL FWKEY(NAMKEY,NUM1,NUM2,NDATWD)
*
*     data header
   20 CALL FRHDR(NAMDAT,NR,NCOL,NROW,FORMAT,NF,IRER)
      IF(IRER.LT.0) GOTO 40
      IF(IRER.GT.0) GOTO 10
      CALL FWHDR(NAMDAT,NR,NCOL,NROW,FORMAT(1:NF))
*
*     data
   30 CALL FRDAT(NDATA,DATA,NDIM)
      CALL FWDAT(NDATA,DATA)
      IF(NDATA.EQ.NDIM) GOTO 30
      GOTO 20
   40 CALL FWEND(IWER)
*
   60 CONTINUE
      GOTO 70
*
*     error
   65 IFLG=IRER
      WRITE(6,*) ' ERROR in reading, FILECOPY stopped. Ret. code =',IFLG
*
   70 CALL FWEOD
*
*     end of copy loop -------------------------------------
*
   80 IF(IPFLAG.GE.2) THEN
         IF(NREC.GE.0) THEN
            WRITE(*,103) NN,'Logical records copied'
         ELSE
            WRITE(*,103) NN,'Physical records copied'
         END IF
  103    FORMAT('0',I16,1X,A)
         CALL FPRFLS
      END IF
*
      LR=LRSAVE
      LW=LWSAVE
*
      RETURN
      END
