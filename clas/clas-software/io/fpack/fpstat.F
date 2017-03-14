*CMZ :  1.00/00 16/12/94  17.00.00  by  Pavel Binko
*-- Author :    Pavel Binko   16/12/94
      SUBROUTINE FPSTAT
*
************************************************************************
*
*     Prints input/output statistics of all files opened with FPACK
*
************************************************************************
*
      CHARACTER DANAME*16, FINAME*256, HONAME*127
      CHARACTER*11 ACCESS(5), FORM(2), ACTION(4)
      DATA ACCESS / ' sequential', '     direct', '      keyed',
     +              '    special', '    ordered' /
      DATA   FORM / '     binary', '       text' /
      DATA ACTION / '       read', '      write',
     +              '  readwrite', '     modify' /
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
*KEND.
      INTEGER LUNPAR(ILPMAX)
*
      WRITE(*,*)
      WRITE(*,*)
      WRITE(*,*)
      WRITE(*,*) '**************************************************'
     +          //'*********************'
      WRITE(*,*) '*                                                 '
     +          //'                    *'
      WRITE(*,*) '*         I N P U T / O U T P U T     S T A T I S '
     +          //'T I C S             *'
      WRITE(*,*) '*                                                 '
     +          //'                    *'
      WRITE(*,*) '**************************************************'
     +          //'*********************'
      WRITE(*,*)
*
      DO 90 I=1,NUNITS
*
      DANAME=DATNAM(I)
*
      IF(DANAME.EQ.' ') GOTO 90
      IF(DANAME.EQ.'SYSIN') GOTO 90
      IF(DANAME.EQ.'SYSOUT') GOTO 90
*
      CALL FQFILE(DANAME,FINAME,HONAME,LUNPAR,IERR)
*
      WRITE(*,*) 'Data name .......... ',DANAME(1:LENB(DANAME))
      IF(FINAME.EQ.' ') THEN
         WRITE(*,*) 'File name .......... ','---'
      ELSE
         LFINAM = LENB(FINAME)
         CALL FPHIDE(FINAME,LFINAM)
         WRITE(*,*) 'File name .......... ',FINAME(1:LFINAM)
      END IF
      IF(HONAME.EQ.' ') THEN
         WRITE(*,*) 'Host name .......... ','---'
      ELSE
         WRITE(*,*) 'Host name .......... ',HONAME(1:LENB(HONAME))
      END IF
      WRITE(*,*)
      WRITE(*,*) 'Unit number ......................................'
     +          //'......... ',LUNPAR(1)
      WRITE(*,*) 'Access ...........................................'
     +          //'......... ',ACCESS(LUNPAR(2))
      WRITE(*,*) 'Form .............................................'
     +          //'......... ',FORM(LUNPAR(3))
      WRITE(*,*) 'Action ...........................................'
     +          //'......... ',ACTION(LUNPAR(4))
      WRITE(*,*) 'Physical record length in bytes ..................'
     +          //'......... ',LUNPAR(5)
      WRITE(*,*)
      IF(LUNPAR(6).EQ.0) GOTO 7
      WRITE(*,*) 'Number of physical records read ..................'
     +          //'......... ',LUNPAR(6)
    7 IF(LUNPAR(7).EQ.0) GOTO 8
      WRITE(*,*) 'Number of physical records written ...............'
     +          //'......... ',LUNPAR(7)
    8 IF(LUNPAR(8).EQ.0) GOTO 9
      WRITE(*,*) 'Number of logical records read ...................'
     +          //'......... ',LUNPAR(8)
    9 IF(LUNPAR(9).EQ.0) GOTO 10
      WRITE(*,*) 'Number of logical records written.................'
     +          //'......... ',LUNPAR(9)
   10 WRITE(*,*)
      IF(LUNPAR(10).EQ.0) GOTO 11
      WRITE(*,*) 'Number of Kbytes read ............................'
     +          //'......... ',LUNPAR(10)
   11 IF(LUNPAR(11).EQ.0) GOTO 14
      WRITE(*,*) 'Number of Kbytes written .........................'
     +          //'......... ',LUNPAR(11)
   14 WRITE(*,*)
      IF(LUNPAR(14).EQ.0) GOTO 15
      WRITE(*,*) 'Average length of logical record on input ........'
     +          //'......... ',LUNPAR(14)
   15 IF(LUNPAR(15).EQ.0) GOTO 16
      WRITE(*,*) 'Average length of logical record written .........'
     +          //'......... ',LUNPAR(15)
   16 IF(LUNPAR(16).EQ.0) GOTO 17
      WRITE(*,*) 'Number of data blocks read .......................'
     +          //'......... ',LUNPAR(16)
   17 IF(LUNPAR(17).EQ.0) GOTO 18
      WRITE(*,*) 'Number of data blocks written ....................'
     +          //'......... ',LUNPAR(17)
   18 IF(LUNPAR(18).EQ.0) GOTO 80
      WRITE(*,*) 'Number of data blocks written without format (B32 '
     +          //'added) .. ',LUNPAR(18)
   80 WRITE(*,*) '--------------------------------------------------'
     +          //'---------------------'
*
   90 CONTINUE
*
      WRITE(*,*)
      WRITE(*,*)
*
  100 RETURN
      END
