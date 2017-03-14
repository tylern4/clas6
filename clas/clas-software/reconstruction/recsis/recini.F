      SUBROUTINE RECINI
c
c  Documentation for subroutine RECINI
c
c  Purpose: Initializes BCS common block and other FPACK variables.
c  -------- Reads in DATA cards.
c
c  Author:   Arne Freyberger      Created:  Tue Mar 14 16:25:18 EST 1995
c  -------
c
      IMPLICIT NONE
      SAVE
c
c  include files :
c  ---------------------
c BOS common block  uncomment the next line for BOS include file
#include "bcsparm.inc"
#include "bcs.inc"
#include "wcs.inc"
#include "run_control.inc"
#include "clasmdl.inc"
#include "re_units.inc"
c
c  Local pre-defined variables:
c  ---------------------------
c  RCS information: 
      CHARACTER*132  CRCSID 
      DATA CRCSID/
     1'$Id: recini.F,v 1.7 1998/05/05 19:20:44 marki Exp $'
     3/
c  Module information:
      CHARACTER*(*)  CRNAME
      CHARACTER*200  CRMESS, FPKCMD
      PARAMETER (CRNAME='RECINI')
c
c  Local User defined variables:
c  -----------------------------
 
      INTEGER LENOCC, ICFNBL, IARGC, INDEXA
      LOGICAL INTRAC
      EXTERNAL LENOCC, ICFNBL, IARGC, INDEXA, INTRAC
c
      INTEGER ILUN
      LOGICAL LEXIST
      INTEGER IJBNUM, ICEND, IRET, IQUIRE, ITRY
      CHARACTER*10 CTEMP
      CHARACTER*10 CPATH
      DATA CTEMP/'RENNNN.LOG'/
      DATA IJBNUM/0/
      data itry/0/
c
c  executable code for routine RECINI:
c  -------------------------------------
c
c   set messaging level
c
      CALL SET_LOG(.TRUE.)

c determine if interactive or batch

       LINTER = .FALSE.
       IF (INTRAC()) THEN
          LINTER = .TRUE.
       ENDIF
c
c  test of increasing bank limits
c
      call bnames(NBNAMES)
c
c  Initialize BOS as per page 22 of FPACK manual
c
      CALL BOS(IW,NBCS)
c     
c     Initialize BOS Working Common Space
c     
      CALL BOS(JW,NWBCS)
c     
c     
c     Save some time at end of job for end job processing 
c     (NSECSAV found in bcs.inc) and place a cap on the number of records
c     
      CALL FSEQP(NSECSAV,NRECLIM)
c     
c     Define header bank
c     
      CALL FSEQH(HEAD_NAME,HEAD_REC,HEAD_RUN,HEAD_EVT)
c     
c     Read in data cards ONLY if file is named on command line or initialization
c     script
c     
c     
      IF (INDEXA(ARG_FPK_INIT) .NE. 0) THEN
         FPK_FILE = ARG_FPK_INIT
      ENDIF
c     Before proceeding make sure file FNAME exists via INQUIRE
c     
      IF (INDEXA(FPK_FILE) .NE. 0) THEN
         INQUIRE(FILE=FPK_FILE, IOSTAT=IQUIRE, EXIST=LEXIST)
c     
         ICEND = LENOCC(FPK_FILE)
         IF (.NOT. LEXIST .AND. ITRY .LE. 5) THEN
            WRITE(CRMESS,99)FPK_FILE(1:ICEND)
 99         FORMAT('Control file :',A,' does not exist!!')
            CALL RECMES(CRNAME,'W',CRMESS)
            WRITE(CRMESS,98)
 98         FORMAT('Proceeding without one....')
            CALL RECMES(CRNAME,'I',CRMESS)
c     GOTO 9
         ELSEIF (.NOT. LEXIST .AND. ITRY .GE. 5) THEN
            WRITE(CRMESS,97)
 97         FORMAT('ABANDONING attempts to obtain control file,', 
     1           'proceeding without one')
            CALL RECMES(CRNAME,'W',CRMESS)
         ELSE
c     
            ILUN = 0
            CALL RESLUN(CRNAME,ILUN,IRET)
c     
            WRITE(FPKCMD,20)ILUN,FPK_FILE(1:ICEND)
 20         FORMAT('OPEN TXTINPUT UNIT=',I2.2,' FILE="',A,
     1           '" READ TEXT STATUS=OLD')
            CALL RECMES(CRNAME,'N',FPKCMD)
            CALL FPARM(FPKCMD)
            CALL FPARMR(ILUN)
            FPKCMD = 'CLOSE TXTINPUT'
            CALL RECMES(CRNAME,'N',FPKCMD)
            CALL FPARM(FPKCMD)
C     CALL RESLUN(CRNAME,-ILUN,IRET)
            CALL FUCLOS(ILUN)
         ENDIF
      ENDIF
      
c     Read in the ddl file that defines the bank definitions
c (read ddl file from CLAS_PACK/inc_derived/;
c  if environment var. CLAS_PACK not set, read from CLAS_PARMS)

      if (indexa(ddl_file) .eq. 0) then
         CALL GETENV('CLAS_PACK',CPATH)
         if (CPATH(1:1).EQ.' ') then
            call revinm('CLAS_PARMS','clasbanks.ddl',ddl_file)
         else
            call revinm('CLAS_PACK','bankdefs/clasbanks.ddl',ddl_file)
         endif
      endif
      call txt2bos(ddl_file)
c     
      RETURN
      END
c
c------------------------------------------------------------------------------




