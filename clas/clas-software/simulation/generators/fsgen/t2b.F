      SUBROUTINE T2B(FILENAME)
c
c_begin_doc
c  RCS ID string
c  $Id: t2b.F,v 1.1.1.1 2006/04/18 21:08:11 stepanya Exp $
c
c  Documentation for subroutine TXT2BOS
c
c  Purpose: reads in an ascii file (FILENAME) and converts that info into
c  -------- BOS bank definition.
c
c  Input Parameters:  FILENAME :C*(*): ascii file contain BOS bank definitions
c  ----------------   Format is as follows:
c anything following a `*' or a `!' is considered a comment
c   *   this is a comment, the following is an example bank definition
c   *   leading blanks are ignored
C   * the first line should contain a 4 character  NAME followed by
c   * the name of the bank and ACTION word (CREATE, WRITE, DELETE, MODIFY)
c   *  TABLE BANKname fmt  ACTION1 ACTION2
c   * The NAME line is followed by a row ordered list of format statements
c   * as defined in page 10 of the BOS manual, for example
c   *           * run and event number 
c   *           * 2 group of 3 floating points
c   * 
c   * ACTION = CREATE implies BKFMT will be called
c   * ACTION = WRITE implies BLIST(IW,'E+','bname') will be called,
c   *          absence of WRITE implies BLIST(IW,'E-','bname') will be called
c   * ACTION = DELETE implies BLIST(IW,'R+','bname') will be called
c   * ACTION = MODIFY implies THIS definition should OVERRIDE definition in
c   *                 datafile
c   * ACTION = DISPLAY implies the bank will be `shipped' to the event store
c              buffer so that CED can display it
c
c  Output Parameters:
c  -----------------
c
c  Other routines:
c  ---------------
c
c  Notes:
c  ------
c
c  Author:   Arne Freyberger      Created:  Fri Oct  6 15:21:23 EDT 1995
c  -------
c
c  Major revisions:
c  ----------------
c     
c
c_end_doc
c
      IMPLICIT NONE
c
c_begin_inc
c  include files :
c  ---------------------
c BOS common block  uncomment the next line for BOS include file
#include "bcs.inc"
#include "bnkfmt.inc"
c                           CLAS control module
#include "clasmdl.inc"
c_end_inc
c
c_begin_var
c  input/output variables:
c  -----------------------
c
c  Local pre-defined variables:
c  ---------------------------
c  RCS information: 
      CHARACTER*132  CFILE, CREVIS, CSTATE, CDATE, CAUTHO, CRCSID
      PARAMETER (CFILE = '$rcsRCSfile$')
      PARAMETER (CREVIS = '$rcsRevision$')
      PARAMETER (CSTATE = '$rcsState$')
      PARAMETER (CDATE = '$rcsDate$')
      PARAMETER (CAUTHO = '$rcsAuthor$')
      DATA CRCSID/   
     1'$Id: t2b.F,v 1.1.1.1 2006/04/18 21:08:11 stepanya Exp $'   
     2/   
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='TXT2BOS')
      PARAMETER (CRAUTH='Arne Freyberger')
c
c  Local User defined variables:
c  -----------------------------
      INTEGER   INDEXA, INDEXN
      EXTERNAL  INDEXA, INDEXN
      INTEGER ICLOC,  ISCAN, lenocc
      EXTERNAL ICLOC, ISCAN, lenocc
      character*132 spaces, strip
      external spaces, strip
c
      CHARACTER*(*) FILENAME
      character*512 LINE
      CHARACTER*256 CNAME, CWORD, CTMP
      CHARACTER*400 BFORMAT
      CHARACTER*4 BNAME
      CHARACTER*20 CWORDS(40)
      character*128  ctest
      LOGICAL LCREATE, LMODIFY, LWRITE, LDELETE
      LOGICAL LDONE, LCLEAR, LDISPLAY, LINIT, LFORMAT_DONE
      INTEGER ILUN, IRET, NLINE
      INTEGER IN_BEG, IN_END, IFORMAT_BEG, IFORMAT_END, IBEG, IEND
      INTEGER IBEGA, IBEGN, IEND1, IEND2
      INTEGER WCNT, IWORDS, ICOLUMN, iretlen
c_end_var
      DATA LINIT/.FALSE./
      DATA BFORMAT/'                              '/
c
c  executable code for routine TXT2BOS:
c----6----------------------------------------------------------------72
c
      IF (.NOT. LINIT) THEN
        LINIT = .TRUE.
        IDELETE = 0
        IWRITE = 0
        IREMOVE = 0
        ICREATE = 0
      ENDIF

c open the ascii file
 
c      CALL RESLUN(CRNAME,ILUN,IRET)
c      CALL REOPEN(FILENAME,ILUN,'OLD',IRET)
      ilun=77
      open(unit=ilun,file=filename,status='OLD')
      if (iret .lt. 0) return
c 
      ICOLUMN = 0
c  do stuff with the file, read in line by line and 
c  parse each line into words

      LCLEAR = .TRUE.
10    CONTINUE
      READ(ILUN,11,ERR=999,END=999)LINE
11    FORMAT(A512)
      NLINE = NLINE + 1
      LINE = SPACES(LINE,1)
      IF (LINE(1:1) .EQ. '*' .OR. 
     1    LINE(1:1) .EQ. '!') GOTO 10

c  convert to upper case

      CALL CLTOU(LINE(1:256))

c construct the BOS bank, first break the line into words
c separated by blanks

      IEND1 = ICLOC('*',1,LINE,1,256)
      IEND2 = ICLOC('!',1,LINE,1,256)
      IEND = 256
      IF (IEND1*IEND2 .NE. 0) THEN
       IEND = MIN(IEND1,IEND2)
      ELSE
       IEND = MAX(IEND1,IEND2)
      ENDIF
      IF (IEND .EQ. 0) GOTO 10
      IBEGA = INDEXA(LINE(1:IEND))
      IBEGN = INDEXN(LINE(1:IEND))
      IF (IBEGN*IBEGA .EQ. 0) THEN
        IBEG = MAX(IBEGA,IBEGN)
      ELSE
        IBEG = MIN(IBEGA,IBEGN)
      ENDIF
      if (ibeg .eq. 0) goto 10
      IF (IEND .EQ. 0) IEND = 256
c
c  separate the line into words then loop over them
c
      CALL REWORD(LINE,IWORDS,CWORDS)
      IF (IWORDS .LE. 1) GOTO 10
      CWORD = CWORDS(1)
      IF (CWORD(1:1) .EQ. '!' .OR. CWORD(1:1) .EQ. '*') GOTO 10
c
c  look for TABLE as the START of the bank definition
c
      IF (CWORD(1:2) .EQ. 'TA') THEN 
        IF (.NOT. LCLEAR .and. IFORMAT_END .gt. 1) THEN
           IN_END = IFORMAT_END - 1
           iretlen = 1
           call fmcre(bformat(1:IN_END+2), ctest, iretlen)
c           IF (LCREATE) THEN
               if (iretlen.LT.6) then
                  CALL BKFMT(BNAME,BFORMAT(1:IN_END))
               else
                  CALL BKFMT(BNAME,ctest(1:iretlen))
               endif
             ICREATE = ICREATE + 1
             CREATE_NAME(ICREATE) = BNAME
             CREATE_FORMAT(ICREATE) = BFORMAT(1:IN_END)
             NCREATE_COL(ICREATE) = ICOLUMN
c           ENDIF
           IF (LWRITE) THEN
              CALL BLIST(IW,'E+',BNAME)
              IWRITE = IWRITE + 1
              CBANK_WRITE(1+4*(IWRITE-1):4+4*(IWRITE-1)) = BNAME
           ELSE
              CALL BLIST(IW,'E-',BNAME)
              IREMOVE = IREMOVE + 1
              CBANK_REMOVE(1+4*(IREMOVE-1):4+4*(IREMOVE-1)) = BNAME
           ENDIF
           IF (LDELETE) THEN 
              CALL BLIST(IW,'R+',BNAME)
              IDELETE = IDELETE + 1
              CBANK_DELETE(1+4*(IDELETE-1):4+4*(IDELETE-1)) = BNAME
           ENDIF
           IF (LDISPLAY) THEN
            NBANK_DISPLAY = NBANK_DISPLAY + 1
            CBANK_DISPLAY(NBANK_DISPLAY) = BNAME
            CBANK_FORMAT(NBANK_DISPLAY) = BFORMAT(1:IN_END)
           ENDIF
           LDONE = .FALSE.
           LFORMAT_DONE = .FALSE.
           IFORMAT_END = 0
           ICOLUMN = 0
           BNAME = '    '
           BFORMAT = '                '
        ENDIF

c  Done writting out previous bank now start over and initialize logicals

        LCLEAR = .FALSE.
        LCREATE = .FALSE.
        LDELETE = .FALSE.
        LWRITE = .FALSE.
        LMODIFY = .FALSE.
        LDISPLAY = .FALSE.
        LFORMAT_DONE = .FALSE.

c BANK name is the SECOND word!!

        BNAME = CWORDS(2)
c
        IF (BNAME .NE. '    ') THEN
c
        DO 23 WCNT=3,IWORDS
         CNAME = cwords(wcnt)
         IF (CNAME(1:3) .EQ. 'B16' .OR.
     1       CNAME(1:3) .EQ. 'B08' .OR.
     2       CNAME(1:3) .EQ. 'B32') THEN 
          BFORMAT = CNAME
          IFORMAT_END = LENOCC(BFORMAT) + 1
          LFORMAT_DONE = .TRUE.
         ELSEIF (CNAME(1:6) .EQ. 'CREATE') THEN
          LCREATE = .TRUE.
         ELSEIF (CNAME(1:5) .EQ. 'WRITE') THEN
          LWRITE = .TRUE.
         ELSEIF (CNAME(1:6) .EQ. 'DELETE') THEN
          LDELETE = .TRUE.
         ELSEIF (CNAME(1:6) .EQ. 'MODIFY') THEN
          LMODIFY = .TRUE.
         ELSEIF (CNAME(1:7) .EQ. 'DISPLAY') THEN
          LDISPLAY = .TRUE.
         ENDIF
23      CONTINUE
        ENDIF

c   check for END or END TABLE presence

      ELSEIF (CWORD(1:3) .EQ. 'END' .AND. BNAME .NE. '    ') THEN
          LDONE = .TRUE.
c
c  the following lines parse the FORMAT lines (not a TAble or END line)
c
      ELSEIF (BNAME .NE. '    ') THEN
         IFORMAT_BEG = IFORMAT_END + 1
         CNAME = CWORDS(3)
         IN_END = ISCAN(CNAME,' ') - 1
         if (in_end .eq. 0) goto 10
c
c  reject relational formats
c
         IF (CNAME(1:1) .EQ. 'D' .OR.
     1       CNAME(1:1) .EQ. 'R' .OR.
     2       CNAME(1:1) .EQ. 'M') GOTO 10
         ICOLUMN = ICOLUMN + 1
         ctmp = cwords(2)
         create_elements(icreate+1,icolumn)= ctmp(1:8)
         if (.not. lformat_done) then
          IN_BEG = 1
          IFORMAT_END = IFORMAT_BEG + IN_END - 1
          BFORMAT(IFORMAT_BEG:IFORMAT_END) = CNAME(1:IN_END)
          IFORMAT_END = IFORMAT_END + 1
          BFORMAT(IFORMAT_END:IFORMAT_END) = ','
         endif
       ENDIF
c
       IF (LDONE .AND. BNAME .NE. '    ') THEN
           IN_END = IFORMAT_END - 1
           CALL BKFMT(BNAME,BFORMAT(1:IN_END))
           ICREATE = ICREATE + 1
           CREATE_NAME(ICREATE) = BNAME
           CREATE_FORMAT(ICREATE) = BFORMAT(1:IN_END)
           NCREATE_COL(ICREATE) = ICOLUMN

           IF (LWRITE) THEN
              CALL BLIST(IW,'E+',BNAME)
              IWRITE = IWRITE + 1
              CBANK_WRITE(1+4*(IWRITE-1):4+4*(IWRITE-1)) = BNAME
           ELSE
              CALL BLIST(IW,'E-',BNAME)
              IREMOVE = IREMOVE + 1
              CBANK_REMOVE(1+4*(IREMOVE-1):4+4*(IREMOVE-1)) = BNAME
           ENDIF
           IF (LDELETE) THEN 
              CALL BLIST(IW,'R+',BNAME)
              IDELETE = IDELETE + 1
              CBANK_DELETE(1+4*(IDELETE-1):4+4*(IDELETE-1)) = BNAME
           ENDIF
           IF (LDISPLAY) THEN
            NBANK_DISPLAY = NBANK_DISPLAY + 1
            CBANK_DISPLAY(NBANK_DISPLAY) = BNAME
            CBANK_FORMAT(NBANK_DISPLAY) = BFORMAT(1:IN_END)
           ENDIF
           iretlen = 1
           call fmcre(bformat, ctest, iretlen)
           LDONE = .FALSE.
           LFORMAT_DONE = .FALSE.
           LCLEAR = .TRUE.
           IFORMAT_END = 0
           ICOLUMN = 0
           BNAME = '    '
           BFORMAT = '            '
       ENDIF
      GOTO 10
c
c  but format last bank before exiting
c
999   CONTINUE
      if (.not. lclear) then
       iretlen = 1
       call fmcre(bformat, ctest, iretlen)
       if (iretlen.LT.6) then
          CALL BKFMT(BNAME,BFORMAT(1:IFORMAT_END-1))
       else
          CALL BKFMT(BNAME,ctest(1:iretlen))
       endif
       ICREATE = ICREATE + 1
       CREATE_NAME(ICREATE) = BNAME
       CREATE_FORMAT(ICREATE) = BFORMAT(1:IFORMAT_END-1)
       NCREATE_COL(ICREATE) = ICOLUMN

       IF (LWRITE) THEN
           CALL BLIST(IW,'E+',BNAME)
           IWRITE = IWRITE + 1
           CBANK_WRITE(1+4*(IWRITE-1):4+4*(IWRITE-1)) = BNAME
       ELSE
           CALL BLIST(IW,'E-',BNAME)
           IREMOVE = IREMOVE + 1
           CBANK_REMOVE(1+4*(IREMOVE-1):4+4*(IREMOVE-1)) = BNAME
       ENDIF
       IF (LDELETE) THEN 
           CALL BLIST(IW,'R+',BNAME)
           IDELETE = IDELETE + 1
           CBANK_DELETE(1+4*(IDELETE-1):4+4*(IDELETE-1)) = BNAME
       ENDIF
       IF (LDISPLAY) THEN
         NBANK_DISPLAY = NBANK_DISPLAY + 1
         CBANK_DISPLAY(NBANK_DISPLAY) = BNAME
         CBANK_FORMAT(NBANK_DISPLAY) = BFORMAT(1:IN_END)
       ENDIF
       IFORMAT_END = 0
      endif
c
      CLOSE(ILUN)
      RETURN
      END
c
c------------------------------------------------------------------------------


