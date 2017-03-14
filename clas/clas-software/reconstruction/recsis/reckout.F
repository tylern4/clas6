      SUBROUTINE RECKOUT
c
c_begin_doc
c  RCS ID string
c  $Id: reckout.F,v 1.2 1998/05/05 19:20:44 marki Exp $
c
c  Documentation for subroutine RECKOUT
c
c  Purpose:  Checks to see that an OUTPUT stream has been opened.  If not then
c  --------  prompts user for filename.
c
c  Calling Sequence:
c  ----------------
c
c  Input Parameters:  (Name - Type - Meaning)
c  ----------------
c
c  Output Parameters:  (Name - Type - Meaning)
c  -----------------
c
c  Called from:
c  ------------
c
c  Other routines:
c  ---------------
c
c  Notes:
c  ------
c
c  Author:   Arne Freyberger      Created:  Mon Mar 20 12:03:16 EST 1995
c  -------
c
c  Major revisions:
c  ----------------
c     
c
c_end_doc
c
      IMPLICIT NONE
      SAVE
c
c_begin_inc
c  include files :
c  ---------------------
c BOS common block  uncomment the next line for BOS include file
#include "bcsparm.inc"
#include "bcs.inc"
#include "run_control.inc"
#include "re_units.inc"
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
      PARAMETER ( CFILE = '$RCSfile: reckout.F,v $')
      PARAMETER ( CREVIS = '$Revision: 1.2 $')
      PARAMETER ( CSTATE = '$State: Exp $')
      PARAMETER ( CDATE = '$Date: 1998/05/05 19:20:44 $')
      PARAMETER ( CAUTHO = '$Author: marki $')
      DATA CRCSID/
     1'$Id: reckout.F,v 1.2 1998/05/05 19:20:44 marki Exp $
     3'/
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='RECKOUT')
      PARAMETER (CRAUTH='Arne Freyberger')
c
c  Local User defined variables:
c  -----------------------------
      INTEGER ICFNBL, LNBLNK, LENOCC, INDEXA
      EXTERNAL ICFNBL, LNBLNK, LENOCC, INDEXA
c
      CHARACTER*200 FNAME,FPKCMD, CTEMP
      INTEGER IRET, ICEND, ICEND2, ICBEG, ITRY
c_end_var
c
c  executable code for routine RECKOUT:
c  -------------------------------------
      itry = 0
c
c   Check if unit 2 (FPACKOUT) has been opened (this is the unit FPACK uses)
c
      CALL CHKLUN(FPACKOUT,IRET)
      IF (IRET .EQ. -2) RETURN
 10   ITRY = ITRY + 1
c
c   If you make to here, it means that FPACK has not opened an OUTPUT file yet
c   so prompt the user for a file name a construct the FPACK open command
c   and send it vis FPARM
c
c   pick the appropriate file name: arg_rec_output  from the command line
c   overrides rec_output from the tcl script
c
      IF (arg_rec_output .eq. 'NoNe') then
       CRMESS='NO output file will be opened '
       CALL RECMES(CRNAME,'I',CRMESS)
       RETURN
      elseif (indexa(arg_rec_output) .ne. 0 .and. itry .eq. 1) then
       fname = arg_rec_output
       rec_output = arg_rec_output
      elseif (indexa(rec_output) .ne. 0 .and. itry .eq. 1) then
       fname = rec_output
      else
       CRMESS = '-----No DATA OUTPUT STREAM found----'
       CALL RECMES(CRNAME,'W',CRMESS)
       call recmes(crname,'n','|')
       call recmes(crname,'n',
     1 'define the output file by: setc rec_output myoutput.evt')
       call recmes(crname,'n','|')
c       lpause = .true.
       return
      ENDIF
C
      ICEND = LENOCC(FNAME)
      IF (ICEND .GT. 0) THEN
       ICBEG = ICFNBL(FNAME(1:ICEND),1,ICEND)
       CTEMP = FCLASOUT
       ICEND2 = LENOCC(CTEMP)
       WRITE(FPKCMD,20)CTEMP(1:ICEND2),FPACKOUT,FNAME(ICBEG:ICEND)
20     FORMAT('OPEN ',A,' UNIT=',I2.2,' FILE="',A,
     1       '" WRITE RECL=36000 STATUS=NEW')
       CALL RECMES(CRNAME,'N',FPKCMD)
c
c  Now send it to FPACK
c
       CALL FPARM(FPKCMD)
      ENDIF
c
c
c  Check again
c
       CALL CHKLUN(FPACKIN,IRET)
       IF (IRET .EQ. -2) RETURN
       IF (ITRY .LE. 5) GOTO 10
       CRMESS = 'No OUTPUT data after 5 attempts'
       CALL RECMES(CRNAME,'W',CRMESS)
       CRMESS='NO output file will be opened '
       CALL RECMES(CRNAME,'W',CRMESS)
      RETURN
      END
c
c------------------------------------------------------------------------------




