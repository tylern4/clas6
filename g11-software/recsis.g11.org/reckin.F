      SUBROUTINE RECKIN
c
c_begin_doc
c  RCS ID string
c  $Id: reckin.F,v 1.10 1999/07/02 01:43:19 gilfoyle Exp $
c
c  Documentation for subroutine RECKIN
c
c  Purpose: Checks to see that an INPUT stream has been opened.  If not then
c  -------- prompts user for filename.
c
c  Calling Sequence:
c  ----------------
c
c  Input Parameters:   NONE
c  ----------------
c
c  Output Parameters:  NONE
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
c  Author:   Arne Freyberger      Created:  Thu Mar 16 13:56:04 EST 1995
c  -------
c
c  Major revisions:  Modified to read data from the DD ring in
c  ----------------  addition to reading from a file.
c                                          gilfoyle 7/97
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
#include "et.inc"
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
      PARAMETER ( CFILE = '$RCSfile: reckin.F,v $')
      PARAMETER ( CREVIS = '$Revision: 1.10 $')
      PARAMETER ( CSTATE = '$State: Exp $')
      PARAMETER ( CDATE = '$Date: 1999/07/02 01:43:19 $')
      PARAMETER ( CAUTHO = '$Author: gilfoyle $')
      DATA CRCSID/
     1'$Id: reckin.F,v 1.10 1999/07/02 01:43:19 gilfoyle Exp $'
     3/
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='RECKIN')
      PARAMETER (CRAUTH='Arne Freyberger')
c
c  Local User defined variables:
c  -----------------------------
      INTEGER ICFNBL, LNBLNK, LENOCC, INDEXA
      EXTERNAL ICFNBL, LNBLNK, LENOCC, INDEXA
      external status_poll_callback, quit_callback, control_callback
c 
      CHARACTER*256 FNAME,FPKCMD, CTEMP 
      CHARACTER*256 CIN_FILE
      CHARACTER*127 CIN_HOST
      INTEGER LIN_PAR(30)
      INTEGER IRET, ICEND, ICEND2, ICBEG
      INTEGER ITRY
c_end_var
c
c  executable code for routine RECKIN:
c  -------------------------------------
c
c first check to see if you 're reading from the dd ring
c or from a file.
c
      if (.not. read_et ) then
         ITRY = 0
c
c  use FPACK FQFILE routine to check status of CLASINPUT
c
         call FQFILE(FCLASIN,cin_file, cin_host, lin_par, iret)
         if (lin_par(4) .eq. 1) return

c   Check if unit 1 has been opened (this is the unit FPACK uses)
c
         CALL CHKLUN(FPACKIN,IRET)
         IF (IRET .EQ. -2) RETURN
 10      ITRY = ITRY + 1
c
c   If you make to here, it means that FPACK has not opened an INPUT file yet.
c   the user may have defined the input file in their tcl script so an
c   fpack command will be generated here to open the file. otherwise warn the 
c   user about this and set lpause to true.
c
         if (indexa(arg_rec_input) .ne. 0 .and. itry .eq. 1) then
            fname = arg_rec_input
            rec_input = arg_rec_input
         elseif (indexa(rec_input) .ne. 0 .and. itry .eq. 1) then
            fname = rec_input
         else
            CRMESS = '-----No DATA INPUT STREAM found----'
            CALL RECMES(CRNAME,'W',CRMESS)
            call recmes(crname,'n','|')
            call recmes(crname,'n',
     1           'define the input file by entering: inputfile myinput.evt')
            call recmes(crname,'n','|')
            lpause = .true.
            return
         endif

c if you made it here, input file name is held in "fname" form the FPACK
c open command and execute it

         ICEND = LENOCC(FNAME)
         ICBEG = ICFNBL(FNAME(1:ICEND),1,ICEND)
         CTEMP = FCLASIN
         ICEND2 = LNBLNK(CTEMP)
         WRITE(FPKCMD,20)CTEMP(1:ICEND2),FPACKIN,FNAME(ICBEG:ICEND)
 20      FORMAT('OPEN ',A,' UNIT=',I2.2,' FILE="',A,
     1        '" READ RECL=36000 STATUS=OLD')
         CALL RECMES(CRNAME,'N',FPKCMD)
c
c  Now send it to FPACK
c
         CALL FPARM(FPKCMD)
         CALL FPACK_MESS
c
c  Check again
c
         CALL CHKLUN(FPACKIN,IRET)
         IF (IRET .EQ. -2) RETURN
         IF (ITRY .LE. 5) GOTO 10
         CRMESS = 'No INPUT data after 5 attempts, do not see any '
         CALL RECMES(CRNAME,'W',CRMESS)
         CRMESS = 'reason to continue....exiting ciao baby! '
         CALL RECMES(CRNAME,'E',CRMESS)
      else
c
c get ready to read from the ET chain. 
c
         call re_et_init
      end if
      RETURN
      END
c
c------------------------------------------------------------------------------











