      SUBROUTINE REJOB_INFO
c
c_begin_doc
c  RCS ID string
c  $Id: rejob_info.F,v 1.2 1998/05/05 19:20:45 marki Exp $
c
c  Documentation for subroutine REJOB_INFO
c
c  Purpose: Fills the reversion common block with Job information.  This is
c  -------- then stored in a bos bank DBBK by routine REVERS
c
c  Calling Sequence:
c  ----------------
c
c  Input Parameters: None
c  ----------------
c
c  Output Parameters: None
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
c  Author:   Arne Freyberger      Created:  Tue Apr 11 17:04:16 EDT 1995
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
#include "reversion.inc"
c#include "fcommon.inc"
#include "run_control.inc"
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
      PARAMETER ( CFILE = '$RCSfile: rejob_info.F,v $')
      PARAMETER ( CREVIS = '$Revision: 1.2 $')
      PARAMETER ( CSTATE = '$State: Exp $')
      PARAMETER ( CDATE = '$Date: 1998/05/05 19:20:45 $')
      PARAMETER ( CAUTHO = '$Author: marki $')
      DATA CRCSID/
     1'$Id: rejob_info.F,v 1.2 1998/05/05 19:20:45 marki Exp $
     3'/
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
c      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='REJOB_INFO')
      PARAMETER (CRAUTH='Arne Freyberger')
c
c  Local User defined variables:
c  -----------------------------
      CHARACTER*8 CD,CT,CDUM
      CHARACTER*8 pgm_name
      CHARACTER*72 host_name
      CHARACTER*132 user_name
      CHARACTER*40 FNAME
c      CHARACTER*256 CIN_FILE
      CHARACTER*127 CIN_HOST
      INTEGER LIN_PAR(30), iret
      INTEGER I, IBEG, IEND, LENOCC, ICHK, ilen
      EXTERNAL LENOCC
c_end_var
c
c  executable code for routine REJOB_INFO:
c  -------------------------------------
c
c  Get the job date and time
c
      CALL DATIMH(CD,CT)
      JOBDATE(1:8) = CD
      JOBDATE(9:16) = CT
c
c  call cernlib to get the program name
c
      call getarg (0, pgm_name)
      JOBEXE(1:8) = pgm_name
c
c  call DNS to get the hostname
c
      call gethostname (host_name, ichk, 72)
      if (ichk .eq. 0) then
       JOBMACH  (1:8) = host_name(1:8)
c      else
c       call recmes(crname,'w',host_name)
      endif
c
c  get the user login name
c
      ilen = 8
c      call whoami(user_name)
c
c   the routine getlogin has an adverse interaction to LSF...that is it
c   causes a core dump.  For a replacement we will use the enviroment
c   variable LOGNAME.  
c      call getlogin (user_name, ichk, ilen)
      
      call getenv('LOGNAME',user_name)
      ichk = lenocc(user_name)
      jobusrnm(1:8) = 'clasuser'
clsf      if (ichk .eq. 0) JOBUSRNM(1:8) = user_name
      if (ichk .ne. 0) JOBUSRNM = user_name(1:ichk)
c
c  Get script name
c
c
c  get more stuff lots more stuff to be added
c
c
c   get initialization script
c
      jobscript = tcl_init
c
c  get the sequence number
c
      CALL GET_SEQ(NSEQ)
c
c  find input and output files

      call FQFILE(FCLASIN,inputdev, cin_host, lin_par, iret)
      call FQFILE(FCLASOUT,outputdev, cin_host, lin_par, iret)
c
c  truncate to last 40 characters
c
      IEND = LENOCC(INPUTDEV)
      IF (IEND .GE. 41) THEN
         IBEG = IEND - 40
         INPUTDEV(1:) = INPUTDEV(IBEG:)
      ENDIF
      IEND = LENOCC(OUTPUTDEV)
      IF (IEND .GE. 41) THEN
         IBEG = IEND - 40
         OUTPUTDEV(1:) = OUTPUTDEV(IBEG:)
      ENDIF
c
      RETURN
      END
c
c------------------------------------------------------------------------------




