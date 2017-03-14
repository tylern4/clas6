      SUBROUTINE RECARG
c
c_begin_doc
c  RCS ID string
c  $Id: recarg.F,v 1.7 1999/07/02 01:43:10 gilfoyle Exp $
c
c  Documentation for subroutine RECARG
c
c  Purpose:  this routines parses the command line arguments into the
c  --------  run_control common block
c
c  Input Parameters:  none
c  ----------------
c
c  Output Parameters: none
c  -----------------
c
c  Other routines:
c  ---------------
c
c  Notes:
c  ------
c
c  Author:   Arne Freyberger      Created:  Thu Feb 22 16:38:07 EST 1996
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
#include "run_control.inc"
#include "et.inc"
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
      PARAMETER (CFILE = '$RCSfile: recarg.F,v $')
      PARAMETER (CREVIS = '$Revision: 1.7 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 1999/07/02 01:43:10 $')
      PARAMETER (CAUTHO = '$Author: gilfoyle $')
      DATA CRCSID/   
     1'$Id: recarg.F,v 1.7 1999/07/02 01:43:10 gilfoyle Exp $'   
     2/   
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='RECARG')
      PARAMETER (CRAUTH='Arne Freyberger')
c
c  Local User defined variables:
c  -----------------------------
      INTEGER LENOCC, ICFNBL, IARGC, FIND_ARG
      EXTERNAL LENOCC, ICFNBL,  FIND_ARG
c
      LOGICAL LINFO, LWARN, LDEBUG, LERROR, LFATAL

      INTEGER NPAR,  arg_index, icend
c
c these following character strings are the ALLOWED command line arguments
c
      CHARACTER*(10) CTCL, CINPUT, COUTPUT, CFPACK, CNOOUTPUT,CNOFPACK,
     1   CHELP, CNOPAUSE, CBATCH, CAPP, CINFO_ON, CDEBUG_ON

      DATA CTCL,CINPUT,COUTPUT,CFPACK,CNOOUTPUT,CNOFPACK, CHELP, CAPP
     1                               /'t         ',  ! tcl file flag
     1                                'i         ',  ! input file flag
     2                                'o         ',  ! output file flag
     3                                'f         ',  ! fpack control file flag
     4                                'no        ',  ! no output file flag
     5                                'nf        ',  ! NO fpack control file
     6                                'h         ',  ! call help routine
     7                                'a         '/  ! application name for online
      DATA CNOPAUSE, CBATCH, CINFO_ON, CDEBUG_ON
     1                               /'np        ',
     2                                'b         ',  ! do NOTpause in beginning
     3                                'info      ',  ! set the log file levels 
     4                                'debug     '/  ! set the log file levels 
c_end_var
c
c  executable code for routine RECARG:
c----6----------------------------------------------------------------72
c
c
c initialize variables
c
      lpause = .true.
      lpause_exit = .true.
      lnever_pause = .false.
c
c  Read in command line arguments
c
      NPAR = iargc()
      if (npar .ne. 0) then
c
c check log file info level
c
        LINFO = .FALSE.
        call get_level(LINFO,LWARN,LDEBUG,LERROR,LFATAL)
        arg_index = find_arg(cinfo_on(1:lenocc(cinfo_on)))
        if (arg_index.gt.0) then
              LINFO = .true.
        endif
        LDEBUG=.false.
        arg_index = find_arg(cdebug_on(1:lenocc(cdebug_on)))
        if (arg_index.gt.0) then
              LDEBUG = .true.
        endif
        call set_level(LINFO,LWARN,LDEBUG,LERROR,LFATAL)
c
c  look for tcl script file name on command line
c
        arg_index = find_arg(ctcl(1:lenocc(ctcl)))
        if ((arg_index.gt.0) .and. (npar .ge. arg_index+1)) then
          call getarg(arg_index+1, tcl_init)
          icend = lenocc(tcl_init)
          if (tcl_init(max(1,icend-3):icend) .ne. '.tcl') then
            tcl_init(icend+1:icend+4) = '.tcl'
          endif
          write(crmess,10)tcl_init(1:lenocc(tcl_init))
 10       format('initializing using Tcl script:',a)
          call recmes(crname,'i',crmess)
        endif 
c
c  now see if a fpack control file name was type in at command line
c
        arg_index = find_arg(cfpack(1:lenocc(cfpack)))
        if ((arg_index.gt.0) .and. (npar .ge. arg_index+1)) then
          call getarg(arg_index+1, arg_fpk_init)
          write(crmess,20)arg_fpk_init(1:lenocc(arg_fpk_init))
 20       format('initializing using fpack control file:',a)
          call recmes(crname,'i',crmess)
        endif 
c
c  now see if an input file name was typed at command line
c
        arg_index = find_arg(cinput)
        if ((arg_index.gt.0) .and. (npar .ge. arg_index+1)) then
          call getarg(arg_index+1, arg_rec_input)
        endif 
c
c  check to see what the user wants to do about output
c
        arg_index = find_arg(cnooutput(1:lenocc(cnooutput)))
        if (arg_index.gt.0) then
          arg_rec_output = 'NoNe'
        else  ! search for the output file note that the -n flag overrides -o
         arg_index = find_arg(coutput(1:lenocc(coutput)))
         if ((arg_index.gt.0) .and. (npar .ge. arg_index+1)) then
          call getarg(arg_index+1, arg_rec_output)
         endif
        endif 
c
c  check to see what the user is asking for help
c
        arg_index = find_arg(chelp(1:lenocc(chelp)))
        if (arg_index.gt.0) then
          call rehelp('help command_line')
        endif 
c
c  check to see what the user whats to turn off initial and final pause
c
        arg_index = find_arg(cnopause(1:lenocc(cnopause)))
        if (arg_index.gt.0) then
          lpause = .false.
          lpause_exit = .false.
        endif 
        arg_index = find_arg(cbatch(1:lenocc(cbatch)))
        if (arg_index.gt.0) then
	  lnever_pause = .true.
        endif 
c
c check for an application name used by the online version.
c
        arg_index = find_arg(capp(1:lenocc(capp)))
        if (arg_index.gt.0 .and. (npar .ge. arg_index+1)) then
           call getarg(arg_index+1, application)
           write (crmess,30) application(1:lenocc(application))
 30        format('The application name is ',a,'.')
           call recmes(crname,'i',crmess)
        endif
      endif
      RETURN
      END
c
c------------------------------------------------------------------------------




