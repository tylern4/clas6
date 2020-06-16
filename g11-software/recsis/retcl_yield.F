      SUBROUTINE RETCL_YIELD(ITCL_ERR)
c
c_begin_doc
c  RCS ID string
c  $Id: retcl_yield.F,v 1.12 2000/12/14 00:41:58 fklein Exp $
c
c  Documentation for subroutine RETCL_YIELD
c
c  Purpose:  performs TKFOR, pulls input out of TCL
c  --------
c
c  Input Parameters:  
c  ----------------
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
c  Author:   Arne Freyberger      Created:  Wed Oct 25 11:50:04 EDT 1995
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
#include "run_control.inc"
#include "rhist.inc"
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
     1'$Id: retcl_yield.F,v 1.12 2000/12/14 00:41:58 fklein Exp $'   
     2/   
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
c      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='RETCL_YIELD')
      PARAMETER (CRAUTH='Arne Freyberger')
c
c  Local User defined variables:
c  -----------------------------
      integer lenocc, indexa
      character spaces*200
      external lenocc, spaces, indexa
c
      integer i, itcl_err,  nbyt, iend
      character*200 cline
      character*4096 ccmd
      logical lprompt, already_stopped
c
      data lprompt/.true./
      data already_stopped/.false./
c_end_var
c
c  executable code for routine RETCL_YIELD:
c----6----------------------------------------------------------------72
c
10    CONTINUE
c
c   now look on the net for input
c
        call scat_active
c
c   look at the terminal input 
c
       IF (LINTER) THEN
        call asynck_c(nbyt)
       else
        nbyt = 0
       endif

        if (nbyt .gt. 0) then
         read(*,13, end=999)cline
 13      format(a200)
         cline = spaces(cline,1)
         iend = lenocc(cline)

c
c watch out for empty lines
c 
         if (iend .eq. 0) then
           call tmpro(rec_prompt(1:lenocc(rec_prompt)+1))
           goto 10
         endif
c
c  look for confused users via ? and help
c
         if (cline(1:1) .eq. '?' .or.
     1       cline(1:1) .eq. 'h' .or.
     2       cline(1:1) .eq. 'H') then
             call rehelp(cline(1:iend))
             call tmpro(rec_prompt(1:lenocc(rec_prompt)+1))
             goto 10
         endif
c
c   differentiate between tcl commands and tcl scripts via the @
c
         if (cline(1:1) .ne. '@') then
          call tcl_eval(cline(1:iend))
         else
          if (cline(max(1,iend-3):iend) .ne. '.tcl') then
            cline(iend+1:iend+4) = '.tcl'
          endif
          call tcl_recsis_evalfile(cline(2:lenocc(cline)))
         endif
         call tmpro(rec_prompt(1:lenocc(rec_prompt)+1))
        endif
 999     continue
c
c-------------------now process the information
c
c  is LSTAT is true dump out the status info
c
      if (lstat) then 
        call restat
        lstat = .false.
      endif
c
c  if FSTAT is true call the FPACK status dump
c
      if (fstat) then 
        call fpstat
        fstat = .false.
      endif

      if (lhbook_init) then
       if (lhist) then 
        call rehist
        lhist = .false.
       endif

       if (lhbook) then
        call rehbk
        lhbook = .false.
       endif
   
       if (lsetvar) then
        call rehvar
        lsetvar = .false.
       endif
      endif
c
c  if lbnkdmp is true dump out the bank
c
      if (lnamdmp) then
       call namdmp(ibos_array)
       lnamdmp = .false.
      endif

      if (lbnkdmp) then
       call bnkdmp(cbname,ibos_array,ncol_screen)
       lbnkdmp = .false.
      endif

      if (levtdmp) then
       call evtdmp(ibos_array, ncol_screen)
       levtdmp = .false.
      endif

      if (lbnkedt) then
       call redit(cbname,nrow_tcl,ncol_tcl,nrec_tcl,
     1            ibos_array,sngl(rnew_value))
       lbnkedt = .false.
      endif

c   look for entered FPACK commands in the fpk_cmd variable field

      if (ifpack .gt. 0) then
       do 80 i=1,min(ifpack,20)
        if (indexa(fpk_cmd(i)) .ne. 0) then
          ccmd = fpk_cmd(i)
          call recmes(crname,'i',ccmd(1:lenocc(ccmd)))
          call fparm(ccmd(1:lenocc(ccmd)))
          fpk_cmd(i) = ' '
        endif
 80    continue
      ifpack = 0
      endif
c
c  check to see if the LEXIT or LQUIT or LPAUSE flags are set
c
      if (lexit) then
        if (.not. read_et ) then
            call fseqe
cfjk: see renjob.F
c        else if (read_et) then
c            if (.not. already_stopped) then
c              call close_et(0)
c              call ipc_close
c              already_stopped = .true.
c            end if
cfjk
        end if
        lpause = .false.
        return
      endif
c
      if (lquit) then
         call recmes(crname,'f','Quitting now due to LQUIT flag set')
      endif
c
      if (lpause .and. .not.lnever_pause) then
        if (lprompt) then
         call recmes(crname,'n',
     1   'RECSIS has been paused, to resume enter:   set lpause 0')
         call tmpro(rec_prompt(1:lenocc(rec_prompt)+1))
         lprompt = .false.
        endif
c        nevt_count = 0
        call sleepf(1)
        goto 10
      endif
c 
      lprompt = .true.
c
      RETURN
      END
c
c------------------------------------------------------------------------------













