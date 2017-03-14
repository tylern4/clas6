      SUBROUTINE RNTCL_INIT
c
c_begin_doc
c  RCS ID string
c  $Id: rntcl_init.F,v 1.33 2009/03/31 19:42:35 myurov Exp $
c
c  Documentation for subroutine RNTCL_INIT
c
c  Purpose: initializes RECSIS runtime variables
c  --------
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
c  Author:   Arne Freyberger      Created:  Fri Dec  1 09:01:07 EST 1995
c  -------
c
cc  Major revisions:  Modified to read data from the DD ring in
c  ----------------   addition to reading from a file.
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
#include "clasmdl.inc"
#include "run_control.inc"
#include "rtclmes.inc"
#include "scat_control.inc"
#include "reversion.inc"
#include "et.inc"
#include "rhist.inc"
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
     1'$Id: rntcl_init.F,v 1.33 2009/03/31 19:42:35 myurov Exp $'   
     2/   
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
c      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='RNTCL_INIT')
      PARAMETER (CRAUTH='Arne Freyberger')
c
c  Local User defined variables:
c  -----------------------------
      integer lenocc
      external lenocc
      integer i, iset, i20
      integer itcl_int_readonly, itcl_int, itcl_string, itcl_double
      integer lstring, lstring_300, lstring_4096, lstring_1024
      character*16 tstring, fstring
c_end_var
      DATA itcl_int,itcl_string,itcl_int_readonly,itcl_double
     1       /1,        4,             129,           2/

      DATA i20/20/
      DATA LSTRING,LSTRING_300,LSTRING_4096/132,300,4096/
      data lstring_1024/1024/
c
c
c   executable code for routine RNTCL_INIT:
c----6----------------------------------------------------------------72
c
c  first initialize run control variables held in common block (can't
c  use DATA statements for common block variables)
c
c
c  RUN_CONTROL variables:
c
      NEVT_TO_PROCESS=-2
      NEVT_TO_SKIP=-1
      NRUN_TO_PROCESS=-1
      NRUN_TO_SKIP=-1
      NSTAMP = 10000

      write (*,*) 'defining ', nstamp
      call tcl_linkint('nstamp',nstamp)
      call tcl_linkint('nevt_to_process',nevt_to_process)
      call tcl_linkint('nevt_to_skip',nevt_to_skip)
      call tcl_linkint('nrun_to_process',nrun_to_process)
      call tcl_linkint('nrun_to_skip',nrun_to_skip)
C
      LSTAT=.FALSE.
      call tcl_linklogical('lstat',lstat)
      FSTAT=.FALSE.
      call tcl_linklogical('fstat',fstat)
C
      LCALL_DO       = .TRUE.
      LCC_DO         = .TRUE.
      LEC1_DO        = .TRUE.
      LEGN_DO        = .TRUE.
      LGPID_DO       = .FALSE.
      LHBID_DO       = .FALSE.
      LIC_DO         = .FALSE.
      LSH_DO         = .FALSE.
      LPID_MAKE_TRKS = .FALSE.
      LPIPLTDC       = .FALSE.
      LREG1_DO       = .FALSE.
      LRF_DO         = .TRUE.
      LSEB_DO        = .TRUE.
      LST_DO         = .TRUE.
      LTAGGER_DO     = .FALSE.
      LTBID_DO       = .FALSE.
      LTBID_NOST_DO  = .FALSE.
      LTIME_DO       = .TRUE.
      LTOF_DO        = .TRUE.
      LTRK_DO        = .TRUE.
      LUSR0_DO       = .TRUE.
      LUSR1_DO       = .TRUE.
      LUSR2_DO       = .TRUE.
      START_TB       = .FALSE.
cfk4/08: most runs don't use BONUS
c      LGEM_DO        = .TRUE.
      LGEM_DO        = .FALSE.
      LCC_EG4        = .FALSE.

      call tcl_linklogical('lcall_do',lcall_do)
      call tcl_linklogical('lcc_do',lcc_do)
      call tcl_linklogical('lec1_do',lec1_do)
      call tcl_linklogical('legn_do',legn_do)
      call tcl_linklogical('lgpid_do',lgpid_do)
      call tcl_linklogical('lhbid_do',lhbid_do)
      call tcl_linklogical('lic_do',lic_do)
      call tcl_linklogical('lsh_do',lsh_do)
      call tcl_linklogical('lpid_make_trks',lpid_make_trks)
      call tcl_linklogical('lpipltdc', lpipltdc)
      call tcl_linklogical('lreg1_do',lreg1_do)
      call tcl_linklogical('lrf_do',lrf_do)
      call tcl_linklogical('lseb_do',lseb_do)
      call tcl_linklogical('lst_do',lst_do)
      call tcl_linklogical('ltagger_do',ltagger_do)
      call tcl_linklogical('ltbid_do',ltbid_do)
      call tcl_linklogical('ltbid_nost_do',ltbid_nost_do)
      call tcl_linklogical('ltof_do',ltof_do)
      call tcl_linklogical('ltrk_do',ltrk_do)
      call tcl_linklogical('lusr0_do',lusr0_do)
      call tcl_linklogical('lusr1_do',lusr1_do)
      call tcl_linklogical('lusr2_do',lusr2_do)
      call tcl_linklogical('lgem_do',lgem_do)
      call tcl_linklogical('lcc_eg4',lcc_eg4)

      call tcl_linkvar('start_tb',start_tb,itcl_int)

C
      LDISPLAY = .FALSE.
      LDISPLAY_ALL = .FALSE.
      call tcl_linklogical('ldisplay',ldisplay)
      call tcl_linklogical('ldisplay_all',ldisplay_all)
C
      LEVENT_ALL = .FALSE.
      call tcl_linklogical('levent_all',levent_all)
C
c     lpause is initialized in RECARG
c
      lpar = .false.
      call tcl_linklogical('lpar',lpar)

      call tcl_linklogical('lpause',lpause)
      call tcl_linklogical('lpause_exit',lpause_exit)
      LEXIT  = .FALSE.
      call tcl_linklogical('lexit',lexit)
      LEXIT_PEND  = .FALSE.
      call tcl_linklogical('lexit_pend',lexit_pend)
      LQUIT  = .FALSE.
      call tcl_linklogical('lquit',lquit)
      LCLOSE  = .FALSE.
      call tcl_linklogical('lclose',lclose)
      LEOF  = .FALSE.
      call tcl_linklogical('leof',leof)
      NTCL_CHECK  = 10
      call tcl_linkint('ntcl_check',NTCL_CHECK)
      ITCL_CATCH  = 10
      call tcl_linkint('itcl_catch',ITCL_CATCH)
C
      FPK_FILE='                    '
      REC_PROMPT='RS> '
      DDL_FILE='                    '
      log_file_name = 'use_default'
      chist_filename = 'use_default'
      call tcl_linkchar('fpk_file',fpk_file,itcl_string,lstring_1024)
      call tcl_linkchar('tcl_init',tcl_init,itcl_string,lstring_1024)
      call tcl_linkchar('rec_output',rec_output,itcl_string,lstring_1024)
      call tcl_linkchar('rec_prompt',rec_prompt,itcl_string,lstring_1024)
      call tcl_linkchar('ddl_file',ddl_file,itcl_string,lstring_1024)
      call tcl_linkchar('log_file_name',log_file_name,itcl_string,
     &     lstring)
      call tcl_linkchar('chist_filename',chist_filename,itcl_string,
     &     lstring)
      call tcl_linkchar('rec_input',rec_input,itcl_string,lstring_4096)
c
c  this is how we do arrays
c
      do i=1,20
       LEVENT_CLASS(I) = .TRUE.
       if (i .lt.10) then
        write(tstring,10)'levent_class(',i,')'
 10     format(a13,i1,a1)
        write(fstring,20)'fpk_cmd(',i,')'
 20     format(a8,i1,a1)
       else
        write(tstring,11)'levent_class(',i,')'
 11     format(a13,i2,a1)
        write(fstring,21)'fpk_cmd(',i,')'
 21     format(a8,i2,a1)
       endif
       call tcl_linklogical(tstring(1:lenocc(tstring)),
     1               levent_class(i))
       call tcl_linkchar(fstring(1:lenocc(fstring)),
     1               fpk_cmd(i),itcl_string,lstring_4096)
      enddo
      LEVENT_CLASS(21) = .FALSE.
      i = 21
      write(tstring,11)'levent_class(',i,')'
      call tcl_linklogical(tstring(1:lenocc(tstring)),
     1               levent_class(i))
c
c  provide duplicate link to fpk_cmd(1) as fpk_cmd
c
       call tcl_linkchar('fpk_cmd',
     1               fpk_cmd(1),itcl_string,lstring)
       ifpack = 0
       call tcl_linkint('ifpack',ifpack)
c
c   link scat variables
c
       lscat = .false.
       call tcl_linklogical('lscat',lscat)
       call tcl_linkint('scat_pgm_no',scat_pgm_no)
       call tcl_linkint('scat_ver_no',scat_ver_no)
       scat_handle(1) = -9999
       call tcl_linkint('scat_handle',scat_handle(1))
       scat_debug = 0
       call tcl_linkint('scat_debug',scat_debug)
       scat_host = jobmach(1:lenocc(jobmach))
       call tcl_linkchar('scat_host',scat_host,itcl_string,i20)
       scat_prog_start = 'EventStore'
       call tcl_linkchar('scat_prog_start',scat_prog_start,itcl_string,lstring)
c
c The following variables are in the CLASMDL include file, and should
c be read only
c
       call tcl_linkint('run_number',nccrun)
       call tcl_linkint('event_number',nevent)

c 
c
c   use CERNLIB routine to deal with buffered STDIN, and put out a prompt
c
      call tminit(iset)
      call tmpro(rec_prompt(1:lenocc(rec_prompt)))
      call set_prompt(rec_prompt(1:lenocc(rec_prompt)))

c   ctcl_buff contains a maximum 10,000 characters of messages 
      lstring = 10000

c      ctcl_buff = ' '
c   do NOT initialize this variable, since it will already contain data by
c   the time this routine is called

      call tcl_linkchar('ctcl_buff',ctcl_buff,itcl_string,lstring)

c 
c The following variables are for manipulating BOS BANK interactively
c
      lnamdmp = .false.
      call tcl_linklogical('lnamdmp',lnamdmp)
      levtdmp = .false.
      call tcl_linklogical('levtdmp',levtdmp)
      lbnkdmp = .false.
      call tcl_linklogical('lbnkdmp',lbnkdmp)
      lbnkedt = .false.
      call tcl_linklogical('lbnkedt',lbnkedt)
      cbname = '    '
      lstring = 4
      call tcl_linkchar('cbname',cbname,itcl_string,lstring)

      nrow_tcl = 1
      call tcl_linkint('nrow_tcl',nrow_tcl)
      ncol_tcl = 1
      call tcl_linkint('ncol_tcl',ncol_tcl)
      nrec_tcl = 0
      call tcl_linkint('nrec_tcl',nrec_tcl)
      ibos_array = 1
      call tcl_linkint('ibos_array',ibos_array)
      rnew_value = 0.
      call tcl_linkdouble('rnew_value',rnew_value)
      mapmanager = 1
      call tcl_linkint('mapmanager',mapmanager)

c  this is used for formatted the screen output

      ncol_screen=80
      call tcl_linkint('ncol_screen',ncol_screen)

c
c read data from the ET chain instead of a file. the first variable
c will supersede reading from a file. the other set the parameters
c of the analysis of data from the ET chain.
c
      read_et = .false.
      call tcl_linklogical('read_et',read_et)

      lstring = 20 
      ipc_name = '         '
      call tcl_linkchar('ipc_name',ipc_name,itcl_string,lstring)

      lstring = 512
      et_outbanks = 'junk'
      call tcl_linkchar('et_outbanks',et_outbanks,itcl_string,lstring)

      RETURN
      END
c
c------------------------------------------------------------------------------




