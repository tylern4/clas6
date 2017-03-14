      SUBROUTINE RE_DD_INIT
c
c  Documentation for subroutine RE_DD_INIT
c
c  Purpose: Sets up reading of data from DD ring and IPC communication.
c  -------- 
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
c  Called from:   RECKIN
c  ------------
c
c  Other routines:
c  ---------------
c
c  Notes:
c  ------
c
c  Author:   Gilfoyle  Created:  Fri Jul 24 1998
c  -------
c
c  Major revisions:  
c  ----------------  
c                                          
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
#include "dd.inc"
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
      PARAMETER ( CFILE = '$RCSfile: re_dd_init.F,v $')
      PARAMETER ( CREVIS = '$Revision: 1.3 $')
      PARAMETER ( CSTATE = '$State: Exp $')
      PARAMETER ( CDATE = '$Date: 1998/10/06 17:14:49 $')
      PARAMETER ( CAUTHO = '$Author: clasrun $')
      DATA CRCSID/
     1'$Id: re_dd_init.F,v 1.3 1998/10/06 17:14:49 clasrun Exp $'
     3/
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='RE_DD_INIT')
      PARAMETER (CRAUTH='Gilfoyle')
c
c  Local User defined variables:
c  -----------------------------
      INTEGER ICFNBL, LNBLNK, LENOCC, INDEXA
      EXTERNAL ICFNBL, LNBLNK, LENOCC, INDEXA
      external status_poll_callback, quit_callback, control_callback
c 
c stuff for dd ring.
c
      integer clasdd_init, ifromc, events_in_fifo
      integer ipc_init
      integer ef, fifonamelen, dd_namelen
      data fifoname/'RECSIS  '/
      data events_in_fifo/15/
      data DD_id/'RECSIS_TEST'/
      data application/'clastest'/
c_end_var
c
c  executable code for routine RE_DD_INIT:
c  -------------------------------------
c
c get ready to read from the dd ring. set the name of the fifo 
c and do some error checking.
c
      fifonamelen = lenocc(fifoname)
      if (fifonamelen.ne.0) then
         write (crmess,100) fifoname
 100     format('Reading DD ring data from FIFO ',a16)
         call recmes(crname,'I',crmess) 
      else
         crmess = 'The FIFO name variable is blank! Please'
         call recmes(crname,'I',crmess)
         crmess = 'set it to the appropriate name.'
         call recmes(crname,'I',crmess)
      end if
      fname_dd(1) = ifromc(fifoname(1:4))
      fname_dd(2) = ifromc(fifoname(5:8))
      fnlen_dd = lenocc(fifoname)
c
c get the DD name from the environment variable.
c
      call getenvf('DD_NAME',dd_name)
      dd_namelen = lenocc(dd_name)
      if (dd_namelen.ne.0) then
         write (crmess,120) fifoname
 120     format('Reading data from DD ring',a20,'.')
         call recmes(crname,'I',crmess) 
      else
         crmess = 'The DD name variable is blank! Please'
         call recmes(crname,'I',crmess)
         crmess = 'set it to the appropriate name.'
         call recmes(crname,'I',crmess)
      end if
c
c initialize dd ring.
c
      ef = clasdd_init()
      if (ef.ne.0) then
         crmess = 'Could not connect to the DD ring!!'
         call recmes(crname,'W',crmess)
         dd_ok = 0
      else
         dd_ok = 1
      end if
c
c set the request count. this is the number of events that
c will be stored in the dd fifo while recsis is processing
c data. the default is one which made the online recsis
c sit in sleep mode most of the time.
c
      call clasdd_reqcnt(events_in_fifo)
c
c initialize the ipc information exchange and the ipc quit
c command.
c
      call ipc_set_application(application)
      call ipc_set_status_callback(status_poll_callback)
      call ipc_set_quit_callback(quit_callback)
      call ipc_set_control_string_callback(control_callback)
c
c  start the interprocess communication (ipc) with the
c  appropriate name.
c
      DD_id(1:7) = 'recsis_'
      DD_id(8:7+lenocc(DD_NAME)) = DD_NAME
      if(ipc_init(DD_id,
     1            'RECSIS online monitoring').lt.0) then
         call recmes(crname,'W',' ')
         call recmes(crname,'W','?Unable to connect to ipc system!')
         call recmes(crname,'W',' ')
         stop
      end if
c
c 'subscribe' to the desired ipc subject (the dd system).
c
      call ipc_subject_subscribe('dd_system')

      RETURN
      END
c
c------------------------------------------------------------------------------
