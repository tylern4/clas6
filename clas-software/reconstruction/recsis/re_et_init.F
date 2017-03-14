      SUBROUTINE RE_ET_INIT
c
c  Documentation for subroutine RE_ET_INIT
c
c  Purpose: Sets up reading of data from Event Transfer (ET) and 
c           IPC communication.
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
c  Author:   Gilfoyle  Created:  Tue Jun 8 1999
c  -------
c
c  Major revisions:  
c  ----------------  
c  add option to write out result banks
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
c#include "run_control.inc"
#include "et.inc"
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
      PARAMETER ( CFILE = '$RCSfile: re_et_init.F,v $')
      PARAMETER ( CREVIS = '$Revision: 1.7 $')
      PARAMETER ( CSTATE = '$State: Exp $')
      PARAMETER ( CDATE = '$Date: 2001/10/26 14:31:42 $')
      PARAMETER ( CAUTHO = '$Author: boiarino $')
      DATA CRCSID/
     1'$Id: re_et_init.F,v 1.7 2001/10/26 14:31:42 boiarino Exp $'
     3/
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*128  CRMESS
      PARAMETER (CRNAME='RE_ET_INIT')
      PARAMETER (CRAUTH='Gilfoyle')
c
c  Local User defined variables:
c  -----------------------------
      INTEGER ICFNBL, LNBLNK, LENOCC, INDEXA, INIT_ET2, itest
      EXTERNAL ICFNBL, LNBLNK, LENOCC, INDEXA, INIT_ET2
      external status_poll_callback, quit_callback, control_callback
c 
c stuff for ET..
c
      integer ipc_init, station_len, ilen
      integer qsize, ET_prescale, ET_id_name_len, blocking_flag
      integer first, last, session_len, ef, init_et
      integer host_len, ptr_colon, INDEX

      character*40  session, station, station2

      data station/'RECON'/
      data qsize/10/
      data ET_prescale/1/

      data application/'clastest'/
c_end_var
c
c  executable code for routine RE_ET_INIT:
c  -------------------------------------
c
c get ready to read from the ET station. get the session name 
c from the environment variable.
c

      ET_host='local'
      ETout_host='local'
      call getenvf('ET_SESSION',session)
      if (session(1:1) .eq. ' ') then
         session = 'clasprod'
      end if
      session_len = lenocc(session)
      ptr_colon=INDEX(session,':')
      if(ptr_colon.gt.1 .and. ptr_colon.lt.session_len) then
         ET_host = session(1:ptr_colon-1)
         ET_session = session(ptr_colon+1:session_len)
         session_len = lenocc(ET_session)
      else
         ET_session = session(1:session_len)
      endif
      host_len = lenocc(ET_host)
c
c get the station name.
c
      station_len = lenocc(station)
      ET_station(1:station_len) = station(1:station_len)

c
c initialize ET.  (fjk: special modes (1) blocking et_system clasprod
c                                     (2) attach to additional et_system clasrecs
      blocking_flag=0
      if(ET_outbanks(1:4).ne.'junk') blocking_flag=1
c
c (mode 1 -> compiled with et_0.cc)
c      ef = init_et(ET_session, ET_station, qsize, ET_prescale, blocking_flag)
      ef = init_et(ET_session, ET_station, qsize, ET_prescale)

c
c (mode 2 -> compiled with et_.cc)

c      ef = init_et(ET_session, ET_host, ET_station, qsize, ET_prescale)

      write(crmess,*)' ET init',ef,' for session,host,station: ',
     &  ET_session(1:session_len),' ',ET_host(1:host_len),' ',ET_station(1:station_len)
      call recmes(crname,'L',crmess)

c      call getenvf('ETOUT_SESSION',session)
c      etout_ok=0
c      if (blocking_flag.ne.0) then
c         if (session(1:1) .eq. ' ') then
c            session = 'clasrecs'
c         end if
c         session_len = lenocc(session)
c         ptr_colon=INDEX(session,':')
c         if(ptr_colon.gt.1 .and. ptr_colon.lt.session_len) then
c            ETout_host = session(1:ptr_colon-1)
c            ETout_session = session(ptr_colon+1:session_len)
c            session_len = lenocc(ETout_session)
c         else
c            ETout_session = session(1:session_len)
c         endif
c         host_len = lenocc(ETout_host)
c         station2=station
c         ilen=LNBLNK(ipc_name)
c         if(ilen.gt.0) then
c           station2=ipc_name
c           call cltou(station2(1:ilen))
c           write(station,'(2A)') 'RE',station2(1:ilen)
c         endif
c         station_len = lenocc(station)
c
c         etout_ok = init_et2(ETout_session, ETout_host, station)
c         write(crmess,*)' ETout init',etout_ok,' for session,host,station: ',
c     & ETout_session(1:session_len),' ',ETout_host(1:host_len),' ',station(1:station_len)
c         call recmes(crname,'L',crmess)
c
c      endif
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
      ET_id_name_len = lenocc(ipc_name)

      ET_id(1:2) = 're'

      first = 3
      last  = ET_id_name_len + 2
      ET_id(first:last) = ipc_name(1:ET_id_name_len)

      first = ET_id_name_len + 3
      last  = ET_id_name_len + 3
      ET_id(first:last) = '_'

      first = ET_id_name_len + 4
      last =  ET_id_name_len + 3 + session_len
      ET_id(first:last) = ET_SESSION(1:session_len)
      call recmes(crname,'L','ET_ID:'//ET_id(1:last)//':')
      itest = ipc_init(ET_ID(1:last),'RECSIS online monitoring')
      if(itest.lt.0) then
         call recmes(crname,'W',' ')
         write(crmess,*)'?Unable to connect to ipc system! Error#:',itest
         call recmes(crname,'W',crmess)
         call recmes(crname,'W',' ')
         stop
      end if
c
c 'subscribe' to the desired ipc subject (the ET system).
c
      call ipc_subject_subscribe('evt_system')

      RETURN
      END
c
c------------------------------------------------------------------------------
