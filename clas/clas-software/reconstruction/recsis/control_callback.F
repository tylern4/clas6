      subroutine control_callback(cstr1,cstr2)

c  handles control callbacks (strings-only version)

c
c  to enable, add the following before ipc_init:
c      external control_callback
c      call ipc_set_control_string_callback(control_callback)

c  et_ok:  1 if ok, 0 if not connected

c  ejw, 9-jul-98 
c  modified for online recsis, gpg 7/21/98
c  modified for ET, gpg 7/6/99


      implicit none
      save

#include "run_control.inc"
#include "et.inc"
      
      character*(*) cstr1,cstr2
      character*120 str1,str2


c  executable code:
c  ----------------
 
c  convert from c strings to fortran char variables

      call strc2f(str1,cstr1)
      call strc2f(str2,cstr2)


      if ((str1.eq.'EVT_STATUS_POLL').or.(str1.eq.'evt_status_poll'))then
         call evt_status_callback(ET_id,ET_session,ET_ok,ET_station,
     1        number_events_read, rec_rate,
     2        number_events_read, rec_rate)

      else
         call ipc_command_parse_str(str1)
      endif

      return
      end


c---------------------------------------------------------------------------------
