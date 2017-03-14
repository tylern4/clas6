      subroutine rec_dump_epics

#include "epics.inc"

      integer i_epics
      character*256 crmess
      character*20 crname
      parameter (crname = 'rec_dump_epics')


         write(crmess, 5) epics_last_run_num, epics_last_evt_num
 5       format('Epics Values as of run:',I7,' and event ',I9)
         call recmes(crname,'i',crmess)
      do i=1,tot_epics
         write(crmess, 10) epics_name(i), epics_value(i)
 10      format(A32,"  =  ",f15.5)
         call recmes(crname,'i',crmess)
      enddo
      return
      end
