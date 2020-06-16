      subroutine hbook_open
c
c opens the histogram file.
c
      implicit none
      save
c
#include "run_control.inc"
#include "rhist.inc"
#include "clasmdl.inc"
#include "re_units.inc"
c
      integer*4 inum, istat

      lhst = 0
      call reslun('hbook_open', lhst, istat)
      if (istat .ne. 1) stop 'hbook_open: could not reserve lhst'

c build the filename for the histogram file. the name of the
c file and the shared memory is different for the online recsis.
c the file name for the online version contains the run number
c and a sequence number. gilfoyle 5/15/98

      if (.not. read_et) then
         if (chist_filename(1:11) .eq. 'use_default') then
            call get_seq(inum)
            write(chist_filename,20)inum
 20         format('re',i4.4,'.rzn')
         endif
      else
         call get_seq(inum)
         write(chist_filename,30) nccrun, inum
 30      format('run',i5.5,'re',i4.4,'.rzn')
c
c blank cshared_name means shared memory is turned off. the
c use of 'csis' for ipc_name is only for the shift-takes
c online recsis.
c
         if (cshared_name(1:1) .ne. 'R' .and.
     1                ipc_name .eq. 'csis') then
            cshared_name = 'RCON'
         end if
      end if

      call hbook_init(chist_filename,lhst,cshared_name)
      lhbook_init = .true.

      return
      end
c---------------------------------
