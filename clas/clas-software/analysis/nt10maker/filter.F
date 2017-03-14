      subroutine filter(nfile, nfile_max, nevent_max, bos_in_id, ntfile, 
     1                  ntid, ntlun, data_type, beam_type)
c      
      include "bcs.inc"
      include "ntup.inc"
c
      integer nfile, nfile_max
      integer nevent_max, bos_in_id, nevent, ntid, ntlun, data_type
      character beam_type
      character*132 ntfile
c
      logical eof
      integer trip
c
      parameter (nwpawc = 5000000)
      common/pawc/ipaw(nwpawc)
c      
      integer iquest ,iret, ierr, icycle
      common/quest/iquest(100)
c
      if (nfile .eq. 1) then
         call hlimit(nwpawc)
c      
         iquest(10) = 65000
         call hropen(ntlun,'CWN',ntfile,'NQ',8191,iret)
         call hbset('BSIZE',8176,iret)
c
         call ntup_init(data_type, beam_type)
c
         call clas_nt_book(ntid, beam_type)
         nevent = 0
      endif
c
      eof = .false.
c
      do while ((.not.eof).and.(nevent.lt.nevent_max))
c
         call frbos(iw,bos_in_id,'E',ierr)
!         print *, "first ", IW(139)
         if (ierr.eq.-1) then
            eof = .true.
         else 
            if(trip().eq.0) call clas_nt_fill(ntid, beam_type)
            call bdrop(iw,'E')
!            print *, "second ", IW(139)
            call bgarb(iw)
!            print *, "third ", IW(139)
            nevent = nevent+1 
            if (nevent/10000*10000.eq.nevent) then
               write(6,'(i10,a8)') nevent,' passed'
            endif
         endif
      enddo
c     
      if (nfile .eq. nfile_max) then
         call hrout(0,icycle,' ')
         call hrend('CWN')	
      endif
      call fclos()
      print *,'RUN completed after reading ',NEVENT,' events.'
c     
      return
      end
      
      
        
