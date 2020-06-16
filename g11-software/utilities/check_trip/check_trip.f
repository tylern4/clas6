      program check_trip

      implicit none
      
      integer iscal_file(100),iscal(100),flag_old(100)
      integer ev_min(100),ev_max(100),nev(100)
      integer scaler_interval(100),last_event_timer(100)
      integer current_scaler_timer(100)
      real    diff_summ(100),diff_scaler_based(100),live_time(100)
      integer i,flag_new(100),ntrip_good,ntrip
      real    rev_good
      logical bad_file_flag
      data ntrip_good/0/

*** open trip file
      open(unit=71,file='trip.dat',status='unknown',err=101)

*** read trip file
      i=0
      do while(.true.)
         i=i+1
         read(71,*,END=102,ERR=102) iscal_file(i),iscal(i),flag_old(i)
     &                             ,ev_min(i),ev_max(i),nev(i)
     &                             ,scaler_interval(i),diff_summ(i)
     &                             ,diff_scaler_based(i)
     &                             ,last_event_timer(i)
     &                             ,current_scaler_timer(i),live_time(i)
      enddo
 102  continue
      close(71)
      ntrip=i-1

*** check if trip info is good
*** file is bad if all scaler events have flag=1
      bad_file_flag=.true.
      do i=2,ntrip-1
         if(flag_old(i).eq.0) bad_file_flag=.false.
      enddo
      if(bad_file_flag) then
         continue
      else
         print *,'trip file is good'
         goto 109
      endif

*** define average event rate
*** skip first and last scaler event
*** skip events with rate below 1kHz
      ntrip_good=0
      do i=2,ntrip-1
         if(nev(i).gt.10000) then
            ntrip_good=ntrip_good+1
            rev_good=rev_good+nev(i)
         endif
      enddo
      if(ntrip_good.gt.0) then
         rev_good=rev_good/ntrip_good
      else
         rev_good=0
      endif

*** reassign trip flag
      if(ntrip_good.gt.0.AND.rev_good.gt.10000) then
         flag_new(1)     = flag_old(1)
         flag_new(ntrip) = flag_old(ntrip)
         do i=2,ntrip-1
            if(nev(i).gt.rev_good*0.8) then
               flag_new(i)=0
            else
               flag_new(i)=1
            endif
         enddo
      else
         print *,'reassignment of trip flag failed'
         goto 109
      endif

*** write corrected trip information in new file
      open(unit=72,file='trip.new.dat',status='unknown',err=109)
      do i=1,ntrip
c         print *,iscal_file(i),iscal(i),flag_new(i)
         write(72,110) iscal_file(i),iscal(i),flag_new(i)
     &             ,ev_min(i),ev_max(i),nev(i)
     &             ,scaler_interval(i),diff_summ(i)
     &             ,diff_scaler_based(i)
     &             ,last_event_timer(i),current_scaler_timer(i)
     &             ,live_time(i)
      enddo
      close(72)
      print *,'reassignment of trip flag completed'
      goto 109

 101  continue
      print *,'trip file not found, check skipped'

 109  continue
      
 110  format(I5,1X,I5,1X,I5,4I11,1X,2f11.0,3X,2I11,f11.6)
      stop
      end
	
