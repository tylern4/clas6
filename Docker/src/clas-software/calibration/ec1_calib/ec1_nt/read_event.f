      subroutine read_event
*  
*  read next event and close file on error or EOF
*  
*
      IMPLICIT NONE
      integer*4 ierr,ieorun,ieotri
      INTEGER*4 ihead,nlink
      CHARACTER *1 flag
*      SAVE
      include "bcs.inc"
*
      include "EC1nt.inc"
      include "ntpl_com.inc"

         CALL frbos (iw,12,'E',ierr)
         if (ierr.eq.-1) then
	    print *,'read ', nstory, ' EC1 events'
            EOF = .true.
         else if ( ierr .ne. 0) then
           flag='n'
	   print *,'error reading BOS INPUT file. Go to next file? (n)'
	   read *,flag
	   if (flag.eq.'y') then
	     EOF = .true.
	   end if
	 else if(n_events.gt.0.and.n_events.le. nevent) then
	   EOF = .true.
         end if
         
         IF(EOF) then
            CALL fparm('CLOSE MCIN')
            ieorun=1
            ieotri=1
            EOF = .true.
            firstTime = .true.
     	    return
         END IF

c  get links to HEAD and EC1 banks ( EC1 )

         ihead  = nlink('HEAD',0)

c get event number and time        
         nevent = iw(ihead+3)
         if (nevent.eq.0) return
c remember first event number in the file
         if (firstTime) then
           firstNum  = nevent
           firstTime = .false.
           time0     = iw(ihead+4)
           nevent0   = nevent
           time1     = time0
         end if  
         time    = iw(ihead+4)
         if(time-time1.ge.10) then
           ntime = ntime+1
           if(ntime.gt.200000) then
             print *,' WARNING! too much points in timing histograms!'
             ntime=200000
           end if
           dtime = time-time1
           deltaT(ntime) = time-time0
           deltaN(ntime) = nevent-nevent0
           CALL hfill (ifile*10+3,1.*(nevent-nevent0)/dtime,0.,1.)
           time1 = time
           nevent0 = nevent+1
         end if

         if (nevent/10000*10000.eq.nevent) then
           write(6,'(i10,a8)') nevent,' passed'
         end if
         return 
       end    
