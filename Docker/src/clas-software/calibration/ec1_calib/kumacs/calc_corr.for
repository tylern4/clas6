	subroutine calc_corr
	
	integer stack,layer,sect,i,k,ist,nst
	real peak,sig,a1_prev_x
	real a1_prev(2,4,40)
	
	COMMON/mypar/peak(2,4,40),sig(2,4,40)
	
	open(unit=22,file='calib_prev.dat',status='OLD')
	do i=1,256
	  icounter=i
	  read (22,*,end=100) i1,k1,ist1,a1_prev_x
100	  a1_prev(i1,k1,ist1) = a1_prev_x
	end do
	  print *,icounter,i1,k1,ist1,a1_prev_x
 	close(22)
	
	open(unit=22,file='calib_e.dat',status='NEW')
	
c    
c	peak_mips = 26.4*0.76 
c
	peak_mips = 26.4 
	
	do i=1,2
	  
	  do k=1,4
	    if (k.eq.1.or.k.eq.3) nst=24
	    if (k.eq.2.or.k.eq.4) nst=40
	    do ist=1,nst
	    
	      if(peak(i,k,ist).eq.0) peak(i,k,ist)=peak_mips
	      corr_fact= (peak_mips-peak(i,k,ist))/peak(i,k,ist)
	      a_new=(1+corr_fact)*a1_prev(i,k,ist)
	      write (22,10) i,k,ist,peak(i,k,ist),sig(i,k,ist),corr_fact
     &      ,a1_prev(i,k,ist),a_new
            
	    end do
          end do
	end do
 10     format(3i4,5f12.4)
 	close(22)
	return
	end
