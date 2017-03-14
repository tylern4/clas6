      integer function cctdccal()
      
      include ?
      integer sec,idd,j,pmt
      real tdc,ecsum(6)
      
      vector cctdcmean(18,2,6)
      
      do i = 1,6
        ecsum(i) = 0
      enddo
      
      do i = 1,nec
        sec = secec(i)
        ecsum(sec) = ecsum(sec)+adcec(i)
      enddo  
    
      do i = 1,ncc
        sec = seccc(i)
        idd = sec*100
        j = mod(idcc(i),2)
        pmt = int(idcc(i)/2)+j
        tdc = float(tdccc(i)-cctdcmean(pmt,2-j,sec)+1000)
c        call hf2(idd,tdc,float(idcc(i)),1.)
c        call hf2(idd+1,tdc,ecsum(sec),1.)
c        call hf2(idd+2,tdc,float(adccc),1.)
       if (ecsum(sec).gt.100.and.adccc(i).gt.20) then
         if (evclass.eq.1) call hf1(idd,tdc,1.)
         if (evclass.eq.14) call hf1(idd+1,tdc,1.)
         if (evweight.eq.1) call hf1(idd+2,tdc,1.)
         if (evweight.eq.3) call hf1(idd+3,tdc,1.)
       endif 
      enddo
      
      end
        
