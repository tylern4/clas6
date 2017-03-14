      integer function nnec(sec)
      
      integer sec
      
      include ?
      
      nnec = 0
      
      do i = 1,nec
        if (secec(i).eq.sec) then
          if (adcec(i).gt.0) nnec = nnec + 1
        endif
      enddo
      
      end
            
      logical function goodpix(n1,io,sec)
      
      include ?
      
      integer sec,n1,io,n(6)
      
      goodpix = .false.
      
      do i = 1,6
        n(i)= 0
      enddo
      
      do i = 1,nec
        lay = layerec(i)
        if (secec(i).eq.sec.and.adcec(i).gt.0) then
            n(lay) = n(lay) + 1
        endif
      enddo
      
      if (io.eq.1) then
        if (n(1).eq.n1.and.n(2).eq.n1.and.n(3).eq.n1) goodpix = .true.
      elseif (io.eq.2) then
        if (n(4).eq.n1.and.n(5).eq.n1.and.n(6).eq.n1) goodpix = .true.
      endif
      
      END
      
      integer function ecsum(nopt1,nopt2,io,sec)
      
      integer sec,adcsumm,io,nopt1,nopt2

      include ?
      
      ecsum = 0
      adcsumm = 0
      n = 0
      
      do i = 1,nec
        if (secec(i).eq.sec) then
          if (io.eq.0) then
            n = n+1
            adcsumm = adcsumm + adcec(i)
          elseif (io.eq.1.and.layerec(i).le.3) then
            n = n+1
            adcsumm = adcsumm + adcec(i)
          elseif (io.eq.2.and.layerec(i).ge.4) then
            n = n+1
            adcsumm = adcsumm + adcec(i)
          endif
        endif
      enddo
      
      if (n.ge.nopt1.and.n.le.nopt2) ecsum = adcsumm 
      
      end

      integer function ectmean(sec)
      
      include ?
      
      integer sec
      ectmean = 0
      n = 0
           
      do i = 1,nec
        if (secec(i).eq.sec) then
          if (tdcec(i).gt.0) then
            ectmean = ectmean + tdcec(i)
            n = n + 1
          endif
        endif
      enddo
      
      ectmean = ectmean/float(n) 
      
      end
           
