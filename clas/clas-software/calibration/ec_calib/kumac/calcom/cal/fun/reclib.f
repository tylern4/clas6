      real function echbsum(lay,sec)
      
      include ?
      
      integer sec,lay
      
      ecrbsum = 0.
      esum = 0.
      n = 0
           
      do i = 1,nechb
          if (secechb(i).eq.sec.and.layerechb(i).eq.lay) then
            n = n + 1
            esum = ehitechb(i)
          endif
      enddo
      
      if (n.eq.1) echbsum = esum
      
      end 
         
      logical function goodpx(sec)
      
      include ?
      logical good(3),pixu,pixv,pixw
      
      goodpx = .false.
      
      do i = 1,3
        good(i) = .false.
      enddo
      
      if (nechb.eq.3.and.secechb(1).eq.sec) then
        do i = 1,nechb
          pixu = nstrpuechb(i).ge.1.and.nstrpuechb(i).le.2
          pixv = nstrpvechb(i).ge.1.and.nstrpvechb(i).le.2
          pixw = nstrpwechb(i).ge.1.and.nstrpwechb(i).le.2
          if (pixu.and.pixv.and.pixw) good(i)=.true.
        enddo
      endif
      
      if(good(1).and.good(2).and.good(3)) goodpx = .true.
      
      end
