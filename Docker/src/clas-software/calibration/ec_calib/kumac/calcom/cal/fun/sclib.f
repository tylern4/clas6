      real function scgmean(sec)
      
      integer sec
      
      include  ?
      
      scgmean = 0.0
            
      do i = 1,nsc
        if (secsc(i).eq.sec) then
          if (adclsc(i).gt.5.and.adcrsc(i).gt.5) then 
            scgmean = scgmean + sqrt(float(adclsc(i)*adcrsc(i)))
          endif
        endif    
      enddo
      
      end
      
      real function scedep()
      
      include  ?
      
      scedep = 0.0
            
      do i = 1,nscrw
        scedep = scedep + ehitscrw(i)
      enddo
      
      end

      integer function sctmean(sec)
      
      integer sec
      
      include  ?
      
      sctmean = 0
      n = 0
            
      do i = 1,nsc
        if (secsc(i).eq.sec) then
          if (tdclsc(i).gt.5.and.tdcrsc(i).gt.5) then 
            sctmean = sctmean + 0.5*(float(tdclsc(i)+tdcrsc(i)))
            n = n + 1
          endif
        endif    
      enddo
      
      sctmean = sctmean/float(n)
      
      end
      
