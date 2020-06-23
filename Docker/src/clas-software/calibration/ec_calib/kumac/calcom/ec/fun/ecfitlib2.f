      subroutine ecfitpmt
      
      real par(3),step(3),pmin(3),pmax(3),sigpar(3),chi2
      integer min(6),max(6),s,ilay,istr,id,pmt
      common/quest/iquest(100)
      common/hfpar/par
      external gau
      
      data pmin/0.,0.,0./
      data pmax/1000.,150.,100./
      data step/0.1,0.1,0.1/
      data par/20.,60.,10./ 
      data min/20,20,20,50,50,50/
      data max/170,170,170,200,200,200/    
      
      vector ecFitadc_mean(36,6,6)
      vector ecFitadc_meanerr(36,6,6)
      vector ecFitadc_sigm(36,6,6)
      
      do s = 6,6
      id = s*100+3
      do ilay = 1,3
      do istr = 1,36 
        pmt = (ilay-1)*36+istr
        iquest(11) = min(ilay)/3
        iquest(12) = max(ilay)/3
        iquest(13) = pmt
        iquest(14) = pmt+1
        par(1) = 20.
        par(2) = 60.
        par(3) = 10.
        call hfith(id,gau,'BQLR',3,par(1),step,pmin,pmax,sigpar,chi2)
        print *, 'Mean for sector',s,
     &' layer',ilay,' strip',istr,' =',par(2)
        ecFitadc_mean(istr,ilay,s) = par(2)
        ecFitadc_sigm(istr,ilay,s) = par(3)
        ecFitadc_meanerr(istr,ilay,s) = sigpar(2)
      enddo
      enddo
      enddo
      
      end
      
      function gau(x)      
      common/hfpar/par(3)
      gau = par(1)*exp(-0.5*((x-par(2))/par(3))**2)
      end

