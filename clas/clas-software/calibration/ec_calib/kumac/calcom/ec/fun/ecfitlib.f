      subroutine ecfit(view)
      
      integer view,strip,i1,i2,i
      
      real x(71),adc(1296),adcerr(1296)
            
      vector pix
      vector pixerr
      vector strt
      vector stp
      
      do i = 1,71
        x(i) = i
      enddo
      
      do i = 1,1296
        adc(i)    = pix(i)
        adcerr(i) = pixerr(i)
      enddo
      
      do strip = 1,36
        i1 = strt(strip)
        i2 = 2*strip-1-stp(strip)
        call ecfitstrip(x,adc,adcerr,view,strip,i1,i2)
      enddo
            
      end
      
      subroutine ecfitstrip(x,adc,adcerr,view,strip,i1,i2)
 
      real x(71),adc(1296),adcerr(1296),y(71),yerr(71)
      real m,b,me,be,lambda,errlam,ymaxfit,errym
      integer view,strip,i1,i2
      
      vector pixwidth
      vector ecFitadc_par(36,6,2)
      vector ecFitadc_errpar(36,6,2) 
      vector ecFitadc_ymaxfit(36,6,2)
      vector ecFitadc_atten(36,6,2)
      vector ecFitadc_chi2(36,6)
      
      call getpixels(view,strip,y,adc)
      call getpixels(view,strip,yerr,adcerr)
      
      if (strip.eq.1) then
        b = y(1)
        be = yerr(1)
        m = 0
        me = 0
        lambda = 0.
        errlam = 0.        
      else
        call linfit(x,y,yerr,i1,i2,b,be,m,me,r)
        wid = pixwidth(view)
        if (m.lt.0) then
          lambda = min(500.,abs(wid/m))
          errlam = max(0.,min(100.,abs(me*lambda/m)))
        else
          lambda = 500.
          errlam = 100.
        endif
      endif
      
      ymaxfit = exp(m+b)
      errym   = ymaxfit*sqrt(me*me+be*be)
      
      ecFitadc_par(strip,view,1) 	= b
      ecFitadc_par(strip,view,2) 	= m
      ecFitadc_errpar(strip,view,1) 	= be
      ecFitadc_errpar(strip,view,2) 	= me
      ecFitadc_ymaxfit(strip,view,1) 	= ymaxfit
      ecFitadc_ymaxfit(strip,view,2) 	= errym
      ecFitadc_atten(strip,view,1) 	= lambda
      ecFitadc_atten(strip,view,2) 	= errlam
      ecFitadc_chi2(strip,view)      	= r 
      
      end
            
      subroutine getpixels(view,strip,out,in)
      
      integer a,b,c,sum,pixel,view,strip,numpix
      real out(71),in(1296)
      
      numpix = 2*strip-1
      
      a = strip
      b = 37-a
      c = 36
      
      do j = 1,numpix
        if (view.eq.1) pixel=a*(a-1)+b-c+1
        if (view.eq.2) pixel=c*(c-1)+a-b+1
        if (view.eq.3) pixel=b*(b-1)+c-a+1
        if (view.eq.4) pixel=a*(a-1)+b-c+1
        if (view.eq.5) pixel=c*(c-1)+a-b+1
        if (view.eq.6) pixel=b*(b-1)+c-a+1
        out(j) = in(pixel)
        sum = a+b+c
        if(sum.eq.73) b=b+1
        if(sum.eq.74) c=c-1
      enddo
      
      end
      
      subroutine linfit(x,y,sigmay,i1,i2,a,sigmaa,b,sigmab,r)
      
      real sum,sumx,sumy,sumx2,sumxy,sumy2
      real x(71),y(71),sigmay(71),weight,delta
      
      sum = 0.
      sumx = 0.
      sumy = 0.
      sumx2 = 0.
      sumxy = 0.
      sumy2 = 0.
      
      a = 0.
      b = 0.
      
      do i = i1,i2
        xi = x(i)
        yi = y(i)
        if (yi.ne.0.and.sigmay(i).ne.0) then
        weight = 1./sigmay(i)**2
        sum = sum + weight
        sumx = sumx + weight*xi
        sumy = sumy + weight*yi
        sumx2 = sumx2 + weight*xi*xi
        sumxy = sumxy + weight*xi*yi
        sumy2 = sumy2 + weight*yi*yi
        endif
      enddo
      
      delta = sum*sumx2-sumx*sumx
      if (delta.eq.0) return
      a = (sumx2*sumy-sumx*sumxy)/delta
      b = (sumxy*sum-sumx*sumy)/delta
      sigmaa = sqrt(sumx2/delta)
      sigmab = sqrt(sum/delta)
      r = (sum*sumxy-sumx*sumy)/sqrt(delta*(sum*sumy2-sumy*sumy))
      
      end

      
      
            

