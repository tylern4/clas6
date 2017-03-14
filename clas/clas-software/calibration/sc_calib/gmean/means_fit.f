      real function means_fit(x)
c
c     returns fit to landau + background
c
      real x,xp,lnc1s,arg
c
      real const(9), xlam(9), gam(9)
c
      data const /0.0368,0.0843,0.0882,0.0647,0.0359,0.0164,0.0064,
     1     0.0021,0.0006/
      data xlam /-1.48,-0.738,0.170,1.33,2.95,5.39,9.40,16.8,30.8/
      data gam /0.737,0.947,1.23,1.68,2.40,3.68,6.18,12.3,39.7/
c
      double precision A,B,C
      double precision fitpad(24),fitfun
c
      common /hcfitd/fitpad,fitfun
c
c     gaussian for testing
c
c      fitfun = fitpad(1)*exp(-0.5*((x-fitpad(2))/fitpad(3))**2)
c
c     approximation to Landau
c
c
      B = fitpad(2)
      C = fitpad(3)
      lnc1 = log(C**2+1)
      xlamp = -0.2570 + 0.3318*lnc1 + 0.02510*lnc1**2 - 0.001750*lnc1**3
      A = fitpad(1) - B*xlamp
      s = (x-A)/B
c      write (6,*) ' const=',const
c      write (6,*) ' xlam=',xlam
c      write (6,*) ' gam=',gam
      fitfun = 0.
      do j=1,9
         arg = (s-xlam(j))**2/(gam(j)**2+C**2)
         if (abs(arg) .lt. 100.) then
            fitfun = fitfun + 
     1         const(j)*gam(j)*exp(-arg)/sqrt(gam(j)**2+C**2)
         endif
      enddo
c
      fitfun = fitpad(4)*fitfun
c
c     include background
c
c      fitfun = fitfun + fitpad(5)*exp(-fitpad(6)*x)
c      fitfun = fitfun + fitpad(5)*exp(-fitpad(6)*x) + fitpad(7)
      fitfun = fitfun + fitpad(5)+ fitpad(6)*x
c
      means_fit = fitfun
c
      end
