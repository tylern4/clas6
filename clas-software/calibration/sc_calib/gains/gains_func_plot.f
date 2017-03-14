        Real function gains_func_plot (x1)
c
c      Energy Loss function
c      Returns fit to landau + background 
c       Approx to Landau : NIM 174 (1980) 531-533.
c
c      fitfun = Landau (peak,scale,res,normalization,const,linear)
c
c       p1 = Landau peak
c       p2 = scale factor
c       p3 = resolution
c       p4 = normalization
c       p5 = constant background term
c       p6 = linear background term
c
       implicit none
       real const(9), xlam(9), gam(9)
       real lnc1,arg
       Real A,B,C,s,xlamp
       integer j
       real x1
c
       vector p(7)

       Real x,fitfun
c
      data const /0.0368,0.0843,0.0882,0.0647,0.0359,0.0164,0.0064,
     1     0.0021,0.0006/
      data xlam /-1.48,-0.738,0.170,1.33,2.95,5.39,9.40,16.8,30.8/
      data gam /0.737,0.947,1.23,1.68,2.40,3.68,6.18,12.3,39.7/
c
c       keep x as single precision
c
       x = x1
c
      B = p(2)
      C = p(3)
      lnc1 = log(C**2+1)
      xlamp = -0.2570 + 0.3318*lnc1 + 0.02510*lnc1**2 - 0.001750*lnc1**3
      A = p(1) - B*xlamp
      s = (x-A)/B
c      write (6,*) ' const=',const
c      write (6,*) ' xlam=',xlam
c      write (6,*) ' gam=',gam
      fitfun = 0.
      do j=1,9
         arg = (s-xlam(j))**2/(gam(j)**2+C**2)
         if (abs(arg) .lt. 300.) then
            fitfun = fitfun + 
     1         const(j)*gam(j)*exp(-arg)/sqrt(gam(j)**2+C**2)
         endif
      enddo
c
      fitfun = p(4)*fitfun
c
c     include background
c
      fitfun = fitfun + p(7) + p(5)*exp(-x/p(6)) 
c
      gains_func_plot = fitfun
c
      end

	







