       Real function time_walk (x)
c
c      Time-walk correction function
c
       vector par(4)
       real x, xnorm, thresh, walk_max, xmax, af, b1, b2, b0
c
c      20 mv Threshold converted to channels (20*1.77)
c
       data thresh, adc_max /35., 600./
       data b1, b2, b0 /13.87, 0.074, 19.56 /   ! constants to be used in the constraint w2=g(w3)
c
       xnorm = x /thresh
        af = (b1*b2/par(3))*(b0**(par(3)-b2))                ! constraint w2=g(w3) or  [af=g(par(3))]
c
       if (xnorm .lt. par(4)) then
          time_walk = af/xnorm**par(3)  
       else
          time_walk = af*(1+par(3))/par(4)**par(3) 
     1       - af*par(3)*xnorm/par(4)**(par(3)+1)
       endif)
c
c       compute function relative to maximum ADC value adc_max
c
       xmax = adc_max/thresh
       if (xmax .lt. par(4)) then
          walk_max = af/xmax**par(3)
       else
          walk_max = af*(1+par(3))/par(4)**par(3) 
     1       - af*par(3)*xmax/par(4)**(par(3)+1)
       endif
c
c       print *,' xnorm=',xnorm,' af=',af,' walk_max=',walk_max,
c     1     ' time_walk=',time_walk
       time_walk = par(1) + time_walk - walk_max
c       print *, ' x=',x,' time_walk=',time_walk,' par=',par
       return
       end
