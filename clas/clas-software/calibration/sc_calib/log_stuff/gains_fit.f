      real function gains_fit(x)
c
c     returns fit to Fermi Function
c
      real x,x_prime
c
      double precision A,B,C
      double precision fitpad(24),fitfun
c
      common /hcfitd/fitpad,fitfun
c
      A = fitpad(1)
      B = fitpad(2)
      C = fitpad(3)
      D = fitpad(4)
      
      fitfun = 0.
      x_prime = x-D
      fitfun = C/(1+exp(-(abs(x_prime)-A)/B)

c     fitfun = fitfun + fitpad(5)*exp(-x/fitpad(6)) + fitpad(7)
c     fitfun = fitfun + fitpad(5)+ fitpad(6)*x
c
      gains_fit = fitfun
c
      end






