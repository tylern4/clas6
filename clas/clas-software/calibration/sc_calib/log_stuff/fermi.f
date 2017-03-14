        Real function fermi (x)
c
c      Returns fit to Fermi Function
c
c      fitfun = Fermi (centroid,slope,normalization)
c
c       p1 = centroid
c       p2 = slope
c       p3 = normalization
c       p4 = offset 
c
       implicit none
      
       real p1, p2, p3, p4
       real x,q,x_prime
       
c
       vector p(4)
c
c       write (6,100) ' parameters = ', p
c 100   format (a,3f10.3)
c
c
       p1 = p(1)
       p2 = p(2)
       p3 = p(3)  
       p4 = p(4)
c
      x_prime = x-p1 
      fermi = p3/(1+exp((abs(x_prime)-p4)/p2)) 
c
      end

	






































