      subroutine ffky_ic
      
      implicit none
      
#include "ffpar_ic.inc"

      call ffkey('ICCUTS',ic_cuts,5,'REAL')
      ic_cuts(1)=0.001
      ic_cuts(2)=0.001
      ic_cuts(3)=0.001
      ic_cuts(4)=0.001
      ic_cuts(5)=0.0001
      
      call ffkey('ICZ0',ic_z0,1,'REAL')
      ic_z0 = 20.0
      
      call ffkey('ICSPAR',ic_spar,1,'INTEGER')
      ic_spar = 10
      
      call ffkey('ICPOIS',ic_pois,1,'REAL')
      ic_pois = 3.5

      call ffkey('ICMOD',ic_mod,1,'INTEGER')
      ic_mod = 2
     
      call ffkey('ICFR',ic_front,3,'REAL')
      ic_front(1) = 0.
      ic_front(2) = 0.
      ic_front(3) = -11.
     
      return
      end
      

