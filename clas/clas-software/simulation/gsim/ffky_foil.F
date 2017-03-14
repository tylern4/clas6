      subroutine ffky_foil
      implicit none
      save
      
#include "ffpar_foil.inc"

c     ffread card BAFF used to position Pb shielding wedges
c     Use ffread card: BAFF nbaf th1 z1 th2 z2 th3 z3 
c     Parameters are nbaf: number of baffles (up to 3)
c                      th: scattering angle of top of wedge
c                       z: distance from target to center of wedge


      call ffkey('BAFFES',foil_baffles,7,'REAL')
      
      foil_baffles(1) = 0
      foil_baffles(2) = 0.
      foil_baffles(3) = 0.
      foil_baffles(4) = 0.
      foil_baffles(5) = 0.
      foil_baffles(6) = 0.
      foil_baffles(7) = 0.

c     Position of the UPST volume (x,y,z) 
c     [default: CLAS center] (S.Morrow 9-feb-01)

      upst_pos_ff(1) = 0.
      upst_pos_ff(2) = 0.
      upst_pos_ff(3) = 0.
      call ffkey('UPSTPOS',upst_pos_ff,3,'REAL')

c     Chose the type of scattering chamber {old one with RIBS (=1)
c     or new one made of low density foam (=2)}
c     [default: old one]

      chamber_type_ff = 1
      call ffkey('CHAMBER',chamber_type_ff,1,'INTEGER')

      RETURN
      END



