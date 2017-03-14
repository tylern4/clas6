      subroutine janr_input_res(resifile)
      
      implicit none
      save
      
#include "resonances.inc"
#include "fit_par.inc"
#include "gpar.inc"

      integer i,k,ii
      real resdat(10,MAXres)  
      character*60 resifile
      
      pi = 2.*acos(0.E0)
      a2 = sqrt(2.E0)
      mn = 0.93827
      mp = 0.13498
      
      meta	= 0.547
      m2pion	= 2.*mp
      mn22	= mn**2
      mp22	= mp**2
      mn2	= 2.*mn
      
      open(unit=24,file=resifile,type='old')
      read(24,*,end=77) ((resdat(i,k),i=1,10),k=1,MAXres)
77    close(24)

C-Resonances with (****),(***),(**) are included
c-(1)	P33(1232)
c-(2)	D33(1700)
c-(3)	F37(1950)
c-(4)	P33(1600)
c-(5)	S31(1620)
c-(6)	F35(1905)
c-(7)	D35(1930)
c-(8)	P31(1925) missing resonance
c-(9)	S31(1900)
c-(10)	P33(1920)+ missing resonance (1975)
c-(11)	P31(1910)
c-(12)	D33(1940)
c-(13)	F35(2000)
c-(21)	P11(1440)
c-(22) 	S11(1535)
c-(23) 	D13(1520)
c-(24) 	D13(1700)
c-(25) 	S11(1650)
c-(26) 	F15(1680)
c-(27) 	P13(1720)
c-(28) 	F17(1990)
c-(29) 	D15(1675)
c-(30)	P11(1710)
c-(31)	P13(1900)+3 missing resonances (1870),(1980),(1955)
c-(32)	D13(2080)
c-(33)	F15(2000)+ missing resonance (1955)

c Langul - photon ang. momenta in B-W 
c Lprime(i) is the degree of photon momenta in Breit-Wigner formula 
c for E(l+), E(l-):
c E(l+):     Lprime(i)=l 
c E(l-) l>1: Lprime(i)=l-2
c E(l-) l=1: Lprime(i)=l for E(l-)
      
      do i=1,MAXres
        ii         = resdat(1,i)
        ires(ii)   = resdat(2,i)
        jres(ii)   = resdat(3,i)
        lres(ii)   = resdat(4,i)
        mres(ii)   = resdat(5,i)/1000.
        gres(ii)   = resdat(6,i)/1000.
        eta(ii)    = resdat(7,i)
        xres(ii)   = resdat(8,i)
        langul(ii) = resdat(9,i)
        lprime(ii) = resdat(10,i)
      enddo
      
      end
      