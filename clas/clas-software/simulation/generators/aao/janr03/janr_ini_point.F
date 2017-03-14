      subroutine janr_ini_point(Q2)
      IMPLICIT NONE
      SAVE

#include "gpar.inc"
#include "resonances.inc"
#include "amplitudes.inc"
#include "fit_par.inc"

      REAL Q2
      INTEGER i,ierr
      REAL a12,a32,s12,gm
      real cc(3)
      
      data cc/-0.57735,0.,1.2247/

c     For I=3/2 AM3=M(3/2)
c     For I=1/2 AM1(1,i)=M(1/2)/3.+M(0)
c     For I=1/2 AM1(2,i)=-M(1/2)/3.+M(0)
C     cMi,cEi,cSi are fitting parameters for multipoles with I=3/2
C     cMPi,cEPi,cSPi are fitting parameters for multipoles pA(1/2)

      call janr_ini_hel(q2)
      
C P11(1440) M1-

      a32 = 0.
      a12 = cc(ires(21))*aa1(21)*cmp1
      s12 = cc(ires(21))*sa1(21)*csp1      
      call atomm(lres(21),a12,a32,s12,am1(1,1),ae1(1,1),as1(1,1))
      
C S11(1535) E0+
  
      a32= 0.
      a12 = cc(ires(22))*aa1(22)*cep2
      s12 = cc(ires(22))*sa1(22)*csp2 
      call atomp(lres(22),a12,a32,s12,am1(1,2),ae1(1,2),as1(1,2))
      
C D13(1520) M2- E2-
      
      a32 = cc(ires(23))*aa3(23)*cep3
      a12 = cc(ires(23))*aa1(23)*cmp3
      s12 = cc(ires(23))*sa1(23)*csp3     
      call atomm(lres(23),a12,a32,s12,am1(1,3),ae1(1,3),as1(1,3))
      
C S11(1650) E0+

      a32 = 0.
      a12 = cc(ires(25))*aa1(25)*cep4
      s12 = cc(ires(25))*sa1(25)*csp4      
      call atomp(lres(25),a12,a32,s12,am1(1,5),ae1(1,5),as1(1,5))
      
C S31(1620)  E0+
      
      a32 = 0.
      a12 = cc(ires(5))*aa1(5)*cep5
      s12 = cc(ires(5))*sa1(5)*csp5      
      call atomp(lres(5),a12,a32,s12,am3(5),ae3(5),as3(5))
      
C F15(1680) M3- E3- 
      
      a32 = cc(ires(26))*aa3(26)*cep6
      a12 = cc(ires(26))*aa1(26)*cmp6
      s12 = cc(ires(26))*sa1(26)*csp6     
      call atomm(lres(26),a12,a32,s12,am1(1,6),ae1(1,6),as1(1,6))
     
C D33(1700) M2- E2-
      
      a32 = cc(ires(2))*aa3(2)*cep7
      a12 = cc(ires(2))*aa1(2)*cmp7
      s12 = cc(ires(2))*sa1(2)*csp7   
      call atomm(lres(2),a12,a32,s12,am3(2),ae3(2),as3(2))
      
C D13(1700) M2- E2-
 
      a32 = cc(ires(24))*aa3(24) 
      a12 = cc(ires(24))*aa1(24) 
      s12 = cc(ires(24))*sa1(24)    
      call atomm(lres(24),a12,a32,s12,am1(1,4),ae1(1,4),as1(1,4))
	
C P11(1710) M1-

      a32 = cc(ires(30))*aa3(30) 
      a12 = cc(ires(30))*aa1(30) 
      s12 = cc(ires(30))*sa1(30)    
      call atomm(lres(30),a12,a32,s12,am1(1,10),ae1(1,10),as1(1,10))
      
C P13(1720) M1+ E1+
      
      a32 = cc(ires(27))*aa3(27)*cep8
      a12 = cc(ires(27))*aa1(27)*cmp8
      s12 = cc(ires(27))*sa1(27)*csp8     
      call atomp(lres(27),a12,a32,s12,am1(1,7),ae1(1,7),as1(1,7))
      
C D15(1675) M2+ E2+

      a32 = cc(ires(29))*aa3(29)*cep9 
      a12 = cc(ires(29))*aa1(29)*cmp9
      s12 = cc(ires(29))*sa1(29)    
      call atomp(lres(29),a12,a32,s12,am1(1,9),ae1(1,9),as1(1,9))
      
C P33(1600) M1+ E1+

      a32 = cc(ires(4))*aa3(4) 
      a12 = cc(ires(4))*aa1(4) 
      s12 = cc(ires(4))*sa1(4)    
      call atomp(lres(4),a12,a32,s12,am3(4),ae3(4),as3(4))
      
C P33(1232) M1+ E1+
C     Tiator parametrization for G_M
      gm     = exp(-0.21*Q2)/((1.+Q2/0.71)**2/3.)
      resM   = cm1*gm*(sqrt(((2.3933+Q2)/2.46)**2-0.88)*6.568*1.013)
      am3(1) = resM
      ae3(1) = -ce1*am3(1)*0.025
      as3(1) = -cs1*am3(1)*0.05
      
C F35(1905) M3- E3- 
      a32 = cc(ires(6))*aa3(6)*cep10 
      a12 = cc(ires(6))*aa1(6)*cmp10
      s12 = cc(ires(6))*sa1(6)
      call atomm(lres(6),a12,a32,s12,am3(6),ae3(6),as3(6))
      
C F37(1950) M3+ E3+
      a32 = cc(ires(3))*aa3(3)*cep11 
      a12 = cc(ires(3))*aa1(3)*cmp11
      s12 = cc(ires(3))*sa1(3)
      call atomp(lres(3),a12,a32,s12,am3(3),ae3(3),as3(3))
      
C F17(1990)
      am1(1,8)=0.
      ae1(1,8)=0.
      as1(1,8)=0.
    
      do i=7,13
        am3(i)=0.
        ae3(i)=0.
        as3(i)=0.
      enddo
      
      do i=11,13
        am1(1,i)=0.
        ae1(1,i)=0.
        as1(1,i)=0.
      enddo
      
      do i=1,13
        am1(2,i)=0.
        ae1(2,i)=0.
        as1(2,i)=0.
      enddo
      
      return
      end
      
      
      subroutine janr_ini_hel(q2)   
      IMPLICIT NONE
      SAVE
      
#include "amplitudes.inc"

      integer i
      real q2
      
      do i=1,50
        aa3(i)=0.0
        aa1(i)=0.0
        sa1(i)=0.0
      enddo
      
c P11(1440) 
      aa3(21) = 0.0
      aa1(21) = -58*(1-0.78*q2-3.08*q2**2-0.0021*q2**3)*exp(-1.24*q2)
      sa1(21) =  27*(1+4.55*q2-2.25*q2**2+0.5100*q2**3)*exp(-1.01*q2)
C S11(1535)
      aa3(22) = 0.0
      aa1(22) =  91*(1+1.55*q2-0.47*q2**2+0.61*q2**3)*exp(-1.16*q2)
      sa1(22) = -30*(1-1.34*q2+3.17*q2**2-0.50*q2**3)*exp(-0.99*q2)
C D13(1520) 
      aa1(23) = -28*(1+8*q2-3.89*q2**2+1.15*q2**3)*exp(-0.96*q2)
      aa3(23) = 146*(1-0.42*q2+3.5*q2**2-0.0089*q2**3)*exp(-2.5*q2)
      sa1(23) = -5*(1+84*q2-97*q2**2+32*q2**3)*exp(-2.1*q2)
C S11(1650) 
      aa3(25) = 0.0
      aa1(25) = 22.0-15.*q2 
      sa1(25) = 10.0              
C S31(1620)  
      aa3(5)  = 0.0
      aa1(5)  = 49.6-15.*q2 
      sa1(5)  = -28.         
C F15(1680)  
      aa1(26) = -17*(1+15.7*q2-19.7*q2**2+14.3*q2**3)*exp(-1.93*q2)
      aa3(26) = 133*(1+1.06*q2-1.42*q2**2+1.37*q2**3)*exp(-2.0*q2)
      sa1(26) = -4*(1+28*q2-18.8*q2**2+4.7*q2**3)*exp(-1.1*q2)
C D33(1700) 
      aa1(2)  = 125.4-92.*q2 
      aa3(2)  = 105.0-77.*q2 
      sa1(2)  = 10.0              
C D13(1700) 
      aa1(24) = 0.
      aa3(24) = 0.
      sa1(24) = 0.
C P11(1710)
      aa1(30) = 0.
      aa3(30) = 0.
      sa1(30) = 0.
C P13(1720) 
      aa1(27) =  96.6+57.*q2 
      aa3(27) = -39.0-61.*q2 
      sa1(27) =  10.             
C D15(1675) 
      aa1(29) = 18.
      aa3(29) = 21.
      sa1(29) = 0.
C P33(1600) 
      aa1(4)  = 0.
      aa3(4)  = 0.
      sa1(4)  = 0.
C F35(1905) 
      aa1(6)  =  21.3 
      aa3(6)  = -45.6 
      sa1(6)  = 0.0              

C F37(1955) 
      aa1(3)  =  10.0
      aa3(3)  = -10.0 
      sa1(3)  = 0.0         
           
      return
      end
      
c Tiator nucl-th/0610113
     
      subroutine mptoa(l,m,e,s,a12,a32,s12)
      real e,m,s,a12,a32,s12,l
      a12 = -0.5*((l+2)*e+l*m)
      a32 =  0.5*sqrt(l*(l+2))*(e-m)
      s12 = -(l+1)*s/sqrt(2.)      
      return
      end
      
      subroutine mmtoa(ll,m,e,s,a12,a32,s12)
      real e,m,s,a12,a32,s12,l,ll     
      l=ll-1
      a12 =  0.5*((l+2)*m-l*e)
      a32 = -0.5*sqrt(l*(l+2))*(e+m)
      s12 = -(l+1)*s/sqrt(2.)
      return
      end
      
      subroutine atomp(l,a12,a32,s12,m,e,s)
      real e,m,s,a12,a32,s12,l
      if (l.gt.0) then
        aa32 = a32/sqrt(l*(l+2))
        m=-2/(2*l+2)*(aa32*(l+2)+a12)
      else
        aa32=0
        m=0
      endif
      e= 2/(2*l+2)*(aa32*l-a12)
      s=-sqrt(2.)*s12/(l+1)
      return
      end
      
      subroutine atomm(ll,a12,a32,s12,m,e,s)
      real e,m,s,a12,a32,s12,l,ll
      l=ll-1
      if (l.gt.0) then
        aa32 = a32/sqrt(l*(l+2))
        e=-2/(2*l+2)*(aa32*(l+2)+a12)
      else
        aa32 = 0
        e=0
      endif
      m=-2/(2*l+2)*(aa32*l-a12)
      s=-sqrt(2.)*s12/(l+1)
      return
      end
            




