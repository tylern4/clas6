       function vertex(skk,ic)
       implicit real*8(a-h,o-y)
       implicit complex*16(z)
       data amn/938.5d0/,api/138.5d0/,pi/3.1415926d0/
       vertex=0.
       if(ic.le.2)return
       if(ic.eq.3)then
c    
c      pi-N-Delta
c
c      bare vertex of Sato-Lee
c
       epi=sqrt(skk**2+api**2)
       en=sqrt(skk**2+amn**2)
       cut=649.
       f=2.049
       vertex=-f/api/sqrt((2.*pi)**3)*sqrt(4.*pi/3.)*skk
     1 /sqrt(2*epi)*sqrt((en+amn)/(2.*en))*(cut**2/(cut**2+skk**2))**2
c
       end if

       if(ic.eq.4)then
c
c     sigma-N
c
       g=0.755/sqrt(api)
       cut=0.522/197.32
       vertex=g/(1.+(cut*skk)**2) 
       end if

       if(ic.eq.5)then
c
c     rho-N
c
      g=0.6684/sqrt(api)
      cut=0.428/197.32
      vertex=g*(skk*cut)/(1.+(skk*cut)**2)**2
      end if
      return
      end

       function zvertex(zskk,ic)
       implicit real*8(a-h,o-y)
       implicit complex*16(z)
       data amn/938.5d0/,api/138.5d0/,pi/3.1415926d0/
       zvertex=0.
       if(ic.le.2)return
       if(ic.eq.3)then
c    
c      pi-N-Delta
c
c      bare vertex of Sato-Lee
c
       zepi=sqrt(zskk**2+api**2)
       zen=sqrt(zskk**2+amn**2)
       cut=649.
       f=2.049
       zvertex=-f/api/sqrt((2.*pi)**3)*sqrt(4.*pi/3.)*zskk
     1 /sqrt(2*zepi)*sqrt((zen+amn)/(2.*zen))
     1   *(cut**2/(cut**2+zskk**2))**2
       end if

       if(ic.eq.4)then
c
c     sigma-N
c
       g=0.755/sqrt(api)
       cut=0.522/197.32
       zvertex=g/(1.+(cut*zskk)**2) 
       end if

       if(ic.eq.5)then
c
c     rho-N
c
      g=0.6684/sqrt(api)
      cut=0.428/197.32
      zvertex=g*(zskk*cut)/(1.+(zskk*cut)**2)**2
      end if
      return
      end




