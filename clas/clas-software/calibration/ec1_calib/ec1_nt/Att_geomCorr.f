      subroutine Att_GeomCorr
c
c-----------------------------------------------------------------------
c   Att. length correction: left =left *(exp(-(x)/lam)+alpha*exp(-(2L-x)/lam)
c                         : right=right*(exp(-(L-x)/lam)+alpha*exp(-(L+x)/lam)
c  
c   alpha = 0.45  Lx, Ly
c  
*  
* corrections:
*             att. length (two exp),
*             diff. thickness crossed,
*             diff. number of scintilator 
*
      IMPLICIT NONE
      INTEGER *4 i , sect
      real *4 La,Lx,Ly,coeff
      real *4 xx,yy,coefX,coefY,alpha,a0
      real *4 lam
      data alpha/0.45/
c  Geom. dimensions refers to mid. plane of LAC 
      data La/518.2/, Lx/421.6/, Ly/228.75/

*      SAVE
      include "bcs.inc"
*
      include "EC1nt.inc"
*
      include "ntpl_com.inc"
*
      DO sect=1,2
        IF(good_sect(sect)) then
        
          xx = Lx*id(sect,2)/40.
          yy = Ly*id(sect,1)/24.
          lam = lambda(sect,1,id(sect,1))
          a0    = exp(-Lx/lam)
          coefX = a0*(1+alpha**2+2.*alpha*a0*cosh((2*xx-Lx)/lam))
          coefX = 1./coefX
          lam = lambda(sect,2,id(sect,2))
          a0    = exp(-Ly/lam)
          coefY = a0*(1+alpha**2+2.*alpha*a0*cosh((2*yy-Ly)/lam))
          coefY = 1./coefY
          sumE(sect,1)  = sumE(sect,1)*sqrt(coefX)
          sumE(sect,2)  = sumE(sect,2)*sqrt(coefY)
c
c Inner short (Layer 2) has one scintilator more then others
c
          sumE(sect,2)  = sumE(sect,2)*8./9.  
c
c OUTER part (because can be idXi<>idXo!)
c
          xx = Lx*id(sect,4)/40.
          yy = Ly*id(sect,3)/24.
          lam = lambda(sect,3,id(sect,3))
          a0    = exp(-Lx/lam)
          coefX = a0*(1+alpha**2+2.*alpha*a0*cosh((2*xx-Lx)/lam))
          coefX = 1./coefX
          lam = lambda(sect,4,id(sect,4))
          a0    = exp(-Ly/lam)
          coefY = a0*(1+alpha**2+2.*alpha*a0*cosh((2*yy-Ly)/lam))
          coefY = 1./coefY
          sumE(sect,3)  = sumE(sect,3)*sqrt(coefX)
          sumE(sect,4)  = sumE(sect,4)*sqrt(coefY)
          tot_ADC_in (sect) = sumE(sect,1)+sumE(sect,2)
          tot_ADC_out(sect) = sumE(sect,3)+sumE(sect,4)
          
C   geometry correction (due to difference in thickness)
      
         coeff = La/sqrt(La*La+(Lx*(id(sect,2)-20.5)/40.)**2
     &           +(Ly*(id(sect,1)-0.5)/24.)**2)
     
c        print *,'coor',id(sect,1),id(sect,2),coeff
        
          DO i=1,4 
            sumE(sect,i) = sumE(sect,i)*coeff
          END DO
           
          tot_ADC_in (sect) =  tot_ADC_in (sect)*coeff
          tot_ADC_out(sect) =  tot_ADC_out(sect)*coeff

         END IF
       END DO
       return
       end
       
