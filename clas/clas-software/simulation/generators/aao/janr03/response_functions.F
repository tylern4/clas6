      SUBROUTINE response_functions (s,azimut,epsilon)
      
      IMPLICIT NONE
      SAVE
      
#include "helicity.inc"
#include "res_func.inc"
#include "janrtab.inc"

      REAL s,azimut,mpion(2),mnuc,qpi,qkratio,epsilon,sinth
      INTEGER NWRITE
      INTEGER i,j
      REAL w

      mnuc=0.93827
      mpion(1)=0.13498
      mpion(2)=0.13957
      
      DO i=1,2
      
        qpi = sqrt(((s-mnuc**2+mpion(i)**2)/2.)**2/s-mpion(i)**2)
        qkratio = qpi/((s-mnuc**2)/2./sqrt(s))  
         
        sigmaT(i)=0.
        sigmaL(i)=0.
        sigmaTT(i)=0.
        sigmaTL(i)=0.
        sigmaTLP(i)=0.
        sigmaTy(i)=0.
        sigmaLy(i)=0.
        sigmaTTx(i)=0.
        sigmaTTy(i)=0.
        sigmaTTz(i)=0.
        sigmaTLx(i)=0.
        sigmaTLy(i)=0.
        sigmaTLz(i)=0.
        sigmaTTPx(i)=0.
        sigmaTTPz(i)=0.
        sigmaTLPx(i)=0.
        sigmaTLPy(i)=0.
        sigmaTLPz(i)=0.
        
        Do j=1,2
c j=1 for real parts
c j=2 for imaginary parts
          sigmaT(i) = sigmaT(i) + 
     +                H1(i,j)**2.+ H2(i,j)**2.+ H3(i,j)**2.+ H4(i,j)**2.
          sigmaL(i) = sigmaL(i) + H5(i,j)**2. + H6(i,j)**2.
          sigmaTT(i) = sigmaTT(i)-H4(i,j)*H1(i,j)+H3(i,j)*H2(i,j)
          sigmaTL(i) = sigmaTL(i)+H5(i,j)*(H1(i,j)-H4(i,j))+ 
     -                 H6(i,j) * ( H2(i,j) + H3(i,j) )
          sigmaTLPx(i)= sigmaTLPx(i)+
     1                  H5(i,j)*(-H3(i,j)+H2(i,j))+H6(i,j)*(H1(i,j)+H4(i,j))
          sigmaTLPy(i)= sigmaTLPy(i)-
     1                  H5(i,j)*( H3(i,j)+H2(i,j))+H6(i,j)*(H1(i,j)-H4(i,j))
          sigmaTLPz(i)= sigmaTLPz(i)+
     1                  H5(i,j)*( H1(i,j)+H4(i,j))+H6(i,j)*(H3(i,j)-H2(i,j))
          sigmaTTPx(i)= sigmaTTPx(i)+
     1                  H1(i,j)*H2(i,j)+H3(i,j)*H4(i,j)
          sigmaTTPz(i)= sigmaTTPz(i)+
     1                  H1(i,j)**2.- H2(i,j)**2.+ H3(i,j)**2.- H4(i,j)**2.
        EndDo
        
          sigmaTLP(i) = sigmaTLP(i)+
     1        H5(i,2)*(H1(i,1)-H4(i,1))- H5(i,1)*(H1(i,2)-H4(i,2))+
     1        H6(i,2)*(H2(i,1)+H3(i,1))- H6(i,1)*(H2(i,2)+H3(i,2))
          sigmaTy(i)  = sigmaTy(i)
     1         +H1(i,2)*H2(i,1)-H1(i,1)*H2(i,2)
     1         +H3(i,2)*H4(i,1)-H3(i,1)*H4(i,2)
          sigmaLy(i)  = sigmaLy(i)
     1                  +H5(i,2)*H6(i,1)-H5(i,1)*H6(i,2)
          sigmaTLx(i) = sigmaTLx(i)+
     1         H5(i,1)*(H3(i,2)-H2(i,2))-H5(i,2)*(H3(i,1)-H2(i,1))
     1         -H6(i,1)*(H1(i,2)+H4(i,2))+H6(i,2)*(H1(i,1)+H4(i,1))
          sigmaTLy(i) = sigmaTLy(i)
     1                 -H5(i,1)*(H3(i,2)+H2(i,2))+H5(i,2)*(H3(i,1)+H2(i,1))
     1                 +H6(i,1)*(H1(i,2)-H4(i,2))-H6(i,2)*(H1(i,1)-H4(i,1))
          sigmaTLz(i) = sigmaTLz(i)
     1                 -H5(i,1)*(H1(i,2)+H4(i,2))+H5(i,2)*(H1(i,1)+H4(i,1))
     1                 +H6(i,1)*(H2(i,2)-H3(i,2))-H6(i,2)*(H2(i,1)-H3(i,1))
          sigmaTTx(i) = sigmaTTx(i)
     1                 +H1(i,1)*H3(i,2)-H1(i,2)*H3(i,1)
     1                 -H2(i,1)*H4(i,2)+H2(i,2)*H4(i,1)
          sigmaTTy(i) = sigmaTTy(i)
     1                 -H1(i,1)*H3(i,2)+H1(i,2)*H3(i,1)
     1                 -H2(i,1)*H4(i,2)+H2(i,2)*H4(i,1)
          sigmaTTz(i) = sigmaTTz(i)
     1                 -H1(i,1)*H4(i,2)+H1(i,2)*H4(i,1)
     1                 -H2(i,1)*H3(i,2)+H2(i,2)*H3(i,1)

      sigmaT(i)    = sigmaT(i)    * qkratio / 2.
      sigmaL(i)    = sigmaL(i)    * qkratio
      sigmaTT(i)   = sigmaTT(i)   * qkratio
      sigmaTL(i)   = sigmaTL(i)   * qkratio / sqrt(2.)
      sigmaTLP(i)  = sigmaTLP(i)  * qkratio / sqrt(2.)
      sigmaTy(i)   = sigmaTy(i)   * qkratio
      sigmaLy(i)   = sigmaLy(i)   * qkratio * 2.
      sigmaTLx(i)  = sigmaTLx(i)  * qkratio / sqrt(2.)
      sigmaTLy(i)  = sigmaTLy(i)  * qkratio / sqrt(2.)
      sigmaTLz(i)  = sigmaTLz(i)  * qkratio / sqrt(2.)
      sigmaTTx(i)  = sigmaTTx(i)  * qkratio 
      sigmaTTy(i)  = sigmaTTy(i)  * qkratio 
      sigmaTTz(i)  = sigmaTTz(i)  * qkratio 
      sigmaTLPx(i) = sigmaTLPx(i) * qkratio / sqrt(2.)
      sigmaTLPy(i) = sigmaTLPy(i) * qkratio / sqrt(2.)
      sigmaTLPz(i) = sigmaTLPz(i) * qkratio / sqrt(2.)
      sigmaTTPx(i) = sigmaTTPx(i) * qkratio 
      sigmaTTPz(i) = sigmaTTPz(i) * qkratio / 2. 

      sigma(i) = sigmaT(i) + epsilon * sigmaL(i) + 
     + epsilon * sigmaTT(i) * cos(2. * azimut) + sqrt( 2. * epsilon *
     *(1. + epsilon)) * sigmaTL(i) * cos( azimut )
     
      ENDDO
      RETURN
      END


