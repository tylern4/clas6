      SUBROUTINE BORN(S,Q2)
      
      IMPLICIT NONE
      SAVE

#include "ettapar.inc"
#include "gpar.inc"
#include "bornterms.inc"
#include "fit_par.inc"

      REAL S,Q2

      REAL GAMMF
      REAL d,d1,tau,f_1(2),f_2(2)
      REAL q1,z,Gmp,Gep,Gmn,Gen,F1p,F1n 
      REAL EF1(3),EF2(3),Re(6,3),Rm(6,3)
      REAL Rro(6,3),Rom(6,3)
      REAL Rro1(6,3),Rom1(6,3)
      REAL Rpi_re(6,3)
      REAL Rro_re(6,3)
      REAL Rom_re(6,3)
      REAL F_pion, F_dipole,gvec,gro,gro2,gom,gom2
      REAL gvec1,gro1,gro21,gom1,gom21
      REAL Lamdom,Lamdro,gvecom,gvecro
      REAL alpha_pi,Reggepi,Reggepi_re,Reggeb_re	
      REAL alpha_ro,Reggero,Reggero_re,Reggea_re	
      REAL sregge
      INTEGER i,j,k

c   Calculate Born term contribution (nucleon and pion poles)
c   into invariant amplitudes.
	d   = 1./(s-mn22)
	d1  = u-mn22
        tau = Q2/4./mn22
	q1  = sqrt(Q2)
	
c- Pion Formfactor
        F_pion = 1./(1.+Q2/0.54)
        
c- Dipole formula
        F_dipole = 1./(1.+Q2/0.71)**2
        
c-  z=F2p/F1p  from the data on Gep/Gmp: PRL 84(2000)1398
        z = 1.793/mn2/(1.+1.2*Q2/(1.+1.1*q1)+0.015*Q2+0.001*Q2*4)
        
c     Bosted parameterization of Gep
c     Phys. Rev. C 51, 409 (1995)
      Gmp = 1.+0.35*q1+2.44*q2+0.5*q1**3+1.04*q1**4+0.34*q1**5
      Gmp = 2.793/Gmp
      
c-  Proton Pauli Formfactors from  Bosted parameterization for Gmp
c-  and from the data on Gep/Gmp: PRL 84 (2000) 1398
        F1p = Gmp/(1.+mn2*z)
	F2p = z*F1p
	
c- Gen and Gmn from Kees de Jager JLAB-PHY-00-01
c	Gep = F_dipole
c	Gmp = 2.793*F_dipole
	Gen = 1.6*0.5*Q2/(1.+25.*Q2**2)
	Gmn = -1.913*F_dipole
	
c- Neutron Pauli Formfactors
	F1n = (Gen+tau*Gmn)/(1.+tau)
	F2n = (Gmn-Gen)/(1.+tau)/mn2
	
c- Isovector and Isoscalar Pauli Formfactors (1 is V, 2 is S). 
	F_1(1) = F1p-F1n
	F_1(2) = F1p+F1n
	F_2(1) = F2p-F2n
	F_2(2) = F2p+F2n
	
c-  This loop counts for isotopic amplitudes (+) and (-),
c-  which are determined by isovector form factors	
	do k=1,2
	  EF1(k) = F_1(1)
	  EF2(k) = F_2(1)
	end do
	
c-  The following are isotopic form factors (0) 
c-  which are equal to isoscalar form factors
	EF1(3) = F_1(2)
	EF2(3) = F_2(2)	
	
c-  This loop counts for isotopic states (+),(-),(0)	
	do k=1,3
	  Re(1,k) =  EF1(k)
	  Re(2,k) = -EF1(k)
	  Re(3,k) = -EF1(k)/2.
	  Re(4,k) = 0.
	  Re(5,k) = 0.
	  Re(6,k) = 0.
	  Rm(1,k) = mn2*EF2(k)
	  Rm(2,k) = 0.
	  Rm(3,k) = 0.
	  Rm(4,k) = EF2(k)
	  Rm(5,k) = -qk*EF2(k)
	  Rm(6,k) =  2.*EF2(k)
	end do
	
c- The coefficient 3.134 (microbarn)**(1/2) comes from
c- g*e/(8piGeV), where g**2/4pi=13.8, e**2/4pi=1/137.

	do i=1,6
	  do j=1,3
	    Re(i,j)=3.134*Re(i,j)*(d+ett(i,j)/d1)
	    Rm(i,j)=3.134*Rm(i,j)*(d+ett(i,j)/d1)
	  end do
	end do
	
c- Pion pole term	
		
c	F_pion = F_1(1)
	Re(3,2) = Re(3,2)-6.268*f_pion/(t-mp22)
c	The omega exchange contribution: Rw
	gvec = F_dipole
	
        Lamdom = 1.2
        Lamdro = 1.5
        
        gvecom = gvec*Lamdom**2/(Lamdom**2-t)/(t-0.612)
        gvecro = gvec*Lamdro**2/(Lamdro**2-t)/(t-0.591)
        
        gvec1 = gvec/(t-0.591)
	gro   =  0.75*gvecro   
	gro2  =   2.2*gvecro    
	gom   =  22.6*gvecom  
	gom2  = -6.44*gvecom  
	gro1  = 1.3*gvec1
	gro21 = 2.2*gvec1
	gom1  = 15.*gvec1
	gom21 = 0.
	
        Rro(1,3) = mn2*gro+t*gro2
        Rro(2,3) = 0.5*(Q2+mp22-t)*gro2
        Rro(3,3) = (u-s)*gro2/4. 
        Rro(4,3) = 0.
        Rro(5,3) = 0.
        Rro(6,3) = 2.*gro
        
        Rom(1,1) = mn2*gom+t*gom2
        Rom(2,1) = 0.5*(Q2+mp22-t)*gom2
        Rom(3,1) = (u-s)*gom2/4.
        Rom(4,1) = 0.
        Rom(5,1) = 0.
        Rom(6,1) = 2.*gom
        
        Rro1(1,3) = mn2*gro1+t*gro21
        Rro1(2,3) = 0.5*(Q2+mp22-t)*gro21
        Rro1(3,3) = (u-s)*gro21/4.
        Rro1(4,3) = 0.
        Rro1(5,3) = 0.
        Rro1(6,3) = 2.*gro1
        
        Rom1(1,1) = mn2*gom1+t*gom21
        Rom1(2,1) = 0.5*(Q2+mp22-t)*gom21
        Rom1(3,1) = (u-s)*gom21/4.
        Rom1(4,1) = 0.
        Rom1(5,1) = 0.
        Rom1(6,1) = 2.*gom1
        
	do i=1,6
          do j=1,2
            k=j+1.
            Rro(i,j)=0.
            Rom(i,k)=0.
            Rro1(i,j)=0.
            Rom1(i,k)=0.
          end do
        end do 
        
	sregge   = (s-u)/2. 
	
	alpha_pi = 0.7*(t-mp22)
	alpha_ro = 0.55+0.8*t
	
	Reggepi = 0.7*pi*(t-mp22)*sregge**alpha_pi
	Reggepi = Reggepi/(2.*sin(pi*alpha_pi)*GAMMF(1.+alpha_pi))
	Reggero = 0.8*pi*(t-0.591)*sregge**(alpha_ro-1.)
	Reggero = Reggero/(2.*sin(pi*alpha_ro)*GAMMF(alpha_ro))
	
	Reggepi_re =  (1.+cos(pi*alpha_pi))*Reggepi
	Reggeb_re  = (-1.+cos(pi*alpha_pi))*Reggepi
	Reggero_re = (-1.+cos(pi*alpha_ro))*Reggero
	Reggea_re  =  (1.+cos(pi*alpha_ro))*Reggero

        do i=1,6
          Rpi_re(i,1)=0.
          Rpi_re(i,2)=Re(i,2)*Reggepi_re
          Rpi_re(i,3)=Re(i,2)*Reggeb_re
        enddo
        
        do i=1,6
          Rro_re(i,1)=0.
          Rro_re(i,3)=Rro1(i,3)*Reggero_re
          Rro_re(i,2)=Rro1(i,3)*Reggea_re
        enddo
        
        do i=1,6
          Rom_re(i,1)=Rom1(i,1)*Reggero_re
          Rom_re(i,2)=0.
          Rom_re(i,3)=0.
        enddo
 	
	do i=1,6
	  do j=1,3
           br(1,i,j)=Rom(i,j)+Rro(i,j)+Re(i,j)+Rm(i,j)
           br(2,i,j)=Rpi_re(i,j)+Rom_re(i,j)+Rro_re(i,j)
	  enddo
	enddo
	
      RETURN
      END
