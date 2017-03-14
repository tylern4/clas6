      subroutine wjc(Q2,W2,csthcm,phicm,th0,ichoice,e_hel,
     >    sigma_0,sigma_t,sigma_l,sigma_tt,sigma_lt,sigma_ltp,t_s)
c****************************************************************
c								*
c	Theoretical calculation for				*
c								*
c	e + p --> e' + K + Y					*
c								*
c	e  = incident electron					*
c	p  = proton target					*
c	e' = scattered electron					*
c	K  = Kaon						*
c	Y  = Lambda or Sigma					*
c								*
c	Authors:   Robert Williams				*
c		   Chueng-Ryong Ji				*
c		   Stephen R. Cotanch				*
c								*
c	Reference: Phys. Rev. Lett., 63, 1352 (1991)		*
c		   Phys. Rev., C46, 1617 (1992)			*
c		   R. Williams, private comm.			*
c								*
c								*
c	wjc.f program:	Paul Gueye				*
c			Hampton University			*
c			January 97				*
c								*
c	July 97: modification to take into account the		*
c	36 polarized observables. Used a new version of		*
c	the code where the expressions are analytically		*
c	expressed as a function of the CGLN amplitudes		*
c	(f1 to f6). We are not calculating the Hamiltonian	*
c	anymore !						*
c								*
c	The different options are:				*
c	1/ Choice of the response function as output:		*
c	-> Done with R_i(ampl)					*
c								*
c	2/ Choice of the diagrams that contribute in the	*
c	calculation: 1 = excluded , 0 = included		*
c	-> Done with FLAGON(diag)				*
c								*
c	Reference: Photo and electroproduction of eta mesons	*
c		   Phys. Rev ????				*
c		   G. Knochlein, D. Drechsel and L. Tiator	*
c								*
c								*
c ==> July 17, 97: P. Gueye, L. Hoffman				*
c ==> Add X(2.000 ; 2.050) in the code				*
c								*
c								*
c ==> December 8, 97: P. Gueye, R. Williams			*
c ==> Version of the code which is correctly normalized to	*
c ==> real photon data (see Phys. Rev. C, 43, #2, p.453)	*
c								*
c****************************************************************
c
c
c ==> Declaration variables
c
      implicit	none
      common /flag/flag_nu_cm2	
	  character*80	hyperon
	  integer		ichoice
	  integer		j, k, flagx, k_max
      integer       flag_nu_cm2	
	  double precision	 	qsq,s,t,Q0,QV,K0,KV
      double precision        Q0L
	  double precision		phi,eps,pi
	  double precision		flagon(15),dsdw,sigL,sigT,sigP,sigI,sigIP
	  double precision		MP,MK,MY,M_lam,M_lam14,M_sig ,ang,x,w,cf
	  double precision		epsl,theta_e
	  double precision	    cc(15)

      real          Q2,W2,csthcm,phicm,th0,
     >              sigma_0,sigma_t,sigma_l,sigma_tt,
     >              sigma_lt,sigma_ltp,t_s
      integer       e_hel,ifirst
      data ifirst/0/
      SAVE
c     
c ==> Constants
c
      if(ifirst.eq.0) then
         pi = dacos(-1.d0) 
         MP=.9382796d0
         MK=0.49367d0
         M_lam=1.1156d0
         M_lam14=1.405d0
         M_sig= 1.19255d0
         CF = 389.3796d0
c
c ==> Open files 
c
         open(unit=1, status='unknown', file='kine.input')
c
c ==> Read input file  
c
         do j=1,15
            read(1,*)flagon(j)
         enddo
         read(1,*)flagx
         close(1)
c     
c ==> Choice for the Hyperon:
c	1 = Lambda: then read file cclam.in for coupling constants
c	2 = Sigma:  then read file ccsig.in for coupling constants
c	3 = Lambda(1405):  then read file cc1405.in for coupling constants
c
         if (ichoice.eq.1) goto 10
         if (ichoice.eq.2) goto 20
         if (ichoice.eq.3) goto 21
         
 10      open(unit=3, status='unknown', file='cclam.in')
         read(3,*)k_max
         do k=1,k_max
            read(3,*)cc(k)
         enddo
         close(3)
         hyperon = 'Lambda'
         MY=M_lam
         goto 25
	
 20      open(unit=4, status='unknown', file='ccsig.in')
         read(4,*)k_max
         do k=1,k_max
            read(4,*)cc(k)
         enddo
         close(4)
         hyperon = 'Sigma'
         MY=M_sig
         goto 25

 21      open(unit=7, status='unknown', file='cc1405.in')
         read(7,*)k_max
         do k=1,k_max
            read(7,*)cc(k)
         enddo
         close(7)
         hyperon = 'Lambda(1405)'
         MY=M_lam14
 25      ifirst = 1
      endif

      s   = dble(W2)
	  qsq = dble(-Q2)
	  X   = dble(csthcm)
	  if (X .ge.  1.0d0) x = +1.0d0
      if (X .le. -1.0d0) x = -1.0d0
	  ANG = DACOS(X)
	  phi = dble(phicm*pi/180.0d0)
	  theta_e = dble(th0)
      W = dsqrt(s)

      Q0 = (S + QSQ - MP**2)/2.d0/W
      if(Q0.le.0.0d0) flag_nu_cm2 = 1
      Q0L = (S - QSQ - MP**2)/2.d0/MP
      EPS= 1. /(1+2.0*(1-Q0L*Q0L/qsq)*dtan(0.5d0*theta_e)**2)

      K0 = (S - MY**2 + MK**2)/2.d0/W
	  QV = dSQRT(S**2-2.d0*(MP**2+QSQ)*S + (MP**2-QSQ)**2 )/2.d0/W
	  KV = dSQRT(S**2-2.d0*(MY**2+MK**2)*S+(MY**2-MK**2)**2 )/2.d0/W 
	  T = 2.d0*QV*KV*X - 2.d0*Q0*K0 + QSQ + MK**2
      t_s = sngl(T)
	  EPSL = (-QSQ/Q0**2)*EPS
      call elfunc(ichoice,flagx,cc,flagon,s,t,qsq,eps,phi,
     >	   e_hel,dsdw,sigT,sigL,sigP,sigI,sigIP)
c
      sigma_0   = sngl(dsdw)
	  sigma_t   = sngl(sigT)
C Need this to get the cross section into a form expected by kaon_norad.F
	  sigma_l   = sngl(sigL*(-QSQ/Q0**2))
	  sigma_tt  = sngl(sigP*dsin(ang)**2)
	  sigma_lt  = sngl(2.0d0*sigI*dsqrt(-QSQ/Q0**2)*dsin(ang))
	  sigma_ltp = sngl(2.0d0*sigIp*dsqrt(-QSQ/Q0**2)*dsin(ang))
c
      return
      end


      SUBROUTINE ELFUNC(ICHOICE,FLAGX,CC,F,S,T,QSQ,EPS,PHI,
     >		e_hel,DSDW,sigT,sigL,sigP,sigI,sigIP)

	  IMPLICIT DOUBLE PRECISION (A-H,K-Z)
	  DOUBLE COMPLEX K(4),L(4),P(4),Q(4) 
	  DOUBLE COMPLEX KL(4),LL(4),PL(4),QL(4)
	  DOUBLE COMPLEX UR,UI,E1(4),E2(4),CGLN(6),Z(6)
	  DOUBLE PRECISION CC(15),F(15),OBS(36)
	  integer e_hel
      INTEGER FLAGX
      integer ifirst/0/

      save PI, MP, ML, MY1, MK, MSIG, MKST, ME, CF, IFIRST

      if (ifirst.eq.0) then
         PI=DACOS(-1.D0)
         MP=.9382796D0
         ML=1.1156D0
         MY1=1.405D0
         MK=0.49367D0
         MSIG= 1.19255D0
         MKST=0.8921D0
         ME =0.000511D0
c     ME = 0.D0
         CF = 389.3796D0
c     Z = 0.D0
         ifirst = 1
      endif
  

      MY = ML*DEL(ICHOICE,1) + MSIG*DEL(ICHOICE,2)
     >                             + MY1*DEL(ICHOICE,3)

	  UR=(1.D0,0.D0)
	  UI=(0.D0,1.D0)	

	  U = QSQ+MP**2+MY**2+MK**2-S-T
	  W = DSQRT(S)
	  Q0 = (S + QSQ - MP**2)/2.D0/W
	  K0 = (S - MY**2 + MK**2)/2.D0/W
	  EP = (S - QSQ + MP**2)/2.D0/W
	  EL = (S + MY**2 - MK**2)/2.D0/W
	  QV = DSQRT(S**2-2.D0*(MP**2+QSQ)*S + (MP**2-QSQ)**2 )/2.D0/W
	  KV = DSQRT(S**2-2.D0*(MY**2+MK**2)*S+(MY**2-MK**2)**2 )/2.D0/W
	  KL0 = (MK**2 + MP**2 - U)/2.D0/MP
	  LL0 = (MP**2 + MY**2 - T)/2.D0/MP
      QL0 = (S - MP**2 - QSQ)/2.D0/MP
	  KLV = DSQRT(KL0**2 - MK**2)
	  QLV = DSQRT(QL0**2 - QSQ)
	  QDK = 0.5D0*(MK**2 + QSQ - T)
	  EPSL = (-QSQ/Q0**2)*EPS
	  C = T - QSQ - MK**2 + 2.D0*Q0*K0
	  X = C/(2.D0*QV*KV)
	  if (X .ge.  1.0d0) x = +1.0d0
      if (X .le. -1.0d0)  x= -1.0d0
	  ANG = DACOS(X)
	  COLANG = X*(QV*KV/(QLV*KLV))*(QL0*KL0-QDK)/(Q0*K0-QDK)
	  if (colang .gt. 1.d0) colang=1.d0
	  LANG = DACOS(COLANG)
C	C.M. HADRONIC 4-VECTORS K,L,P

	  K(1)=KV*DSIN(ANG)*DCOS(PHI)*UR
	  K(2)=KV*DSIN(ANG)*DSIN(PHI)*UR
	  K(3)=KV*DCOS(ANG)*UR
	  K(4)=K0*UR

	  L(1)=-K(1)
	  L(2)=-K(2)
	  L(3)=-K(3)
	  L(4)=EL*UR

	  P(1)=0.D0*UR
	  P(2)=0.D0*UR
	  P(3)=-QV*UR
	  P(4)=EP*UR

C	C.M. PHOTON 4-VECTOR Q(MU):

	  Q(1)=0.D0*UR
	  Q(2)=0.D0*UR
	  Q(3)=QV*UR
	  Q(4)=Q0*UR

	  QDK2= DOT(Q,K)

	  X1 = DOT(K,L)
	  X2 = DOT(P,L)
	  X3 = DOT(Q,L)
	  X4 = DOT(P,P)

C	LAB PHOTON 4-VECTOR QL(MU):

	  QL(4) = QL0*UR
	  QL(3) = QLV*UR
	  QL(2) = 0.D0*UR
	  QL(1) = 0.D0*UR

C	LAB HADRONIC 4-VECTORS KL,LL,PL:

	  KL(4) = (MK**2 + MP**2 - U)/2.D0/MP*UR
	  KL(3) = KLV*DCOS(LANG)*UR
	  KL(2) = KLV*DSIN(LANG)*DSIN(PHI)*UR
	  KL(1) = KLV*DSIN(LANG)*DCOS(PHI)*UR

	  LL(4) = (MP**2 + MY**2 - T)/2.D0/MP*UR
	  LL(3) = (QLV - KLV*DCOS(LANG))*UR
	  LL(2) = - KL(2)
	  LL(1) = - KL(1)

	  PL(4) = MP*UR
	  PL(3) = 0.D0*UR
	  PL(2) = 0.D0*UR
	  PL(1) = 0.D0*UR

	  HAFANG = DATAN( DSQRT(QSQ*(EPS-1.D0)/2.D0/EPS)/QLV )

C 	LEPTONIC 4-VECTORS E1,E2 IN LAB FRAME

	  E1(4)=.5D0*QL(4)*(1.D0 +
     >         CDSQRT( 1.D0-QSQ/((QL(4)*DSIN(HAFANG))**2) ) )*UR
	  E1(3)= (QL(3)**2 - QL(4)*(QL(4)-2.D0*E1(4)))/2.D0/QL(3)*UR
	  E1(2)= 0.D0*UR
	  E1(1)= CDSQRT(E1(4)**2-E1(3)**2-ME**2)*UR

	  E2(1)= E1(1) - QL(1)
	  E2(2)= E1(2) - QL(2)
	  E2(3)= E1(3) - QL(3)
	  E2(4)= E1(4) - QL(4)


C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C	HYPERAMPB:  
c	SUBROUTINE CALL FOR INVARIANT AMPLITUDES Z(J) AND
C	CGLN AMPLITUDES CGLN(J)
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	
	CALL HYPERAMPB(ICHOICE,FLAGX,CC,F,S,T,U,QSQ,Z,CGLN)

C	OBSERVABLES IN TERMS OF THE CGLN AMPLITUDES:
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

      f11 = CGLN(1)*dconjg(CGLN(1)) 
      f12 = 0.5*( dconjg(CGLN(1))*CGLN(2) + dconjg(CGLN(2))*CGLN(1) )
	  f13 = 0.5*( dconjg(CGLN(1))*CGLN(3) + dconjg(CGLN(3))*CGLN(1) )
	  f14 = 0.5*( dconjg(CGLN(1))*CGLN(4) + dconjg(CGLN(4))*CGLN(1) )
	  f15 = 0.5*( dconjg(CGLN(1))*CGLN(5) + dconjg(CGLN(5))*CGLN(1) )
	  f16 = 0.5*( dconjg(CGLN(1))*CGLN(6) + dconjg(CGLN(6))*CGLN(1) )
	  f22 = CGLN(2)*dconjg(CGLN(2))
	  f23 = 0.5*( dconjg(CGLN(2))*CGLN(3) + dconjg(CGLN(3))*CGLN(2) )
	  f24 = 0.5*( dconjg(CGLN(2))*CGLN(4) + dconjg(CGLN(4))*CGLN(2) )
	  f25 = 0.5*( dconjg(CGLN(2))*CGLN(5) + dconjg(CGLN(5))*CGLN(2) )
	  f26 = 0.5*( dconjg(CGLN(2))*CGLN(6) + dconjg(CGLN(6))*CGLN(2) )
	  f33 = CGLN(3)*dconjg(CGLN(3))
	  f34 = 0.5*( dconjg(CGLN(3))*CGLN(4) + dconjg(CGLN(4))*CGLN(3) )
	  f35 = 0.5*( dconjg(CGLN(3))*CGLN(5) + dconjg(CGLN(5))*CGLN(3) )
	  f36 = 0.5*( dconjg(CGLN(3))*CGLN(6) + dconjg(CGLN(6))*CGLN(3) )
 	  f44 = CGLN(4)*dconjg(CGLN(4)) 
 	  f45 = 0.5*( dconjg(CGLN(4))*CGLN(5) + dconjg(CGLN(5))*CGLN(4) )
 	  f46 = 0.5*( dconjg(CGLN(4))*CGLN(6) + dconjg(CGLN(6))*CGLN(4) )
	  f55 = CGLN(5)*dconjg(CGLN(5))                        
      f56 = 0.5*( dconjg(CGLN(5))*CGLN(6) + dconjg(CGLN(6))*CGLN(5) )	
	  f66 = CGLN(6)*dconjg(CGLN(6))
	
	  f21 = f12
	  f31 = f13
	  f41 = f14
	  f51 = f15
	  f61 = f16	
	  f32 = f23 
	  f42 = f24
	  f52 = f25
	  f62 = f26
	  f43 = f34
	  f53 = f35
	  f63 = f36	
	  f54 = f45
	  f64 = f46	
      f65 = f56       
     
      OBS(1) = ( f11 + f22 + 0.5*dsin(ang)**2*(f33 + f44)
     >           + dsin(ang)**2*(f23 + f14 + dcos(ang)*f34)
     >           - 2.*dcos(ang)*f12 ) 

c%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
c	Unpolarized transverse cross section sigT
     
      sigT = CF*(KV/QV)*OBS(1)
c%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        
      OBS(2) = (dsin(ang)*(f13 - f24 + dcos(ang)*(f14 - f23) 
     >            - dsin(ang)**2*f34))  
     	
      OBS(3) = (dsin(ang)*(-2.*f12 - f13 + f24 + dcos(ang)* 
     >           (f23 - f14) + dsin(ang)**2*f34)) 
   
      OBS(4) = ( dsin(ang)**2*(-f13 - f24 - f34 - 
     >           0.5*dcos(ang)*(f33 + f44)) )   
                  	
      OBS(5) = dsin(ang)*(f11 - f22 + 0.5*dsin(ang)**2*(f44-f33) 
     >            - f23 + f14 + dcos(ang)*(f13 - f24))  
                
      OBS(6) = (dsin(ang)*(-f23 + f14 + dcos(ang)*(f13 - f24)
     >           + 0.5*dsin(ang)**2*(f44 - f33)))
     
      OBS(7) = (2.*f12 - dcos(ang)*(f11 + f22) 
     >            + dsin(ang)**2*(f13 + f24 + f34)
     >            + 0.5*dcos(ang)*dsin(ang)**2*(f33 + f44))
     
                 
      OBS(8) = (f55 + f66 + 2.*dcos(ang)*f56)   
   
c%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
c	Unpolarized longitudinal cross section sigL

	  sigL = CF*(KV/QV)*OBS(8)
	
c%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	   
        
      OBS(9) = -2.*dsin(ang)*f56
        
      OBS(10) = (-2.*f56 - dcos(ang)*(f55 + f66))
        
      OBS(11) = dsin(ang)*(f66 - f55)
        
      OBS(12) = dsin(ang)*(-f25 - f35 - f16 - f46 - 
     >             dcos(ang)*(f45 + f36))

c%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
c	OBS(12) Corresponds to unpolarized interference cross section 'sigI'

	  sigI = CF*(KV/QV)*(-f25-f35-f16-f46-dcos(ang)*(f45+f36)) 
	
c%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	
        
      OBS(13) = (-f15 + f26 + dcos(ang)*(f25 - f16))
        
      OBS(14) = (-f15 + f26 + dcos(ang)*(f25 - f16) + 
     >              dsin(ang)**2*(f36 - f45))
    
      OBS(15) = dsin(ang)*(f25 + f16)
        
      OBS(16) = (-f25 + f16 + dcos(ang)*(f15 - f26)) 
        
      OBS(17)  = dsin(ang)*(f15 + f26)
        
      OBS(18) = dsin(ang)*( f15 + f45 + f26 + f36 
     >             + dcos(ang)*(f35 + f46) )
    
      OBS(19) = (f25 - f16 + dcos(ang)*(f26 - f15) + dsin(ang)**2*
     >             (f35 - f46))
    
      OBS(20) = 0.5*dsin(ang)**2*(f33 + f44) + dsin(ang)**2* 
     >             (f23 + f14 + dcos(ang)*f34)  
 
c%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
c	OBS(20): Corresponds to unpolarized transverse cross section 'sigP'        
    
        
      sigP = CF*(KV/QV)*(0.5d0*(f33+f44)+f23+f14+dcos(ang)*f34 )				

c%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%        
             
        
      OBS(21) =  dsin(ang)*(2*f12 + f13 - f24 + dcos(ang)*(f14 - f23))
        
      OBS(22) = -dsin(ang)**2*(f23 + f14)
        
      OBS(23) = dsin(ang)*(f23 - f14 + dcos(ang)*(f24- f13))
        
      OBS(24) = -dsin(ang)**2*(f13 + f24)
        
      OBS(25) = -dsin(ang)*( f25 + f35 + f16 + f46 
     >            + dcos(ang)*(f45 + f36) )
c%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
c	OBS(20): Corresponds to unpolarized transverse cross section 'sigIP'        
            
      sigIP = -CF*(KV/QV)*( f25 + f35 + f16 + f46 
     >            + dcos(ang)*(f45 + f36) )
c%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%  
           
      OBS(26) = (-f15 + f26 + dcos(ang)*(f25 - f16)) 
        
      OBS(27) = ( f15 - f26 + dcos(ang)*(f16 - f25) 
     >             + dsin(ang)**2*(f45 - f36) )

      OBS(28) = dsin(ang)*(f25 + f16)
       
      OBS(29) = (-f25 + f16 + dcos(ang)*(f15- f26))
        
      OBS(30) = dsin(ang)*(f15 + f26)
        
      OBS(31) = dsin(ang)*( f15 + f45 + f26 + f36 + dcos(ang) + 
     >            dcos(ang)*(f35 + f46) )
         
      OBS(32) = ( f25 - f16 + dcos(ang)*(-f15 + f26) 
     >        + dsin(ang)**2*(f35 - f46) ) 
  
      OBS(33) = dsin(ang)*(f13 - f24 + dcos(ang)*(-f23 + f14))
        
      OBS(34) = -f11 - f22 + 2*dcos(ang)*(
     >             f12 - dsin(ang)**2*(f23 + f14)  )
                           
      OBS(35) = dsin(ang)*(-f11 + f22 + f23 - f14 
     >             + dcos(ang)*(f24 - f13) )
  
      OBS(36) = -2.*f12 + dcos(ang)*(f11 + f22) 
     >             - dsin(ang)**2*(f13 + f24)

      DSDW = sigT + EPSL*sigL
     >		+ EPS*sigP*dsin(ang)**2*dcos(2.d0*PHI)
     >		+ dsqrt(2.d0*EPSL*(EPS+1.d0))*sigI*dsin(ang)*dcos(PHI)
     >      + e_hel*dsqrt(2.d0*EPSL*(-EPS+1.d0))*
     >        sigIP*dsin(ang)*dsin(PHI) 
c      print *, sigT, sigL, sigP, sigI, sigIP 

	RETURN
	END



        FUNCTION DEL(I1,I2)

        DOUBLE PRECISION DEL
        INTEGER I1,I2

        IF (I1.EQ.I2) THEN
        DEL = 1.D0
        END IF

        IF (I1.NE.I2) THEN
        DEL = 0.D0
        END IF

        RETURN
        END



	FUNCTION DOT(V1,V2)

	DOUBLE PRECISION DOT
	DOUBLE COMPLEX UR,UI,V1(4),V2(4)
	
	UR = (1.d0,0.d0)
	UI = (0.d0,1.d0)
	
	DOT = dimag(UI*(V1(4)*V2(4)-V1(1)*V2(1)-V1(2)*V2(2)-V1(3)*V2(3)))
	
	RETURN
	END



	SUBROUTINE FORMFACT(QSQ,ECP,ECK,GP,GK,GKST,UP,UL,US,USL)
 
	IMPLICIT DOUBLE PRECISION (A-H,K-Z)
	DOUBLE COMPLEX UR,UI,ECP,ECK,GP,GK,GKST,UP,UL,US,USL
	DOUBLE COMPLEX delr,delphi,delw,delr1,delr2
        DOUBLE COMPLEX delw1,delw2,delphi1

c	COMMON /MASS/ ME,MP,ML,MK,MSIG,MY1,MKST,WKST	
c	COMMON /EMAG/ UP0,UL0,US0,USL0
c	COMMON /FFPARM/ zp,wp,ap,bp,cp,dp

	UR = (1.d0,0.d0)
	UI = (0.d0,1.d0)

        MP = 0.9382796D0
        ML = 1.1156D0
        MK = 0.49367D0
        MSIG = 1.19246D0

	MPI = 0.139560d0
	PI = DACOS(-1.D0)
	ALP = 1.D0/137.036D0
	E0 = DSQRT(4.D0*PI*ALP)
	U0 = E0/(2.d0*MP)
	UP0 = 1.793D0*(E0/(2.D0*MP)) 
	UL0 = -0.6138D0*(E0/(2.D0*MP))
 	US0 = 0.81D0*(E0/(2.D0*MP))
	USL0 =   1.613D0*(E0/(2.D0*MP))
c	UL0 = -0.6138D0*(E0/(2.D0*ML))
c 	US0 = 0.81D0*(E0/(2.D0*MSIG))
c	USL0 =   1.613D0*(E0/(MSIG+ML))

C	THOM'S VALUES (USED BY SHIAWN'S SET I & II)
c	UL0 = - 0.841D0*(E0/(2.D0*MP))
c	USL0 = - 2.0D0*(E0/(MSIG+ML))


	zp = 1.680
        wp = 0.150
        ap = 0.50
        bp = 0.17
        cp = 0.77
        dp = 0.63


c	XSQ = (0.77D0)**2
	XSQ = (0.842615D0)**2
c	XSQ = (1.02D0)**2
c	XSQ = (0.95D0)**2

c	YSQ = (0.77D0)**2
c 	YSQ = (0.95D0)**2
	YSQ = (1.02D0)**2
c	YSQ = (1.5D0)**2

c	ZSQ = (1.02)**2
c	ZSQ = (1.51D0)**2
c	ZSQ = (1.68D0)**2

c      	GP = 1.D0/(1.D0 - QSQ/XSQ)**2
c	GP = 1.d0/(1.d0 - qsq/xsq)

c	GK = 1.D0/(1.D0 - QSQ/YSQ)
c	GKST = -QSQ/(ZSQ - QSQ)
c	GKST = ZSQ/(ZSQ - QSQ)

c	ECP = GP*(E0 - (2.D0*MP*UP0)*QSQ/((2.D0*MP)**2 - QSQ) )
c	ECK = E0*GK
c	UP = UP0*GP/(1.D0 - QSQ/((2.D0*MP)**2))
c	UL = UL0*GP/(1.D0 - QSQ/((2.D0*ML)**2))
c        US = US0*GP/(1.D0 - QSQ/((2.D0*MSIG)**2))
c	USL = USL0*GP/(1.D0 - QSQ/((ML+MSIG)**2))	


c	xsq = (0.842615d0)**2
c	ysq = (1.02d0)**2
c	zsq = ysq

	zsq = (zp)**2

c	ap & bp are mixing parameters for the VMD K and K* formfactors.
c	-1 < ap, bp < +1

	PSNORM = 1.d0/(ap + dsqrt(1.d0-ap**2) )
	VNORM = 1.d0/(bp + dsqrt(1.d0-bp**2) )

	gp = (xsq/(xsq - qsq))**2	
c	gk =  psnorm*( dsqrt(1.d0-ap**2)*ysq/(ysq - qsq) 
c     >                 + ap*zsq/(zsq - qsq) )

c______________________________________________________________________________
c	Felicetti and Srivastava Kaon Form Factor (Phys. Lett. 107B, 227 (1981).
c______________________________________________________________________________

	mr = 0.76830d0	
	mr1 = 1.270d0
	mr2 = 1.540d0
	mw = 0.781950d0
	mw1 = 1.390d0
	mw2 = 1.660d0
	mphi = 1.0194120d0
	mphi1 = 1.500d0
	mphist = 1.680d0

	wr = 0.14910d0
	wr1 = 0.110d0
	wr2 = 0.220d0
	ww = 0.00843d0
	ww1 = 0.050d0
	ww2 = 0.050d0
	wphi = 0.0041d0
	wphi1 = 0.040d0
	wphist = 0.150d0

	CPHI = 0.330d0
	CR = 0.50d0
	CW = 1.d0 - CR - CPHI
	CR1 = -0.0350d0
	CR2 = -0.0350d0
	CW1 = -0.0150d0
	CW2 = -0.0150d0
	CPHI1 = -0.020d0
	CNORM = 1.0d0/(CR+CW+CPHI+CR1+CR2+CW1+CW2+CPHI1)

	delr = mr**2 - qsq - UI*mr*wr*(qsq/mr**2)
	delr1 = mr1**2 - qsq - UI*mr1*wr1*(qsq/mr1**2)
	delr2 = mr2**2 - qsq - UI*mr2*wr2*(qsq/mr2**2)
	delw = mw**2 - qsq - UI*mw*ww*(qsq/mw**2)
	delw1= mw1**2 - qsq - UI*mw1*ww1*(qsq/mw1**2)
	delw2= mw2**2 - qsq - UI*mw2*ww2*(qsq/mw2**2)
	delphi = mphi**2 - qsq - UI*mphi*wphi*(qsq/mphi**2)
	delphi1= mphi1**2 - qsq - UI*mphi1*wphi1*(qsq/mphi1**2)
	delphist = mphist**2 - qsq - UI*mphist*wphist*(qsq/mphist**2)

	gk = CNORM*( CR*(mr**2)/delr + CW*(mw**2)/delw 
     >     + CPHI*(mphi**2)/delphi + CR1*(MR1**2)/delr1 
     >     + CR2*(MR2**2)/delr2 + CW1*(MW1**2)/delw1 
     >     + CW2*(MW2**2)/delw2
     >     + CPHI1*(MPHI1**2)/delphi1 )
c__________________________________________________________________________
c	Gounaris-Sakuria (Non-Modified rho propagator) Restricted VMD Kaon FF

	RKK = 0.50d0
	WKK = 0.170d0	
	PHIKK = 0.33d0
	qs0 = (2.d0*MPI)**2

c	CRHO = mr*wr*(mr/dsqrt(-qsq))*( 
c     >         ((qs0-qsq)/(qs0-mr**2))**(1.5) )
c	DRHO = (1.d0/pi)*CRHO*dlog( (-qsq
c	delrho = (p + q*qsq + DRHO)*UR + CRHO*UI

	delrho = delr
c	gk = PHIKK*(mphi**2)/delphi + WKK*(mw**2)/delw
c     >     + RKK*(mr**2)/delrho

c___________________________________________________________________________
c	Restricted VMD Nucleon FF (Non-modified rho propagator)
c	Gari-Krumpelmann Z. Phys. A. 322, 689(1985)

c	p1 = 0.80d0
	p1 = 0.70d0
c	f0 = (p1**2)/(p1**2-qsq)

	f0 = 1.d0 + qsq/delr

	skappa = -0.120d0
	vkappa = 3.7060d0
	wkappa = 0.1630d0
	rkappa = 6.620d0

	COMEGA = 0.4110d0
	CRHO = 0.3770d0

	gep = f0*( 0.50d0*(1.0d0+skappa*qsq/4.d0/MP**2)
     >           + 0.50d0*(1.0d0+vkappa*qsq/4.d0/MP**2)
     >  + 0.50d0*COMEGA*(qsq/delw)*(1.d0+wkappa*qsq/4.d0/MP**2)
     >  + 0.50d0*CRHO*(qsq/delr)*(1.d0+rkappa*qsq/4.d0/MP**2) )
	
        gmp = f0*( 0.50d0*(1.0d0+skappa)
     >           + 0.50d0*(1.0d0+vkappa)
     >  + 0.50d0*COMEGA*(qsq/delw)*(1.d0+wkappa)
     >  + 0.50d0*CRHO*(qsq/delr)*(1.d0+rkappa) )

c___________________________________________________________________________
        zsq = (zp)**2
 	
c	gr = -0.5d0
c	gw = -0.17d0
c	gphi = -0.33d0

	gr = ap
	gphi = cp
	gw = gr - gphi
c	gphist = gr - gphi - gw

	gr = 0.5
	gw = 0.17
	gphi = 0.77
	gphist = 0.63

	gr = ap
	gw = bp
	gphi = cp
	gphist = dp

	gam = 1.d0 - gr - gw - gphi - gphist

	gkst = f0*( 1.0d0
     >       + gr*qsq/delr + gphi*qsq/delphi 
     >       + gw*qsq/delw + gphist*qsq/delphist )
      

	gkst = f0*gam + gr*(mr**2)/delr + gphi*(mphi**2)/delphi
     >       + gw*(mw**2)/delw + gphist*(mphist**2)/delphist 


c	gk = 0.5d0*(mr**2)/delr + 0.33d0*(mphi**2)/delphi
c     >       + 0.17d0*(mw**2)/delw
 
	xsq = (0.8430d0)**2
	gp = (xsq/(xsq - qsq))**2
	
	ecp = gep*e0
	eck = gk*e0
	up = (gmp-1.d0)*(u0)
	ul = ul0*(gmp-1.d0)*(u0/up0)
	us = us0*(gmp-1.d0)*(u0/up0)
	usl = usl0*(gmp-1.d0)*(u0/up0)

	RETURN
	END


      SUBROUTINE HYPERAMPB(ICHOICE,FLAGX,CC,F,S,T,U,QSQ,AMP,CGLN)

      IMPLICIT DOUBLE PRECISION (A-H,K-Z)
      DOUBLE PRECISION CC(15),F(15),A(15)
      DOUBLE COMPLEX B(6),AMP(6),CGLN(6),CG(6),UR,UI
      DOUBLE COMPLEX ECP,ECK,GP,GK,GKST,UP,UL,US,USL
      DOUBLE COMPLEX DENOMKST,DENOMK1,DENOMY1,DENOMY2,DENOMY3
      DOUBLE COMPLEX DENOMN1,DENOMN4,DENOMN6
      DOUBLE COMPLEX DENOMDEL1,DENOMDEL2,DENOMDEL3

      double complex DENOMMX1, DENOMMX2, RX1, RX2, RX
      double precision MX2000, MX2050, WX2000, WX2050
      integer j,flagX

c	ICHOICE := hyperon switch (1=Lambda, 2=Sigma, 3=Lambda(1405))
c	CC(J) := effective coupling constants corresponding to
C     gL (Lambda)     = CC(1) (proton/kaon and ICHOICE graphs) 
C     ** Note: CC(1) always corresponds to the ICHOICE hyperon
C     gS/L (Sigma/Lambda)    = CC(2) 
C     GV (K* Vector)  = CC(3) 
C     GT (K* Tensor)  = CC(4)
C     GV1 (K1 Vector) = CC(5)
C     GT1 (K1 Tensor) = CC(6)
C     GY1 (Lam(1405))   = CC(7)
C     GY2 (Lam(1670))   = CC(8)
C     GY3 (Lam(1800))   = CC(9)
C     GN1 (N*(1440))    = CC(10)
C     GN4 (N*(1650))    = CC(11)
C     GN6 (N*(1710))    = CC(12)
c 	S,T,U := Mandelstam variables
c	QSQ := photon 4-momentum squared     	 
c	AMP(J) := invariant amplitudes (B(J); J=1,2,...6) 
c       corresponding to the bilinear
c	covariant (script N) matrices defined in WJC PRC46, 1617 (1992).
c
c ==> July 17, 97: P. Gueye, L. Hoffman
c ==> Add X(2.000 ; 2.050) in the code
c
        UR = (1.D0,0.D0)
        UI = (0.D0,1.D0)

       PI = DACOS(-1.D0)
       CF = 389.386D0
       ALP= (1.D0/137.036D0)
       EC = DSQRT(4.D0*PI*ALP)
       
       W = dsqrt(S)


      MP = 0.9382796D0
      ML = 1.1156D0
      MK = 0.49367D0
      MSIG = 1.19246D0
      MKST = 0.89210D0
      MK1 = 1.280D0
      M  = 1.000D0
      MY1 = 1.405D0
      MY2 = 1.670D0
      MY3 = 1.800D0
      MN1 = 1.440D0
      MN4 = 1.650D0
      MN6 = 1.710D0
      MDEL1=1.62D0
      MDEL2=1.90D0
      MDEL3=1.91D0
      
      MX2000 = 1.996d0
      MX2050 = 2.052d0
  
      MY = ML*DEL(ICHOICE,1) + MSIG*DEL(ICHOICE,2)
     >     + MY1*DEL(ICHOICE,3)


      E0 = DSQRT(4.D0*PI*ALP)
      UB = E0/(2.d0*MP)
      UP0 = 1.793D0*(E0/(2.D0*MP))
      UL0 = -0.6138D0*(E0/(2.D0*MP))
      US0 =   0.81D0*(E0/(2.D0*MP))
      USL0 =  1.613D0*(E0/(2.D0*MP))
        
      RKst = 2.d0*Mp/(MP+MY)
      RK1 = 2.d0*Mp/(MP+MY)
      RY1 = 2.d0*Mp/(MY1+MY)
      RY2 = 2.d0*Mp/(MY2+MY)
      RY3 = 2.d0*Mp/(MY3+MY)
      RN1 = 2.d0*Mp/(MN1+MP)
      RN4 = 2.d0*Mp/(MN4+MP)
      RN6 = 2.d0*Mp/(MN6+MP)
      RD1 = 2.d0*Mp/(MDEL1+MP)
      RD2 = 2.d0*Mp/(MDEL2+MP)
      RD3 = 2.d0*MP/(MDEL3+MP)
        

      WKST= 0.0511D0
      WK1 = .090D0
      WY1 = 0.040D0
      WY2 = 0.040D0
      WY3 = 0.300D0
      WN4 = 0.150D0
      WN6 = 0.120D0
c      WN6 = 0.09d0
      WN1 = 0.200D0
      WDEL1=0.14D0
      WDEL2=0.15D0
      WDEL3=0.22D0


      DENOMKST = (T-MKST**2)*UR + UI*MKST*WKST
      DENOMK1 = (T-MK1**2)*UR + UI*MK1*WK1
      DENOMY1 = (U-MY1**2)*UR + UI*MY1*WY1
      DENOMY2 = (U-MY2**2)*UR + UI*MY2*WY2
      DENOMY3 = (U-MY3**2)*UR + UI*MY3*WY3
      DENOMN1 = (S-MN1**2)*UR + UI*MN1*WN1
      DENOMN4 = (S-MN4**2)*UR + UI*MN4*WN4
      DENOMN6 = (S-MN6**2)*UR + UI*MN6*WN6
      DENOMDEL1 = (S-MDEL1**2)*UR + UI*MDEL1*WDEL1
      DENOMDEL2 = (S-MDEL2**2)*UR + UI*MDEL2*WDEL2
      DENOMDEL3 = (S-MDEL3**2)*UR + UI*MDEL3*WDEL3

      DENOMMX1 = (S - MX2000**2 + UI*W*WX2000)
      DENOMMX2 = (S - MX2050**2 + UI*W*WX2050)

      CALL FORMFACT(QSQ,ECP,ECK,GP,GK,GKST,UP,UL,US,USL)

      do 10 J=1,15
         A(J) = CC(J)
 10   continue

C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	
      IF (ICHOICE.EQ.1) THEN
         
         B(1) =  F(1)*A(1)*(ECP+2.D0*MP*UP)/(S-MP**2)
     >          + F(3)*2.D0*ML*UL*A(1)/(U-ML**2)
     >          + F(4)*USL*(ML+MSIG)*A(2)/(U-MSIG**2)
     >          + F(5)*GKST*A(3)*(ML+MP)/M/DENOMKST
     >          + RKst*F(5)*GKST*A(4)*T/(2.d0*MP)/M/DENOMKST
     >          + RY1*F(7)*(UL/UL0)*UB*A(7)*(MY1-ML)/DENOMY1
     >          + RY2*F(8)*(UL/UL0)*UB*A(8)*(MY2-ML)/DENOMY2
     >        + RY3*F(9)*(UL/UL0)*UB*A(9)*(MY3-ML)/DENOMY3
     >    + RN1*F(10)*(UP/UP0)*UB*A(10)*(MN1 + MP)/DENOMN1
     >    + RN4*F(11)*(UP/UP0)*UB*A(11)*(MN4 - MP)/DENOMN4
     >    + RN6*F(12)*(UP/UP0)*UB*A(12)*(MN6 + MP)/DENOMN6
     
c        B(2) = 2.d0*A(1)*( F(1)*ECP/(S-MP**2) + F(2)*ECK/(T-MK**2) )
c     >         + F(5)*0.5d0*A(4)*GKST*(QSQ + ML**2 - U)/M/MP/DENOMKST
c     >         - F(6)*0.5d0*A(6)*GKST*(QSQ + ML**2 - U)/M/MP/DENOMK1


 	B(2) = 2.d0*A(1)*F(2)*ECK*(QSQ + ML**2 - U)/(S-Mp**2)/(T-Mk**2)       
     >       + F(5)*A(4)*GKST*(QSQ + ML**2 - U)/M/(MP+ML)/DENOMKST
     >       - F(6)*A(6)*GKST*(QSQ + ML**2 - U)/M/(MP+ML)/DENOMK1
     
        B(3) = -F(2)*2.d0*A(1)*ECK/(T-MK**2)
     >       - F(5)*A(4)*GKST*(S - QSQ - MP**2)/M/(MP+ML)/DENOMKST
     >       + F(6)*A(6)*GKST*(S - QSQ - MP**2)/M/(MP+ML)/DENOMK1

     
     	B(4) = F(1)*2.d0*UP*A(1)/(S-MP**2)   
     >       + F(5)*GKST*( A(3) - A(4)*(ML-MP)/(MP+ML) )/M/DENOMKST
     >       + F(6)*GKST*( A(5) + A(6)*(ML-MP)/(MP+ML) )/M/DENOMK1
     >       + RN1*F(10)*2.d0*(UP/UP0)*UB*A(10)/DENOMN1
     >       - RN4*F(11)*2.d0*(UP/UP0)*UB*A(11)/DENOMN4
     >       + RN6*F(12)*2.d0*(UP/UP0)*UB*A(12)/DENOMN6
     
     	B(5) = F(3)*2.d0*UL*A(1)/(U - ML**2) 
     >      + F(4)*2.d0*USL*A(2)/(U - MSIG**2) 
     >      + F(5)*GKST*( A(3) + A(4)*(ML-MP)/(MP+ML) )/M/DENOMKST
     >      - F(6)*GKST*( A(5) + A(6)*(ML-MP)/(MP+ML) )/M/DENOMK1   
     >       - RY1*F(7)*(UL/UL0)*2.d0*UB*A(7)/DENOMY1
     >       - RY2*F(8)*(UL/UL0)*2.d0*UB*A(8)/DENOMY2
     >       - RY3*F(9)*(UL/UL0)*2.d0*UB*A(9)/DENOMY3	
     
     	B(6) = -F(1)*UP*A(1)/(S-MP**2) + F(3)*UL*A(1)/(U-ML**2) 
     >          + F(4)*USL*A(2)/(U-MSIG**2)
     >          - RY1*F(7)*(UL/UL0)*UB*A(7)/DENOMY1  
     >          - RY2*F(8)*(UL/UL0)*UB*A(8)/DENOMY2
     >          - RY3*F(9)*(UL/UL0)*UB*A(9)/DENOMY3
     >          - RN1*F(10)*(UP/UP0)*UB*A(10)/DENOMN1
     >          + RN4*F(11)*(UP/UP0)*UB*A(11)/DENOMN4
     >          - RN6*F(12)*(UP/UP0)*UB*A(12)/DENOMN6
	
	
        END IF
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

        IF (ICHOICE.EQ.2) THEN

        B(1) =  F(1)*A(1)*(ECP+2.D0*MP*UP)/(S-MP**2)
     >  + F(3)*(ML+MSIG)*USL*A(2)/(U-ML**2)
     >  + F(4)*(US*(MSIG+MSIG))*A(1)/(U-MSIG**2)
     >  +  F(5)*GKST*A(3)*(MSIG+MP)/M/DENOMKST
     >  + RKst*F(5)*GKST*A(4)*T/(2.d0*MP)/M/DENOMKST
     >  + RY1*F(7)*(UL/UL0)*UB*A(7)*(MY1-MSIG)/DENOMY1
     >  + RY2*F(8)*(UL/UL0)*UB*A(8)*(MY2-MSIG)/DENOMY2
     >  + RY3*F(9)*(UL/UL0)*UB*A(9)*(MY3-MSIG)/DENOMY3
     >  + RN1*F(10)*(UP/UP0)*UB*A(10)*(MN1 + MP)/DENOMN1
     >  + RN4*F(11)*(UP/UP0)*UB*A(11)*(MN4 - MP)/DENOMN4
     >  + RN6*F(12)*(UP/UP0)*UB*A(12)*(MN6 + MP)/DENOMN6
     >  + RD1*F(13)*(UP/UP0)*UB*A(13)*(MDEL1-MP)/DENOMDEL1
     >  + RD2*F(14)*(UP/UP0)*UB*A(14)*(MDEL2-MP)/DENOMDEL2
     >  + RD3*F(15)*(UP/UP0)*UB*A(15)*(MDEL3+MP)/DENOMDEL3

c       B(2) = 2.d0*A(1)*( F(1)*ECP/(S-MP**2) + F(2)*ECK/(T-MK**2) )
        
   	B(2) = 2.d0*A(1)*F(2)*ECK*(QSQ+MSIG**2-U)/(S-Mp**2)/(T-Mk**2)       
     >  + F(5)*A(4)*GKST*(QSQ + MSIG**2 - U)/M/(MP+MSIG)/DENOMKST
     >  - F(6)*A(6)*GKST*(QSQ + MSIG**2 - U)/M/(MP+MSIG)/DENOMK1

        B(3) =  -F(2)*2.d0*A(1)*ECK/(T-MK**2)
     >  - F(5)*A(4)*GKST*(S - QSQ - MP**2)/M/(MP+MSIG)/DENOMKST
     >  + F(6)*A(6)*GKST*(S - QSQ - MP**2)/M/(MP+MSIG)/DENOMK1
  
     	B(4) = F(1)*2.d0*UP*A(1)/(S-MP**2)   
     >  + F(5)*GKST*( A(3) - A(4)*(MSIG-MP)/(MP+MSIG) )/M/DENOMKST
     >  + F(6)*GKST*( A(5) + A(6)*(MSIG-MP)/(MP+MSIG) )/M/DENOMK1
     >  + RN1*F(10)*2.d0*(UP/UP0)*UB*A(10)/DENOMN1
     >  - RN4*F(11)*2.d0*(UP/UP0)*UB*A(11)/DENOMN4
     >  + RN6*F(12)*2.d0*(UP/UP0)*UB*A(12)/DENOMN6
     >	- RD1*F(13)*2.d0*(UP/UP0)*UB*A(13)/DENOMDEL1
     >  - RD2*F(14)*2.d0*(UP/UP0)*UB*A(14)/DENOMDEL2
     >  + RD3*F(15)*2.d0*(UP/UP0)*UB*A(15)/DENOMDEL3     
     
     	B(5) = F(3)*2.d0*USL*A(2)/(U - ML**2) 
     >  + F(4)*2.d0*US*A(1)/(U - MSIG**2) 
     >  + F(5)*GKST*( A(3) + A(4)*(MSIG-MP)/(MP+MSIG) )/M/DENOMKST
     >  - F(6)*GKST*( A(5) + A(6)*(MSIG-MP)/(MP+MSIG) )/M/DENOMK1   
     >  - RY1*F(7)*(UL/UL0)*2.d0*UB*A(7)/DENOMY1
     >  - RY2*F(8)*(UL/UL0)*2.d0*UB*A(8)/DENOMY2
     >  - RY3*F(9)*(UL/UL0)*2.d0*UB*A(9)/DENOMY3	
     
     	B(6) = -F(1)*UP*A(1)/(S-MP**2) + F(3)*USL*A(2)/(U-ML**2) 
     >  +  F(4)*US*A(1)/(U-MSIG**2)
     >  - RY1*F(7)*(UL/UL0)*UB*A(7)/DENOMY1  
     >  - RY2*F(8)*(UL/UL0)*UB*A(8)/DENOMY2
     >  - RY3*F(9)*(UL/UL0)*UB*A(9)/DENOMY3
     >  - RN1*F(10)*(UP/UP0)*UB*A(10)/DENOMN1
     >  + RN4*F(11)*(UP/UP0)*UB*A(11)/DENOMN4
     >  - RN6*F(12)*(UP/UP0)*UB*A(12)/DENOMN6
     >  + RD1*F(13)*(UP/UP0)*UB*A(13)/DENOMDEL1
     >  + RD2*F(14)*(UP/UP0)*UB*A(14)/DENOMDEL2
     >  - RD3*F(15)*(UP/UP0)*UB*A(15)/DENOMDEL3
 
 
c         B(6) =  US*A(1)/(U-MSIG**2) 
c     >        + USL*A(2)/(U-ML**2)
c     >        - UP*A(1)/(S-MP**2)
c     >        - (UL/UL0)*E0*A(7)/(MSIG+MY1)/DENOMY1
c     >        - (UL/UL0)*E0*A(8)/(MSIG+MY2)/DENOMY2
c     >        - (UL/UL0)*E0*A(9)/(MSIG+MY3)/DENOMY3
c     >        - (UP/UP0)*E0*A(10)/(MP+MN1)/DENOMN1
c     >        + (UP/UP0)*E0*A(11)/(MP+MN4)/DENOMN4
c     >        - (UP/UP0)*E0*A(12)/(MP+MN6)/DENOMN6
c     >        + (UP/UP0)*E0*A(13)/(MP+MDEL1)/DENOMDEL1
c     >        + (UP/UP0)*E0*A(14)/(MP+MDEL2)/DENOMDEL2
c     >        - (UP/UP0)*E0*A(15)/(MP+MDEL3)/DENOMDEL3

	RX1 = (-W * WX2000)/DENOMMX1
	RX2 = (-W * WX2050)/DENOMMX2
	
	if (flagX .eq. 0) RX = 0.d0
	if (flagX .eq. 1) RX = RX1
	if (flagX .eq. 2) RX = RX2
	if (flagX .eq. 3) RX = RX1 + RX2
	
	do 100 j=1,6
	B(j) = B(j)*(UR + RX)
100	continue

        END IF
	
	
        AMP(1) = B(1)
        AMP(2) = B(2)
        AMP(3) = B(3)
        AMP(4) = B(4)
        AMP(5) = B(5)
        AMP(6) = B(6)

	Eq = (0.5/dsqrt(s))*(s + QSQ - Mp**2)
	Ep = (0.5/dsqrt(s))*(s - QSQ + Mp**2)
	Ek = (0.5/dsqrt(s))*(s - My**2 + Mk**2)
	El = (0.5/dsqrt(s))*(s + My**2 - Mk**2)
	
	Pq = dsqrt(Eq**2 - QSQ)
	Pk = dsqrt(Ek**2 - Mk**2)
	
	Rq = Pq/(Ep+Mp)
	Rk = Pk/(El+My)
	
	qdp = 0.5*(s - Mp**2 - QSQ)
	qdl = 0.5*(QSQ + My**2 - u)
	
	xnp1 = dsqrt((Mp+Ep)/(2.*Mp))
	xnl1 = dsqrt((My+El)/(2.*My)) 

c	xnp1 = 1.d0
c	xnl1 = 1.d0
	
	xnp2 = 1./dsqrt(2.*Mp*(Mp+Ep))
	xnl2 = 1./dsqrt(2.*My*(My+El))
	
	Hkq = (qdl - Eq*El)/(Pq*Pk)
	W = dsqrt(s)

	xnorm1 = xnp1*xnl1*dsqrt(Mp*My)/(4.*pi*W)
	xnorm2 = xnp2*xnl2*dsqrt(Mp*My)/(4.*pi*W)
	 
	CGLN(1) = xnorm1*(
     >	B(1)*(Pq*Rq+Eq) - B(4)*qdp - B(5)*qdl + B(6)*QSQ )
     
        CG(1) = xnorm2*(
     >  -B(1)*(El+My)*(Pq**2) - B(1)*Eq*(El+My)*(Ep+Mp)
     >  +(El+My)*(Ep+Mp)*(B(4)*qdp + B(5)*qdl - B(6)*QSQ) )   
     
        CGLN(2) = -xnorm1*(
     > 	B(1)*Rk*(Pq+Eq*Rq) + Rq*Rk*(B(4)*qdp+B(5)*qdl-B(6)*QSQ) )
     
        CG(2) = Pq*Pk*xnorm2*(
     >  B(1)*(Ep+Mp+Eq) + B(4)*qdp + B(5)*qdl - B(6)*QSQ )  
     
        CGLN(3) = xnorm1*(
     > 	B(3)*Pk*Rq + B(5)*Pk*(Pq+Eq*Rq) )
     
        CG(3) = Pq*Pk*xnorm2*(
     >  -B(3)*(El+My) - B(5)*(El+My)*(Ep+Mp) - B(5)*Eq*(El+My) )   
        
        CGLN(4) = xnorm1*(
     >	-B(3)*Pk*Rk + B(5)*Pk*Rk*(Eq+Pq*Rq) )
 
 	CG(4) = Pk*Pk*xnorm2*(
     >  B(3)*(Ep+Mp) - B(5)*(Pq**2 + Eq*(Ep+Mp)) )  
     	       	       	                                                   	    
	CGLN(5) = xnorm1*(
     > 	B(1)*Eq + B(2)*Pq*Rq*(1.+qdp/QSQ) 
     > + B(3)*Rq*(Pq*qdl/QSQ + Pk*Hkq) + B(4)*Eq*(Pq*Rq-Ep)
     > + B(5)*( Hkq*Pk*(Pq+Eq*Rq) - qdl ) 
     > + B(6)*Eq*(Eq + Pq*Rq) )
     
        CGLN(6) = xnorm1*(
     > -B(1)*Eq*Rq*Rk - B(2)*Pq*Rk*(1.+qdp/QSQ) 
     > -B(3)*Rk*(Pq*qdl/QSQ + Pk*Hkq) + B(4)*Rk*Eq*(Pq-Rq*Ep)
     > +B(5)*( Hkq*Pk*Rk*(Eq+Pq*Rq) - Rk*Rq*qdl )        	
     > +B(6)*( Pq*Rk*(Eq+Pq*Rq) + Rk*Rq*QSQ ) ) 
     
 	CG(5) = (Eq/Pq)*Pq*xnorm2*( 
     >  (Ep+Mp)*(El+My)*( B(1) - Ep*B(4) - El*B(5) + Eq*B(6) )
     > -(El+My)*( B(2)*(Ep-Eq*qdp/QSQ) + B(3)*(El-Eq*qdl/QSQ)
     >       -B(4)*(Pq**2) + (Eq*El-qdl)*B(5) -B(6)*(Pq**2) ) )
     
        CG(6) = (Eq/Pq)*Pk*xnorm2*(
     >   (Pq**2)*(-B(1) -El*B(5) + Eq*B(6) - Ep*B(4) )
     > +(Ep+Mp)*( B(2)*(Ep-Eq*qdp/QSQ) + B(3)*(El-Eq*qdl/QSQ)
     > + B(4)*(Pq**2) + B(5)*(qdl-Eq*El) + B(6)*(Pq**2) )  )       	
     
     	
c     	write(*,*) CGLN(1),CG(1)
c     	write(*,*) CGLN(2),CG(2)
c     	write(*,*) CGLN(3),CG(3)
c     	write(*,*) CGLN(4),CG(4)
c     	write(*,*) CGLN(5),CG(5)
c    	write(*,*) CGLN(6),CG(6)
c     	write(*,*)
    	
      RETURN
      END
      

