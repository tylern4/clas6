C======================================================================
      function get_spin(iseed)
C----------------------------------------------------------------------
C-
C-   Purpose : Get spin (1 or -1)
C-
C-   Inputs  : random seed
C-
C-   Outputs : get_spin
C----------------------------------------------------------------------
      implicit none
	  integer get_spin
	  integer iseed
      real    random, sran 

	  random = sran(iseed)
      random = 0.5 - sran(iseed)
      get_spin = 1
      if(random.lt.0) get_spin = -1

	  return
	  end

C======================================================================
      function dot(v1,v2)
C----------------------------------------------------------------------
C-
C-   Purpose : Make a DOT product
C-
C-   Inputs  : v1(3) and v2(3) are vector (vx,vy,vz) 
C-
C-   Outputs : dot
C----------------------------------------------------------------------
      implicit none

	  real 	dot
	  real 	v1(3)
	  real 	v2(3)

	  integer i

	  dot = 0
	  do i = 1,3
	    dot = dot + v1(i)*v2(i)
	  enddo

      return
	  end
C======================================================================
      subroutine extractang(th,thd,ph,phd,vec)
C----------------------------------------------------------------------
C-
C-   Purpose : Extract theta and phi from the vector
C-
C-   Inputs  : vec(3) are vector (vx,vy,vz) 
C-
C-   Outputs : th, thd, ph, phd
C----------------------------------------------------------------------
      implicit none

      real 	dot
      real 	vec(3)
      real 	th, thd, ph, phd
      real    vec_mag

      vec_mag   = sqrt(dot(vec,vec))

      th  = acos(vec(3)/vec_mag)
      thd = th*90./acos(0.)
      ph    = atan2(vec(2),vec(1))
      phd   = ph*90./acos(0.)
	  return
	  end

C======================================================================
      SUBROUTINE rotate(idir,the,phi,p1,p2)    
C----------------------------------------------------------------------
C-
C-   Purpose : Rotation of a 3-vector: {x,y,z} <---> {x',y',z'}
C-
C-   Inputs  : the, phi  are angles of new z' axis defined
C-                       in master reference frame (x,y,z).
C-             idir = 1  Rotation from (x',y',z') to (x,y,z) reference frame.
C-             idir =-1  Rotation from (x,y,z) to (x',y',z') reference frame.
C-             p1(3)      is 3-vector {Px,Py,Pz} or any 3-vector {x,y,z} 
C- 
C-   Outputs : p2(3)      is 3-vector {Px,Py,Pz} or any 3-vector {x,y,z} 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
C
C Input variables
      integer idir
      REAL    the, phi, p1(3), p2(3)
C local variables
      INTEGER j
      REAL    ROT(3,3), pv(3)  
C
      IF(the**2+phi**2 .GT. 1E-20) THEN   
        ROT(1,1) =  COS(the)*COS(phi)  
        ROT(1,2) = -SIN(phi)  
        ROT(1,3) =  SIN(the)*COS(phi)  
        ROT(2,1) =  COS(the)*SIN(phi)  
        ROT(2,2) =  COS(phi)   
        ROT(2,3) =  SIN(the)*SIN(phi)  
        ROT(3,1) = -SIN(the)  
        ROT(3,2) =  0. 
        ROT(3,3) =  COS(the)
C   
        DO j = 1,3    
          pv(j) = p1(j)
        ENDDO    
        DO j = 1,3    
          IF(idir.GE.0) THEN
            p2(j) = ROT(j,1)*pv(1) + ROT(j,2)*pv(2) + ROT(j,3)*pv(3)
          ELSE
            p2(j) = ROT(1,j)*pv(1) + ROT(2,j)*pv(2) + ROT(3,j)*pv(3)
          ENDIF 
        ENDDO
      ENDIF
C
      RETURN    
      END   

C======================================================================
      SUBROUTINE boost(BETA,P1,E1,P2,E2)    
C----------------------------------------------------------------------
C-
C-   Purpose : Makes Lorentz Boost (Lab <--> CM)
C-
C-   Inputs  : P1(3),E1 is four vector (Px,Py,Pz,E) 
C-             BETA is CM velocity
C-                  If BETA < 0. Boost from Lab to center mass system.
C-                  If BETA > 0. Boost from center mass to Lab system.
C- 
C-   Outputs : P2(3),E2
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
C
C Input variables
      real BETA, P1(3), E1, P2(3), E2
C
C Local variables  
      real GAMMA
C
      P2(1) = P1(1)
      P2(2) = P1(2)
      IF(BETA**2.GT.1E-20) THEN    
        GAMMA = 1.0 / SQRT(1.0 - BETA**2)  
        P2(3) = GAMMA*(P1(3) + E1*BETA)    
        E2 = GAMMA*(E1 + P1(3)*BETA) 
      ENDIF
C 
      RETURN    
      END   
C======================================================================
      FUNCTION SRAN(IDUM)
C----------------------------------------------------------------------
C-
C- Purpose and Methods : Flat Random Numbers (from Numerical Recipes:"RAN2").
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
C
      SAVE
C
C Function parameters
      INTEGER IDUM
      REAL    SRAN
C
C Local parameters
      INTEGER M, IA, IC
      REAL RM
      PARAMETER (M=714025, IA=1366, IC=150889, RM=1.4005112E-6)
C
C Local variables
      INTEGER J, IY, IFF, IR(97)
      DATA IFF /0/
C
C
      IF(IDUM.LT.0 .OR. IFF.EQ.0) THEN
        IFF = 1
        IDUM = MOD(IC-IDUM,M)
        DO 11 J=1,97
          IDUM = MOD(IA*IDUM+IC,M)
          IR(J) = IDUM
 11     CONTINUE
        IDUM = MOD(IA*IDUM+IC,M)
        IY = IDUM
      ENDIF
C
      J = 1 + (97*IY)/M
      IF(J.GT.97 .OR. J.LT.1) PAUSE
      IY   = IR(J)
      SRAN = IY*RM
      IDUM = MOD(IA*IDUM+IC,M)
      IR(J)= IDUM
C
      RETURN
      END
C======================================================================
      FUNCTION newphi(phi)
C----------------------------------------------------------------------
C-
C- Purpose and Methods : 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
C
C
      real newphi, phi
      
      if (phi.gt.330.) then
        newphi = phi-360.
      elseif (phi.ge.0.0.and.phi.le.30.) then
        newphi = phi
      elseif (phi.gt.30.0.and.phi.le.90.) then
        newphi = phi-60.
      elseif (phi.gt.90.0.and.phi.le.150.) then
        newphi = phi-120.
      elseif (phi.gt.150.0.and.phi.le.210.) then
        newphi = phi-180.
      elseif (phi.gt.210.0.and.phi.le.270.) then
        newphi = phi-240.
      elseif (phi.gt.270.0.and.phi.le.330.) then
        newphi = phi-300.
      endif
      
      end  
C======================================================================
      FUNCTION accvb1(part_type,p,theta,phi,t_current)
C----------------------------------------------------------------------
C-
C- Purpose and Methods : 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
C
      REAL p,theta,phi,t_current,acc  
      REAL t_max
      REAL phi0_el, phi0_nh, phi0_ph
      REAL theta0_el, theta0_nh, theta0_ph
      REAL thetas_el, thetas_nh, thetas_ph
      REAL p_shift, cel_ex, pel_ex
      REAL ch_ex,theta_cut
      REAL theta_min, theta_max,delta_phi, exp
      INTEGER part_type,electron,pos_hadron, neg_hadron
      REAL d2r
      real accvb1
      
      data t_max/3375./
      data phi0_el/30./
      data phi0_nh/25./
      data phi0_ph/25./
      data theta0_el/12.5/
      data theta0_nh/10./
      data theta0_ph/5./
      data thetas_el/15./
      data thetas_nh/15./
      data thetas_ph/25./
      data theta_max/50./
      data p_shift/0.15/
      data pel_ex/0.333/
      data cel_ex/0.35/
      data theta_cut/75./
      data ch_ex/0.3/
      data electron/0/
      data pos_hadron/1/
      data neg_hadron/-1/
c      data pi/3.1415926/
      data d2r/0.01754533/      
     
      Acc = 0.0
      
      IF(part_type.EQ.electron) THEN
        theta_min = theta0_el+thetas_el/(p*t_max/t_current+p_shift)
        if(theta.gt.theta_min.and.theta.lt.theta_max)then
          exp = cel_ex*(p*t_max/t_current)**pel_ex
          delta_phi = phi0_el*sin((theta-theta_min)*d2r)**exp
          if(abs(phi).lt.delta_phi)Acc=1.
        endif
      ELSEIF(part_type.EQ.pos_hadron) THEN
        theta_min = theta0_ph+thetas_ph*(1.-p*t_max/t_current/5.)**24
        if(theta.gt.theta_min)then
          exp=(p*t_max/t_current/5.)**(1./8.)
          delta_phi = phi0_ph*cos((theta-theta_cut)*d2r)**exp
          if(abs(phi).lt.delta_phi)Acc=1.
        endif
      ELSEIF(part_type.EQ.neg_hadron) THEN
        theta_min = theta0_nh+thetas_nh/(p*t_max/t_current+p_shift)
        if(theta.gt.theta_min)then
          exp = ch_ex*(p*t_max/t_current)**pel_ex
          delta_phi = phi0_nh*sin((theta-theta_min)*d2r)**exp
          if(abs(phi).lt.delta_phi)Acc=1.
        endif
      ENDIF
      
      accvb1 = acc
      
      end

C======================================================================
      FUNCTION accvb2(part_type,p,theta,phi,t_current)
C----------------------------------------------------------------------
C-
C- Purpose and Methods : 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
C
      REAL p,theta,phi,t_current,acc  
      REAL t_max
      REAL phi0_el, phi0_nh, phi0_ph
      REAL theta0_el, theta0_nh, theta0_ph
      REAL thetas_el, thetas_nh, thetas_ph
      REAL p_shift, cel_ex, pel_ex
      REAL ch_ex,theta_cut
      REAL theta_min, theta_max,delta_phi, exp
      INTEGER part_type,electron,pos_hadron, neg_hadron
      REAL d2r
      real accvb2
      
      data t_max/3375./
      data phi0_el/32./
      data phi0_nh/27./
      data phi0_ph/27./
      data theta0_el/10.5/
      data theta0_nh/8./
      data theta0_ph/8./
      data thetas_el/15./
      data thetas_nh/15./
      data thetas_ph/25./
      data theta_max/50./
      data p_shift/0.15/
      data pel_ex/0.333/
      data cel_ex/0.35/
      data theta_cut/75./
      data ch_ex/0.3/
      data electron/0/
      data pos_hadron/1/
      data neg_hadron/-1/
c      data pi/3.1415926/
      data d2r/0.01754533/      
     
      Acc = 0.0
      
      IF(part_type.EQ.electron) THEN
        theta_min = theta0_el+thetas_el/(p*t_max/t_current+p_shift)
        if(theta.gt.theta_min.and.theta.lt.theta_max)then
          exp = cel_ex*(p*t_max/t_current)**pel_ex
          delta_phi = phi0_el*sin((theta-theta_min)*d2r)**exp
          if(abs(phi).lt.delta_phi)Acc=1.
        endif
      ELSEIF(part_type.EQ.pos_hadron) THEN
        theta_min = theta0_ph+thetas_ph*(1.-p*t_max/t_current/5.)**24
        if(theta.gt.theta_min)then
          exp=(p*t_max/t_current/5.)**(1./8.)
          delta_phi = phi0_ph*cos((theta-theta_cut)*d2r)**exp
          if(abs(phi).lt.delta_phi)Acc=1.
        endif
      ELSEIF(part_type.EQ.neg_hadron) THEN
        theta_min = theta0_nh+thetas_nh/(p*t_max/t_current+p_shift)
        if(theta.gt.theta_min)then
          exp = ch_ex*(p*t_max/t_current)**pel_ex
          delta_phi = phi0_nh*sin((theta-theta_min)*d2r)**exp
          if(abs(phi).lt.delta_phi)Acc=1.
        endif
      ENDIF
      
      accvb2 = acc
      
      end   
