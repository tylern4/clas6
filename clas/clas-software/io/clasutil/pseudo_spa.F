      SUBROUTINE pseudo_spa(part_t,p,theta,phi,t_current,acp,sect,acc)
c-      
c-  Pseudo Single Particle acceptance function from Volker Burkert.
c-
c-  Inputs:
c-          part_t    - INTEGER: 0=electron, 1=positive hadron, -1=negative hadron.
c-          p         - REAL:    particle momentum GeV/c.
c-          theta     - REAL:    particle polar angle in degrees (0 to 180.).
c-          phi       - REAL:    particle azimuthal angle in degrees in the sector (-30. to 30.)
c-          t_current - REAL:    torus current in A.
c-	        acp       - REAL:    fraction of phi acceptance; acp = 1. gives the widest range.
c-          sect      - INTEGER: sector number.    
c-
c-  Output:
c-          acc       - REAL:    acceptance - equals 0. if particle is not in the fiducial region, 
c-                               equals the solid angle weight if it is in the fiducial region.
c-
      IMPLICIT NONE
      REAL p,theta,phi,t_current,acp    ! Input parameter
      INTEGER part_t,sect                 ! 
c
      REAL acc                          ! Output parameter
c
      REAL t_max
      parameter (t_max = 3375.)
      REAL phi0_el, phi0_nh, phi0_ph
      parameter (phi0_el = 30.)
      parameter (phi0_nh = 25.)
      parameter (phi0_ph = 25.)    ! Changed for better discription.
      REAL theta0_el, theta0_nh, theta0_ph
      parameter (theta0_el = 15.5)
      parameter (theta0_nh = 10.)
      parameter (theta0_ph = 10.)
      REAL thetas_el, thetas_nh, thetas_ph
      parameter (thetas_el = 15.)
      parameter (thetas_nh = 15.)
      parameter (thetas_ph = 25.)
      REAL p_shift, cel_ex, pel_ex, pim_ex
      parameter (p_shift = 0.05)
      parameter (pel_ex = 0.333)
      parameter (pim_ex = 0.5)
      parameter (cel_ex = 0.35)
      REAL  ch_ex,theta_cut
      parameter (theta_cut = 75.)
      parameter (ch_ex = 0.1)
      INTEGER electron,pos_hadron, neg_hadron
      parameter (electron = 0)
      parameter (pos_hadron = 1)
      parameter (neg_hadron = -1)
c- New, for very forward electrons -
      REAL theta_nip,cel_ex_nip,phi0_el_nip
      parameter (theta_nip = 2.)
      parameter (cel_ex_nip = 0.5)
      parameter (phi0_el_nip = 20.)
c
      REAL pi,d2r,u_acc
      parameter (pi = 3.1415926)
      parameter (d2r = 0.0174533)
      parameter (u_acc = 0.20944)
      INTEGER err_count
      data err_count/0/
      REAL theta_min, delta_phi, exp, pnorm,dtheta
c-      
      Acc=0.0
      pnorm=p*t_max/t_current
      IF(part_t.EQ.electron) THEN
        if(sect.eq.2)then
          dtheta=6./pnorm
          if(theta.gt.18.+dtheta.and.theta.lt.20.+dtheta)return
        elseif(sect.eq.5)then
          dtheta=6./(p*t_max/t_current)
          if(theta.gt.19.+dtheta.and.theta.lt.21.5+dtheta)return
        endif
        theta_min = theta0_el+thetas_el/(p*t_max/t_current+p_shift)
        if(theta.gt.theta_min.and.theta.lt.50.)then
          exp = cel_ex*(p*t_max/t_current)**pel_ex
          delta_phi = acp*phi0_el*sin((theta-theta_min)*d2r)**exp
          if(abs(phi).lt.delta_phi)Acc=sin(theta*d2r)*delta_phi*u_acc
c- New, for very forward electrons -
        elseif(theta.gt.theta_min-theta_nip.and.theta.lt.50.)then
          exp = cel_ex_nip*(p*t_max/t_current)**pel_ex
          delta_phi = acp*phi0_el_nip
     *   *sin((theta-theta_min+theta_nip)*d2r)**exp
          if(abs(phi).lt.delta_phi)Acc=sin(theta*d2r)*delta_phi*u_acc
        endif
      ELSEIF(part_t.EQ.pos_hadron) THEN
        if(sect.eq.3)then
	if(pnorm.gt.0.825+1.5*(theta/35.)**6.and.
     *	(pnorm.lt.1.0125+1.35*(theta/32.)**6))return
	if(pnorm.gt.0.15+ 0.75*(theta/65.)**8.and.
     *	(pnorm.lt.0.15+0.75*(theta/50.)**6))return
        elseif(sect.eq.4)then
        if(pnorm.lt.theta/5..and.pnorm.gt.theta/7.)return
        if(pnorm.gt.0.7+1.5*(theta/41.5)**8.and.
     *  pnorm.lt.0.85+1.5*(theta/35.5)**8)return
        elseif(sect.eq.5)then
	if(pnorm.gt.1.+2.*(theta/20.)**2.and.
     *	(pnorm.lt.1.2+1.8*(theta/15.)**2))return
        if(pnorm.gt.0.5+0.7*(theta/40.)**8.and.
     *  pnorm.lt.0.7+2.*(theta/40.)**8)return
        elseif(sect.eq.6)then
         if(pnorm.gt.0.2+0.5*(theta/80.)**6.and.
     *    pnorm.lt.0.25+0.5*(theta/70.)**6)return
        endif
        theta_min = theta0_ph+thetas_ph*(1.-p*t_max/t_current/5.)**24
        if(theta.gt.theta_min)then
          exp=(p*t_max/t_current/5.)**(1./8.)
          delta_phi = acp*phi0_ph*cos((theta-theta_cut)*d2r)**exp
          if(abs(phi).lt.delta_phi)Acc=sin(theta*d2r)*delta_phi*u_acc
        endif
      ELSEIF(part_t.EQ.neg_hadron) THEN
        theta_min = theta0_nh+thetas_nh/(p*t_max/t_current+p_shift)
        if(theta.gt.theta_min.and.theta.lt.130.)then
          exp = ch_ex*(p*t_max/t_current)**pim_ex
          delta_phi = acp*phi0_nh*sin((theta-theta_min)*d2r)**exp
          if(abs(phi).lt.delta_phi)Acc=sin(theta*d2r)*delta_phi*u_acc
        endif
      ELSE
      	err_count=err_count+1
      	if(err_count.le.10)WRITE(6,*)'Illegal particle type'
      		ACC=0.0
      ENDIF
      RETURN 
      END
