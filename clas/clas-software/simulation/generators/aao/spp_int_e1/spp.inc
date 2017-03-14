      real W,Q2
      real csthcm,phicm,epsilon,qv_mag_cm,ppi_mag_cm
      real nu,nu_cm,fkt,gamma_v,pn,pl,pt,pol_e,pol_t
	  
      common /kinematics/w,q2,csthcm,phicm,epsilon,
     1       qv_mag_cm,ppi_mag_cm,nu,nu_cm,fkt,gamma_v,
     1       pn,pl,pt,pol_e,pol_t

      real m_p, m_n, m_pip, m_pi0, m_eta       
      real pi, alpha              

      parameter(alpha = .729735e-02)
      parameter(pi    = 3.141592654)
      parameter(m_p   = .9382720)	! PDG2005
      parameter(m_n   = .9395654)	! PDG2005
      parameter(m_pi0 = .1349766)	! PDG2005
      parameter(m_pip = .1395702)	! PDG2005
      parameter(m_eta = .547311)	! hep-ex/0505006
