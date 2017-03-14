C*******************************************************************************
CEV The following is for the kinematic variables.
C*******************************************************************************
C	 
      real W, Q2
	  real csthcm, phicm, epsilon, qv_mag_cm, ppi_mag_cm
	  real nu, nu_cm, fkt, gamma_v
	  
	  common /kinematics/W, Q2, csthcm, phicm, epsilon, 
     1       qv_mag_cm, ppi_mag_cm, nu, nu_cm, fkt, gamma_v
C
	  integer Q2_n, W_n, csthcm_n, phicm_n
C
      data Q2_n, W_n, csthcm_n, phicm_n /10, 10, 40, 36/
C
C Physical quantities
C
      real m_p, m_pip, m_pi0           ! masses
      real pi, alpha                   ! constants

      parameter(alpha = .729735e-02)
      parameter(pi    = 3.141592654)
      parameter(m_p   = .9382799)
      parameter(m_pi0 = .1349764)
      parameter(m_pip = .1395700)
C*******************************************************************************
