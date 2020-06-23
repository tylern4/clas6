      real function sigma0_calc(ki_mag,theory_opt,
     1              channel_opt,resonance_opt)

      implicit none

      include 'spp.inc'   

      real ki_mag,kf_mag,the_e,cthe
      real sigma0,sigu,sigt,sigl,sigi
      integer theory_opt, channel_opt,resonance_opt
      real jacob,evirt
 
      nu 	= 0.5*(w**2 + q2 - m_p**2)/m_p
      kf_mag = ki_mag-nu
      cthe	= 1. - q2/(2.0*ki_mag*kf_mag)
      the_e = acos(cthe)
      epsilon = 1. /(1+2.0*(1+nu*nu/q2)*tan(0.5*the_e)**2)
      pt  = kf_mag*sin(the_e)/sqrt(q2+nu**2)
      pl  = sqrt(1-pt**2)
      pn  =-pt * sin(phicm*pi/180.0)
      pt  = pt * cos(phicm*pi/180.0)

      if(theory_opt.eq.1) call aao(q2,w,epsilon,csthcm,phicm,channel_opt,
     1             sigma0,sigu,sigt,sigl,sigi)
      if(theory_opt.eq.100) then
        call aao_test(q2,w,epsilon,csthcm,phicm,channel_opt,sigma0,
     1             sigu,sigt,sigl,sigi)
      endif
      if(theory_opt.eq.2) 
     1   call daresbury(q2,w,epsilon,csthcm,phicm,1,sigma0)
      if(theory_opt.ge.3.and.theory_opt.le.5) 
     1   call maid_lee(q2,w,epsilon,csthcm,phicm,theory_opt,channel_opt,
     1             resonance_opt,sigma0,sigu,sigt,sigl,sigi)

		evirt=(W**2-m_p**2)/2./m_p
		gamma_v=alpha/4/pi**2*kf_mag/ki_mag*evirt/Q2*
     +          (2. + 1./tan(the_e/2)**2/(1.+nu**2/Q2))
		jacob=Q2**2/(2.*ki_mag*kf_mag)
	        sigma0=gamma_v*sigma0*jacob*10000
c                write(*,*) 'sigma', gamma_v*jacob,sigma0

c
      sigma0_calc = sigma0
c      alpi = 0.5*alpha/(pi)**2
c      gamma_v = alpi*(W**2-m_p**2)*kf_mag/(ki_mag*Q2*2*m_p*(1-eps))   
c      sigma_0_5fold = gamma_v*sigma0_calc

c 100  format(5F10.3,2E12.3)

      end
