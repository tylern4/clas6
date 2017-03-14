      real function sigma0_calc(ki_mag,theory_opt,
     1              channel_opt,resonance_opt,iopt,
     1sigu,sigt,sigl,sigi,sige,siget,sigpt,sig5,rltp)

      implicit none

      include 'spp.inc'   

      real ki_mag,kf_mag,the_e,cthe
      real sigma0,asym
      real sigu,sigt,sigl,sigi,sige,siget,sigpt,pol_elec,pol_targ
      integer theory_opt,channel_opt,resonance_opt
      real crs,alpi,eps,sig5,rltp,jacob,sltp,the_cm
      real e_pi_cm,sph,frltp,fac1,fac2
      real m_pi,ebeam
      integer first,iopt

      data first/1/ 
      
      m_pi  = m_pip
      if (channel_opt.eq.1) m_pi  = m_pi0
      if (channel_opt.eq.4) m_pi  = m_eta
      
      if (ki_mag.gt.0) then
        nu 	= 0.5*(w**2 + q2 - m_p**2)/m_p
        kf_mag 	= ki_mag-nu
        cthe	= 1. - q2/(2.0*ki_mag*kf_mag)
        the_e	= acos(cthe)
        epsilon = 1. /(1+2.0*(1+nu*nu/q2)*tan(0.5*the_e)**2)
      endif
      
      the_cm    = acos(csthcm)*180./pi
      e_pi_cm    = 0.5*(w*w+m_pi**2-m_p**2)/w
      ppi_mag_cm = e_pi_cm**2 - m_pi**2
      ppi_mag_cm = sqrt(ppi_mag_cm)
      qv_mag_cm  = ((w*w+q2+m_p**2)/2.0/w)**2-m_p**2
      qv_mag_cm  = sqrt(qv_mag_cm)
      
      nu_cm      = (w*w-m_p**2-q2)/(2*w)        
      fkt 	 = 2.0*w*ppi_mag_cm/(w**2 - m_p**2)   
      
      sph	 = sin(phicm*pi/180.)
      
      fac1	 = sqrt(2*epsilon*(1-epsilon))*sph
      fac2       = fkt*sqrt(1-csthcm**2)*sqrt(q2)/qv_mag_cm
      
c      frltp      = fkt*sqrt(2*epsilon*(1-epsilon))*sqrt(q2)*sph/qv_mag_cm
c      frltp      = frltp*sqrt(1-csthcm**2)
       
c     Calculate direction of target polarization vector with respect
c     to r.h. coordinate system x,y,z with z along q vector, y normal to 
c     hadron decay plane.      
      
      pt  = kf_mag*sin(the_e)/sqrt(q2+nu**2)
      pl  = sqrt(1-pt**2)
      pn  = -pt * sin(phicm*pi/180.0)
      pt  =  pt * cos(phicm*pi/180.0)
      
      pol_elec = pol_e
      pol_targ = pol_t
      
c      if(theory_opt.eq.1) call aao(q2,w,epsilon,csthcm,phicm,channel_opt,
c     1pol_elec,pol_targ,sigu,sigt,sigl,sigi,sige,siget,sigpt)
     
c      if(theory_opt.eq.8) then
c        call aao_test(q2,w,epsilon,csthcm,phicm,channel_opt,sigma0,
c     1             sigu,sigt,sigl,sigi)
c      endif
      
c      if(theory_opt.eq.2) 
c     1   call daresbury(q2,w,epsilon,csthcm,phicm,1,sigma0)
     
      if(theory_opt.ge.1.and.theory_opt.le.7) 
     1   call maid_lee(q2,w,epsilon,csthcm,phicm,
     1   theory_opt,channel_opt,resonance_opt,iopt,
     1   pol_elec,pol_targ,sigu,sigt,sigl,sigi,sige,siget,sigpt)
     
      if(theory_opt.eq.8) then
         call gwepr08(q2,w*1e3,epsilon,the_cm,phicm,channel_opt,
     1   pl,pn,pt,
     1   sigu,sigt,sigl,sigi,sige,siget,sigpt)     
   
c       call helout(q2,channel_opt) 
       
       sige   =  pol_elec*sige
       sigpt  =  pol_targ*sigpt
       siget  =  pol_elec*pol_targ*siget
         
      endif
      
c      if (theory_opt.eq.10) then
c      if (first.eq.1) then
c        call lang_def
c        call janr_input_said
c        call mult_ana
c        call read_multables
cc        call regge_multables        
c        call high_multables
c        first = 0
c      endif
c      call janr_spp(q2,w,epsilon,csthcm,phicm,channel_opt,crs,sltp,asym)
c      sigu = crs
c      sigl = 0.
c      sigt = 0.
c      sigi = 0.
c      sige = sltp*fac1
c      siget = 0.
c      sigpt = 0.
c      endif
       
      sigma0_calc = sigu+sigt+sigl+sigi+sige+siget+sigpt
      rltp  = sltp/fac2

      ebeam   = abs(ki_mag)
      alpi    = 0.5*alpha/(pi)**2
      eps     = epsilon
      gamma_v = alpi*(W**2-m_p**2)*kf_mag/(ebeam*Q2*2*m_p*(1-eps))   
      jacob   = w*pi/(ebeam*kf_mag*m_p)
      sig5    = gamma_v*sigma0_calc*jacob

      end
