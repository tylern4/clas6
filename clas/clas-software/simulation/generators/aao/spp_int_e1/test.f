      program spp_int

      implicit none
      
      integer iw,ic,ip
      real w,q2,cscm,phicm,eps
      real the_e,kf_mag,ki_mag,nu,cthe
      real m_p,sigma0
      
      real ebeam
      data m_p/0.93828/
      
      ebeam = 1.645
      q2 = 0.4
            
      do iw = 1,2
        w = 1.1+(iw-1)*0.02
        do ic = 1,10
          cscm = -0.9+(ic-1)*0.2
          do ip = 1,12
            phicm = 15.+(ip-1)*30
            nu = (w**2 + q2 - m_p**2) / (2.0*m_p)
            ki_mag = ebeam
            kf_mag = ki_mag - nu
            cthe = 1. - q2/(2.0*ki_mag*(ki_mag - nu))
            print *, nu,q2,cthe,acos(cthe)
            the_e = acos(cthe)
            print *, the_e,nu,q2
            eps = 1. /(1+2.0*(1+nu*nu/q2)*tan(0.5*the_e)**2)
            print *, w,q2,eps,cscm,phicm,sigma0,sigma0*0.1
            print *, ''
          enddo
        enddo
      enddo

      end