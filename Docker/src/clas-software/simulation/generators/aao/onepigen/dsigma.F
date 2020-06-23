      subroutine dsigma(the,q2,w,cscm,phicm,opt1,opt2,opt3
     *     ,sig0,sigu,sigt,sigl,sigi,sigip,asym_p,ehel,w_min,w_max,
     *     mt)

      implicit none

      real the,ki_mag,q2,w,cscm,phicm,kf_mag,s2
      real sig0,sigu,sigt,sigl,sigi,sigip,asym_p
      real nu,eps,eps1
      integer ehel
      real cthe     
      integer opt1,opt2,opt3

c     Added W cut:
      real w_min, w_max

c     Added variable target mass:
      real mt
      
      logical test1,test2,test3
 
      nu 	= 0.5*(w**2 + q2 - mt**2)/mt
      s2	= sin(0.5*the)**2
      ki_mag	= (nu+sqrt(q2/s2+nu**2))*0.5

      kf_mag 	= ki_mag-nu
      eps 	= 1. /(1+2.0*(1+nu*nu/q2)*tan(0.5*the)**2)

      test1	= ki_mag.lt.0.1.or.kf_mag.lt.0.01
c     test3	= opt1.ge.4.and.(w.lt.1.1.or.w.gt.1.7)
      test3	= opt1.ge.4.and.
     +     ((w.lt.1.1.or.w.gt.2.0) !natural limits
     +     .or.
     +     (w.lt.w_min.or.w.gt.w_max))
      
      if (test1.or.test3) then
c        print *, 'ABORT',ki_mag,kf_mag,q2,w        
        sig0 = 0.
        sigu = 0.
        sigt = 0.
        sigl = 0.
        sigi = 0.
        sigip = 0.
        asym_p = 0.
        return
      endif

      if(opt1.eq.1) call aao(q2,w,eps,cscm,phicm,1,sig0,
     1             sigu,sigt,sigl,sigi)
      if(opt1.eq.2) 
     1   call daresbury(q2,w,eps,cscm,phicm,1,sig0)
      if(opt1.ge.3)
     1     call maid_lee(q2,w,eps,cscm,phicm,opt1,opt2,opt3,
     1     sig0,sigu,sigt,sigl,sigi,sigip,asym_p,ehel)     
c      print *, 'DSIGMA: ',sig0,sigu,sigl,sigt,sigi,sigip,asym_p,ehel 
      end
