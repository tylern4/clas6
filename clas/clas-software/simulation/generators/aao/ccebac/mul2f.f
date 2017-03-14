c  input el+ el- ml+ ml- sl+' sl-' sl+ sl-
c  out put f1 f2 f3 f4 
c          f7' f8' f7 f8
c     f7' = f6 + z f4, f8' = f1+f5+z f3
c
c    multipile amplitude -> F
c
      subroutine mul2f(cmul,zz,cout,lmaxgm)
      implicit real*8(a-b,d-h,o-z)
      implicit complex*16(c)
      parameter (maxpar=500,maxmb=5 ,maxres=5,maxlsj=20,maxl=10)
      parameter (maxwcm=40,maxq2=20,maxmom=50,maxtheta=24)
      dimension 
     &   fle(0:maxl),fled(-1:maxl)
     1   ,fledd(-1:maxl),cout(8),cmul(0:maxl,8)
c
      fled(-1) = 0
      fled(0)  = 0
      fledd(-1)= 0
      fledd(0) = 0
      fledd(1) = 0
c
c    multipole amplitude to F
c
      call legen(zz,fle)
      z2     = zz*zz
      fac    = z2 - 1.d0
      do 200 lx = 1,maxl
        fled(lx) = (zz*fle(lx) - fle(lx-1))*dble(lx)/fac
c> bjd210607
        if (zz.eq.1.) fled(lx)=dble(lx)*dble(lx+1)/2.
        if (zz.eq.-1.) fled(lx)=(-1.)**(lx)*dble(lx)*dble(lx+1)/2.
c< bjd210607
  200 continue
      do 210 lx = 2,maxl
        fledd(lx)= (-2.d0*zz*fled(lx)+dble(lx*(lx+1))*fle(lx))/fac
c> bjd210607
        if (zz.eq.1.) fledd(lx)=dble(lx-1)*dble(lx)
     1   *dble(lx+1)*dble(lx+2)/8.
        if (zz.eq.-1.) fledd(lx)=dble(lx-1)*dble(lx)
     1   *dble(lx+1)*dble(lx+2)/8.*(-1.)**lx 
c< bjd210607
  210 continue
c
      do 249 ixx = 1,8
 249  cout(ixx)  = (0.d0,0.d0)

      do 220 kx = 0,lmaxgm
        celp     = cmul(kx,1)
        celm     = cmul(kx,2)
        cmlp     = cmul(kx,3)
        cmlm     = cmul(kx,4)
        cslpp    = cmul(kx,5)
        cslmp    = cmul(kx,6)
        cslp     = cmul(kx,7)
        cslm     = cmul(kx,8)
        cout(1)  = cout(1)
     &    + fled(kx+1)*(celp+dble(kx)  *cmlp)
     &    + fled(kx-1)*(celm+dble(kx+1)*cmlm)
        cout(2)  = cout(2)
     &    + fled(kx)*(dble(kx+1)*cmlp+dble(kx)*cmlm)
        cout(3)  = cout(3)
     &    + fledd(kx+1)*(celp-cmlp) + fledd(kx-1)*(celm+cmlm)
        cout(4)  = cout(4)
     &    + fledd(kx)*(-celp-celm+cmlp-cmlm)
        fac11 = -dble(kx+1)*fled(kx)
        fac12 = dble(kx)*fled(kx)
        fac21 = dble(kx+1)*fled(kx+1)
        fac22 = -dble(kx)*fled(kx-1)
        cout(5) = cout(5) + fac11*cslpp+fac12*cslmp
        cout(6) = cout(6) + fac21*cslpp+fac22*cslmp
        cout(7) = cout(7) + fac11*cslp+fac12*cslm
        cout(8) = cout(8) + fac21*cslp+fac22*cslm
  220 continue
 240  continue
c
      return
      end

