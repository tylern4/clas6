      subroutine spp_interp(v1,v2)

      implicit none

      include 'mpintp.inc'

      real  v1,v2,x(2),fint,sftmp(5,5)
      integer mpp,j1,j2,i,j,k,nq,nw
      integer nq1,nq2,nw1,nw2,jj1,jj2,nna(2)
      real y,bound,divdif,dq,dw
      real ytmp(100),yytmp(100),y2tmp(100),vvar(10)
      real qmin,qmax,wmin,wmax
      
      save bound
      data bound/1.e30/
      data nna/5,5/
          
      qmin=var(1)
      qmax=var(nv1)
      wmin=var(nv1+1)
      wmax=var(nv1+nv2)
      
      if (v1.lt.qmin.or.v1.gt.qmax) print *, 'WARNING Q2=',v1,' OUTSIDE ',qmin,qmax
      if (v2.lt.wmin.or.v2.gt.wmax) print *, 'WARNING  W= ',v2,' OUTSIDE ',wmin,wmax
          
      do i=1,nv1
        if (v1.ge.var(i)) nq=i
      enddo
      do i=nv1+1,nv1+nv2
        if (v2.ge.var(i)) nw=i-nv1
      enddo

      nq1=max(1,nq-2)
      nq2=min(nv1,nq1+4)
      nq1=nq2-4
      nw1=max(1,nw-2)
      nw2=min(nv2,nw1+4)
      nw1=nw2-4
      
      jj1=0
      do j1=nq1,nq2
        jj1=jj1+1
        vvar(jj1)=var(j1)
      enddo
      do j2=nw1,nw2
        jj1=jj1+1
        vvar(jj1)=var(nv1+j2)
      enddo
      
      if (method_spline.eq.1) then
        do k=1,max_mp
          do i=1,nvar1
            do j=1,nvar2
              ytmp(j)=sf(i,j,k)
              y2tmp(j)=d2sf(i,j,k)
            enddo
            call splint(var2,ytmp,y2tmp,nvar2,v2,yytmp(i))
          enddo
          call spline(var1,yytmp,nvar1,bound,bound,y2tmp)
          call splint(var1,yytmp,y2tmp,nvar1,v1,y)      
          vec_f(k) = y
        enddo
      elseif (method_spline.eq.2) then
        x(1) = v1
        x(2) = v2
        do i=1,max_mp
          jj2=0
          do j2=nw1,nw2
            jj2=jj2+1
            jj1=0
            do j1=nq1,nq2
              jj1=jj1+1
              sftmp(jj1,jj2)=sf(j1,j2,i)
            enddo
          enddo
          vec_f(i)=fint(2,x,nna,vvar,sftmp)
c Use this to bypass interpolation          
c          vec_f(i)=sf(nq,nw,i)
        enddo
      elseif (method_spline.eq.3) then
        do i=1,max_mp
          do j2=1,nvar2
            ytmp(j2)=sf(1,j2,i)
            if (var2(j2).eq.0) go to 777
          enddo
777       k = j2-1
          vec_f(i)=divdif(ytmp,var2,k,v2,3)
        enddo
      endif

      return
      end
