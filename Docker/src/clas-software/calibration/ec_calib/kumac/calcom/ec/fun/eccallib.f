      subroutine ecfillang
      real x,y,r,r0,off(2)      
      integer u,j,jmax,m,pixel,sign
      data off/3.453,6.907/
      
      vector theta(1296)
      vector phi(1296)
      
c     Ec_u_edge = 418.7994
c     Ec_v_edge = 381.7069
      
      r0 = 510.3
      pixel = 0
      
      do u = 1,36
        jmax = 2*u-1
        do j = 1,jmax
          m = -u+j
          pixel = pixel + 1
          sign = mod(j,2)+1
          x = (18-u)*10.36-off(sign)+3.453
          y = m*5.305
          r = sqrt(x**2+y**2)
          angle = atan(r/r0)*180./3.14159
          theta(pixel) = angle
          if (y.ne.0) then
            phi(pixel) = atan(x/y)*180./3.14159
          else
            phi(pixel) = 90.
          endif
          if (phi(pixel).lt.0) phi(pixel)=180.+phi(pixel)
          if (x.lt.0)          phi(pixel)=180.+phi(pixel)
        enddo
      enddo

      end
      
      subroutine ecattcor
      
      integer sec,u,v,w,vpix,wpix,pixel,str      
      integer index1(6),index2(6)

      vector att(36,6,6)
      vector attfit(36,6,6)
      vector gain(36,6,6)
      vector alc(1296,6,6)
      vector alcfit(1296,6,6)
      vector pixgain(1296,6,6)
      vector pixmap(1296,3)
      vector pixwidth
      
      do sec=1,6
      print *, 'Sector ',sec
      do u=1,36
        jmax = 2*u-1
        v = 37-u
        w = 36
        vpix = 73-2*u
        wpix = jmax
        do j = 1,jmax
          pixel = u*(u-1)+v-w+1
          index1(1) = j
          index1(2) = vpix
          index1(3) = wpix
          index1(4) = j
          index1(5) = vpix
          index1(6) = wpix
          index2(1) = u
          index2(2) = v
          index2(3) = w
          index2(4) = u
          index2(5) = v
          index2(6) = w
          do lay = 1,6
            i1  = index1(lay)
            str = index2(lay)
            atlen = att(str,lay,sec)
            atlenfit = attfit(str,lay,sec)
            x = (i1-0.5)*pixwidth(lay)
            alc(pixel,lay,sec) = exp(-x/atlen)
            alcfit(pixel,lay,sec) = exp(-x/atlenfit)
            pixgain(pixel,lay,sec) = gain(str,lay,sec)
c            alcdb(pixel,lay,sec) = ecattsum(i1,str,lay,sec)
            pixmap(pixel,1) = u
            pixmap(pixel,2) = v
            pixmap(pixel,3) = w
          enddo
          if (int(j/2)*2.eq.j) then
            w = w-1
            vpix = vpix-1
          else
            v = v+1
            vpix = vpix+1
          endif
          wpix = wpix - 1
        enddo
      enddo
      enddo
      
      end
      
      subroutine getpixels(view,strip,out,in)
      
      integer a,b,c,sum,pixel,view,strip,numpix
      real out(71),in(1296)
      
      numpix = 2*strip-1
      
      a = strip
      b = 37-a
      c = 36
      
      do j = 1,numpix
        if (view.eq.1) pixel=a*(a-1)+b-c+1
        if (view.eq.2) pixel=c*(c-1)+a-b+1
        if (view.eq.3) pixel=b*(b-1)+c-a+1
        if (view.eq.4) pixel=a*(a-1)+b-c+1
        if (view.eq.5) pixel=c*(c-1)+a-b+1
        if (view.eq.6) pixel=b*(b-1)+c-a+1
        out(j) = in(pixel)
        sum = a+b+c
        if(sum.eq.73) b=b+1
        if(sum.eq.74) c=c-1
      enddo
      
      end
      
      subroutine ecsumcor
      
      integer sec,io,lay,pix
      
      vector alc(1296,6,6)
      vector alcfit(1296,6,6)
      vector sumcor(1296,2,6)
      vector sumcorfit(1296,2,6)
      
      do sec = 1,6
        print *, 'Sector ',sec
        io = 1
        do lay = 1,3
          do pix = 1,1296
            sumcor(pix,io,sec) = sumcor(pix,io,sec) + alc(pix,lay,sec)/3.
            sumcorfit(pix,io,sec) = sumcorfit(pix,io,sec) + 
     1      alcfit(pix,lay,sec)/3.
          enddo
        enddo
        io = 2
        do lay = 4,6
          do pix = 1,1296
            sumcor(pix,io,sec) = sumcor(pix,io,sec) + alc(pix,lay,sec)/3.
            sumcorfit(pix,io,sec) = sumcorfit(pix,io,sec) + 
     1      alcfit(pix,lay,sec)/3.
          enddo
        enddo
      enddo
      
      return
      end
      
      real function ecattsum(i1,str,lay,sec)
      
      integer index(13,3)
      integer nstrip(2)
      integer sec,lay,str,strip,io,n,off,i1
      real err,wgt
      
      vector attdb(36,39,6)
      vector attedb(36,39,6)
      vector weight
      vector pixwidth
      
      data nstrip/5,8/
      
      data index/1,4,7,10,13,16,19,22,25,28,31,34,37,
     &           2,5,8,11,14,17,20,23,26,29,32,35,38,
     &           3,6,9,12,15,18,21,24,27,30,33,36,39/

      avg3 = 0
      avg4 = 0
      io = 1
      view = lay
      if (lay.gt.3) then
        io = 2
        view = lay-3
      endif
      off = 5*(io-1)
      
      strip = 37-str
      
      do n = 1,nstrip(io)
        i = index(n+off,view)
        wgt = weight(i)
        atlen  =  attdb(strip,i,sec)
        atlene = attedb(strip,i,sec)
c        if (view.eq.1) x = (i1-0.5)*0.05*(94.701+0.2256*(i-1))/0.89
c        if (view.eq.2) x = (i1-0.5)*0.05*(103.655+0.2476*(i-2))/0.89
c        if (view.eq.3) x = (i1-0.5)*0.05*(103.655+0.2476*(i-3))/0.89
        x = (i1-0.5)*pixwidth(lay)
        expon  = exp(-x/atlen)
        expone = expon*x*atlene
        avg3 = avg3 + wgt/max(1.,expone)
        avg4 = avg4 + wgt/(x*atlene)
      enddo
      
      err  = avg3
      ecattsum  = avg4/err
      
      end

