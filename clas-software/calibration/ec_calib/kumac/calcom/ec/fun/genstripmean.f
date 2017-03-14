      subroutine genstripmean(s1,s2)
      
      vector in1(1296,6,6)
      vector in2(1296,6)
      vector out(36,6,6)
      
      real tmp1(71),tmp2(71),inn1(1296),inn2(1296)
      integer s1,s2
      integer str,pix,npix,lay
      
      if (s2.eq.7) return
            
      do i = 1,1296
        inn2(i) = in2(i,s2)
      enddo      

      do lay = 1,6
        do i = 1,1296
          inn1(i) = in1(i,lay,s1)
        enddo
        do str = 1,36
          npix = 2*str-1
          call getpixels(lay,str,tmp1,inn1)
          call getpixels(lay,str,tmp2,inn2)
          sum1 = 0.
          sum2 = 0.
          do pix = 1,npix
            sum1 = sum1 + tmp1(pix)*tmp2(pix)
            sum2 = sum2 + tmp2(pix)
            out(str,lay,s2) = 1.
            if (sum2.gt.0) out(str,lay,s2) = sum1/sum2
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
      
