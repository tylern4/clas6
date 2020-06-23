      subroutine test(s1,s2)
      
      vector in1(1296,6,6)
      vector in2(1296,6)
      vector out(36,6,6)
      
      real tmp1(71),tmp2(71)
      real inn1(1296),inn2(1296)
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
      
            
