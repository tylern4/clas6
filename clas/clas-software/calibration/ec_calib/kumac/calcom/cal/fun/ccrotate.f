      subroutine ccrotate(n)
      
      vector cc1(18,2,6)
      vector cc2(2,18,6)
      
      integer i,j,k,n
      
      do i = 1,6
        do j = 1,2
          do k = 1,18
            if (n.eq.1) cc2(j,k,i) = cc1(k,j,i)
            if (n.eq.2) cc1(k,j,i) = cc2(j,k,i)
          enddo
        enddo
      enddo
      
      end
            