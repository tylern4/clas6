c     Sets result to lorentz transformation matrix from beta 3-vector
      function lorentz_boost(beta,result)
      implicit none

      integer lorentz_boost
      real beta(3)
      real result(4,4)

      real b2                   ! beta^2
      real b                    ! beta magnitude
      real gamma

      integer i,j

      b2 = 0
      do i=1,3
         b2=b2+beta(i)**2
      enddo

      if(b2 .gt. 1.) then
         lorentz_boost = 0
         goto 1
      endif

      lorentz_boost = 1
      
      b=sqrt(b2)
      gamma=1/sqrt(1-b2)

c     no boost case:
      if(b2 .eq. 0) then
         do i=1,4
            do j=1,4
               if(i .eq. j) then
                  result(i,j)=1
               else
                  result(i,j)=0
               endif
            enddo
         enddo
         goto 1
      endif

c     boost case: matrix is symmetric, so we can fill one side and then
c     set the other ot be the same

c     main diagonal:

c     Top-left corner:
      result(1,1)=gamma

c     spatial diagonal:
      do i=2,4
         result(i,i) = 1 + (gamma-1)*beta(i-1)*beta(i-1)/b2
      enddo

c     spatial off-diagonal:
      do i=2,3
         do j=i+1,4
            result(i,j) = (gamma-1)*beta(i-1)*beta(j-1)/b2
            result(j,i) = result(i,j)
         enddo
      enddo

c     rest of first row and first column
      do i=2,4
         result(1,i) = -(gamma*beta(i-1))
         result(i,1) = result(1,i)
      enddo

 1    return
      end
