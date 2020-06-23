c     Some matrix routines

c     each routine returns 0 for error, 1 for success

c     Take care not to supply a matrix as both an input argument and a
c     result argument

      function matrix_mult(ar,ac,am,br,bc,bm,result)
      implicit none

      integer ar, ac, br, bc
      real am(ar,ac)
      real bm(br,bc)
      real result(ar,bc)
      integer matrix_mult

      integer i,j,k

      if(ac .ne. br) then
         matrix_mult=0
         goto 1
      endif

      matrix_mult=1

      do i=1,ar
         do j=1,bc
            result(i,j) = 0
            do k=1,ac
               result(i,j) = result(i,j) + am(i,k)*bm(k,j)
            enddo
         enddo
      enddo

 1    return
      end
