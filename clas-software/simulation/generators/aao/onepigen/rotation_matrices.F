      subroutine beam_inverse_rotation(beam, invrotate)
c     Subroutine for performing the inverse rotation on the final state
c     particles to bring them back to the proper orientation.
      implicit none

      real beam(4), invrotate(3,3)

      real dotprod, crossprod(3)

      data dotprod/0.0/, crossprod/3*0.0/

      real old_axis(3), new_axis(3)
      data old_axis/3*0.0/, new_axis/3*0.0/

      real mag
      data mag/0.0/

      real angle

      integer i, j

c     Initialize invrotate:
      do i=1,3
         do j=1,3
            invrotate(i,j)=0
         enddo
      enddo

c     To rotate a single axis of a coordinate system into its new
c     location, the cross product
c
c     old_axis x new_axis
c
c     yields a vector whose direction is the normal of the rotation
c     plane, and whose magnitude is given by
c
c     mag(old_axis) * mag(new_axis) * sin(theta)
c
c     with zero meaning that no change is necessary to complete the
c     rotation.

      do i=1,3
         old_axis(i)=beam(i+1)
         mag=mag+old_axis(i)**2
      enddo
      mag=sqrt(mag)

      do i=1,3
         old_axis(i)=old_axis(i)/mag
      enddo

      new_axis(3)=1

      crossprod(1) = old_axis(2)*new_axis(3) -
     +     old_axis(3)*new_axis(2)
      crossprod(2) = old_axis(3)*new_axis(1) -
     +     old_axis(1)*new_axis(3)
      crossprod(3) = old_axis(1)*new_axis(2) -
     +     old_axis(2)*new_axis(1)

c     Since both axes had magnitudes of 1, the magnitude of the cross
c     product must be sin(theta)

      mag=0
      do i=1,3
         mag = crossprod(i)**2+mag
      enddo
      mag=sqrt(mag)

      angle = asin(mag)

c     Normalize cross product vector

      do i=1,3
         crossprod(i)=crossprod(i)/mag
      enddo

      call rotation_matrix(crossprod,-angle,invrotate)

      return
      end 

      subroutine rotation_matrix(axis,angle,result)
      implicit none

      real axis(3), angle, result(3,3)

c     The three terms of the matrix:
c
c     R = cos(theta)*identity +
c     + sin(theta)*crossprod +
c     + (1-cos(theta))*tensorprod
      real identity(3,3)
      data identity/9*0.0/

      real crossprod(3,3)
      data crossprod/9*0.0/
      
      real tensorprod(3,3)
      data tensorprod/9*0.0/

      integer i, j

c     Setup identity matrix
      do i=1,3
         identity(i,i) = 1
      enddo

c     Setup cross product matrix
c     First row
      crossprod(1,2) = -axis(3)
      crossprod(1,3) = axis(2)

c     Second row
      crossprod(2,1) = axis(3)
      crossprod(2,3) = -axis(1)

c     Third row
      crossprod(3,1) = -axis(2)
      crossprod(3,2) = axis(1)

c     Setup tensor product matrix
      do i=1,3
         do j=1,3
            tensorprod(i,j) = axis(i)*axis(j)
         enddo
      enddo

c     Perform matrix sum:
      do i=1,3
         do j=1,3
            result(i,j) = identity(i,j)
     +           + sin(angle) * crossprod(i,j)
     +           + (1 - cos(angle)) * tensorprod(i,j)
         enddo
      enddo
      
      return
      end

c     debug
      subroutine beam_rotation(beam, rotate)
c     Subroutine for performing the rotation on the final state
c     particles to bring them back to the proper orientation.
      implicit none

      real beam(4), rotate(3,3)

      real dotprod, crossprod(3)

      data dotprod/0.0/, crossprod/3*0.0/

      real old_axis(3), new_axis(3)
      data old_axis/3*0.0/, new_axis/3*0.0/

      real mag
      data mag/0.0/

      real angle

      integer i, j

c     Initialize rotate:
      do i=1,3
         do j=1,3
            rotate(i,j)=0
         enddo
      enddo

c     To rotate a single axis of a coordinate system into its new
c     location, the cross product
c
c     old_axis x new_axis
c
c     yields a vector whose direction is the normal of the rotation
c     plane, and whose magnitude is given by
c
c     mag(old_axis) * mag(new_axis) * sin(theta)
c
c     with zero meaning that no change is necessary to complete the
c     rotation.

      do i=1,3
         old_axis(i)=beam(i+1)
         mag=mag+old_axis(i)**2
      enddo
      mag=sqrt(mag)

      do i=1,3
         old_axis(i)=old_axis(i)/mag
      enddo

      new_axis(3)=1

      crossprod(1) = old_axis(2)*new_axis(3) -
     +     old_axis(3)*new_axis(2)
      crossprod(2) = old_axis(3)*new_axis(1) -
     +     old_axis(1)*new_axis(3)
      crossprod(3) = old_axis(1)*new_axis(2) -
     +     old_axis(2)*new_axis(1)

c     Since both axes had magnitudes of 1, the magnitude of the cross
c     product must be sin(theta)

      mag=0
      do i=1,3
         mag = crossprod(i)**2+mag
      enddo
      mag=sqrt(mag)

      angle = asin(mag)

c     Normalize cross product vector

      do i=1,3
         crossprod(i)=crossprod(i)/mag
      enddo

      call rotation_matrix(crossprod,angle,rotate)

      return
      end 
c     end debug
