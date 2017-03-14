      program lee_dummy

      implicit none

C Passed variables:

C Local variables:

      integer n1, n2, n3, i, j
      character*72  dummy
      character*8   dummy1
      character*7   dummy2
      character*9   name_poles(6)

      real mp(6)
      real w, q2
      data  name_poles / '   S_{L+}','   S_{L-}',
     1                   '   E_{L+}','   E_{L-}',
     1                   '   M_{L+}','   M_{L-}'/
      data  mp / 0.0, 0.0, 0.0, 0.0, 0.0, 0.0/
C
      dummy1  = 'W=      '
      dummy2  = ' Q2=   '
C
C First open the file.
      open(unit=10,file='hlee_pi0.dat',form='formatted',status='old')
      open(unit=11,file='hlee_pi0.new',form='formatted',status='new')

C Begin reading the file.

c
       do n1 = 1, 77         ! Q2
         do n2 = 1, 26       ! W
           W = 1.1 + (n2 - 1)*0.01
           Q2= 0.2 + (n1 - 1)*0.05
           write(11,FMT=15) dummy1, W, dummy2, Q2
           read(10,'(a)')  dummy
           do n3 = 1, 18
             read(10,'(a)')  dummy
             write(11,'(a)') dummy
           enddo
         enddo
         do n2 = 27, 61
           W = 1.1 + (n2 - 1)*0.01
           Q2= 0.2 + (n1 - 1)*0.05
           write(11,FMT=15) dummy1, W, dummy2, Q2
 15        format(A8,f4.2,A7,f7.5)
           do i = 1, 6
             write(11,FMT=16) name_poles(i)
 16          format(A9)
             write(11,FMT=17) (mp(j), j = 1, 6)
             write(11,FMT=17) (mp(j), j = 1, 6)
 17          format(6E12.4)
           enddo
         enddo
       enddo

       stop
       end
