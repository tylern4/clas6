      subroutine janr_input_ebac()
      
#include "ebac.inc"

      print *, 'JANR_INPUT_EBAC: Reading EBAC tables'
      
c     Read EBAC tables
      
      open(UNIT=67,file='ebac/s11.dat',status='old')
      do i=1,48
      read(67,*,end=111)ewsa(i),es11r(i),es11i(i),at8,at9
	 ewsa(i)=ewsa(i)/1000.
      enddo
      
 111  close(67)
 
      open(UNIT=67,file='ebac/s31.dat',status='old')
      do i=1,48
      read(67,*,end=112)ewsa(i),es31r(i),es31i(i),at8,at9
	 ewsa(i)=ewsa(i)/1000.
      enddo
      
 112  close(67)
 
      open(UNIT=67,file='ebac/p11.dat',status='old')
      do i=1,48
      read(67,*,end=113)ewsa(i),ep11r(i),ep11i(i),at8,at9
	 ewsa(i)=ewsa(i)/1000.
      enddo
      
 113  close(67)
 
      open(UNIT=67,file='ebac/p13.dat',status='old')
      do i=1,48
      read(67,*,end=114)ewsa(i),ep13r(i),ep13i(i),at8,at9
	 ewsa(i)=ewsa(i)/1000.
      enddo
 
 114  close(67)
 
      open(UNIT=67,file='ebac/p31.dat',status='old')
      do i=1,48
      read(67,*,end=115)ewsa(i),ep31r(i),ep31i(i),at8,at9
	 ewsa(i)=ewsa(i)/1000.
      enddo
      
 115  close(67)
 
      open(UNIT=67,file='ebac/p33.dat',status='old')
      do i=1,48
      read(67,*,end=116)ewsa(i),ep33r(i),ep33i(i),at8,at9
	 ewsa(i)=ewsa(i)/1000.
      enddo

 116  close(67)
 
      open(UNIT=67,file='ebac/d13.dat',status='old')
      do i=1,48
      read(67,*,end=117)ewsa(i),ed13r(i),ed13i(i),at8,at9
	 ewsa(i)=ewsa(i)/1000.
      enddo
      
 117  close(67)
 
      open(UNIT=67,file='ebac/d15.dat',status='old')
      do i=1,48
      read(67,*,end=118)ewsa(i),ed15r(i),ed15i(i),at8,at9
	 ewsa(i)=ewsa(i)/1000.
      enddo
      
 118  close(67)
 
      open(UNIT=67,file='ebac/d33.dat',status='old')
      do i=1,48
      read(67,*,end=119)ewsa(i),ed33r(i),ed33i(i),at8,at9
	 ewsa(i)=ewsa(i)/1000.
      enddo
      
 119  close(67)
 
      open(UNIT=67,file='ebac/f15.dat',status='old')
      do i=1,48
      read(67,*,end=120)ewsa(i),ef15r(i),ef15i(i),at8,at9
	 ewsa(i)=ewsa(i)/1000.
      enddo
      
 120  close(67)
      
      open(UNIT=67,file='ebac/f17.dat',status='old')
      do i=1,48
      read(67,*,end=121)ewsa(i),ef17r(i),ef17i(i),at8,at9
	 ewsa(i)=ewsa(i)/1000.
      enddo
      
 121  close(67)
      
      open(UNIT=67,file='ebac/f35.dat',status='old')
      do i=1,48
      read(67,*,end=122)ewsa(i),ef35r(i),ef35i(i),at8,at9
	 ewsa(i)=ewsa(i)/1000.
      enddo
      
 122  close(67)
      
      open(UNIT=67,file='ebac/f37.dat',status='old')
      do i=1,48
      read(67,*,end=123)ewsa(i),ef37r(i),ef37i(i),at8,at9
	 ewsa(i)=ewsa(i)/1000.
      enddo
      
 123  close(67)


      end
      
      




