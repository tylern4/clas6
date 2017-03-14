      subroutine janr_input_said()
      
#include "multipoles.inc"

      print *, 'JANR_INPUT_SAID: Reading SAID tables'
      
c     Read SAID tables

      open(UNIT=67,file='multipols/mp33.dat',status='old')
      do i=1,51
      read(67,*,end=110)wsa(i),mp33r(i),mp33i(i),at2,at3,at4
	 wsa(i)=wsa(i)/1000.
	 mp33r(i)=mp33r(i)/52.437
	 mp33i(i)=mp33i(i)/52.437
      enddo
      
 110  close(67)
 
      open(UNIT=67,file='multipols/s11.dat',status='old')
      do i=1,51
      read(67,*,end=111)wsa(i),at2,at3,at4,at5,s11r(i),s11i(i),at8,at9
	 wsa(i)=wsa(i)/1000.
      enddo
      
 111  close(67)
 
      open(UNIT=67,file='multipols/s31.dat',status='old')
      do i=1,51
      read(67,*,end=112)wsa(i),at2,at3,at4,at5,s31r(i),s31i(i),at8,at9
	 wsa(i)=wsa(i)/1000.
      enddo
      
 112  close(67)
 
      open(UNIT=67,file='multipols/p11.dat',status='old')
      do i=1,51
      read(67,*,end=113)wsa(i),at2,at3,at4,at5,p11r(i),p11i(i),at8,at9
	 wsa(i)=wsa(i)/1000.
      enddo
      
 113  close(67)
 
      open(UNIT=67,file='multipols/p13.dat',status='old')
      do i=1,51
      read(67,*,end=114)wsa(i),at2,at3,at4,at5,p13r(i),p13i(i),at8,at9
	 wsa(i)=wsa(i)/1000.
      enddo
 
 114  close(67)
 
      open(UNIT=67,file='multipols/p31.dat',status='old')
      do i=1,51
      read(67,*,end=115)wsa(i),at2,at3,at4,at5,p31r(i),p31i(i),at8,at9
	 wsa(i)=wsa(i)/1000.
      enddo
      
 115  close(67)
 
      open(UNIT=67,file='multipols/p33.dat',status='old')
      do i=1,51
      read(67,*,end=116)wsa(i),at2,at3,at4,at5,p33r(i),p33i(i),at8,at9
	 wsa(i)=wsa(i)/1000.
      enddo

 116  close(67)
 
      open(UNIT=67,file='multipols/d13.dat',status='old')
      do i=1,51
      read(67,*,end=117)wsa(i),at2,at3,at4,at5,d13r(i),d13i(i),at8,at9
	 wsa(i)=wsa(i)/1000.
      enddo
      
 117  close(67)
 
      open(UNIT=67,file='multipols/d15.dat',status='old')
      do i=1,51
      read(67,*,end=118)wsa(i),at2,at3,at4,at5,d15r(i),d15i(i),at8,at9
	 wsa(i)=wsa(i)/1000.
      enddo
      
 118  close(67)
 
      open(UNIT=67,file='multipols/d33.dat',status='old')
      do i=1,51
      read(67,*,end=119)wsa(i),at2,at3,at4,at5,d33r(i),d33i(i),at8,at9
	 wsa(i)=wsa(i)/1000.
      enddo
      
 119  close(67)
 
      open(UNIT=67,file='multipols/f15.dat',status='old')
      do i=1,51
      read(67,*,end=120)wsa(i),at2,at3,at4,at5,f15r(i),f15i(i),at8,at9
	 wsa(i)=wsa(i)/1000.
      enddo
      
 120  close(67)
      
      open(UNIT=67,file='multipols/f37.dat',status='old')
      do i=1,51
      read(67,*,end=121)wsa(i),at2,at3,at4,at5,f37r(i),f37i(i),at8,at9
	 wsa(i)=wsa(i)/1000.
      enddo
      
 121  close(67)
 
      open(UNIT=67,file='multipols/pim.dat',status='old')
      do i=1,13
         read(67,*,end=122)wsa(i),pimr(i),pimi(i)
         wsa(i)=wsa(i)/1000.
      enddo
      
 122  close(67)
 
      open(UNIT=67,file='multipols/sim.dat',status='old')
      do i=1,13
         read(67,*,end=123)wsa(i),simr(i),simi(i)
         wsa(i)=wsa(i)/1000.
      enddo
      
 123  close(67)

      end
      
      

