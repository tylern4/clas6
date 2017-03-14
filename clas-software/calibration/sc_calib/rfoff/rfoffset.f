      subroutine rfoffset(ftype)
c Routine to read results of fit to rfoffsets for each paddle, from 
c generated 6 files, and make one file for p2p offsets for DB.
c In the case of electrons only paddles 1 to 22 are involved.
c In case of charge hadrons all paddles are involved. However, only
c pi+ case have meaning, in that case paddles above 22 are shifted to 
c line up with paddles 10 to 17, assuming that they are calibrated 
c already with electrons. 
c Input parameter - 0. for electrons, 1. for pi+, 2. for protons, 3. for pi-
      real ftyp
c
      real x(50),y(50),dy,z,dz,ysum,shift
      integer i,j,k,k1,sector_pd,iread
      integer npd1,npd2
      data npd1,npd2/10,17/
c
      open(unit=21,file='rfoff1.dat',status='old')
      open(unit=22,file='rfoff2.dat',status='old')
      open(unit=23,file='rfoff3.dat',status='old')
      open(unit=24,file='rfoff4.dat',status='old')
      open(unit=25,file='rfoff5.dat',status='old')
      open(unit=26,file='rfoff6.dat',status='old')
      open(unit=20,file='p2p_off_pp.dat',status='unknown')
c
      do j=1,50
         x(j)=0.
         y(j)=0.
      enddo
c
      do i=1,6
         iread=20+i
         do j=1,50
            sector_pd=i*100+j
            read(iread,*,end=101)x(j),y(j),dy,z,dz
 222        format(2i7,f12.3)
         enddo
 101     close(iread)
         ysum=0.
         if(ftype.eq.1.)then
            do k=1,j-1
               if(x(k).ge.npd1.and.x(k).le.npd2)then
                  ysum=ysum+y(k)
               endif
            enddo
            shift=ysum/float(npd2-npd1+1)
         else
            shift=0.
         endif
c
         Do k=1,j-1
            if(ftype.eq.0..or.x(k).gt.npd2)then
               write(20,222)i,int(x(k)),y(k)-shift
            endif
         enddo
      enddo
      close(20)
c
      return
      end









