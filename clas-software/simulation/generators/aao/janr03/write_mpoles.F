      subroutine write_mpoles(ww,q2,rat)
      
#include "multipoles.inc"
#include "highmult.inc"

      real Reac(2,2,3,12),fac(2),rat,ww,q2
      
      data fac/0.141383,0.141383/
	
c i=1,2 (real,imag) j=1,2 (pi0,pi+) k=1,2,3 (M,E,S) l=1-12 (0-,0+,1-,1+,2-,2+,etc.)	

        do i=1,2
          do j=1,2
            do k=1,3
              do l=1,12
                Reac(i,j,k,l) = 0
              enddo
            enddo
          enddo
        enddo
	
        do i=1,2
            Reac(i,1,2,2)= ME1(i,1,2)+ME3(i,5)/1.5
            Reac(i,2,2,2)= sqrt(2.)*(ME1(i,1,2)-ME3(i,5)/3.)
            Reac(i,1,3,2)= MS1(i,1,2)+MS3(i,5)/1.5
            Reac(i,2,3,2)= sqrt(2.)*(MS1(i,1,2)-MS3(i,5)/3.)
            Reac(i,1,1,3)= MM1(i,1,1)+MM3(i,8)/1.5
            Reac(i,2,1,3)= sqrt(2.)*(MM1(i,1,1)-MM3(i,8)/3.)*rat
            Reac(i,1,3,3)= MS1(i,1,1)+MS3(i,8)/1.5
            Reac(i,2,3,3)= sqrt(2.)*(MS1(i,1,1)-MS3(i,8)/3.)*rat
            Reac(i,1,1,4)= MM1(i,1,7)+MM3(i,1)/1.5
            Reac(i,2,1,4)= sqrt(2.)*(MM1(i,1,7)-MM3(i,1)/3.)*rat
            Reac(i,1,2,4)= ME1(i,1,7)+ME3(i,1)/1.5
            Reac(i,2,2,4)= sqrt(2.)*(ME1(i,1,7)-ME3(i,1)/3.)*rat
            Reac(i,1,3,4)= MS1(i,1,7)+MS3(i,1)/1.5
            Reac(i,2,3,4)= sqrt(2.)*(MS1(i,1,7)-MS3(i,1)/3.)*rat
            Reac(i,1,1,5)= MM1(i,1,3)+MM3(i,2)/1.5
            Reac(i,2,1,5)= sqrt(2.)*(MM1(i,1,3)-MM3(i,2)/3.)*rat**2
            Reac(i,1,2,5)= ME1(i,1,3)+ME3(i,2)/1.5
            Reac(i,2,2,5)= sqrt(2.)*(ME1(i,1,3)-ME3(i,2)/3.)*rat**2
            Reac(i,1,3,5)= MS1(i,1,3)+MS3(i,2)/1.5
            Reac(i,2,3,5)= sqrt(2.)*(MS1(i,1,3)-MS3(i,2)/3.)*rat**2
            Reac(i,1,1,6)= MM1(i,1,9)+MM3(i,7)/1.5
            Reac(i,2,1,6)= sqrt(2.)*(MM1(i,1,9)-MM3(i,7)/3.)*rat**2
            Reac(i,1,2,6)= ME1(i,1,9)+ME3(i,7)/1.5
            Reac(i,2,2,6)= sqrt(2.)*(ME1(i,1,9)-ME3(i,7)/3.)*rat**2
            Reac(i,1,3,6)= MS1(i,1,9)+MS3(i,7)/1.5
            Reac(i,2,3,6)= sqrt(2.)*(MS1(i,1,9)-MS3(i,7)/3.)*rat**2
            Reac(i,1,1,7)= MM1(i,1,6)+MM3(i,6)/1.5
            Reac(i,2,1,7)= sqrt(2.)*(MM1(i,1,6)-MM3(i,6)/3.)*rat**3
            Reac(i,1,2,7)= ME1(i,1,6)+ME3(i,6)/1.5
            Reac(i,2,2,7)= sqrt(2.)*(ME1(i,1,6)-ME3(i,6)/3.)*rat**3
            Reac(i,1,3,7)= MS1(i,1,6)+MS3(i,6)/1.5
            Reac(i,2,3,7)= sqrt(2.)*(MS1(i,1,6)-MS3(i,6)/3.)*rat**3
            Reac(i,1,1,8)= MM1(i,1,8)+MM3(i,3)/1.5
            Reac(i,2,1,8)= sqrt(2.)*(MM1(i,1,8)-MM3(i,3)/3.)*rat**3
            Reac(i,1,2,8)= ME1(i,1,8)+ME3(i,3)/1.5
            Reac(i,2,2,8)= sqrt(2.)*(ME1(i,1,8)-ME3(i,3)/3.)*rat**3
            Reac(i,1,3,8)= MS1(i,1,8)+MS3(i,3)/1.5
            Reac(i,2,3,8)= sqrt(2.)*(MS1(i,1,8)-MS3(i,3)/3.)*rat**3
         enddo
         
         Reac(1,1,1,9) = M4m(1)+M4m(3)/1.5
         Reac(1,2,1,9) = sqrt(2.)*(M4m(1)-M4m(3)/3.) 
         Reac(1,1,2,9) = E4m(1)+E4m(3)/1.5
         Reac(1,2,2,9) = sqrt(2.)*(E4m(1)-E4m(3)/3.) 
         Reac(1,1,3,9) = S4m(1)+S4m(3)/1.5
         Reac(1,2,3,9) = sqrt(2.)*(S4m(1)-S4m(3)/3.) 
         Reac(1,1,1,10) = M4p(1)+M4p(3)/1.5
         Reac(1,2,1,10) = sqrt(2.)*(M4p(1)-M4p(3)/3.) 
         Reac(1,1,2,10) = E4p(1)+E4p(3)/1.5
         Reac(1,2,2,10) = sqrt(2.)*(E4p(1)-E4p(3)/3.) 
         Reac(1,1,3,10) = S4p(1)+S4p(3)/1.5
         Reac(1,2,3,10) = sqrt(2.)*(S4p(1)-S4p(3)/3.) 
         Reac(1,1,1,11) = M5m(1)+M5m(3)/1.5
         Reac(1,2,1,11) = sqrt(2.)*(M5m(1)-M5m(3)/3.) 
         Reac(1,1,2,11) = E5m(1)+E5m(3)/1.5
         Reac(1,2,2,11) = sqrt(2.)*(E5m(1)-E5m(3)/3.) 
         Reac(1,1,3,11) = S5m(1)+S5m(3)/1.5
         Reac(1,2,3,11) = sqrt(2.)*(S5m(1)-S5m(3)/3.) 
         Reac(1,1,1,12) = M5p(1)+M5p(3)/1.5
         Reac(1,2,1,12) = sqrt(2.)*(M5p(1)-M5p(3)/3.) 
         Reac(1,1,2,12) = E5p(1)+E5p(3)/1.5
         Reac(1,2,2,12) = sqrt(2.)*(E5p(1)-E5p(3)/3.) 
         Reac(1,1,3,12) = S5p(1)+S5p(3)/1.5
         Reac(1,2,3,12) = sqrt(2.)*(S5p(1)-S5p(3)/3.) 
         
         do j=1,2
          write(j,330) ww,q2
          write(j,331) 'SL+'
          write(j,332) ((Reac(i,j,3,l)/fac(j),i=1,2),l=2,6,2)
          write(j,332) ((Reac(i,j,3,l)/fac(j),i=1,2),l=8,12,2)
          write(j,331) 'SL-'
          write(j,332) ((Reac(i,j,3,l)/fac(j),i=1,2),l=1,5,2)
          write(j,332) ((Reac(i,j,3,l)/fac(j),i=1,2),l=7,11,2)
          write(j,331) 'EL+'
          write(j,332) ((Reac(i,j,2,l)/fac(j),i=1,2),l=2,6,2)
          write(j,332) ((Reac(i,j,2,l)/fac(j),i=1,2),l=8,12,2)
          write(j,331) 'EL-'
          write(j,332) ((Reac(i,j,2,l)/fac(j),i=1,2),l=1,5,2)
          write(j,332) ((Reac(i,j,2,l)/fac(j),i=1,2),l=7,11,2)
          write(j,331) 'ML+'
          write(j,332) ((Reac(i,j,1,l)/fac(j),i=1,2),l=2,6,2)
          write(j,332) ((Reac(i,j,1,l)/fac(j),i=1,2),l=8,12,2)
          write(j,331) 'ML-'
          write(j,332) ((Reac(i,j,1,l)/fac(j),i=1,2),l=1,5,2)
          write(j,332) ((Reac(i,j,1,l)/fac(j),i=1,2),l=7,11,2)
         enddo
         
         do j=1,2
          write(20+j,333) ((Reac(i,j,2,l)/fac(j),i=1,2),l=2,12,2)
          write(20+j,333) ((Reac(i,j,2,l)/fac(j),i=1,2),l=1,11,2)
          write(20+j,333) ((Reac(i,j,1,l)/fac(j),i=1,2),l=2,12,2)
          write(20+j,333) ((Reac(i,j,1,l)/fac(j),i=1,2),l=1,11,2)
          write(20+j,333) ((Reac(i,j,3,l)/fac(j),i=1,2),l=2,12,2)
          write(20+j,333) ((Reac(i,j,3,l)/fac(j),i=1,2),l=1,11,2)
         enddo
         
 330  format('W= ',f5.3,1x,'Q2= ',f5.3)
 331  format(1x,a3)
 332  format(4x,6e15.5)
 333  format(6f9.4,6f9.4)
 
      end
         