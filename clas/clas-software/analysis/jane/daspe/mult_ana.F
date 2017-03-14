      Subroutine mult_ana
c
      IMPLICIT NONE
      SAVE
c
#include "daspetab.inc" 
#include "fit_par.inc"  
#include "remult.inc"   
c
      Integer Ierr

c
      INTEGER iw,iq2,icos,iphi,i,j
      INTEGER Nw,Nq2,Ncos,Nphi
      REAL Q2, W, CosTh, Phi, Epsilon, nu, ep, mp, El
      REAL Fint, gauss,e0p,s0p,m1m,s1m,m1p,e1p,s1p,m2m,e2m,s2m
      REAL s,w2,mpion,mnucleon,E,Egamma,Epion,k22,q22,qpion,kgamma
      REAL aae(4,3),aas(4,3),ae0p,as0p,am1p,ae1p,as1p,am1m,as1m
      REAL am2m,ae2m,as2m
      external gauss,fint
      integer lm,is,ii,ifile,lun,jj
      character*10 fnamr(24),fnami(24)
      data fnamr/'rm0p.dat','rm0m.dat','re0p.dat','re0m.dat','rs0p.dat',
     +'rs0m.dat',
     +'rm1p.dat','rm1m.dat','re1p.dat','re1m.dat','rs1p.dat','rs1m.dat',
     +'rm2p.dat','rm2m.dat','re2p.dat','re2m.dat','rs2p.dat','rs2m.dat',
     +'rm3p.dat','rm3m.dat','re3p.dat','re3m.dat','rs3p.dat','rs3m.dat'/
      data fnami/'im0p.dat','im0m.dat','ie0p.dat','ie0m.dat','is0p.dat',
     +'is0m.dat',
     +'im1p.dat','im1m.dat','ie1p.dat','ie1m.dat','is1p.dat','is1m.dat',
     +'im2p.dat','im2m.dat','ie2p.dat','ie2m.dat','is2p.dat','is2m.dat',
     +'im3p.dat','im3m.dat','ie3p.dat','ie3m.dat','is3p.dat','is3m.dat'/
      common/ifint/j,lm,is,ii,w,q2
      DATA mp/0.938/
c_end_var
c
c  executable code for routine Mult_ana:
c----6----------------------------------------------------------------72
      ierr=0
      	cM=start_value(1)
      	cE=start_value(2)
      	cS=start_value(3)
         ap11=start_value(4)
         sp11=start_value(5)
         as11=start_value(6)
         ss11=start_value(7)
         a1d13=start_value(8)
         a3d13=start_value(9)
         sd13=start_value(10)
         cas11=start_value(11)
         css11=start_value(12)
         cas31=start_value(13)
         css31=start_value(14)
         a1f15=start_value(15)
         a3f15=start_value(16)
         sf15=start_value(17)
         ca1d33=start_value(18)
         ca3d33=start_value(19)
         csd33=start_value(20)
         ca1d13=start_value(21)
         ca3d13=start_value(22)
         csd13=start_value(23)
         cap11=start_value(24)
         csp11=start_value(25)
         a1d15=start_value(26)
         a3d15=start_value(27)
         sd15=start_value(28)
         a1p13=start_value(29)
         a3p13=start_value(30)
         sp13=start_value(31)
         a1p33=start_value(32)
         a3p33=start_value(33)
         sp33=start_value(34)
         cw1=start_value(35)
         cw2=start_value(36)
         cw3=start_value(37)
         cw4=start_value(38)
         cw5=start_value(39)
         cw6=start_value(40)
         cw7=start_value(41)
         cw8=start_value(42)
         cw9=start_value(43)
         cw10=start_value(44)
         cw11=start_value(45)
         cw12=start_value(46)
         cw13=start_value(47)
         cw14=start_value(48)
         cw15=start_value(49)
C
      IF ( Wstep.GT.0 .AND. Wmin+Wstep.LT.Wmax )THEN
         Nw = INT( (Wmax - Wmin) / Wstep ) + 1
      ELSE
         If( Wmin.Eq.Wmax )Then
            Nw=1
         EndIf
      ENDIF
      IF ( Q2step.GT.0 .AND. Q2min+Q2step.LT.Q2max )THEN
         Nq2 = INT( (Q2max - Q2min) / Q2step ) + 1
      ELSE
         If( Q2min.Eq.Q2max )Then
            Nq2=1
         EndIf
      ENDIF
      write(6,*)'Number of W points = ',nw,'     Number of Q2 points = ',nq2
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      do j=1,2
      do lm=0,3
         do is=1,6
            lun=(lm+2)*10+is+(j-1)*40
            ifile=lm*6+is
            if(j.eq.1)then
               open(unit=lun,file=fnamr(ifile),status='UNKNOWN')
                write(lun,*)
            else
               open(unit=lun,file=fnami(ifile),status='UNKNOWN')
                write(lun,*)
            endif
         enddo
      enddo   
      enddo
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      DO iq2=1,Nq2
c Loop over Q2
          Q2 = Q2min + float(iq2-1)*Q2step
          Do iw=1,Nw
c Loop over W
             W = Wmin + float(iw-1)*Wstep
             el=(w**2-mp**2)/2./mp
      mnucleon=0.938
      mpion=0.138	
      s=w**2
      w2=2.*w
      E=s-mnucleon**2
      Egamma=(E-Q2)/w2
      Epion=(E+mpion**2)/w2  
      q22=Epion**2-mpion**2
      k22=(w-Egamma)**2-mnucleon**2
      qpion=sqrt(q22)
      kgamma=sqrt(k22)
C**************************************************************************
          do j=1,2
CCC Re     j=1
CCC Im     j=2

             do lm=0,3
CCC lm=0,1,2,3 -> 0+,1+(1-),2+(2-),3+(3-)
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
                do is=1,3
CCC is=1 -> 3/2
CCC is=2 -> 1/2
CCC is=3 -> 0
                   do ii=1,6
                      Rint(lm,is,ii)=gauss(Fint,-1.,1.,IntAccur)
                   enddo
                enddo
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C                   do ii=1,6
C	 1 -> 3/2
C	 2 -> p1/2
C	 3 -> n1/2
C                      Rint(lm,2,ii)= Rint(lm,2,ii)/3.+Rint(lm,3,ii)
C                      Rint(lm,3,ii)= -Rint(lm,2,ii)+Rint(lm,3,ii)*2.
C                   enddo
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
                do is=1,3
                   call re_mult()
                enddo
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
                lun=(lm+2)*10+(j-1)*40
                write(lun+1,201)w,(Mlp(lm,is),is=1,3)
                write(lun+2,201)w,(Mlm(lm,is),is=1,3)
                write(lun+3,201)w,(Elp(lm,is),is=1,3)
                write(lun+4,201)w,(Elm(lm,is),is=1,3)
                write(lun+5,201)w,(Slp(lm,is),is=1,3)
                write(lun+6,201)w,(Slm(lm,is),is=1,3)
CCC lm=0,1,2,3 -> 0+(0-),1+(1-),2+(2-),3+(3-)
             enddo
CCC j=1,2 -> Re, Im
             enddo
C************************************************************************
CCC          Q2 = Q2min + float(iq2-1)*Q2step
          EndDo
CCC      DO iq2=1,Nq2
      ENDDO
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 201  format(2x,2F7.3,2x,3F12.5)
      RETURN
 999   continue
       return
       end
      
