      subroutine mult_ana(qq)
  
      IMPLICIT NONE
      SAVE

#include "inpar.inc"
#include "janrtab.inc"
#include "fit_par.inc"
#include "remult.inc"
#include "multampl.inc"
     
      Integer Ierr

      INTEGER iw,iq2,icos,iphi,i,j
      INTEGER Nw,Nq2,Ncos,Nphi
      REAL qq,Q2,W,CosTh, Phi, Epsilon, nu, ep, mp, El
      REAL gauss, Fintg, Finthigh, W_min, W_step
      external gauss,Fintg,Finthigh
      integer lm,is,ii,jj,ifile,lun
      character*10 fnamr(24)
      data fnamr/'rm0p.dat','rm0m.dat','re0p.dat','re0m.dat','rs0p.dat','rs0m.dat',
     +     'rm1p.dat','rm1m.dat','re1p.dat','re1m.dat','rs1p.dat','rs1m.dat',
     +     'rm2p.dat','rm2m.dat','re2p.dat','re2m.dat','rs2p.dat','rs2m.dat',
     +     'rm3p.dat','rm3m.dat','re3p.dat','re3m.dat','rs3p.dat','rs3m.dat'/
      character*10 fnamrr(24)
      data fnamrr/'rrm0p.dat','rrm0m.dat','rre0p.dat','rre0m.dat',
     +     'rrs0p.dat','rrs0m.dat',
     +     'rrm1p.dat','rrm1m.dat','rre1p.dat','rre1m.dat','rrs1p.dat','rrs1m.dat',
     +     'rrm2p.dat','rrm2m.dat','rre2p.dat','rre2m.dat','rrs2p.dat','rrs2m.dat',
     +     'rrm3p.dat','rrm3m.dat','rre3p.dat','rre3m.dat','rrs3p.dat','rrs3m.dat'/
      character*10 fnamhigh(12)
      data fnamhigh/'rm4p.dat','rm4m.dat','re4p.dat','re4m.dat','rs4p.dat','rs4m.dat',
     +     'rm5p.dat','rm5m.dat','re5p.dat','re5m.dat','rs5p.dat','rs5m.dat'/
      common/ifintg/lm,is,ii,jj,w,q2
      
      DATA mp/0.938/
      
      if (IntAccur.eq.0) IntAccur=0.05
      print *, 'MULT_ANA: IntAccur = ',IntAccur
      
      ierr=0
      cM1=start_value(1)
      cE1=start_value(2)
      cS1=start_value(3)
      cMP1=start_value(4)
      cSP1=start_value(5)
      cEP2=start_value(6)
      cSP2=start_value(7)
      cMP3=start_value(8)
      cEP3=start_value(9)
      cSP3=start_value(10)
      cEP4=start_value(11)
      cSP4=start_value(12)
      cEP5=start_value(13)
      cSP6=start_value(14)
      cMP6=start_value(15)
      cEP6=start_value(16)
      cSP6=start_value(17)
      cMP7=start_value(18)
      cEP7=start_value(19)
      cSP7=start_value(20)
      cMP8=start_value(21)
      cEP8=start_value(22)
      cSP8=start_value(23)
      cMP9=start_value(24)
      cEP9=start_value(25)
      cMP10=start_value(26)
      cEP10=start_value(27)
      cMP11=start_value(28)
      cEP11=start_value(29)
      cSPA=start_value(30)
      
      write(6,111)(start_value(i),i=1,parms)
 111  format(25x,'Value of parameters => 1-30 ',/,
     +'   cM1 =',f9.5,'   cE1 =',f9.5,'   cS1 =',f9.5,/,
     +'   cMP1 =',f9.5,'  cSP1 =',f9.5,'  cEP2 =',f9.5,'   cSP2 =',f9.5,/,
     +'   cMP3 =',f9.5,'  cEP3 =',f9.5,'  cSP3 =',f9.5,/,
     +'   cEP4 =',f9.5,'  cSP4 =',f9.5,'  cEP5 =',f9.5,'   cSP5 =',f9.5,/,
     +'   cMP6 =',f9.5,'  cEP6 =',f9.5,'  cSP6 =',f9.5,/,
     +'   cMP7 =',f9.5,'  cEP7 =',f9.5,'  cSP7 =',f9.5,/,
     +'   cMP8 =',f9.5,'  cEP8 =',f9.5,'  cSP8 =',f9.5,/,
     +'   cMP9 =',f9.5,'  cEP9 =',f9.5,'  cMP10 =',f9.5,'  cEP10 =',f9.5,/,  
     +'   cMP11 =',f9.5,' cEP11 =',f9.5,/,
     +'   cSPA =',f9.5,/)   
     
      write(6,*) 'MULT_ANA: Evaluating background at Q2= ',qq
      
      Nw = 101
      W_min=1.1
      W_step=0.01
      
      write(6,*)'MULT_ANA: Number of W points = ',nw
      write(6,*)'MULT_ANA: W_min,W_step = ',W_min,W_step

      DO jj=1,1
      
      do lm=0,3
         do is=1,6
            lun=(lm+2)*10+is
            ifile=lm*6+is
            open(unit=lun,file='multipols/'//fnamr(ifile),status='UNKNOWN')
            write(lun,*)fnamr(ifile)
            write(lun,*)
         enddo
      enddo   
     
      Q2 = qq

      Do iw=1,Nw
c     Loop over W
         W = W_min + float(iw-1)*W_step
         el=(w**2-mp**2)/2./mp
         do lm=0,3
            do is=1,3
               do ii=1,6
                  Rint(lm,is,ii,jj)=gauss(Fintg,-1.,1.,IntAccur)
               enddo
            enddo
            do is=1,3
               call re_mult()
            enddo
            lun=(lm+2)*10
            write(lun+1,201)W,El,(Mlp(lm,is,jj),is=1,3)
            write(lun+2,201)W,El,(Mlm(lm,is,jj),is=1,3)
            write(lun+3,201)W,El,(Elp(lm,is,jj),is=1,3)
            write(lun+4,201)W,El,(Elm(lm,is,jj),is=1,3)
            write(lun+5,201)W,El,(Slp(lm,is,jj),is=1,3)
            write(lun+6,201)W,El,(Slm(lm,is,jj),is=1,3)
         enddo
      EndDo
      
      do lm=0,3
         do is=1,6
            lun=(lm+2)*10+is
            close(lun)
         enddo
      enddo
         
      endDO  
       
      DO jj=2,2
      
      do lm=0,3
         do is=1,6
            lun=(lm+2)*10+is
            ifile=lm*6+is
            open(unit=lun,file='multipols/'//fnamrr(ifile),status='UNKNOWN')
            write(lun,*)fnamrr(ifile)
            write(lun,*)
         enddo
      enddo   
     
      Q2 = qq

      Do iw=1,Nw
c     Loop over W
         W = W_min + float(iw-1)*W_step
         el=(w**2-mp**2)/2./mp
         do lm=0,3
            do is=1,3
               do ii=1,6
                  Rint(lm,is,ii,jj)=gauss(Fintg,-1.,1.,IntAccur)
               enddo
            enddo
            do is=1,3
               call re_mult()
            enddo
            lun=(lm+2)*10
            write(lun+1,201)W,El,(Mlp(lm,is,jj),is=1,3)
            write(lun+2,201)W,El,(Mlm(lm,is,jj),is=1,3)
            write(lun+3,201)W,El,(Elp(lm,is,jj),is=1,3)
            write(lun+4,201)W,El,(Elm(lm,is,jj),is=1,3)
            write(lun+5,201)W,El,(Slp(lm,is,jj),is=1,3)
            write(lun+6,201)W,El,(Slm(lm,is,jj),is=1,3)
         enddo
      EndDo
      
      do lm=0,3
         do is=1,6
            lun=(lm+2)*10+is
            close(lun)
         enddo
      enddo
         
      endDO   
      
      do lm=4,5
         do is=1,6
            lun=(lm+2)*10+is
            ifile=(lm-4)*6+is
            open(unit=lun,file='multipols/'//fnamhigh(ifile),status='UNKNOWN')
            write(lun,*)fnamhigh(ifile)
            write(lun,*)
         enddo
      enddo   
     
      Q2 = qq
      jj = 1

      Do iw=1,Nw
c     Loop over W
         W = W_min + float(iw-1)*W_step
         el=(w**2-mp**2)/2./mp
         do lm=4,5
            do is=1,3
               do ii=1,6
                  Rint(lm,is,ii,jj)=gauss(Finthigh,-1.,1.,IntAccur)
               enddo
            enddo
            do is=1,3
               call re_mult()
            enddo
            lun=(lm+2)*10
            write(lun+1,201)W,El,(Mlp(lm,is,jj),is=1,3)
            write(lun+2,201)W,El,(Mlm(lm,is,jj),is=1,3)
            write(lun+3,201)W,El,(Elp(lm,is,jj),is=1,3)
            write(lun+4,201)W,El,(Elm(lm,is,jj),is=1,3)
            write(lun+5,201)W,El,(Slp(lm,is,jj),is=1,3)
            write(lun+6,201)W,El,(Slm(lm,is,jj),is=1,3)
         enddo
      EndDo
      do lm=4,5
         do is=1,6
            lun=(lm+2)*10+is
            close(lun)
         enddo
      enddo  
      
 201  format(2x,2F7.3,2x,3F12.5)
 
      return
      end
      
