      subroutine hjanr_init(fname)
      integer nwpaw
      parameter (nwpaw=100000)
      character*60 fname(4),name
      
      common /pawc/ h(nwpaw)
      
      call hlimit(100000)
      call hplint(3)
      call hplsiz(25,20,' ')
      call hjanr_loadpar(1)
      do i=1,4
       name=fname(i)
       if(fname(i).ne.'JUNK') call hjanr_input(i,name)
       if(fname(i).eq.'JUNK') then
         name=fname(1)
         call hjanr_input(i,name)
       endif
      enddo
 
      end
      
      subroutine hjanr_input(if,file)
      
      real xxk(3)
      character*60 file
      logical iflag(3)
      
#include "hjanr_hcrs.inc"
      
      character*20 lab(4)
      character*8 lab1(2)
      character*6 lab2(4)
      common/hlab/lab
      data lab1/'([p]^0!)','([p]^+!)'/
      data lab2/'  d[s]','ALT^/!','ALT','[S]'/
            
      do i=1,3
        iflag(i) = .true.
        xxk(i)   = -1000.
      enddo
      
      print *, 'HJANR_INPUT: Reading file ',file
      
      open (unit=67,file=file,type='old')
      
      do k=1,7200
        read(67,*,end=100) xx(k,3,if),xx(k,4,if),xx(k,5,if),xx(k,2,if),
     1                     xx(k,1,if),crs(k,if),crse(k,if),d,d,d,
     1			   xx(k,6,if),xx(k,7,if)
        if (xx(k,3,if).eq.0.0) go to 100
        do i=1,3
          if (xx(k,i,if).gt.xxk(i).and.iflag(i)) then
            ndims(i,if)=ndims(i,if)+1
          elseif (xx(k,i,if).lt.xxk(i)) then
            iflag(i)=.false.
          endif
          xxk(i)=xx(k,i,if)
        enddo
      enddo
          
100   close(67)
      print *, 'nphi,ncos,nw=',ndims(1,if),ndims(2,if),ndims(3,if)
      
      lab(if) = lab2(xx(1,7,if))//lab1(xx(1,6,if))
      nplt = if
      
      end
            
      subroutine hjanr_plot 
      
      character*3 opt
      real xtit(4)
      character*20 lab(4)
      common/hlab/lab
      
#include "hjanr_hlim.inc"
#include "hjanr_hplt.inc"
#include "hjanr_hcrs.inc"

      data c/9,4,7,8/
      data f/3,2,3,3/
      data w/6,19,9,11/
c      data c/1,1,1,1/
c      data f/1,1,1,1/
c      data w/1,1,1,1/
      data xtit/7.1,10.7,14.3,17.9/
      
      call hplzon(1,5,4,' ')      
      call hjanr_plot_par 
      
      call hplset('NDVX',24.)
      call hplset('XWIN',0.65)
      call hplset('YWIN',0.55)
      call hplset('VSIZ',0.2)
      call hplset('XVAL',100.)
      call hplset('YTIC',0.0001)
      
      do i=1,nplt
        call hplsof(xtit(i),18.2,lab(i),0.4,0.,dum,0)
      enddo
      
      call hplzon(6,5,2,'S')	! W plots 
      call hplsof(5.5,16.5,'W',0.4,90.,dum,0)
      do i=1,nplt
      call hjanr_plot_fit(i,f(i),f(i),c(i),c(i),1,ndims(3,i),w(i))
      enddo
      
      call hplzon(6,5,8,'S')  	! Cos plots	
      call hplsof(5.5,13.2,'Cos[q]^*!',0.4,90.,dum,0)
      do i=1,nplt
      call hjanr_plot_fit(i,f(i),f(i),1,ndims(2,i),w(i),w(i),c(i))
      enddo
      
      call hplzon(6,5,14,'S')   ! Phi plots
      call hplsof(5.5,10.0,'[F]^*!',0.4,90.,dum,0)
      do i=1,nplt
      call hjanr_plot_fit(i,1,ndims(1,i),c(i),c(i),w(i),w(i),f(i))
      enddo
      
      call hplset('NDVX',0.)
      call hplset('XWIN',0.)
      call hplset('YWIN',0.)
      call hplset('VSIZ',0.)
      call hplset('XVAL',0.)
      call hplset('YTIC',0.)
            
      call iselnt(1)
      call txtout1
      call txtout2
      call igterm
      
      end
      
      subroutine hjanr_pick
      
      logical slide
#include "hjanr_val.inc"
#include "hjanr_hplt.inc"
#include "hjanr_hlim.inc"
#include "fit_par.inc"
      real ysca(30)
      data slide/.false./
      data ysca/0.5,0.5,0.5,20*10.,7*1./
           
      if (slide) go to 77
      call hploc(-1,ntloc,xloc,yloc,idh,icx,icy,istat)
      if (istat.eq.1.and.ntloc.ne.40) go to 80

c     Adjust fit parameters      
77    call iselnt(40)
79    call irqlc(1,-10,istat,ntloc,xloc,yloc)
      if (istat.eq.13.and.ntloc.eq.30) call hjanr_exit
      if (istat.eq.12) call hjanr_loadpar(4)
      if (istat.eq.13) call hjanr_loadpar(3)
      if (istat.eq.-1.and.ntloc.eq.40) then
        n=int(xloc)
        if (slide.and.n.ne.nsave) go to 79
        xnew(n)=yloc
        slide = .true.
        nsave = n
      endif
      if (istat.eq.11.and.ntloc.eq.40) then
        n=int(xloc)
        xnew(n)=yloc
        slide = .false.
      endif
      if (istat.eq.11.and.ntloc.eq.0) slide=.false.
      return
      
c     Pick new kinematic points              
80    nt=ntloc/10
      dif=0.5*(xpltt(2,nt)-xpltt(1,nt))
      do n=1,100
        if (abs(xloc-xpltt(n,nt)).lt.dif) go to 78
      enddo
            
      return
      
78    if (nt.eq.nt1+1)  w(1)=n
      if (nt.eq.nt1+5)  c(1)=n
      if (nt.eq.nt1+9)  f(1)=n
      if (nt.eq.nt1+2)  w(2)=n
      if (nt.eq.nt1+6)  c(2)=n
      if (nt.eq.nt1+10) f(2)=n
      if (nt.eq.nt1+3)  w(3)=n
      if (nt.eq.nt1+7)  c(3)=n
      if (nt.eq.nt1+11) f(3)=n
      if (nt.eq.nt1+4)  w(4)=n
      if (nt.eq.nt1+8)  c(4)=n
      if (nt.eq.nt1+12) f(4)=n
            
      end
      
      subroutine hjanr_plot_par 
      
#include "hjanr_hplt.inc"
#include "hjanr_val.inc"
#include "fit_par.inc"

      dimension  raty(35),ratx(35),dum(35)
     
      do i=1,31
        xplt(i)=i+0.5
        raty(i)=xnew(i) 
      enddo
      
      call hplabl(1,30,pname)
      call hplset('NDVX',30.15)
      call hplset('VSIZ',0.18)
      call hplfra(1.,31.,-2.,2.,' ')
      call hplerr(xplt,raty,dum,dum,31,' ',25,0.15) 
      call drawline(0.,32.,1.,1.)     
      call hplset('NDVX',0.)
      call hplset('VSIZ',0.)
      
      call igqwk(1,'NTNB',val)
      nt1=val/10
                
      call hjanr_storex(31)
      
      end
      
      subroutine hjanr_plot_fit(ifi,f1,f2,c1,c2,w1,w2,itg)
      
      integer ifi,f1,f2,c1,c2,w1,w2
      real dumd(100),dum(100),dume(100),obs(100),obs1(100)
      real xmin(3),xmax(3)
      
#include "hjanr_hcrs.inc"
#include "hjanr_hplt.inc"
#include "res_func.inc"
      
      data xmin/-5.,  -1.1,1.08/
      data xmax/365., 1.1,1.70/
      
      iplt = 1
      
      if (c1.ne.c2) iplt=2
      if (w1.ne.w2) iplt=3
      if (f1.ne.f2) iplt=1
      
      k=0
      ymin=0
      ymax=0
      xmx=-100.
      
      do iw=w1,w2
        iww=(iw-1)*ndims(2,ifi)*ndims(1,ifi)
        do ic=c1,c2
          icc=(ic-1)*ndims(1,ifi)
          do if=f1,f2
            indx=iww+icc+if
            
            if (xx(indx,iplt,ifi).lt.xmx) go to 99
            k=k+1
            xplt(k)=xx(indx,iplt,ifi)
            dumd(k)=crs(indx,ifi)
            dume(k)=crse(indx,ifi)
            ymin=min(ymin,dumd(k))
            ymax=max(ymax,dumd(k))
            i1=xx(indx,7,ifi)
            i2=xx(indx,6,ifi)
            call hjanr_loadpar(1)
            dthe=xx(indx,2,ifi)
            if (dthe.gt.1.0) dthe=cos(dthe*3.14159/180.)      
            call janr_run(xx(indx,3,ifi),xx(indx,4,ifi),
     1                    xx(indx,5,ifi),dthe,
     1                    xx(indx,1,ifi),.TRUE.)
            obs(k)=robs(i1,i2)
            call hjanr_loadpar(2)
            call janr_run(xx(indx,3,ifi),xx(indx,4,ifi),
     1                    xx(indx,5,ifi),dthe,
     1                    xx(indx,1,ifi),.TRUE.)
            obs1(k)=robs(i1,i2)
            xmx=xplt(k)
          enddo
        enddo
      enddo
      
99    if (xx(1,7,ifi).eq.1.or.xx(1,7,ifi).eq.5) then
        ymin = 0.0
        ymax = ymax*1.2
      else
        ymax=max(abs(ymin),abs(ymax))
        ymin=-ymax*1.5
        ymax= ymax*1.5
      endif
      
      if (iplt.eq.2.and.xx(1,2,1).gt.1.0) then
        xmin(iplt)=-5.
        xmax(iplt)=180.
      endif
      
      if (iplt.eq.3) xmax(iplt)=xplt(k)+0.02
      
      call hplfra(xmin(iplt),xmax(iplt),ymin,ymax,' ')
      call hplerr(xplt,dumd,dum,dume,k,' ',25,0.15)  
      call hplset('PMCI',1.)
      call hplerr(xplt(itg),dumd(itg),dum(itg),dume(itg),1,' ',21,0.15)    
      call hplset('PLCI',2.)
      if (k.gt.1) call hplfun(xplt,obs1,k,' ')	! new fit
      call hplset('PLCI',4.)
      if (k.gt.1) call hplfun(xplt,obs,k,' ')	! old fit
      call hplset('PLCI',1.)
      call hplset('LTYP',3.)
      if (k.gt.1) call drawline(xmin(iplt),xmax(iplt),0.,0.)
      call hplset('LTYP',1.)
      
      call hjanr_storex(k)
                  
      end
      
      subroutine hjanr_storex(k)
      
#include "hjanr_hplt.inc"
      
      call igqwk(1,'NTNB',val)
      ival=val/10
      do n=1,k
        xpltt(n,ival)=xplt(n)
      enddo
      
      end
      
      subroutine hjanr_loadpar(n)
      character*10 pnamei(35)
      real xload(35)
      
#include "fit_par.inc"
#include "hjanr_val.inc"

      if (n.eq.1) then
        do i=1,30
          xload(i)=start_value(i)
        enddo
      elseif (n.eq.2) then
        do i=1,30
          xload(i)=xnew(i)
        enddo
      elseif (n.eq.3) then
        do i=1,30
          xnew(i)=start_value(i)
        enddo
      elseif (n.eq.4) then
        do i=1,30
          start_value(i)=xnew(i)
        enddo  
      elseif (n.eq.5) then
        do i=1,30
          call mnpout(i,pnamei(i),parv,parerr,bn1,bn2,iv)
          xload(i)=parv+parerr
        enddo   
      elseif (n.eq.6) then
        do i=1,30
          call mnpout(i,pnamei(i),parv,parerr,bn1,bn2,iv)
          xload(i)=parv
        enddo   
      endif
       
        cM1=xload(1)
        cE1=xload(2)
        cS1=xload(3)
        cMP1=xload(4)
        cSP1=xload(5)
        cEP2=xload(6)
        cSP2=xload(7)
        cMP3=xload(8)
        cEP3=xload(9)
        cSP3=xload(10)
        cEP4=xload(11)
        cSP4=xload(12)
        cEP5=xload(13)
        cSP5=xload(14)
        cMP6=xload(15)
        cEP6=xload(16)
        cSP6=xload(17)
        cMP7=xload(18)
        cEP7=xload(19)
        cSP7=xload(20)
        cMP8=xload(21)
        cEP8=xload(22)
        cSP8=xload(23)
        cMP9=xload(24)
        cEP9=xload(25)
        cMP10=xload(26)
        cEP10=xload(27)
        cMP11=xload(28)
        cEP11=xload(29)
        
      end 
      
      subroutine txtout1
      character*6 c
      real cen(12)
#include "amplitudes.inc"
      data sca/0.16/
      data cen/3.1,4.8,6.2,8.0,9.7,11.2,12.9,15.0,17.0,18.8,20.2,21.6/

      call igtext(2.3,4.2,'AM=',0.2,0,'R')
      call igtext(2.3,3.9,'AE=',0.2,0,'R')
      call igtext(2.3,3.6,'AS=',0.2,0,'R')
      call igtext(2.3,3.2,'REM=',0.2,0,'R')
      call igtext(2.3,2.9,'RSM=',0.2,0,'R')

      call igtext(cen(1),7.5,'P33(1232)',sca,0,'C')

      write(c,'(F6.3)') am3(1)
      call igtext(cen(1)+0.3,4.2,c,0.2,0,'R')
      write(c,'(F6.3)') ae3(1)
      call igtext(cen(1)+0.3,3.9,c,0.2,0,'R')
      write(c,'(F6.3)') as3(1)
      call igtext(cen(1)+0.3,3.6,c,0.2,0,'R')
      rem = ae3(1)/am3(1)
      write(c,'(F6.4)') rem
      call igtext(cen(1)+0.3,3.2,c,0.2,0,'R')
      rsm = as3(1)/am3(1)
      write(c,'(F6.4)') rsm
      call igtext(cen(1)+0.3,2.9,c,0.2,0,'R')
            
      call igtext(cen(2),7.5,'P11(1440)',sca,0,'C')
      
      write(c,'(F6.1)') am1(1,1)
      call igtext(cen(2)+0.2,4.2,c,0.2,0,'R')
      write(c,'(F6.1)') as1(1,1)
      call igtext(cen(2)+0.2,3.6,c,0.2,0,'R')
      
      call igtext(cen(3),7.5,'S11(1535)',sca,0,'C')
      
      write(c,'(F6.1)') ae1(1,2)
      call igtext(cen(3)+0.2,3.9,c,0.2,0,'R')
      write(c,'(F6.1)') as1(1,2)
      call igtext(cen(3)+0.2,3.6,c,0.2,0,'R')
      
      call igtext(cen(4),7.5,'D13(1520)',sca,0,'C')
      
      write(c,'(F6.1)') am1(1,3)
      call igtext(cen(4)+0.3,4.2,c,0.2,0,'R')
      write(c,'(F6.1)') ae1(1,3)
      call igtext(cen(4)+0.3,3.9,c,0.2,0,'R')
      write(c,'(F6.1)') as1(1,3)
      call igtext(cen(4)+0.3,3.6,c,0.2,0,'R')
                  
      call igtext(cen(5),7.5,'S11(1650)',sca,0,'C')
      
      write(c,'(F6.1)') ae1(1,5)
      call igtext(cen(5)+0.3,3.9,c,0.2,0,'R')
      write(c,'(F6.1)') as1(1,5)
      call igtext(cen(5)+0.3,3.6,c,0.2,0,'R')
      
      call igtext(cen(6),7.5,'S31(1620)',sca,0,'C')
      
      write(c,'(F6.1)') ae3(5)
      call igtext(cen(6)+0.3,3.9,c,0.2,0,'R')
      write(c,'(F6.1)') as3(5)
      call igtext(cen(6)+0.3,3.6,c,0.2,0,'R')
      
      call igtext(cen(7),7.5,'F15(1680)',sca,0,'C')
      
      write(c,'(F6.1)') am1(1,6)
      call igtext(cen(7)+0.3,4.2,c,0.2,0,'R')
      write(c,'(F6.1)') ae1(1,6)
      call igtext(cen(7)+0.3,3.9,c,0.2,0,'R')
      write(c,'(F6.1)') as1(1,6)
      call igtext(cen(7)+0.3,3.6,c,0.2,0,'R')
      
      call igtext(cen(8),7.5,'D33(1700)',sca,0,'C')
      
      write(c,'(F6.1)') am3(2)
      call igtext(cen(8)+0.3,4.2,c,0.2,0,'R')
      write(c,'(F6.1)') ae3(2)
      call igtext(cen(8)+0.3,3.9,c,0.2,0,'R')
      write(c,'(F6.1)') as3(2)
      call igtext(cen(8)+0.3,3.6,c,0.2,0,'R')
      
      call igtext(cen(9),7.5,'P13(1720)',sca,0,'C')
      
      write(c,'(F6.1)') am1(1,7)
      call igtext(cen(9)+0.3,4.2,c,0.2,0,'R')
      write(c,'(F6.1)') ae1(1,7)
      call igtext(cen(9)+0.3,3.9,c,0.2,0,'R')
      write(c,'(F6.1)') as1(1,7)
      call igtext(cen(9)+0.3,3.6,c,0.2,0,'R')
      
      call igtext(cen(10),7.5,'D15(1675)',sca,0,'C')
      
      write(c,'(F6.1)') am1(1,9)
      call igtext(cen(10)+0.3,4.2,c,0.2,0,'R')
      write(c,'(F6.1)') ae1(1,9)
      call igtext(cen(10)+0.3,3.9,c,0.2,0,'R')
       
      call igtext(cen(11),7.5,'F35(1905)',sca,0,'C')
      
      write(c,'(F6.1)') am3(6)
      call igtext(cen(11)+0.3,4.2,c,0.2,0,'R')
      write(c,'(F6.1)') ae3(6)
      call igtext(cen(11)+0.3,3.9,c,0.2,0,'R')
       
      call igtext(cen(12),7.5,'F37(1950)',sca,0,'C')
      
      write(c,'(F6.1)') am3(3)
      call igtext(cen(12)+0.3,4.2,c,0.2,0,'R')
      write(c,'(F6.1)') ae3(3)
      call igtext(cen(12)+0.3,3.9,c,0.2,0,'R')
       
      end
      
      subroutine txtout2
      character*6 c
      real cen(12)
#include "amplitudes.inc"
#include "fit_par.inc"
      data sca/0.16/
      data cen/3.1,4.8,6.2,8.0,9.7,11.2,12.9,15.0,17.0,18.8,20.2,21.6/

      call igtext(2.3,2.5,'A32=',0.2,0,'R')
      call igtext(2.3,2.2,'A12=',0.2,0,'R')
      call igtext(2.3,1.9,'S12=',0.2,0,'R')

      
      write(c,'(F6.1)') aa1(21)*cmp1
      call igtext(cen(2)+0.2,2.2,c,0.2,0,'R')
      write(c,'(F6.1)') sa1(21)*csp1
      call igtext(cen(2)+0.2,1.9,c,0.2,0,'R')
      
      write(c,'(F6.1)') aa1(22)*cep2
      call igtext(cen(3)+0.2,2.2,c,0.2,0,'R')
      write(c,'(F6.1)') sa1(22)*csp2
      call igtext(cen(3)+0.2,1.9,c,0.2,0,'R')
      
      write(c,'(F6.1)') aa3(23)*cep3
      call igtext(cen(4)+0.3,2.5,c,0.2,0,'R')
      write(c,'(F6.1)') aa1(23)*cmp3
      call igtext(cen(4)+0.3,2.2,c,0.2,0,'R')
      write(c,'(F6.1)') sa1(23)*csp3
      call igtext(cen(4)+0.3,1.9,c,0.2,0,'R')
                  
      write(c,'(F6.1)') aa1(25)*cep4
      call igtext(cen(5)+0.3,2.2,c,0.2,0,'R')
      write(c,'(F6.1)') sa1(25)*csp4
      call igtext(cen(5)+0.3,1.9,c,0.2,0,'R')
      
      write(c,'(F6.1)') aa1(5)*cep5
      call igtext(cen(6)+0.3,2.2,c,0.2,0,'R')
      write(c,'(F6.1)') sa1(5)*csp5
      call igtext(cen(6)+0.3,1.9,c,0.2,0,'R')
      
      write(c,'(F6.1)') aa3(26)*cep6
      call igtext(cen(7)+0.3,2.5,c,0.2,0,'R')
      write(c,'(F6.1)') aa1(26)*cmp6
      call igtext(cen(7)+0.3,2.2,c,0.2,0,'R')
      write(c,'(F6.1)') sa1(26)*csp6
      call igtext(cen(7)+0.3,1.9,c,0.2,0,'R')
      
      write(c,'(F6.1)') aa3(2)*cep7
      call igtext(cen(8)+0.3,2.5,c,0.2,0,'R')
      write(c,'(F6.1)') aa1(2)*cmp7
      call igtext(cen(8)+0.3,2.2,c,0.2,0,'R')
      write(c,'(F6.1)') sa1(2)*csp7
      call igtext(cen(8)+0.3,1.9,c,0.2,0,'R')
      
      write(c,'(F6.1)') aa3(27)*cep8
      call igtext(cen(9)+0.3,2.5,c,0.2,0,'R')
      write(c,'(F6.1)') aa1(27)*cmp8
      call igtext(cen(9)+0.3,2.2,c,0.2,0,'R')
      write(c,'(F6.1)') sa1(27)*csp8
      call igtext(cen(9)+0.3,1.9,c,0.2,0,'R')
      
      write(c,'(F6.1)') aa3(29)*cep9
      call igtext(cen(10)+0.3,2.5,c,0.2,0,'R')
      write(c,'(F6.1)') aa1(29)*cmp9
      call igtext(cen(10)+0.3,2.2,c,0.2,0,'R')
       
      write(c,'(F6.1)') aa3(6)*cep10
      call igtext(cen(11)+0.3,2.5,c,0.2,0,'R')
      write(c,'(F6.1)') aa1(6)*cmp10
      call igtext(cen(11)+0.3,2.2,c,0.2,0,'R')
      
      write(c,'(F6.1)') aa3(3)*cep11
      call igtext(cen(12)+0.3,2.5,c,0.2,0,'R')
      write(c,'(F6.1)') aa1(3)*cmp11
      call igtext(cen(12)+0.3,2.2,c,0.2,0,'R')
       
      end
      
      subroutine drawline(x1,x2,y1,y2)
      
      real linx(2),liny(2)      
      linx(1)=x1
      linx(2)=x2
      liny(1)=y1
      liny(2)=y2
      
      call hpline(linx,liny,2,' ')
      
      end
      
      subroutine hjanr_exit
      real r(2)
      
      open(unit=10,file='junk.ps',form='formatted',status='unknown')
      call igzset('G')
      call hplopt('NZFL',1)
      call igmeta(10,-111)
      call hjanr_plot
      call igmeta(0,0)
      call iclwk(2)
      close(10)
      call exit
      
      end
      
      



