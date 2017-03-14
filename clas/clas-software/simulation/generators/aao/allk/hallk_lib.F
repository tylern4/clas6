      subroutine hmpol_init
      integer nwpaw
      parameter (nwpaw=100000)
      common /pawc/ h(nwpaw)
      
#include "hmpol_hcrs.inc"
#include "allk_fit.inc"
      
      call hlimit(100000)
      call hplint(3)
      call hplsiz(25,20,' ')
      do i=1,4
        if (fname(i).ne.'JUNK') call hmpol_input(i,fname(i))
        if (fname(i).eq.'JUNK') call hmpol_input(i,fname(1))
      enddo
      call hmpol_allkqw
      call hmpol_loadmp(0)
      call hmpol_loadmp(1)
      
      end
      
      subroutine hmpol_allkqw
      implicit real*8(a-b,d-h,o-z)
      common / c2001 / wcmmev(1000),qsgev(1000),fact(1000),
     &ebeam,qmin,wmin,delq,delw,meshq,meshw,imaxdt
     
#include "hmpol_hcrs.inc"

      do iw=1,ndims(3,1)
        iww=(iw-1)*ndims(2,1)*ndims(1,1)+1
        win=dble(xx(iww,3,1)*1000.)
        qin=dble(xx(iww,4,1))
        call allkqw(win,qin,iw)
      enddo
      imaxdt=ndims(3,1)
      end
      
      subroutine hmpol_input(if,name)
      
      real xxk(3)
      character*60 file(4),name
      logical iflag(3)
      
#include "hmpol_hcrs.inc"
      
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
      
      print *, 'hmpol_INPUT: Reading file ',name
      
      open (unit=67,file=name,type='old')
      
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
      ndims(3,if)=min(30,ndims(3,if))
      print *, 'nphi,ncos,nw=',ndims(1,if),ndims(2,if),ndims(3,if)
      
      lab(if) = lab2(xx(1,7,if))//lab1(xx(1,6,if))
      nplt = if
      
      end
            
      subroutine hmpol_plot 
      
      character*3 opt
      real xtit(4)
      character*20 lab(4)
      common/hlab/lab
      
#include "hmpol_hlim.inc"
#include "hmpol_hplt.inc"
#include "hmpol_hcrs.inc"

c      data c/8,8,3,8/
c      data f/3,10,2,21/
c      data w/7,8,8,7/
      data c/1,1,1,1/
      data f/1,1,1,1/
      data w/1,1,1,1/
      data xtit/7.1,10.7,14.3,17.9/
      
      call hplzon(1,5,4,' ')      
      call hmpol_plot_par 
      
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
      call hmpol_plot_fit(i,f(i),f(i),c(i),c(i),1,ndims(3,i),w(i))
      enddo
      
      call hplzon(6,5,8,'S')  	! Cos plots	
      call hplsof(5.5,13.2,'Cos[q]^*!',0.4,90.,dum,0)
      do i=1,nplt
      call hmpol_plot_fit(i,f(i),f(i),1,ndims(2,i),w(i),w(i),c(i))
      enddo
      
      call hplzon(6,5,14,'S')   ! Phi plots
      call hplsof(5.5,10.0,'[F]^*!',0.4,90.,dum,0)
      do i=1,nplt
      call hmpol_plot_fit(i,1,ndims(1,i),c(i),c(i),w(i),w(i),f(i))
      enddo
      
      call hplset('NDVX',0.)
      call hplset('XWIN',0.)
      call hplset('YWIN',0.)
      call hplset('VSIZ',0.)
      call hplset('XVAL',0.)
      call hplset('YTIC',0.)
      
      call iselnt(1)
      call txtout1
      call igterm
      
      end
      
      subroutine hmpol_pick
      
      logical slide
#include "hmpol_val.inc"
#include "hmpol_hplt.inc"
#include "hmpol_hlim.inc"
      data slide/.false./
           
      if (slide) go to 77
      call hploc(-1,ntloc,xloc,yloc,idh,icx,icy,istat)
      if (istat.eq.1.and.ntloc.ne.40) go to 80

c     Adjust fit parameters      
77    call iselnt(40)
79    call irqlc(1,-10,istat,ntloc,xloc,yloc)
      if (istat.eq.13.and.ntloc.eq.30) call exit
      if (istat.eq.12) call hmpol_loadpar(4)
      if (istat.eq.13) call hmpol_loadpar(3)
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
      do n=1,30
        if (abs(xloc-xpltt(n,nt)).lt.dif) go to 78
      enddo      
      
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
      
      subroutine hmpol_plot_par 
      
#include "hmpol_hplt.inc"
#include "hmpol_val.inc"

      dimension  raty(35),ratx(35),dum(35)
      character*3 pname(30)
      data pname/'1','2','3','4','5','6','7','8','9','10','11',
     1'12','13','14','15','16','17','18','19','20','21','22',
     1'23','24','25','26','27','28','29','30'/
     
      do i=1,31
        xplt(i)=i+0.5
        raty(i)=xnew(i) 
      enddo
      
      call hplabl(1,30,pname)
      call hplset('NDVX',30.15)
      call hplset('VSIZ',0.18)
      call hplfra(1.,31.,0.5,1.5,' ')
      call hplerr(xplt,raty,dum,dum,31,' ',25,0.15) 
      call drawline(0.,32.,0.,0.)     
      call hplset('NDVX',0.)
      call hplset('VSIZ',0.)
      
      call igqwk(1,'NTNB',val)
      nt1=val/10
                
      call hmpol_storex(31)
      
      end
      
      subroutine hmpol_plot_fit(ifi,f1,f2,c1,c2,w1,w2,itg)
      
      integer ifi,f1,f2,c1,c2,w1,w2
      real dumd(100),dum(100),dume(100)
      real obs(100,2),ob1(100),ob2(100)
      real xmin(3),xmax(3)
      logical iupd
      
#include "hmpol_hcrs.inc"
#include "hmpol_hplt.inc"
#include "mp.inc"
      
      data xmin/-5.,  -1.1,1.08/
      data xmax/365., 1.1,1.70/
      
      iplt=1
      
      if (c1.ne.c2) iplt=2
      if (w1.ne.w2) iplt=3
      if (f1.ne.f2) iplt=1
            
      do ip=1,2
      k    = 0
      ymin = 0
      ymax = 0
      xmx  = -100.
      iupd = .true.
      call hmpol_loadpar(ip)
      
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
            dthe=xx(indx,2,ifi)
            ipid=ifix(xx(indx,6,ifi))
            if (dthe.gt.1.0) dthe=cos(dthe*3.14159/180.) 
            call allk_run(xx(indx,4,ifi),xx(indx,3,ifi),
     1                    xx(indx,5,ifi),dthe,
     1                    xx(indx,1,ifi),ipid,iupd,iw)
            obs(k,ip)=robs(xx(indx,7,ifi),ipid)
            xmx=xplt(k)
          enddo
        enddo
      enddo
      enddo
      
99    if (xx(1,7,ifi).eq.1) then
        ymin = 0.0
        ymax = ymax*1.2
      else 
        ymax=max(abs(ymin),abs(ymax))
        ymin=-ymax*1.5
        ymax= ymax*1.5
      endif
      
      do i=1,100
        ob1(i)=obs(i,1)
        ob2(i)=obs(i,2)
      enddo
      
      if (iplt.eq.2.and.xx(1,2,1).gt.1.0) then
        xmin(iplt)=-5.
        xmax(iplt)=180.
      endif
      
      if (iplt.eq.3) xmax(iplt)=xplt(k)+0.02
            
      call hplfra(xmin(iplt),xmax(iplt),ymin,ymax,' ')      
      call hplerr(xplt,dumd,dum,dume,k,' ',25,0.15)  
      call hplset('PMCI',2.)
      call hplerr(xplt(itg),dumd(itg),dum(itg),dume(itg),1,' ',25,0.15)    
      call hplset('PLCI',2.)
      if (k.gt.1) call hplfun(xplt,ob2,k,' ')
      call hplset('PMCI',1.)
      call hplset('PLCI',4.)
      if (k.gt.1) call hplfun(xplt,ob1,k,' ')
      call hplset('PLCI',1.)
      call hplset('LTYP',3.)
      if (k.gt.1) call drawline(xmin(iplt),xmax(iplt),0.,0.)
      call hplset('LTYP',1.)
      
      call hmpol_storex(k)
                  
      end
      
      subroutine hmpol_storex(k)
      
#include "hmpol_hplt.inc"
      
      call igqwk(1,'NTNB',val)
      ival=val/10
      do n=1,k
        xpltt(n,ival)=xplt(n)
      enddo
      
      end
      
      subroutine hmpol_loadpar(n)
      character*10 pnamei(35)
      
#include "hmpol_val.inc"

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
      elseif (n.eq.6) then
      endif
       
      call hmpol_loadmp(2)
        
      end 
      
      subroutine txtout1
      character*5 c
      real cen(10)
      data sca/0.16/
      data cen/3.1,4.8,6.2,8.0,9.7,11.2,12.9,15.0,17.0,19.1/
      data dum/1.0/
      
#include "hmpol_val.inc"

      call igtext(2.3,4.2,'AM=',0.2,0,'R')
      call igtext(2.3,3.9,'AE=',0.2,0,'R')
      call igtext(2.3,3.6,'AS=',0.2,0,'R')
      call igtext(2.3,3.2,'REM=',0.2,0,'R')
      call igtext(2.3,2.9,'RSM=',0.2,0,'R')

      call igtext(cen(1),7.5,'P33(1232)',sca,0,'C')

      write(c,'(F6.3)') xload(1)
      call igtext(cen(1)+0.3,4.2,c,0.2,0,'R')
      write(c,'(F6.3)') xload(2)
      call igtext(cen(1)+0.3,3.9,c,0.2,0,'R')
      write(c,'(F6.3)') xload(3)
      call igtext(cen(1)+0.3,3.6,c,0.2,0,'R')
      rem = -3.0
      write(c,'(F6.4)') rem
      call igtext(cen(1)+0.3,3.2,c,0.2,0,'R')
      rsm = -3.0
      write(c,'(F6.4)') rsm
      call igtext(cen(1)+0.3,2.9,c,0.2,0,'R')
            
      call igtext(cen(2),7.5,'P11(1440)',sca,0,'C')
      
      write(c,'(F6.3)') xload(4)
      call igtext(cen(2)+0.2,4.2,c,0.2,0,'R')
      write(c,'(F6.3)') xload(5)
      call igtext(cen(2)+0.2,3.6,c,0.2,0,'R')
      
      call igtext(cen(3),7.5,'S11(1535)',sca,0,'C')
      
      write(c,'(F6.3)') xload(6)
      call igtext(cen(3)+0.2,3.9,c,0.2,0,'R')
      write(c,'(F6.3)') xload(7)
      call igtext(cen(3)+0.2,3.6,c,0.2,0,'R')
      
      call igtext(cen(4),7.5,'D13(1520)',sca,0,'C')
      
      write(c,'(F6.3)') xload(8)
      call igtext(cen(4)+0.3,4.2,c,0.2,0,'R')
      write(c,'(F6.3)') xload(9)
      call igtext(cen(4)+0.3,3.9,c,0.2,0,'R')
      write(c,'(F6.3)') xload(10)
      call igtext(cen(4)+0.3,3.6,c,0.2,0,'R')
                  
      call igtext(cen(5),7.5,'S11(1650)',sca,0,'C')
      
      write(c,'(F6.3)') dum
      call igtext(cen(5)+0.3,3.9,c,0.2,0,'R')
      write(c,'(F6.3)') dum
      call igtext(cen(5)+0.3,3.6,c,0.2,0,'R')
      
      call igtext(cen(6),7.5,'S31(1620)',sca,0,'C')
      
      write(c,'(F6.3)') dum
      call igtext(cen(6)+0.3,3.9,c,0.2,0,'R')
      write(c,'(F6.3)') dum
      call igtext(cen(6)+0.3,3.6,c,0.2,0,'R')
      
      call igtext(cen(7),7.5,'F15(1680)',sca,0,'C')
      
      write(c,'(F6.3)') dum
      call igtext(cen(7)+0.3,4.2,c,0.2,0,'R')
      write(c,'(F6.3)') dum
      call igtext(cen(7)+0.3,3.9,c,0.2,0,'R')
      write(c,'(F6.3)') dum
      call igtext(cen(7)+0.3,3.6,c,0.2,0,'R')
      
      call igtext(cen(8),7.5,'D33(1700)',sca,0,'C')
      
      write(c,'(F6.3)') dum
      call igtext(cen(8)+0.3,4.2,c,0.2,0,'R')
      write(c,'(F6.3)') dum
      call igtext(cen(8)+0.3,3.9,c,0.2,0,'R')
      write(c,'(F6.3)') dum
      call igtext(cen(8)+0.3,3.6,c,0.2,0,'R')
      
      call igtext(cen(9),7.5,'P13(1720)',sca,0,'C')
      
      write(c,'(F6.3)') dum
      call igtext(cen(9)+0.3,4.2,c,0.2,0,'R')
      write(c,'(F6.3)') dum
      call igtext(cen(9)+0.3,3.9,c,0.2,0,'R')
      write(c,'(F6.3)') dum
      call igtext(cen(9)+0.3,3.6,c,0.2,0,'R')
      
c      call igtext(cen(10),7.5,'D13(1700)',sca,0,'C')
      
c      write(c,'(F6.3)') am1(1,4)
c      call igtext(cen(10)+0.3,4.2,c,0.2,0,'R')
c      write(c,'(F6.3)') ae1(1,4)
c      call igtext(cen(10)+0.3,3.9,c,0.2,0,'R')
c      write(c,'(F6.3)') as1(1,4)
c      call igtext(cen(10)+0.3,3.6,c,0.2,0,'R')

      end
      
      subroutine drawline(x1,x2,y1,y2)
      
      real linx(2),liny(2)      
      linx(1)=x1
      linx(2)=x2
      liny(1)=y1
      liny(2)=y2
      
      call hpline(linx,liny,2,' ')
      
      end
      
      subroutine hmpol_loadmp(iopt)
            
      implicit real*8(a-b,d-h,o-z)
      implicit complex*16(c)
      parameter(ndim=20,maxpot=8,nedim=50,npdim=20,nbmas=10,
     &          ladim=10)
      common /mult/ cdmul(nedim,4,0:ladim,8),cbmul(nedim,4,0:ladim,8)
     & ,cbrmul(nedim,4,0:ladim,8),cbdmul(nedim,4,0:ladim,8)
     & ,ctmul (nedim,4,0:ladim,8)
     & ,cxb(4,0:ladim,8),cxbr(4,0:ladim,8),cxbd(4,0:ladim,8)
     & ,cxd(4,0:ladim,8),cxt(4,0:ladim,8)

      common / c2001 / wcmmev(1000),qsgev(1000),fact(1000),
     &ebeam,qmin,wmin,delq,delw,meshq,meshw,imaxdt
     
#include "hmpol_val.inc"
      
      if (iopt.eq.1) then
      
      do n=1,10
        start_value(n) = 1.0
      enddo
      
      do ip=1,2  
      do n=1,imaxdt
        store_value(n,ip,6)=ctmul(n,ip,0,1)  ! E0+
        store_value(n,ip,7)=ctmul(n,ip,0,7)  ! S0+
        store_value(n,ip,1)=ctmul(n,ip,1,3)  ! M1+
        store_value(n,ip,2)=ctmul(n,ip,1,1)  ! E1+
        store_value(n,ip,3)=ctmul(n,ip,1,7)  ! S1+
        store_value(n,ip,4)=ctmul(n,ip,1,4)  ! M1-
        store_value(n,ip,5)=ctmul(n,ip,1,8)  ! S1-
        store_value(n,ip,8)=ctmul(n,ip,2,4)  ! M2-
        store_value(n,ip,9)=ctmul(n,ip,2,2)  ! E2-
        store_value(n,ip,10)=ctmul(n,ip,2,8) ! S2-
      enddo
      enddo
      
      elseif (iopt.eq.2) then
      
      do ip=1,2
      do n=1,imaxdt
        ctmul(n,ip,0,1)=store_value(n,ip,6)*dble(xload(6))   ! E0+
        ctmul(n,ip,0,7)=store_value(n,ip,7)*dble(xload(7))   ! S0+
        ctmul(n,ip,1,3)=store_value(n,ip,1)*dble(xload(1))   ! M1+
        ctmul(n,ip,1,1)=store_value(n,ip,2)*dble(xload(2))   ! E1+
        ctmul(n,ip,1,7)=store_value(n,ip,3)*dble(xload(3))   ! S1+
        ctmul(n,ip,1,4)=store_value(n,ip,4)*dble(xload(4))   ! M1-
        ctmul(n,ip,1,8)=store_value(n,ip,5)*dble(xload(5))   ! S1-
        ctmul(n,ip,2,4)=store_value(n,ip,8)*dble(xload(8))   ! M2-
        ctmul(n,ip,2,2)=store_value(n,ip,9)*dble(xload(9))   ! E2-
        ctmul(n,ip,2,8)=store_value(n,ip,10)*dble(xload(10)) ! S2-
      enddo
      enddo
      
      elseif (iopt.eq.0) then
      
      do n=1,10
        xnew(n) = 1.0
      enddo
       
      endif
      
      end
