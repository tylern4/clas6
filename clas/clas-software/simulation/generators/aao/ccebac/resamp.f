       subroutine resamp(ie,ichnl,zramp,iflag)

      parameter (maxpar=500,maxmb=5 ,maxres=5,maxlsj=20,maxl=10)
      parameter (maxwcm=40,maxq2=20,maxmom=50,maxtheta=24)

       implicit real*8(a-h,o-y)
       implicit complex*16(z)
       complex*16 p2,p1

       dimension zramp(5,maxmb,5,maxmb)
       dimension zgstar(maxres,maxres),zsigma(maxres,maxres)
       dimension zf(5,maxmb,maxmom,maxres)

       common/ELEM2/zgstar_sato(maxwcm,maxlsj,maxres,maxres)
     &            ,zf_sato(maxwcm,maxlsj,5,maxmb,maxmom,maxres)


      common/chdat1/njLs,jpind(maxlsj) ,Lpind(maxlsj)
     &                  ,ispind(maxlsj),itpind(maxlsj)
      common/chdat2/nch,ich0(maxmb)

       COMPLEX*16 VTT(maxres,maxres),DETERM
       complex*16 e0,wp,green,p0,p

       common/mesh00/nc,ne0,np1,ich(maxmb),
     1   e0(maxwcm),am1(maxmb),am2(maxmb),istab(maxmb)
     1  ,p(maxmom,maxmb,maxwcm),wp(maxmom,maxmb,maxwcm)
     1  ,p0(maxmb,maxwcm),green(maxmom,maxmb,maxwcm)


      common/wfdt/zwf(5,maxmb,maxmom,5,maxmb,maxmom)
      common/pot1/nchh,Ldt(5,maxmb,maxlsj),isdt(5,maxmb,maxlsj)
     &                                    ,nLsdt(maxmb,maxlsj)

c
      common/nstardt/nstar0(maxlsj),amstar(maxres,maxlsj)
     &    ,coup(5,maxmb,maxres,maxlsj),cut(5,maxmb,maxres,maxlsj)
c ts 05-19-2007 ztpin, ztpinen mv common
       common /ELEM3/ ztpin(maxwcm,maxlsj)
     1                ,ztmx(maxwcm,maxlsj)
     2               ,ztres(maxwcm,maxlsj)
     3            ,zsigsato(maxwcm,maxlsj,maxres,maxres)



c       write(*,*)'*******',nchh,ichnl,np1
      
      

       zramp=0

c
c      calculate dressed vertex function
c
       nstar=nstar0(ichnl)

       do 1 i=1,nstar

       do 11 ic1=1,nchh
       nLs1=nLsdt(ic1,ichnl)

       do 11 iLs1=1,nLs1
       L1=Ldt(iLs1,ic1,ichnl)
       g1=coup(iLs1,ic1,i,ichnl)
       cc1=cut(iLs1,ic1,i,ichnl)

       do 11 ip1=1,np1

       zf(iLs1,ic1,ip1,i)=0.

       do 2 ic2=1,nchh
       nLs2=nLsdt(ic2,ichnl)

       do 2 iLs2=1,nLs2
       L2=Ldt(iLs2,ic2,ichnl)
       g2=coup(iLs2,ic2,i,ichnl)
       cc2=cut(iLs2,ic2,i,ichnl)


       do 2 ip2=1,np1
       p2=p(ip2,ic2,ie)

       zf(iLs1,ic1,ip1,i)=zf(iLs1,ic1,ip1,i)
     1 + zwf(iLs1,ic1,ip1,iLs2,ic2,ip2)
     1 * zformf(g2,cc2,L2,p2,i,ichnl,ic2)

c       if(ic2.eq.1.and.ic1.eq.1.and.ip1.eq.np1) then
c          write(*,9998)ip2,zwf(iLs1,ic1,ip1,iLs2,ic2,ip2)
c     &                    ,zformf(g2,cc2,L2,p2,i,ichnl,ic2),g2
c 9998     format(1h ,i2,5e15.5)
c       end if

    2  continue
   11  continue
    1  continue

c ELAM
       IF (IFLAG.EQ.3) THEN
       do 141 i=1,nstar
       do 142 ic1=1,nchh
       nLs1=nLsdt(ic1,ichnl)
       do 142 iLs1=1,nLs1
       do 142 ip1=1,np1
       zf_sato(ie,ichnl,ils1,ic1,ip1,i)=zf(ils1,ic1,ip1,i)
 142    continue
 141    continue
       ENDIF



c
c      calculate N* propagator matrix
c
       do 3 i1=1,nstar
       do 3 i2=1,nstar
       zsigma(i1,i2)=0.
       zgstar(i1,i2)=0.

       do 31 ic1=1,nchh
       nLs1=nLsdt(ic1,ichnl)
       do 31 iLs1=1,nLs1
       L1=Ldt(iLs1,ic1,ichnl)
       g1=coup(iLs1,ic1,i1,ichnl)
       cc1=cut(iLs1,ic1,i1,ichnl)
       do 31 ip1=1,np1
       p1=p(ip1,ic1,ie)
c sato phases 06
       zsigma(i1,i2)=zsigma(i1,i2)+
     1 conjg(zformf(g1,cc1,L1,p1,i1,ichnl,ic1))*
     1 green(ip1,ic1,ie)*zf(iLs1,ic1,ip1,i2)

   31  continue
       zgstar(i1,i2)=-zsigma(i1,i2)
       if(i1.eq.i2)zgstar(i1,i2)=e0(ie)-amstar(i1,ichnl)
     1                                 + zgstar(i1,i2)

    3  continue

       ncm=nstar

       call matinc(zgstar,ncm,vtt,0,determ,maxres)

c ---- inversion check ----
        do 103 i1=1,nstar
        do 103 i2=1,nstar
        zmx=0.
        do 104 i3=1,nstar
        zd=-zsigma(i1,i3)
        if(i2.eq.i3)zd=zd+e0(ie)-amstar(i2,ichnl)
        zmx=zmx+zgstar(i1,i3)*zd
  104  continue
c        write(6,105)i1,i2,zmx
  105  format(' i1,i2',2i3,' zg*(zg)^(-1):',2e12.4)
  103  continue

c ------

c------------------------ N* propagator ------------------------------

       do i1=1,5
       do i2=1,5
c > ELEM-sato-12-16
       zgstar_sato(ie,ichnl,i1,i2)=zgstar(i1,i2)
       if(i1.eq.i2) then
       zsigsato(ie,ichnl,i1,i2) = amstar(i1,ichnl) + zsigma(i1,i2)
       else
       zsigsato(ie,ichnl,i1,i2) = zsigma(i1,i2)
       end if
c < ELEM-sato-12-16
       enddo
       enddo

c
c      calculate on-shell resonant amplitudes
c
       do 41 ic1=1,nchh
       nLs1=nLsdt(ic1,ichnl)
       do 41 iLs1=1,nLs1

       do 42 ic2=1,nchh
       nLs2=nLsdt(ic2,ichnl)
       do 42 iLs2=1,nLs2

       zramp(iLs1,ic1,iLs2,ic2)=0.

       ipf = np1

       do 43 i1=1,nstar
       do 43 i2=1,nstar
c sato phases 06
       xisign=1.
       if (ich0(ic2).eq.1) xisign=-1.      ! pi N
       if (ich0(ic2).eq.2) xisign=-1.      ! eta N
       if (ich0(ic2).eq.3) xisign=-1.      ! pi Delta

       ztmpx =xisign * zf(iLs1,ic1,ipf,i1)*zgstar(i1,i2)
     &               * zf(iLs2,ic2,np1,i2)
       zramp(iLs1,ic1,iLs2,ic2)=zramp(iLs1,ic1,iLs2,ic2) + ztmpx
   43  continue

   42  continue
   41  continue

       return
       end

