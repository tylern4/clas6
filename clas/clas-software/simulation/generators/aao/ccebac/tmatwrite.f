      subroutine tmatwrite(isw3)
      implicit real*8(a-h,o-y)
      implicit complex*16(z)

      parameter (maxpar=500,maxmb=5 ,maxres=5,maxlsj=20,maxl=10)
      parameter (maxwcm=40,maxq2=20,maxmom=50,maxtheta=24)


c  from ts 
      common / const / pi, fm, scale
      common / cmass / fnuc,fpio,fdel,fdelgm,fmrho,fmomg,feta,fsigm

c   from bruno

      common/pot1/nchh,Ldt(5,maxmb,maxlsj),isdt(5,maxmb,maxlsj)
     &                                    ,nLsdt(maxmb,maxlsj)
      common/chdat1/njLs,jpind(maxlsj) ,Lpind(maxlsj)
     &                  ,ispind(maxlsj),itpind(maxlsj)
      common/chdat2/nch,ich0(maxmb)

       COMMON/ELEM / zwf(maxwcm,maxlsj,maxmom,maxmb*5,2)
       common/ELEM2/zsigma(maxwcm,maxlsj,maxres,maxres)
     &            ,zf(maxwcm,maxlsj,5,maxmb,maxmom,maxres)

       common /ELEM3/ ztpin(maxwcm,maxlsj)
     1                ,ztmx(maxwcm,maxlsj)
     2               ,ztres(maxwcm,maxlsj)
     3            ,zsigsato(maxwcm,maxlsj,maxres,maxres)

       common/ELEM4/ zfmb(maxwcm,maxlsj,maxmom,maxmb,5,maxres)

      common/nstardt/nstar0(maxlsj),amstar(maxres,maxlsj)
     &    ,coup(5,maxmb,maxres,maxlsj),cut(5,maxmb,maxres,maxlsj)
       common/mesh00/nc,ne0,np1,ich(maxmb),
     1   ze0(maxwcm),am1(maxmb),am2(maxmb),istab(maxmb)
     1  ,zp(maxmom,maxmb,maxwcm),zwp(maxmom,maxmb,maxwcm)
     1  ,zp0(maxmb,maxwcm),zgreen(maxmom,maxmb,maxwcm)

c==========================================================
      if(isw3.eq.1) then


      write(51)zsigma,zsigsato,zf,ztpin,ztmx,ztres,zwf,zfmb

      return

      end if
c=================================================================
c
c  initial settings
c
      ion      = np1

      do 100 ie  = 1,ne0
      do 180 i = 1,njls

      kres   = nstar0(i)

      if(kres.ne.0) then

c  1/G(N*), zsigsato = m_0 + Sigma

      do irf = 1,kres
      do iri = 1,kres
      write(51,5800)irf,iri,zsigma(ie,i,irf,iri)
     &                     ,zsigsato(ie,i,irf,iri)
 5800 format(1h ,2i4,4d20.10)
      end do
      end do

c   <pi N|Gamma|N*>

      do ire1 = 1,kres
      write(51,5810)ire1,zf(ie,i,1,1,ion,ire1)
     &                  ,zf(ie,i,1,2,ion,ire1)
 5810 format(1h ,i4,4d20.10)
      end do

      end if
c
c   <pi N| T|pi N> amplitude
c
c     full  non-res  res
c
      write(51,5001)ztpin(ie,i),ztmx(ie,i),ztres(ie,i)
 5001 format(1h ,20d20.10)
c

c  <pi N|(1 + gV)^{^1}|MB>
c  <N*|Gamma G_0|MB>

      do imb = 1,nch
      do lsj = 1,nLsdt(imb,i)   !
      do ip  = 1,np1

      kkki   = (imb - 1)*5 + lsj
      write(51,5820)imb,lsj,ip,zwf(ie,i,ip,kkki,1),zwf(ie,i,ip,kkki,2)
 5820 format(1h ,3i4,4d20.10)

      if(kres.ne.0) then
      imb0 = ich0(imb)  !!!!!!!!!!!! correct?????
      do ires = 1,kres
      write(51,5810)ires,zfmb(ie,i,ip,imb,lsj,ires)
      end do
      end if

      end do
      end do
      end do

 180  continue  ! lsj    loop
 100  continue  ! energy loop

      return
      end

