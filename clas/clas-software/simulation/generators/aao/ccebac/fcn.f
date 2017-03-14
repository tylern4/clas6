      SUBROUTINE fcn(npar,grad,chi,par,iflag)

      IMPLICIT REAL*8 (A-H,O-Y),integer(i-n)
      implicit complex*16(z)

      parameter (maxpar=500,maxmb=5 ,maxres=5,maxlsj=20,maxl=10)
      parameter (maxwcm=40,maxq2=20,maxmom=50,maxtheta=24)

      DIMENSION PAR(maxpar)
      DIMENSION GRAD(*)
      character allwaves*3

      common/freepar/cpar(maxpar) 
      COMMON/FIT11/NPAR00,PARAIN0(maxpar),IVARY(40),EPP0(40)
      common/nstardt/nstar0(maxlsj),amstar(maxres,maxlsj)
     &    ,coup(5,maxmb,maxres,maxlsj),cut(5,maxmb,maxres,maxlsj)
      common/cutpar/cut0(maxres,maxlsj)
      common/waves/allwaves(maxlsj)

      common/chdat1/njLs,jpind(maxlsj) ,Lpind(maxlsj)
     &                  ,ispind(maxlsj),itpind(maxlsj)
      common/pot1/nchh,Ldt(5,maxmb,maxlsj),isdt(5,maxmb,maxlsj)
     &                                    ,nLsdt(maxmb,maxlsj)


      nc = nchh

       do  I       = 1,maxpar
        PARAIN0(I) = PAR(I)
        CPAR(I)    = PAR(I)
       enddo
       
        nparpot=29

c
c      redefine N^* parameters
c
c and writes them in a friendly way for Sato and Matsuyama

       nparres = nparpot
       do ijLs = 1,njLs
       nstar   = nstar0(ijLs)
       do 947 istar=1,nstar
       nparres=nparres+1
       amstar(istar,ijLs)=parain0(nparres)
       nparres=nparres+1
       cut0(istar,ijLS)=parain0(nparres)
       write(88,1199) allwaves(ijls),amstar(istar,ijls)
 1199  format(a3,2x,'MASS:',f8.3)
       write(88,1200)  allwaves(ijls),cut0(istar,ijls)
 1200  format(a3,2x,'cut0:',f8.3)

       do 947 ic0=1,nc

c explicit filter: no isospin 3/2 for etaN and sigmaN
c       if (itpind(ijls).gt.it1d(ic0)+it2d(ic0)) goto 947

       nLs=nLsdt(ic0,ijLs)
       do 948 iLs=1,nLs
       nparres=nparres+1
       coup(iLs,ic0,istar,ijLs)=parain0(nparres)
       nparres=nparres+1
       cut(iLs,ic0,istar,ijLS)=parain0(nparres)

  948  continue
  947  continue
       end do

c--------------------------------------------------------------
c          call ccfit(xmychi,iflag)
c-------------------------------------------------------------

       RETURN
       END

