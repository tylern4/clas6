      subroutine digi_ec1

c_begin_doc

c  Documentation for subroutine digi_ec1

c  $Id: digi_ec1.F,v 1.5 1999/07/06 12:52:09 vvsap Exp $

c  Purpose:
c  --------
c	performs ec1 digitizations

c  Author:
c  -------
c     Elliott Wolin, College of William and Mary, 13-apr-95


c  Major revisions:
C    Created:  23-Dec-1993   S.Boiarinov
C    Modified: 22-Mar-1995   H.Avakian
C    Purpose and methods : Digitization of the LAC
CC
C      HITS:     numsv(1) - number of sector (1-2)
C                numsv(2) - number of layer (1-33)
C                numsv(3) - number of strip (1-24/40)
C                hitsh(1) - X/Y-coordinate in strip system (cm)
C                hitsh(2) - Z-coordinate in strip system (cm)
C                hitsh(3) - lenght of light in strip (cm)
C                           (from negative end)
C                hitsh(4) - energy lost (MeV)
C                hitsh(5) - TOF (nsec)
C
C      DIGI:     numvs(1) - number of sector (1-2)
C                numvs(2) - number of block (1-4)
C                numvs(3) - number of strip (1-24 or 1-40)
C                ldigi(1) - ADC- (count) 
C                ldigi(2) - TDC- (count) 
C                ldigi(3) - ADC+ (count)  
C                ldigi(4) - TDC+ (count) 
c  ----------------

c_end_doc


      implicit none
      character*(*) cfile,crevis,cstate,cdate,cautho,crname,crauth
      character*132 crcsid
      parameter (cfile=  '$RCSfile: digi_ec1.F,v $')
      parameter (crevis= '$Revision: 1.5 $')
      parameter (cstate= '$State: Exp $')
      parameter (cdate=  '$Date: 1999/07/06 12:52:09 $')
      parameter (cautho= '$Author: vvsap $')
      parameter (crname= 'digi_ec1')
      parameter (crauth= 'Elliott Wolin')
      data crcsid/'
     1$Id: digi_ec1.F,v 1.5 1999/07/06 12:52:09 vvsap Exp $
     1 '/
      save



c  include files:
#include "gcunit.inc"
#include "gcbank.inc"
#include "ec1geom.inc"
#include "ffpar_ec1.inc"
c  --------------
c_end_inc

c  local variables:
c  ----------------
      integer i,j,k,l,nbeg,nend,kod,nsub,nabs
      integer nset,iset,js,ndet,idet,idig
      integer numvs(3),numsv(3,10000),itrack(10000),nhit
      integer ldigi(4),itemp,ndigi(2)
      real*4 hitsh(5,10000),hitshs(7,10000),lstrip,attleng,temp(2)
      real*4 Nel_to_charge,sigma_x5,sigma_x7,sigma5,sigma7
      real*4 ggauss, pe_mevl,pe_mevr,sigma_pel,sigma_per,lstack
      parameter (Nel_to_charge=0.605) ! Gain*el_charge (G=4.5E+6)[PC/electron]

C  External references:
      external ggauss

c_end_var

c  executable code:
c  ----------------
      if (jset.le.0) go to 999
      nset = iq(jset-1)
      if (nset.le.0) go to 999
      call glook('ECSD',iq(jset+1),nset,iset)
      if (iset.eq.0) go to 999
      js = lq(jset-iset)
      ndet = iq(js-1)
      if (ndet.le.0) go to 999
      call glook('ECL-',iq(js+1),ndet,idet)
      if (idet.eq.0) go to 999
C
C-- retrieve hits:
C
      numvs(1)=0
      numvs(2)=0
      numvs(3)=0

      call GFHITS('ECSD','ECL-',3,5,10000,0,numvs,
     &                              itrack,numsv,hitsh,nhit)
C        10000 maximum number of hits numsv(3,10000),hitsh(7,10000)
C        0 all tracks are taken
C       hitsh(7,10000) contain NHIT hits
C
      if(nhit.le.0) goto 999
C
C attenuation, Nlayer-->Nblock
C
      do i=1,nhit
C-- copy hitsh --> hitshs ( split energy between 2 ends ):
        hitshs(1,i)=hitsh(1,i)
        hitshs(2,i)=hitsh(2,i)
        hitshs(3,i)=hitsh(3,i)
        hitshs(4,i)=hitsh(4,i)/2.
        hitshs(5,i)=hitsh(5,i)
        hitshs(6,i)=hitsh(4,i)/2.
        hitshs(7,i)=hitsh(5,i)
C-- index for attenuation array:
        j=numsv(1,i)
        k=numsv(2,i)
        l=numsv(3,i)
C-- strip length:
        nsub=(k-1)-((k-1)/2)*2
        if(nsub.eq.0) then
          lstrip=(HS1+DS*(k-1))*2. ! short strip
         else
          lstrip=(HB1+DB*k)*2. ! long strips 
        endif
C-- layer number ---> block number:
        itemp=numsv(2,i)
        numsv(2,i)=numsv(2,i)-((numsv(2,i)-1)/2)*2
C
        if(itemp.gt.17) numsv(2,i)=numsv(2,i)+2
c       the block number 1-4
        k=numsv(2,i)
C-- atten. correction of amplitude (take 1/2 energy for each PM):
        if(iatten.ne.0) then
          if(iatten.lt.0) then
             nabs=128*(j-1)+8*mod(k+1,2)+32*(k-1)+l
           if((nabs.ge.1).and.(nabs.le.256)) then
C--          get the corresponding attenuation length
             attleng=float(lattlen(nabs))
           else
c            generate some warning message
             print *,'nnnnnnnnnnnnaaaaaaaaaaaaaaabbbbbbbbbbbssss',nabs
             attleng=-float(iatten)
           endif
          elseif(iatten.gt.0) then
             attleng=float(iatten)
          endif
           hitshs(4,i)=hitshs(4,i)*(exp(-hitshs(3,i)/attleng)
     1              +reflect*exp((-2.0*lstrip+hitshs(3,i))/attleng))
           hitshs(6,i)=hitshs(6,i)*(exp(-(lstrip-hitshs(3,i))/
     1       attleng)+reflect*exp((-lstrip-hitshs(3,i))/attleng))
       endif  ! non 0 attlengths
ccc       print *,'first: ijk,nabs, attlen, lstrip',i,j,k,nabs,attleng,lstrip
      enddo
C
C find the first 
C
      if(nhit.gt.0) then
        nbeg=1
        nend=nhit
        i=nbeg
        kod=numsv(1,1)*10000+numsv(2,1)*100+numsv(3,1)
        do while(nbeg.LT.nend)
          i=i+1
          k=numsv(1,i)*10000+numsv(2,i)*100+numsv(3,i)
          do while(k.eq.kod.and.i.le.nend)
            hitshs(4,nbeg)=hitshs(4,nbeg)+hitshs(4,i)
            hitshs(6,nbeg)=hitshs(6,nbeg)+hitshs(6,i)
            IF(hitshs(5,i).LT.hitshs(5,nbeg)) hitshs(5,nbeg)=hitshs(5,i)
            if(hitshs(7,i).lt.hitshs(7,nbeg)) hitshs(7,nbeg)=hitshs(7,i)
            numsv(1,i)=numsv(1,nend)
            numsv(2,i)=numsv(2,nend)
            numsv(3,i)=numsv(3,nend)
            hitshs(3,i)=hitshs(3,nend)
            hitshs(4,i)=hitshs(4,nend)
            hitshs(5,i)=hitshs(5,nend)
            hitshs(6,i)=hitshs(6,nend)
            hitshs(7,i)=hitshs(7,nend)
            k=numsv(1,i)*10000+numsv(2,i)*100+numsv(3,i)
            nend=nend-1
          enddo
          if(i.eq.nend.or.i.eq.nend+1) then
            nbeg=nbeg+1
            kod=numsv(1,nbeg)*10000+numsv(2,nbeg)*100+numsv(3,nbeg)
            i=nbeg
          endif
        enddo
      endif
      do i=1,nend

C-- tof:
C
C Compute sigma for tof distribution
C
C-- index for strip length:
        j=numsv(1,i)
        k=numsv(2,i)
        l=numsv(3,i)
C-- strip length:
c        nsub=(k-1)-((k-1)/2)*2
        if(k.eq.1) then
          lstack=(HS1+(HS2-HS1)/2.)*2. ! short stack inner
         else if(k.eq.2) then
          lstack=(HB1+(HB2-HB1)/2.)*2. ! long stack inner
         else if(k.eq.3) then
          lstack=(HS2)*2. ! short stack outer 
         else if(k.eq.4) then
          lstack=(HB2)*2. ! long stack outer
        endif
c---------------
           if(iatten.lt.0) then
             nabs=128*(j-1)+8*mod(k+1,2)+32*(k-1)+l
           if((nabs.ge.1).and.(nabs.le.256)) then
C--  get the corresponding N p.e.
             pe_mevl=laclumi(nabs,1)
             pe_mevr=laclumi(nabs,2)
           else
c            generate some warning message
             print *,'nnnnnnnnnnnnaaaaaaaaaaaaaaabbbbbbbbbbbssss',nabs
             pe_mevl=photoel
             pe_mevr=photoel
           endif
          elseif(iatten.gt.0) then
             pe_mevl=photoel
             pe_mevr=photoel
          endif
c
        sigma_pel      = min(1.,
     &          0.14+7./(4.*hitshs(4,i)*pe_mevl*Nel_to_charge))   ! nsec
        sigma_per      = min(1.,
     &          0.14+7./(4.*hitshs(6,i)*pe_mevr*Nel_to_charge))   ! nsec
        sigma_x5      = 0.53*exp(hitshs(3,i)/590.-1.)
        sigma_x7      = 0.53*exp((lstack-hitshs(3,i))/590.-1.)
        sigma5        = sqrt(sigma_pel*sigma_pel + sigma_x5*sigma_x5)
        sigma7        = sqrt(sigma_per*sigma_per + sigma_x7*sigma_x7)
        hitshs(5,i)=hitshs(5,i)+delay*hitshs(3,i)/100.
        hitshs(7,i)=hitshs(7,i)+delay*(lstack-hitshs(3,i))/100.
        hitshs(5,i)=ggauss(hitshs(5,i),sigma5)
        hitshs(7,i)=ggauss(hitshs(7,i),sigma7)
      
C
C Poisson, store DIGI
C
C-- energy : MeV --> N p.m.
        hitshs(4,i)=hitshs(4,i)*pe_mevl
        hitshs(6,i)=hitshs(6,i)*pe_mevr
C-- Poisson
        temp(1)=ggauss(0.,1.)
        temp(2)=ggauss(0.,1.)
        temp(1)=max(0.,hitshs(4,i)+temp(1)*sqrt(hitshs(4,i)))   
        temp(2)=max(0.,hitshs(6,i)+temp(2)*sqrt(hitshs(6,i))) 
C-- N p.e. --> ADC counts
        ndigi(1)=temp(1)*Nel_to_charge*charge_to_adc
        ndigi(2)=temp(2)*Nel_to_charge*charge_to_adc
C
C store digi
C
        if(ndigi(1).ge.ithreshold) then
            ldigi(1)=ndigi(1)
        else
            ldigi(1)=0.0
        endif
        if(ndigi(2).ge.ithreshold) then
            ldigi(3)=ndigi(2)
        else
            ldigi(3)=0.0
        endif
        if(ldigi(1).gt.0.0.or.ldigi(3).gt.0.0) then
c-- tof:
          ldigi(2)=hitshs(5,i)*20.    ! TDC counts
          ldigi(4)=hitshs(7,i)*20.
C-- sector number
          numvs(1)=numsv(1,i)
C-- block number
          numvs(2)=numsv(2,i)
C-- strip number
          numvs(3)=numsv(3,i)
          call GSDIGI(iset,idet,itrack,1,numvs,ldigi,idig)
                     if (idig.eq.0) then
                        write (chmail,1200) numvs(1),numvs(3)
                        call recmes(crname,'I',chmail)
                     endif
        endif
      enddo
C*********************************************************************
 1200 format ('?ec1digi error   : digitization for LAEC '
     1       ,'no. ',i3,' in sector ',i2,' could not be stored.')
C
  999   continue
       return
       end

c---------------------------------------------------------------------------------
