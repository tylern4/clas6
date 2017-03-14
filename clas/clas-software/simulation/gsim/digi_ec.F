      subroutine digi_ec

c_begin_doc

c  Documentation for subroutine digi_ec

c  $Id: digi_ec.F,v 1.4 1998/03/24 13:07:49 burin Exp $

c  Purpose:
c  --------
c      ******************************************************************
c      *                                                                *
c      *      Digitization of the shower counter                        *
c      *                                                                *
c      *    ==>Called by : user_digi                                    *
c      *       Author    T.Henkes  ********                             *
c      *       Modified  September 18, 1986   Kersten                   *
c      *       Modified  October   15, 1987   M.Guckes                  *
c      *                                                                *
c      *                                                                *
c      *          MODIFIED by M. Guidal March 93                        *
c      *          ==============================                        *
c      *                                                                *
c      *       Modified  February  23, 1994   E.Burtin  		*
c      *                                                                *
c      ******************************************************************

c  Major revisions:
c  ----------------
c     Elliott Wolin, College of William and Mary, 22-nov-94
c_end_doc

      implicit none
      character*(*) cfile,crevis,cstate,cdate,cautho,crname,crauth
      character*132 crcsid
      parameter (cfile=  '$RCSfile: digi_ec.F,v $')
      parameter (crevis= '$Revision: 1.4 $')
      parameter (cstate= '$State: Exp $')
      parameter (cdate=  '$Date: 1998/03/24 13:07:49 $')
      parameter (cautho= '$Author: burin $')
      parameter (crname= 'DIGI_EC')
      parameter (crauth= 'Elliott Wolin')
      data crcsid/'
     1$Id: digi_ec.F,v 1.4 1998/03/24 13:07:49 burin Exp $
     1 '/
      save


c  include files:
c  --------------
#include "gcunit.inc"
#include "ffpar_ec.inc"
#include "ecgeom.inc"
#include "ecsets.inc"
#include "lcgeom.inc"
c_end_inc

c  local variables:
c  ----------------
      integer nhmax
      parameter (nhmax=1000)

      integer numbv(nvec,nhmax),itra(nhmax),numvs(nvec),kdigi(ndec)
      real hits(nhec,nhmax)

c  -  pedes : pedestal
c  -  sign  : sigma for the gaussian noise distribution [gev]
c  -  thre  : threshold [gev]
c  -  fact  : factor for digitisation
      real pedes,sign,thre,fact
      data  pedes,sign,thre,fact/0.,0.3e-3,1.0e-3,1.0e4/

c  the following were automatically generated...ejw
      real eff,em,ggauss,anode,xmean,sigma
      integer det,idig,isect,istrip,j,ngamma,nhits
      integer ngamma_vect(1)
      real tdc,valmoy
c_end_var


c  executable code:
c  ----------------

c  ignore ECAL, take any module
      numvs(2)=1
      numvs(3)=0

c  loop over sector,strip, then U,V,W inner and outer detectors
      do isect = 1,nsect 
         numvs(1)=isect

         do istrip=1,nstrip
            numvs(4)=istrip

            do det=1,nviews*2

               call gfhits('EC  ',cdet_ec(det),nvec,nhec,nhmax,0,
     1              numvs,itra,numbv,hits,nhits)
c  check for too many hits
               if (nhits.gt.nhmax) then
                  write (chmail,1100) isect, nhmax, nhits
                  call recmes(crname,'I',chmail)
                  nhits=nhmax
               endif
                  
c  sum all adc hits for this strip, get earliest tdc hit
               if(nhits.gt.0)then
                  em=0.
                  ngamma = 0
                  tdc = 999999.
                  do j=1,nhits
                     em=em+hits(7,j)
                     tdc=min(tdc,hits(5,j))
                  enddo
                  
c  recalculate adc if poisson statistics requested
                  if(poiss.ne.0.)then
                     valmoy = em * poiss * 1000.
                     call gpoiss(valmoy,ngamma_vect,1)
                     ngamma = ngamma_vect(1)
                     
c Fluctuations in PMT gain distributed using Gaussian with sigma SNR 
c SNR = sqrt(ngamma)/sqrt(del/del-1) del = dynode gain = 3 (From RCA PMT Handbook) p. 169)
                     xmean = float(ngamma)
                     sigma = sqrt(xmean)*1.15
                     anode = ggauss(xmean,sigma)
                     em = anode / (poiss * 1000.)
                  endif  
                     
c  make hit only if there is energy deposit 
                  if(em.gt.0.)then

c  tdc converted to number of channels (20 channels/ns)
                     eff = 1.
                     kdigi(1) = ifix( fact * em * eff )
                     kdigi(2) = tdc * ec_tdc_to_channel
                     if(kdigi(2).gt.4095) kdigi(2) = 4095 
                     
                     call gsdigi(set_ec,det_ec(det),itra,nhits,
     1                    numvs,kdigi,idig)
     
                     if (idig.eq.0) then
                        write (chmail,1200) isect
                        call recmes(crname,'I',chmail)
                     endif

                  endif         !ngamma > 0

               endif
            enddo               !U,V,W inner or outer

         enddo                  !strip
      enddo                     !sector
      
      return


 1100 format ('?ecdigi error   : hits overflow for calorimeter '
     1       ,' in sector ',i2,'. only ',i4,' of ',i4,' hits '
     2       ,'could not be processed.')
 1200 format ('?ecdigi error   : digitization for calorimeter '
     1       ,' in sector ',i2,' could not be stored.')

      end

c---------------------------------------------------------------------------
