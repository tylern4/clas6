      subroutine bos_dc

c_begin_doc

c  Documentation for subroutine bos_dc

c  $Id: bos_dc.F,v 1.6 2009/03/14 20:27:52 fklein Exp $

c  Purpose:
c  --------
c	dc bos output routine

c  Author:
c  -------
c     Created  22 June 1995 E.Doroshkevich

c  Major revisions:
c  ----------------
c     Elliott Wolin, College of William and Mary, 30-jun-95
c     Kyungseon Joo, 14-apr-1998
c
c_end_doc


      implicit none
      character*(*) cfile,crevis,cstate,dcate,cautho,crname,crauth
      character*132 crcsid
      parameter (cfile=  '$RCSfile: bos_dc.F,v $')
      parameter (crevis= '$Revision: 1.6 $')
      parameter (cstate= '$State: Exp $')
      parameter (dcate=  '$Date: 2009/03/14 20:27:52 $')
      parameter (cautho= '$Author: fklein $')
      parameter (crname= 'BOS_DC')
      parameter (crauth= 'Elliott Wolin')
      data crcsid/'
     1$Id: bos_dc.F,v 1.6 2009/03/14 20:27:52 fklein Exp $
     1 '/
      save


c  include files:
c  --------------
#include "gcbank.inc"
#include "bcs.inc"
#include "lcgeom.inc"
#include "dcpar.inc"
#include "dcgeom.inc"
#include "dcsets.inc"      
#include "gsimpar.inc"
#include "gsimcontrol.inc"
c_end_inc

c  local variables:
c   ----------------
      integer ndmax
      parameter (ndmax=1000)

      integer ntrmax
      parameter (ntrmax=400)
      integer i,j,nw,idig,mbank
      integer idc,ndsec(6)
      integer numvs(nvdc), itrack(ntrmax,ndmax), ntra(ndmax)
      integer numsv(nvdc,ndmax), kdigi(nddc,ndmax), ndig

c     numsv(1,) - number of sector (1-6)
c     numsv(2,) - number of superlayer (1-6)
c     numsv(3,) - sublayer (1-6)
c     256*((numsv(2,i)-1)*6 + numsv(3,i)) - number of layer
c     kdigi(1,) - number of wire (1-192)
c     kdigi(2,) - adc (mev)
c     kdigi(3,) - tdc (channel) (nsec*20.)
c     kdigi(4,) - doca  (micron)

c  for DCH hits
      integer nhmax
      parameter   (nhmax=10000)

      integer nbos_dch
      parameter (nbos_dch=nhdc+1)
      integer numbvh(nvdc,nhmax),itrah(nhmax),numvsh(nvdc)
      real hitsh(nhdc,nhmax),wirelayer(nhmax)
      integer ihit,nhits,geant_to_pdg,idch,nhita,nhitb,sector,is
c_end_var


c  executable code:
c  ----------------

c  mc hits
      if(.not.nomcdata(dc))then

c  loop over all sectors
         do sector=1,nsect
            numvsh(1) = sector
            numvsh(2) = 0
            numvsh(3) = 0
            
c  get hits for each superlayer in sector
            nhits=0
            do is=1,ndc_sl

c  CTUB hits
               call gfhits('DC  ',cdet_dcs(is),nvdc,nhdc,nhmax-nhits,
     1              0,numvsh,itrah(nhits+1),numbvh(1,nhits+1),
     1              hitsh(1,nhits+1),nhita)
               if(nhita.gt.0)then
                  do ihit=nhits+1,nhits+nhita
                     wirelayer(ihit)=6*(is-1)+numbvh(nvdc,ihit)
                  enddo
                  nhits=nhits+nhita
               endif
               
c  straight section hits
               if(is.le.2)then
                  call gfhits('DC  ',cdet_sts(is),nvdc,nhdc,nhmax-nhits,
     1                 0,numvsh,itrah(nhits+1),numbvh(1,nhits+1),
     1                 hitsh(1,nhits+1),nhitb)
                  if(nhitb.gt.0)then
                     do ihit=nhits+1,nhits+nhitb
                        wirelayer(ihit)=6*(is-1)+numbvh(nvdc,ihit)
                     enddo
                     nhits=nhits+nhitb
                  endif
               endif

            enddo   !superlayer loop


            if (nhits.gt.0)then
               if (nhits.gt.nhmax) then
                  call recmes(crname,'I','?too many hits in sector')
                  nhits = nhmax
               endif
               
               idch=mbank(iw,'DCH ',sector,nbos_dch,nhits)
               do ihit=1,nhits
                  call ucopy(hitsh(1,ihit),
     1                 rw(idch+(ihit-1)*nbos_dch+1),10)
                  iw(idch+(ihit-1)*nbos_dch+11)=itrah(ihit)+
     1                 1000*int(hitsh(11,ihit))
                  iw(idch+(ihit-1)*nbos_dch+12)=
     1                 geant_to_pdg(int(hitsh(12,ihit)))
                  iw(idch+(ihit-1)*nbos_dch+13)=wirelayer(ihit)

               enddo
            endif
               
         enddo   !sector loop
      endif


c  dc data hits
      if(.not.nodata(dc))then
         idig=0
         do i=1,ndc_sl
            call gfdigi('DC  ',cdet_dcs(i),ntrmax,nvdc,nddc,(ndmax-idig),numvs,
     &           itrack(1,idig+1),ntra(idig+1),numsv(1,idig+1),
     &           kdigi(1,idig+1),ndig)
            idig=idig+ndig
         enddo
         
         call vzero(ndsec,6)

         if(idig.gt.ndmax)then
            write(6,*) 'AAAAAAAAAHHH!!! --- bos_dc too many hits:',idig
            idig=ndmax
         endif

         do i=1,idig
            ndsec(numsv(1,i)) = ndsec(numsv(1,i)) + 1
         enddo
      
c  fill bos banks
c  n.b. switch to DC0 (no adc) ejw, 30-nov-95
         do j=1,6               !sector loop
            if(ndsec(j).gt.0) then
c               go to 555
               nw = (ndsec(j)*2+1)/2 !the number of integer*2 words
               idc=mbank(iw,'DC0 ',j,2,ndsec(j))
               idc=idc*2        !because of integer*2 words
               
               do i=1,idig
                  if(numsv(1,i).eq.j) then
                     iw16(idc+1)=256*
     1                    ((numsv(2,i)-1)*6+numsv(3,i))+kdigi(1,i)
                     iw16(idc+2)=kdigi(3,i) !tdc
                     idc = idc + 2
                  endif
               enddo
c 555           continue
               nw = (ndsec(j)*2+1)/2 !the number of integer*2 words
               idc=mbank(iw,'DOCA',j,2,ndsec(j))
               idc=idc*2        !because of integer*2 words
               
               do i=1,idig
                  if(numsv(1,i).eq.j) then
                     iw16(idc+1)=256*
     1                    ((numsv(2,i)-1)*6+numsv(3,i))+kdigi(1,i)
                     iw16(idc+2)=kdigi(4,i) !doca
                     idc = idc + 2
                  endif
               enddo
               
            endif
         enddo
      endif
      
      return
      end

c------------------------------------------------------------------------

