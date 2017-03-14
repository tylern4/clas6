      subroutine bos_ec1_t

c_begin_doc

c  Documentation for subroutine bos_ec1

c  $Id: bos_ec1_t.F,v 1.3 2009/03/14 20:27:52 fklein Exp $

c  Purpose:
c  --------
c	ec1 bos output routine

c  Author:
c  -------
c     Elliott Wolin, College of William and Mary, 30-jun-95

c  Major revisions:
c  Harout Avakian 5 Jan 1996
c ----------------

c_end_doc


      implicit none
      character*(*) cfile,crevis,cstate,cdate,cautho,crname,crauth
      character*132 crcsid
      parameter (cfile=  '$RCSfile: bos_ec1_t.F,v $')
      parameter (crevis= '$Revision: 1.3 $')
      parameter (cstate= '$State: Exp $')
      parameter (cdate=  '$Date: 2009/03/14 20:27:52 $')
      parameter (cautho= '$Author: fklein $')
      parameter (crname= 'bos_EC1')
      parameter (crauth= 'Elliott Wolin')
      data crcsid/'
     1$Id: bos_ec1_t.F,v 1.3 2009/03/14 20:27:52 fklein Exp $
     1 '/
      save


c  include files:
c  --------------
#include "gcbank.inc"
#include "bcs.inc"
#include "wcs.inc"
#include "ec1sets.inc"
#include "gsimpar.inc"
#include "gsimcontrol.inc"
c_end_inc

c  local variables:
c   ----------------
      integer ndmax
      parameter (ndmax=400)
      integer ntrkmax
      parameter (ntrkmax=1000)

      integer i,j,idig,mbank,mlink,indx,ind,ind2,ic0,used(ndmax)
      integer iec1,ndsec(2)
      integer numvs(3), itrack(ntrkmax,ndmax), ntra(ndmax)
      integer numsv(3,ndmax), ldigi(4,ndmax), ndig
      integer*2 idn16


c     numsv(1,) - number of sector (1-2)
c     numsv(2,) = module (1 - 4)
c     numsv(3,) - strip (1 - 24 or 40)
c     ldigi(1,) - i_adc- 
c     ldigi(2,) - i_tdc-
c     ldigi(3,) - i_adc+
c     ldigi(4,) - i_tdc+ 

c_end_var


c  executable code:
c  ----------------
c  fill EC1 bos banks from digis
      if(.not.nodata(ec1))then
         call vzero(numvs,3)
         
         idig=0
         call gfdigi('ECSD','ECL-',ntrkmax,3,4,(ndmax-idig),numvs,
     &           itrack(1,idig+1),ntra(idig+1),numsv(1,idig+1),
     &           ldigi(1,idig+1),ndig)

         idig=idig+ndig
         
         call vzero(ndsec,2)
         do i=1,idig
            ndsec(numsv(1,i)) = ndsec(numsv(1,i)) + 1
         enddo
         
         do j=1,2               !sector loop
           ind=mlink(jw,'EC1 ',j)
           ind2=ind*2
            if(ndsec(j).gt.0) then
              ic0=0
              if(ind.gt.0)then
                do indx=1,jw(ind-4)
                  ic0=ic0+1
                  used(indx)=1
                  do i=1,idig 
                    if (numsv(2,i).eq.1) then 
                         idn16=256*2+numsv(3,i)
                    else if(numsv(2,i).eq.2) then
                         idn16=256+numsv(3,i)
                    else if(numsv(2,i).eq.3) then
                         idn16=256*4+numsv(3,i)
                    else if(numsv(2,i).eq.4) then
                         idn16=256*3+numsv(3,i)
                    endif
                    if(numsv(1,i).eq.j.and.idn16.eq.jw16(ind2+1+2*(indx-1))) then
                      used(indx)=-1
                      ic0=ic0-1
                      ldigi(1,i)=ldigi(1,i)+jw16(ind2+3+2*(indx-1)) !adc-
                      ldigi(3,i)=ldigi(3,i)+jw16(ind2+5+2*(indx-1)) !adc+
                      if(ldigi(2,i).gt.jw16(indx+1))ldigi(2,i)=jw16(ind2+2+2*(indx-1)) !tdc-
                      if(ldigi(4,i).gt.jw16(indx+3))ldigi(4,i)=jw16(ind2+4+2*(indx-1)) !tdc+
                    endif
                  enddo  
                enddo        
              endif
              iec1=mbank(iw,'EC1 ',j,5,ndsec(j)+ic0)
              iec1=iec1*2        !because of integer*2 words
              do i=1,idig 
                if(numsv(1,i).eq.j) then
                  if (numsv(2,i).eq.1) then 
                    iw16(iec1+1)=256*2+numsv(3,i)
                  else if(numsv(2,i).eq.2) then
                    iw16(iec1+1)=256+numsv(3,i)
                  else if(numsv(2,i).eq.3) then
                    iw16(iec1+1)=256*4+numsv(3,i)
                  else if(numsv(2,i).eq.4) then
                    iw16(iec1+1)=256*3+numsv(3,i)
                  endif
                  iw16(iec1+2)=ldigi(2,i) !tdc-
                  iw16(iec1+3)=ldigi(1,i) !adc-
                  iw16(iec1+4)=ldigi(4,i) !tdc+
                  iw16(iec1+5)=ldigi(3,i) !adc+
                  iec1=iec1+5
                endif
              enddo        
              if(ind.gt.0)then
                do indx=1,jw(ind-1)
                  if(used(indx).gt.0)then
                    iw16(iec1+1)=jw16(ind2+1+2*(indx-1))
                    iw16(iec1+2)=jw16(ind2+2+2*(indx-1)) !tdc-
                    iw16(iec1+3)=jw16(ind2+3+2*(indx-1)) !adc-
                    iw16(iec1+4)=jw16(ind2+4+2*(indx-1)) !tdc+
                    iw16(iec1+5)=jw16(ind2+5+2*(indx-1)) !adc+
                    iec1=iec1+5
                  endif
                enddo
              endif    
            else
c- No new hits in LAC, just transfer EC1 from wcs to bcs            
              if(ind.gt.0)then
                iec1=mbank(iw,'EC1 ',j,5,jw(ind-4))
                call ucopy(jw(ind+1),iw(iec1+1),jw(ind))
              endif
            endif
         enddo
      endif
      return
      end

c------------------------------------------------------------------------

