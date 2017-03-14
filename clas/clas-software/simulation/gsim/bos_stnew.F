      subroutine bos_stnew

c_begin_doc

c  Documentation for subroutine bos_st

c  $Id: bos_stnew.F,v 1.2 2009/03/14 20:27:52 fklein Exp $

c  Purpose:
c  --------
c	st bos output routine

c  Author:
c  -------
c     Elliott Wolin, College of William and Mary, 17-oct-95
c_end_doc


      implicit none
      character*(*) cfile,crevis,cstate,cdate,cautho,crname,crauth
      character*132 crcsid
      parameter (cfile=  '$RCSfile: bos_stnew.F,v $')
      parameter (crevis= '$Revision: 1.2 $')
      parameter (cstate= '$State: Exp $')
      parameter (cdate=  '$Date: 2009/03/14 20:27:52 $')
      parameter (cautho= '$Author: fklein $')
      parameter (crname= 'BOS_ST')
      parameter (crauth= 'Elliott Wolin')
      data crcsid/'
     1$Id: bos_stnew.F,v 1.2 2009/03/14 20:27:52 fklein Exp $
     1 '/
      save


c  include files:
c  --------------
#include "gcbank.inc"
#include "bcs.inc"
#include "gsimpar.inc"
#include "gsimcontrol.inc"
#include "stsets.inc"
#include "gcsets.inc"
c_end_inc

c  local variables:
c   ----------------

      integer i,j,k,nw,mbank
      integer ist,ndsec(6,4),isect,ipaddle,ip
      integer TDC,ADC,TDC_min(6,4),ADC_max(6,4)

c  for ST data
      integer nbos_st
      parameter (nbos_st=3)
c      parameter (nbos_st=24)

      integer max_digi,max_tkdigi
      parameter (max_digi=100)
      parameter (max_tkdigi=1000)

      integer numvs(nvol_st),itrak(max_tkdigi,max_digi),ntra(max_digi)
      integer numsv(nvol_st,max_digi),kdigi(ndigi_st,max_digi),ndig,totdig


c  for STH hits
      integer nvdimh,nhdimh,nhmaxh,nbos_sth
      parameter (nvdimh=2)
      parameter (nhdimh=nhits_sth)
      parameter (nhmaxh=500)
      parameter (nbos_sth=nhits_sth)
      integer numvsh(nvdimh)
      integer numbvh(nvdimh,nhmaxh)
      integer itrah(nhmaxh)
      real hitsh(nhdimh,nhmaxh)
      integer nhits,ihit,isth,geant_to_pdg
c_end_var

      integer paddle_nhits,jj,kk
      real    paddle_hitsh(nhdimh,nhmaxh)
      integer paddle_ndig
      real    paddle_kdigi(ndigi_st,max_digi)


c  executable code:
c  ----------------

c  fill STH MC hits bank
      if(.not.nomcdata(st))then
c loop over sector
	 do isect = 1, 6
            call vzero(numvsh,nvdimh)
	    numvsh(1) = isect
c loop over detector elements
	    do ip = 1, 10
	       numvsh(2)=idet
	       
               paddle_nhits=0
	       if (ip.eq.1) then
                  call gfhits('ST  ','LEH1',nvdimh,nhdimh,nhmaxh,0,numvsh,
     1                 itrah,numbvh,hitsh(1,1),nhits)
                   do jj=1,nhits
                     do kk=1,10
                        paddle_hitsh(kk,jj+paddle_nhits)=hitsh(kk,jj)
                      enddo
                   enddo
                   paddle_nhits=paddle_nhits+nhits
               elseif (ip.eq.2) then
                  call gfhits('ST  ','NOH1',nvdimh,nhdimh,nhmaxh,0,numvsh,
     1                 itrah,numbvh,hitsh(1,1),nhits)
                   do jj=1,nhits
                     do kk=1,10
                        paddle_hitsh(kk,jj+paddle_nhits)=hitsh(kk,jj)
                      enddo
                   enddo
                   paddle_nhits=paddle_nhits+nhits



               elseif (ip.eq.3) then
                  call gfhits('ST  ','LEH2',nvdimh,nhdimh,nhmaxh,0,numvsh,
     1                 itrah,numbvh,hitsh(1,1),nhits)
                   do jj=1,nhits
                     do kk=1,10
                        paddle_hitsh(kk,jj+paddle_nhits)=hitsh(kk,jj)
                      enddo
                   enddo
                   paddle_nhits=paddle_nhits+nhits
               elseif (ip.eq.4) then
                  call gfhits('ST  ','NOH2',nvdimh,nhdimh,nhmaxh,0,numvsh,
     1                 itrah,numbvh,hitsh(1,1),nhits)
                   do jj=1,nhits
                     do kk=1,10
                        paddle_hitsh(kk,jj+paddle_nhits)=hitsh(kk,jj)
                      enddo
                   enddo
                   paddle_nhits=paddle_nhits+nhits
               elseif (ip.eq.5) then
                  call gfhits('ST  ','NNH2',nvdimh,nhdimh,nhmaxh,0,numvsh,
     1                 itrah,numbvh,hitsh(1,1),nhits)
                   do jj=1,nhits
                     do kk=1,10
                        paddle_hitsh(kk,jj+paddle_nhits)=hitsh(kk,jj)
                      enddo
                   enddo
                   paddle_nhits=paddle_nhits+nhits

               elseif (ip.eq.6) then
                  call gfhits('ST  ','LEH3',nvdimh,nhdimh,nhmaxh,0,numvsh,
     1                 itrah,numbvh,hitsh(1,1),nhits)
                   do jj=1,nhits
                     do kk=1,10
                        paddle_hitsh(kk,jj+paddle_nhits)=hitsh(kk,jj)
                      enddo
                   enddo
                   paddle_nhits=paddle_nhits+nhits
               elseif (ip.eq.7) then
                  call gfhits('ST  ','NOH3',nvdimh,nhdimh,nhmaxh,0,numvsh,
     1                 itrah,numbvh,hitsh(1,1),nhits)
                   do jj=1,nhits
                     do kk=1,10
                        paddle_hitsh(kk,jj+paddle_nhits)=hitsh(kk,jj)
                      enddo
                   enddo
                   paddle_nhits=paddle_nhits+nhits
               elseif (ip.eq.8) then
                  call gfhits('ST  ','NNH3',nvdimh,nhdimh,nhmaxh,0,numvsh,
     1                 itrah,numbvh,hitsh(1,1),nhits)
                   do jj=1,nhits
                     do kk=1,10
                        paddle_hitsh(kk,jj+paddle_nhits)=hitsh(kk,jj)
                      enddo
                   enddo
                   paddle_nhits=paddle_nhits+nhits
               elseif (ip.eq.9) then
                  call gfhits('ST  ','LEH4',nvdimh,nhdimh,nhmaxh,0,numvsh,
     1                 itrah,numbvh,hitsh(1,1),nhits)
                   do jj=1,nhits
                     do kk=1,10
                        paddle_hitsh(kk,jj+paddle_nhits)=hitsh(kk,jj)
                      enddo
                   enddo
                   paddle_nhits=paddle_nhits+nhits
               elseif (ip.eq.10) then
                  call gfhits('ST  ','NOH4',nvdimh,nhdimh,nhmaxh,0,numvsh,
     1                 itrah,numbvh,hitsh(1,1),nhits)
                   do jj=1,nhits
                     do kk=1,10
                        paddle_hitsh(kk,jj+paddle_nhits)=hitsh(kk,jj)
                      enddo
                   enddo
                   paddle_nhits=paddle_nhits+nhits

     	       endif




               if(paddle_nhits.gt.0) then
c ipaddle = paddle number 1-24
                  if(ip.le.2) ipaddle=(isect-1)*4+1
                  if(ip.ge.3.and.ip.le.5) ipaddle=(isect-1)*4+2
                  if(ip.ge.6.and.ip.le.8) ipaddle=(isect-1)*4+3
                  if(ip.ge.9) ipaddle=(isect-1)*4+4

                  isth=mbank(iw,'STH ',ipaddle,nbos_sth,paddle_nhits)
                  do ihit=1,paddle_nhits
                     call ucopy(paddle_hitsh(1,ihit),
     1                    rw(isth+(ihit-1)*nbos_sth+1),7)
                     iw(isth+(ihit-1)*nbos_sth+8)=itrah(ihit)+
     1                   1000*int(paddle_hitsh(8,ihit))
                     iw(isth+(ihit-1)*nbos_sth+9)=
     1                   geant_to_pdg(int(paddle_hitsh(9,ihit)))
     		     rw(isth+(ihit-1)*nbos_sth+10)=paddle_hitsh(10,ihit)
                  enddo
               endif
            enddo  
         enddo
      endif


c  fill data banks from digi info
      
      if(.not.nodata(st))then   ! if(1)
         totdig = 0
         do i=1,6
            do j=1,4
               TDC_min(i,j) = 9999
               ADC_max(i,j) = 0
            enddo   
         enddo
      
c  loop over the ST pairs(3 pairs)
         do isect=1,6   ! do(1)

c  get hits
	   numvs(1)=isect
	
	   do ip=1,4    ! do(2)
	      numvs(2)=0
              
              paddle_ndig=0
   	      if (ip.eq.1) then      
                 call gfdigi('ST  ','LEG1',max_tkdigi,nvol_st,ndigi_st,
     1               max_digi,numvs,itrak,ntra,numsv,kdigi,ndig)
                   do jj=1,ndig
                     do kk=1,4
                        paddle_kdigi(kk,jj+paddle_nhits)=kdigi(kk,jj)
                      enddo
                   enddo
                  paddle_ndig=paddle_ndig+ndig
  	      else if (ip.eq.2) then      
                 call gfdigi('ST  ','LEG2',max_tkdigi,nvol_st,ndigi_st,
     1               max_digi,numvs,itrak,ntra,numsv,kdigi,ndig)
                   do jj=1,ndig
                     do kk=1,4
                        paddle_kdigi(kk,jj+paddle_nhits)=kdigi(kk,jj)
                      enddo
                   enddo
                  paddle_ndig=paddle_ndig+ndig

  	      else if (ip.eq.3) then      
                 call gfdigi('ST  ','LEG3',max_tkdigi,nvol_st,ndigi_st,
     1               max_digi,numvs,itrak,ntra,numsv,kdigi,ndig)
                   do jj=1,ndig
                     do kk=1,4
                        paddle_kdigi(kk,jj+paddle_nhits)=kdigi(kk,jj)
                      enddo
                   enddo
                  paddle_ndig=paddle_ndig+ndig
  	      else if (ip.eq.4) then      
                 call gfdigi('ST  ','LEG4',max_tkdigi,nvol_st,ndigi_st,
     1               max_digi,numvs,itrak,ntra,numsv,kdigi,ndig)
                   do jj=1,ndig
                     do kk=1,4
                        paddle_kdigi(kk,jj+paddle_nhits)=kdigi(kk,jj)
                      enddo
                   enddo
                  paddle_ndig=paddle_ndig+ndig
	      endif


              ipaddle=(isect-1)*4+ip
              ndsec(isect,ip) = 0
              if(paddle_ndig.gt.0)then   ! if(2)
                 do k=1,paddle_ndig
		    numsv(1,k) = ipaddle !paddle
		    ndsec(isect,ip) = ndsec(isect,ip)+1 !
                 enddo
            
                 do i=1,paddle_ndig
                    if(numsv(1,i).eq.ipaddle) then
                       TDC = kdigi(2,i)
                       ADC = kdigi(1,i)
                       if(TDC_min(isect,ip).gt.TDC) then
                          TDC_min(isect,ip) = TDC
                          ADC_max(isect,ip) = ADC
                        endif
                    endif
                 enddo
              endif   ! if(2)
              
           enddo    ! do(2)
 
         enddo  ! do(1)  
         
c check if there is a hit in each pair         
  
           do i=1,6             ! sector loop
              do j=1,4          ! paddle loop
                 if(ndsec(i,j).ne.0) totdig = totdig+1
              enddo
           enddo

           nw = (totdig*nbos_st+1) !the number of integer*2 words
c first save TDC info in STN0 bank
           ist=mbank(iw,'STN0',1,nbos_st,totdig)
           ist=ist*2            !because of integer*2 words
           do i=1,6             ! sector loop
              do j=1,4          ! paddle loop
                 if(ndsec(i,j).gt.0) then
                    iw16(ist+1)=4*(i-1)+j	 
                    iw16(ist+2)=TDC_min(i,j)
                    ist = ist+2
                 endif
              enddo           
           enddo   
c then save ADC info in STN1 bank
           ist=mbank(iw,'STN1',1,nbos_st,totdig)
           ist=ist*2            !because of integer*2 words
           do i=1,6             ! sector loop
              do j=1,4          ! paddle loop
                 if(ndsec(i,j).gt.0) then
                    iw16(ist+1)=4*(i-1)+j	 
                    iw16(ist+2)=ADC_max(i,j)
                    ist = ist+2
                 endif
              enddo           
           enddo   
                            
      endif   ! if(1)

      return
      end

c------------------------------------------------------------------------

