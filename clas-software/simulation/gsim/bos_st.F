      subroutine bos_st

c_begin_doc

c  Documentation for subroutine bos_st

c  $Id: bos_st.F,v 1.6 2009/03/14 20:27:52 fklein Exp $

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
      parameter (cfile=  '$RCSfile: bos_st.F,v $')
      parameter (crevis= '$Revision: 1.6 $')
      parameter (cstate= '$State: Exp $')
      parameter (cdate=  '$Date: 2009/03/14 20:27:52 $')
      parameter (cautho= '$Author: fklein $')
      parameter (crname= 'BOS_ST')
      parameter (crauth= 'Elliott Wolin')
      data crcsid/'
     1$Id: bos_st.F,v 1.6 2009/03/14 20:27:52 fklein Exp $
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
      integer ist,ndsec(6),isect,ipair,ip,sect(6)
      integer TDC1,TDC2,ADC1,ADC2,TDC1_min(6),TDC2_min(6)
      integer ADC1_max(6),ADC2_max(6)

c  for ST data
      integer nbos_st
      parameter (nbos_st=3)

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


c  executable code:
c  ----------------

c  fill STH MC hits bank
      if(.not.nomcdata(st))then
	 do ipair = 1, 3
            call vzero(numvsh,nvdimh)
	    numvsh(1) = ipair
	    do ip = 1, 2
	       numvsh(2)=idet
	       isect = (numvsh(1)-1)*2+ip
	       
	       if (ip.eq.1) then
                  call gfhits('ST  ','STH1',nvdimh,nhdimh,nhmaxh,0,numvsh,
     1                 itrah,numbvh,hitsh(1,1),nhits)
               else
                  call gfhits('ST  ','STH2',nvdimh,nhdimh,nhmaxh,0,numvsh,
     1                 itrah,numbvh,hitsh(1,1),nhits)
     	       endif

               if(nhits.gt.0) then
                  isth=mbank(iw,'STH ',isect,nbos_sth,nhits)
                  do ihit=1,nhits
                     call ucopy(hitsh(1,ihit),
     1                    rw(isth+(ihit-1)*nbos_sth+1),7)
                     iw(isth+(ihit-1)*nbos_sth+8)=itrah(ihit)+
     1                   1000*int(hitsh(8,ihit))
                     iw(isth+(ihit-1)*nbos_sth+9)=
     1                   geant_to_pdg(int(hitsh(9,ihit)))
     		     rw(isth+(ihit-1)*nbos_sth+10)=hitsh(10,ihit)
                  enddo
               endif
            enddo  
         enddo
      endif


c  fill data banks from digi info
      
      if(.not.nodata(st))then   ! if(1)
         totdig = 0
         do i=1,6
            TDC1_min(i) = 9999
            TDC2_min(i) = 9999
            ADC1_max(i) = 0
            ADC2_max(i) = 0
         enddo   
      
c  loop over the ST pairs(3 pairs)
         do ipair=1,3   ! do(1)

c  get hits
	   numvs(1)=ipair
	
	   do ip=1,2    ! do(2)
	      numvs(2)=0
	      isect = (ipair-1)*2+ip
	     
   	      if (ip.eq.1) then      
                 call gfdigi('ST  ','ST1 ',max_tkdigi,nvol_st,ndigi_st,
     1               max_digi,numvs,itrak,ntra,numsv,kdigi,ndig)
	      else	
                 call gfdigi('ST  ','ST2 ',max_tkdigi,nvol_st,ndigi_st,
     1               max_digi,numvs,itrak,ntra,numsv,kdigi,ndig)
	      endif
              ndsec(isect) = 0
              if(ndig.gt.0)then   ! if(2)
                 do k=1,ndig
		    numsv(1,k) = isect
		    ndsec(isect) = ndsec(isect)+1
                 enddo
            
                 do i=1,ndig
                    if(numsv(1,i).eq.isect) then
                       TDC1 = kdigi(2,i)
                       TDC2 = kdigi(4,i)
                       ADC1 = kdigi(1,i)
                       ADC2 = kdigi(3,i)
		       IF(mod(isect,2).eq.0) THEN
		          if(TDC1_min(isect-1).gt.TDC1) TDC1_min(isect-1) = TDC1
		          if(TDC2_min(isect).gt.TDC2) TDC2_min(isect) = TDC2
                          if(TDC1_min(isect-1).gt.TDC2_min(isect)) then
                             ADC1_max(isect-1) = ADC1
                             ADC2_max(isect) = ADC2
                          else
                             ADC1_max(isect-1) = ADC2
                             ADC2_max(isect) = ADC1
                          endif   
		       ELSE   	            
                          if(TDC1_min(isect).gt.TDC1) TDC1_min(isect) = TDC1
                          if(TDC2_min(isect+1).gt.TDC2) TDC2_min(isect+1) = TDC2
                          if(TDC1_min(isect).gt.TDC2_min(isect+1)) then
                             ADC1_max(isect) = ADC1
                             ADC2_max(isect+1) = ADC2
                          else
                             ADC1_max(isect) = ADC2
                             ADC2_max(isect+1) = ADC1
                          endif    
                       ENDIF   
                    endif
                 enddo
              endif   ! if(2)
              
           enddo    ! do(2)
 
         enddo  ! do(1)  
         
c check if there is a hit in each pair         
         do i=1,6,2
            if((ndsec(i)+ndsec(i+1)).ne.0) totdig = totdig+1
            if(ndsec(i).ne.0.and.ndsec(i+1).ne.0) ndsec(i+1) = 0
         enddo   
              
              nw = (2*totdig*nbos_st+1)/2 !the number of integer*2 words
              ist=mbank(iw,'ST  ',1,nbos_st,totdig*2)
              ist=ist*2     !because of integer*2 words

              do j=1,6            !sector loop
                 if(ndsec(j).gt.0) then
                     if(mod(j,2).eq.0) then
                       iw16(ist+1)=j-1	 
                       iw16(ist+2)=TDC1_min(j-1)
                       iw16(ist+3)=ADC1_max(j-1)
                       iw16(ist+4)=j
                       iw16(ist+5)=TDC2_min(j)
                       iw16(ist+6)=ADC2_max(j) 
                     else if(mod(j,2).eq.1.and.ndsec(j).gt.0) then 
                       iw16(ist+1)=j	 
                       iw16(ist+2)=TDC1_min(j)
                       iw16(ist+3)=ADC1_max(j)
                       iw16(ist+4)=j+1
                       iw16(ist+5)=TDC2_min(j+1)
                       iw16(ist+6)=ADC2_max(j+1)
                     endif 
                 ist = ist+6
                 endif

              enddo   
                            
      endif   ! if(1)

      return
      end

c------------------------------------------------------------------------

