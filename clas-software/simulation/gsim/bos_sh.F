      subroutine bos_sh

      implicit none
      save
      
#include "gcbank.inc"
#include "bcs.inc"
#include "shgeom.inc"
#include "shsets.inc"
#include "gsimpar.inc"
#include "gsimcontrol.inc"
#include "ffpar_sh.inc"
#include "gctrak.inc"

      integer ndmax
      parameter (ndmax=2000)
      
      integer ntrkmax
      parameter (ntrkmax=2000)

      integer i,idig,nw,mbank,ipix,icorner,id
      integer ish 
      integer numvs(nvol_hd), itrack(ntrkmax,ndmax), ntra(ndmax)
      integer numbv(nvol_hd,ndmax), kdigi(ndigi_hd,ndmax), ndig
      integer ii,iii

      integer shid_pix(56)
      data shid_pix /211, 206, 102, 116, 111, 106, 216, 212,
     +               207, 203, 117, 112, 107, 102, 217, 213,
     +               208, 204, 118, 113, 108, 103, 118, 214,
     +               209, 114, 109, 104, 304, 309, 314, 409,
     +               414, 418, 303, 308, 313, 318, 404, 408,
     +               413, 417, 302, 307, 312, 317, 403, 407,
     +               412, 416, 306, 311, 316, 402, 406, 411/ 
      
      
      
c     kdigi(1,) - i_adc 

c     kdigi(2,) - i_tdc 

c  fill SH bos banks from digis


      if(.not.nodata(sh))then                      
         call vzero(numvs,nvol_hd)
         
         idig=0

         call gfdigi('SH  ','SPIX',ntrkmax,nvol_hd,ndigi_hd,(ndmax-idig)
     $               ,numvs,itrack(1,idig+1),ntra(idig+1)
     $               ,numbv(1,idig+1),kdigi(1,idig+1),ndig)
         
c     
         idig = idig+ndig
         
c 
         ish=mbank(iw,'SH  ',0,3,idig)
         ish=ish*2      !because of integer*2 words
         
          do i=1,idig
            do ii = 1,56
                 if(ii.eq.numbv(1,i))then 
c for compatibility with reconstraction code 
                    ipix=shid_pix(ii)
                     icorner = ipix/100
                     ipix=ipix - icorner*100
                     id = 256*icorner+ipix
                    go to 1
                 endif                    
             enddo
 1                continue
                       
            iw16(ish+1)=id      
            iw16(ish+2)=kdigi(2,i)    !tdc
            iw16(ish+3)=kdigi(1,i)    !adc 
            ish=ish+3
	   enddo
      endif                              
      
      return
      end

