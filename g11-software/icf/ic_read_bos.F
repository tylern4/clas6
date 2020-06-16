      Subroutine ic_read_bos(ierr)
c     Read ADC's and TDC's from IC online bos bank
      IMPLICIT NONE
      SAVE
#include "bcs.inc"
#include "clasmdl.inc"
#include "icraw.inc"
#include "iccalib.inc"
#include "ictcl.inc"
      integer mamind
      external mamind,ICswap
      integer nami, nami0, nd, ind, itest, ncol, nrow, indx
      data nami,nami0/0,0/
      integer*2 I16(2000) , icid_in, icid, ICswap    ! 16 bits
      integer icid_int,iys,ixs
      integer*4 I32(1000)       ! 32 bits
      integer*4 ix,iy, ierr,i, j, adctot, tdcmin
      equivalence (I16(1), I32(1))
      real xps,yps
c     
      nmod=0
      do i=1,MaxRaw
         istat(i)=0
         ixm(i)=0
         iym(i)=0
      enddo
c     Read IC data bank (ADC)
      If (nami.eq.0) nami = MAMIND(IW,'IC  ')
      if (nami.le.0) Go To 999
      ind=IW(nami)
      
      
      IF(ind.GT.0)THEN
         nd=iw(ind)
         itest=mod(nd,3)
         DO i=1,nd
            i32(i)=iw(ind+i)
         ENDDO
c     
         DO i=1,2*nd-int(itest/2),3
            if(nmod.ge.maxraw)go to 998
            icid_in=i16(i)
            icid_int=icid_in
            iys=int(icid_int/256)
            ixs=icid_int-iys*256
            if(iswap.ne.0)then
               icid=ICswap(icid_int)
            else
               icid=icid_int
            endif
            iy=int(icid/256)
            ix=icid-iy*256
c            print *,ic_status(ix,iy),ix,iy,iswap
            if(ic_status(ix,iy).gt.0)then
               nmod=nmod+1
               istat(nmod)=-101
               adc(nmod)=i16(i+2)
               ixm(nmod)=ix
               iym(nmod)=iy
            endif
         ENDDO
         ierr=nmod

 998    Continue
      ENDIF
 999  continue
  

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     Read IC0 data bank (TDC)
      If (nami0.eq.0) nami0 = MAMIND(IW,'IC0 ')
      if (nami0.le.0) Go To 1000
      ind=IW(nami0)
c      
c      
      IF(ind.GT.0)THEN
         nd=iw(ind)
         itest=mod(nd,3)
         ncol=iw(ind-5)
         nrow=iw(ind-4)
         DO i=1,nd
            i32(i)=iw(ind+i)
         ENDDO
c     
         Do i=1,1000
            ntdc(i)=0
         EndDo
         DO i=1,nd
            if(nmod.ge.maxraw)go to 1001
            nmod=nmod+1
            iy=int(i16(i)/256)
            ix=i16(i)-iy*256
            do j=1,ierr
               if(ix.eq.ixm(j).and.iy.eq.iym(j))then
                  ntdc(j)=ntdc(j)+1
                  tdc(j,ntdc(j))=i16(i+1)
               endif
            enddo
         ENDDO
c
 1001    Continue
      ENDIF
 1000  continue
      call hcdir('//PAWC',' ')
      return
      end
