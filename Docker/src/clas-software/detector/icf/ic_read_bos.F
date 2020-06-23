      Subroutine ic_read_bos(ierr)
c     Read ADC's and TDC's from IC online bos bank
      IMPLICIT NONE
      SAVE
#include "bcs.inc"
#include "clasmdl.inc"
#include "icraw.inc"
#include "iccalib.inc"
#include "ictcl.inc"
#include "ichit.inc"

      integer mamind
      external mamind,ICswap
      integer nami, nami0, nd, ind, itest, ncol, nrow, indx
      data nami,nami0/0,0/
      integer*2 I16(2000) , icid_in, icid, ICswap    ! 16 bits
      integer icid_int,iys,ixs
      integer*4 I32(1000)       ! 32 bits
      integer*4 ix,iy, ierr,i, j, jj, adctot
c, tdcmin, tdcmin2
      equivalence (I16(1), I32(1))
      real xps,yps
      real LoopTDC, LoopDist
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
c         print *,nd,iw(ind-5),iw(ind-4)
         DO i=1,2*nd-int(itest/2),3
            if(nmod.ge.maxraw)go to 998
            icid_in=i16(i)
            icid_int=icid_in
            iys=int(icid_int/256)
            ixs=icid_int-iys*256
            icid=icid_int
            iy=int(icid/256)
            ix=icid-iy*256
            if(ic_status(ix,iy).gt.0)then
               nmod=nmod+1
               istat(nmod)=-101
               adc(nmod)=i16(i+2)
               ixm(nmod)=ix
               iym(nmod)=iy
c               print *,'ADCs ',nmod,ix,iy,adc(nmod),ixs,iys
            endif
         ENDDO
         ierr=nmod

 998    Continue
      ENDIF
 999  continue
  

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     Read IC0 data bank (TDC)
      If (nami0.eq.0) nami0 = MAMIND(IW,'IC0 ')
      if (nami0.le.0) then
         print *,'No IC0 ',nmod
        Go To 1000
      endif
      ind=IW(nami0)
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
c         print *,nd,ncol,nrow
         DO i=1,2*nd,2
            if(nmod.ge.maxraw)go to 1001
            nmod=nmod+1
            iy=int(i16(i)/256)
            ix=i16(i)-iy*256
c            print *,'TDCs ',nmod,ix,iy,i16(i+1)
            do j=1,ierr
               if(ix.eq.ixm(j).and.iy.eq.iym(j))then
                  ntdc(j)=ntdc(j)+1
                  tdc(j,ntdc(j))=i16(i+1)
               endif
            enddo
         ENDDO

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      
ccc Find the correct hit among several
        Do j=1,ierr
         if(ntdc(j).eq.0)then
            ticm(j)=-9999.
            NextHit(j)=-9999.
         else
            LoopTDC = 9999.
            LoopDist = 9999.
            tdcmin(j)=1
            LoopTDC = abs(tdc(j,1)*tchan(ixm(j),iym(j))-IC_TDC0(ixm(j),iym(j)))
            Do jj=2,ntdc(j)
               LoopDist = abs(tdc(j,jj)*tchan(ixm(j),iym(j))-IC_TDC0(ixm(j),iym(j)))
               if(LoopDist.lt.LoopTDC)then
                  LoopTDC = abs(tdc(j,jj)*tchan(ixm(j),iym(j))-IC_TDC0(ixm(j),iym(j)))
                  tdcmin(j) = jj
               endif
            EndDo
c            ticm(j,1) = IC_TDC0(ixm(j),iym(j))+
c     +           IC_TDC1(ixm(j),iym(j))/(adc(j)-pedestal(ixm(j),iym(j))-
c     +           IC_TDC2(ixm(j),iym(j)))-tdc(j,tdcmin)*tchan(ixm(j),iym(j))
            if(ntdc(j).eq.1)then
               NextHit(j)=0.
            else
ccc Now search for next closest hit
               LoopTDC = 9999.
               LoopDist = 9999.
               tdcmin2(j)=0.
               Do jj=1,ntdc(j)
                  if(tdcmin(j).ne.jj)then
                     LoopDist = (tdc(j,tdcmin(j))-tdc(j,jj))*tchan(ixm(j),iym(j))
                     if(abs(LoopDist).lt.abs(LoopTDC))then
                        LoopTDC = LoopDist
                        tdcmin2(j) = jj
                     endif
                  endif
               EndDo
               
c               ticm(j,2) = IC_TDC0(ixm(j),iym(j))+
c     +              IC_TDC1(ixm(j),iym(j))/(adc(j)-pedestal(ixm(j),iym(j))-
c     +              IC_TDC2(ixm(j),iym(j)))-tdc(j,tdcmin2)*tchan(ixm(j),iym(j))
c
c               NextHit(j) = ticm(j,1)-ticm(j,2)
            endif
         endif
      EndDo


ccccccccccccccccccccccccccccccccccccccccccccccccccccccccc


c
 1001    Continue
      ENDIF
 1000  continue
      call hcdir('//PAWC',' ')
      return
      end
