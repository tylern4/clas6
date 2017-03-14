      subroutine bos_ec_t

c_begin_doc

c  Documentation for subroutine bos_ec

c  $Id: bos_ec_t.F,v 1.4 2010/01/09 01:48:21 fklein Exp $

c  Purpose:
c  --------
c	ec bos output routine

c  Author:
c  -------
c     Created  22 June 1995 E.Doroshkevich

c  Major revisions:
c  ----------------
c     Elliott Wolin, College of William and Mary, 30-jun-95
c     Raphael Demirchyan , 26-aug-1995
c_end_doc


      implicit none
      character*(*) cfile,crevis,cstate,cdate,cautho,crname,crauth
      character*132 crcsid
      parameter (cfile=  '$RCSfile: bos_ec_t.F,v $')
      parameter (crevis= '$Revision: 1.4 $')
      parameter (cstate= '$State: Exp $')
      parameter (cdate=  '$Date: 2010/01/09 01:48:21 $')
      parameter (cautho= '$Author: fklein $')
      parameter (crname= 'BOS_EC')
      parameter (crauth= 'Elliott Wolin')
      data crcsid/'
     1$Id: bos_ec_t.F,v 1.4 2010/01/09 01:48:21 fklein Exp $
     1 '/
      save

c  include files:
c  --------------
#include "gcbank.inc"
#include "bcs.inc"
#include "wcs.inc"
#include "ecgeom.inc"
#include "ecsets.inc"
#include "gsimpar.inc"
#include "gsimcontrol.inc"
c_end_inc

c  local variables:
c   ----------------
      integer ndmax
      parameter (ndmax=400)
      integer ntrkmax
      parameter (ntrkmax=1000)

      integer itra(20000),lhit(1000),jk,jv,nl,ng,l,ll,juk,ip1,ip2
      real edep,edep1,size(1000)
      logical ordered,swap

      integer i,j,idig,nw,mbank,mlink,ind2,indx,ind
      integer iec,ndsec(6),ic0
      integer nview(ndmax),strip_num(ndmax),used(ndmax)
      integer numvs(4), itrack(ntrkmax,ndmax), ntra(ndmax)
      integer numsv(4,ndmax), kdigi(2,ndmax), ndig
      integer*2 idn16
      
c     numsv(1,) - number of sector (1-6)
c     numsv(2,) = 1 
c     numsv(3,) - module (1-5) for inner, (1-8) for outer
c     numsv(4,) - ec strip (1 - 36)
c     kdigi(1,) - i_adc 
c     kdigi(2,) - i_tdc 

c  for ECH hits
      integer nvdimh,nhdimh,nhmaxh,nbos_ech
      parameter (nvdimh=1)
      parameter (nhdimh=nhits_ech)
      parameter (nhmaxh=500)
      parameter (nbos_ech=11)
      integer numvsh(nvdimh)
      integer numbvh(nvdimh,nhmaxh)
      integer itrah(nhmaxh)
      real hitsh(nhdimh,nhmaxh)
      integer nhits,ihit,iech,geant_to_pdg,sector
c_end_var


c  executable code:
c  ----------------

c  fill ECH bos bank from hits
      if(.not.nomcdata(ec)) then
        call vzero(numvsh,nvdimh)
        do sector=1,6
          numvsh(1)=sector
          call gfhits('ECH ','EC  ',nvdimh,nhdimh,nhmaxh,0,numvsh,
     1           itrah,numbvh,hitsh,nhits)
          if(nhits.gt.0) then
            iech=mbank(iw,'ECH ',sector,nbos_ech,nhits)
c reordering ...
            do ihit=1,nhits
              lhit(ihit)=ihit
              size(ihit)=hitsh(7,ihit)
            enddo
c
            ordered= .FALSE.
            do while(.not.ordered)
              ordered= .TRUE.
              do i=1,nhits-1
                j=i+1
                swap=size(lhit(i)).LT.size(lhit(j))
                if(swap) then
                  ll=lhit(i)
                  lhit(i)=lhit(j)
                  lhit(j)=ll
                  ordered=.FALSE.
                endif
              enddo
            enddo
c
            do ihit=1,nhits
              call ucopy(hitsh(1,lhit(ihit)),
     1                 rw(iech+(ihit-1)*nbos_ech+1),7)
              iw(iech+(ihit-1)*nbos_ech+8)=
     1                 geant_to_pdg(int(hitsh(8,lhit(ihit))))
              rw(iech+(ihit-1)*nbos_ech+9)= hitsh(9,lhit(ihit))
c
c deposit energy filling
c
      edep=0.
      edep1=0.
      ip1=1
      ip2=2
      itra(ip1)=itrah(lhit(ihit))
C
      IF(JKINE.GT.0.AND.JVERTX.GT.0) THEN
        DO WHILE(ip1.LT.ip2)
          JK=LQ(JKINE-itra(ip1)) ! pointer to the bank of track itra(ip1)
          IF(JK.NE.0) THEN
            NG=Q(JK+7)+7 ! the number of vertices
            do l=8,ng
              JV=LQ(JVERTX-INT(Q(JK+L)))
              IF(JV.NE.0.AND.(Q(JV+4)*1.E+9).GT.hitsh(9,lhit(ihit))) THEN
                NW=Q(JV+7)+7 ! the number of tracks for present vertix
                do ll=8,nw
                  itra(ip2)=INT(Q(JV+LL))
                  ip2=ip2+1
                enddo
              ENDIF
            enddo
            NL=IQ(JK-2) ! the number of user words
            IF(NL.GT.0) THEN
              JUK = LQ(JK-1)
              if(JUK.GT.0) then
                do L=6,IQ(JUK-1)
                  if(Q(JUK+L).GT.0.) edep=edep+Q(JUK+L)
                  if(Q(JUK+L).LT.0.) edep1=edep1-Q(JUK+L)
                enddo
              endif
            ENDIF
          ENDIF
          ip1=ip1+1
        ENDDO ! while
C
        rw(iech+(ihit-1)*nbos_ech+10)=edep
        rw(iech+(ihit-1)*nbos_ech+11)=edep1
c temp
c        print *,'edep,edep1=',edep,edep1
c        ed1=ed1+edep+edep1
c temp
      ENDIF
c
c end of deposit energy filling
c
            enddo
          endif
        enddo
      endif

c  fill EC bos banks from digis
      if(.not.nodata(ec))then
         call vzero(numvs,4)
         
c  fetch all digis (U,V,W inner and outer) so can sort by sector
c  note...for some odd reason V,W strips numbered backwards
         idig=0
         do i=1,2*nviews
            call gfdigi('EC  ',cdet_ec(i),ntrkmax,4,2,(ndmax-idig),numvs,
     &           itrack(1,idig+1),ntra(idig+1),numsv(1,idig+1),
     &           kdigi(1,idig+1),ndig)
            if(ndig.gt.0) then
               do j=idig+1,idig+ndig
                  nview(j) = i
                  if((i.eq.1).or.(i.eq.4))then            !U strips
                     strip_num(j)=numsv(4,j)
                  else                                    !V,W strips
                     strip_num(j)=nstrip-numsv(4,j)+1
                  endif
               enddo
            endif
            idig=idig+ndig
         enddo
         
c  add up number of digis per sector
         call vzero(ndsec,6)
         do i=1,idig
            ndsec(numsv(1,i)) = ndsec(numsv(1,i)) + 1
         enddo
         
c  write digis to bos banks by sector
         do j=1,6
            ind=mlink(jw,'EC  ',j)
            ind2=2*ind
            if(ndsec(j).gt.0) then
              ic0=0
              if(ind.gt.0)then
c there are preexisting hits in EC sector j in this event.
                 do indx=1,jw(ind-4)
                   ic0=ic0+1
                   used(indx)=1
                   do i=1,idig
                     idn16=256*nview(i) + strip_num(i) !id
                     if(numsv(1,i).eq.j.and.jw16(ind2+1+(indx-1)*3).eq.idn16)then
                       used(indx)=-1
                       ic0=ic0-1
                       if(jw16(ind2+2+(indx-1)*3).lt.kdigi(2,i))kdigi(2,i)=jw16(ind2+2+(indx-1)*3)  ! tdc
                       kdigi(1,i)=jw16(ind2+3+(indx-1)*3)+kdigi(1,i)      ! adc
                     endif
                   enddo
                 enddo
              endif
              iec=mbank(iw,'EC  ',j,3,ndsec(j)+ic0)
              iec=iec*2        !because of integer*2 words
              do i=1,idig
                if(numsv(1,i).eq.j) then
                  iw16(iec+1)=256*nview(i) + strip_num(i) !id
                  iw16(iec+2)=kdigi(2,i)                  !tdc
                  iw16(iec+3)=kdigi(1,i)                  !adc
                  iec=iec+3
                endif
              enddo
              if(ind.gt.0)then
                do indx=1,jw(ind-4)
                  if(used(indx).gt.0)then
                    iw16(iec+1)=jw16(ind2+1+(indx-1)*3)
                    iw16(iec+2)=jw16(ind2+2+(indx-1)*3)
                    iw16(iec+3)=jw16(ind2+3+(indx-1)*3)
                    iec=iec+3
                  endif
                enddo
              endif
            else
              if(ind.gt.0)then
                iec=mbank(iw,'EC  ',j,3,jw(ind-4))
                if(iec.eq.0)then
c Error creating EC in bcs
                else
                  call ucopy(jw(ind+1),iw(iec+1),jw(ind))
                endif
              endif
            endif
         enddo
      endif
c
      return
      end

c------------------------------------------------------------------------

