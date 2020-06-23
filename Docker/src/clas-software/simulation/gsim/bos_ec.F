      subroutine bos_ec

c_begin_doc

c  Documentation for subroutine bos_ec

c  $Id: bos_ec.F,v 1.5 2010/01/09 01:48:21 fklein Exp $

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
      parameter (cfile=  '$RCSfile: bos_ec.F,v $')
      parameter (crevis= '$Revision: 1.5 $')
      parameter (cstate= '$State: Exp $')
      parameter (cdate=  '$Date: 2010/01/09 01:48:21 $')
      parameter (cautho= '$Author: fklein $')
      parameter (crname= 'BOS_EC')
      parameter (crauth= 'Elliott Wolin')
      data crcsid/'
     1$Id: bos_ec.F,v 1.5 2010/01/09 01:48:21 fklein Exp $
     1 '/
      save

c  include files:
c  --------------
#include "gcbank.inc"
#include "bcs.inc"
#include "ecgeom.inc"
#include "ecsets.inc"
#include "gsimpar.inc"
#include "gsimcontrol.inc"
c_end_inc

c  local variables:
c   ----------------
      integer ndmax
      parameter (ndmax=400)
      integer itra(20000),lhit(1000),jk,jv,nl,ng,l,ll,juk,ip1,ip2
      real edep,edep1,size(1000)
      logical ordered,swap

      integer ntrkmax
      parameter (ntrkmax=1000)
c temp
c      real ed1,edeposit
c      common/edep/edeposit
c      data ed1/0./,edeposit/0./
c      integer namind,pdg_to_geant
c temp
      integer i,j,idig,nw,mbank
      integer iec,ndsec(6)
      integer nview(ndmax),strip_num(ndmax)
      integer numvs(4), itrack(ntrkmax,ndmax), ntra(ndmax)
      integer numsv(4,ndmax), kdigi(2,ndmax), ndig
      
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
c temp
c      print *,'Edeposit=',edeposit
c      print *,'ed1=',ed1
c      edeposit=0.
c      ed1=0.
c temp

c  fill EC bos banks from digis
      if(.not.nodata(ec))then
         call vzero(numvs,4)
         
c  fetch all digis (U,V,W inner and outer) so can sort by sector
c  note...for some odd reason V,W strips numbered backwards
         idig=0
         do i=1,2*nviews
            call gfdigi('EC  ',cdet_ec(i),ntrkmax,4,2,(ndmax-idig),numvs
     $           ,itrack(1,idig+1),ntra(idig+1),numsv(1,idig+1),kdigi(1
     $           ,idig+1),ndig)
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
            if(ndsec(j).gt.0) then
               nw = (ndsec(j)*3+1)/2 !the number of integer*2 words
               iec=mbank(iw,'EC  ',j,3,ndsec(j))
               iec=iec*2        !because of integer*2 words
               
               do i=1,idig
                  if(numsv(1,i).eq.j) then
                     iw16(iec+1)=256*nview(i) + strip_num(i) !id
                     iw16(iec+2)=kdigi(2,i)                  !tdc
                     iw16(iec+3)=kdigi(1,i)                  !adc
                     iec=iec+3
                  endif
               enddo
               
            endif
         enddo
      endif
c temp
c check ECH bank
c
c      iech=namind('ECH ')+1
c  90  iech=iw(iech-1)
c      if(iech.ne.0) then
c        print *,'Bank # ',iw(iech-2),' row # ',iwrow(iech)
c        do l=1,iwrow(iech)
c          ll=(l-1)*iwcol(iech)
c          print *,'part # ',iw(iech+ll+8),' ( geant code ',
c     &       pdg_to_geant(iw(iech+ll+8)),' )'
c          print *,'x,y,z,px,py,pz=',rw(iech+ll+1),rw(iech+ll+2),
c     &       rw(iech+ll+3),rw(iech+ll+4),rw(iech+ll+5),rw(iech+ll+6)
c          print *,'p,tof,edep1,edep2=',rw(iech+ll+7),rw(iech+ll+9),
c     &       rw(iech+ll+10),rw(iech+ll+11)
c        enddo
c        goto 90
c      endif
c
c temp
c
      return
      end

c------------------------------------------------------------------------

