      Subroutine ichb(ierr) 
c     c Finish with sorting the groups of hits and move to ICHB bos bank
      IMPLICIT NONE
      SAVE
#include "icraw.inc"
#include "iccalib.inc"
#include "ichit.inc"
#include "ictcl.inc"
#include "bcs.inc"
#include "clasmdl.inc"
      integer ierr
      real xh,yh,xh2,yh2,sum_weights,de,fraction1,fraction2,dr,etot,etotg
      integer i, j, ix, iy, igroup, ind, indx, ihit, i1, j1
      integer sortgroup(MaxGroup), ig, itest
      logical groupout(MaxGroup)
      integer mbank
      external mbank
c      
      if(nmix.gt.0)then
         DO i=1,ngroup
            Do j=1,nmember(i)
               IF (groupmember(i,j).lt.0)THEN
                  do i1=i+1,ngroup
                     dO j1=1,nmember(i1)
                        if(groupmember(i1,j1).eq.groupmember(i,j))then
                           ihit=-groupmember(i,j)
                           groupmember(i,j)=ihit
                           groupmember(i1,j1)=ihit
                           etot=eicg(i)+eicg(i1)
                           dr=sqrt((xhg(i)-xhp(ihit))**2+(yhg(i)-yhp(ihit))**2)
                           fraction1=eicg(i)/etot
c     *exp(-dr/sqrt(ywg(i)**2+xwg(i)**2))
                           dr=sqrt((xhg(i1)-xhp(ihit))**2+(yhg(i1)-yhp(ihit))**2)
                           fraction2=eicg(i1)/etot
c     *exp(-dr/sqrt(ywg(i1)**2+xwg(i1)**2))
                           fre(i,j)=fraction1
c     /(fraction1+fraction2)
                           fre(i1,j1)=fraction2
c     /(fraction1+fraction2)
                           eicg(i)=eicg(i)+eicm(ihit)*fre(i,j)
                           eicg(i1)=eicg(i1)+eicm(ihit)*fre(i1,j1)
                        endif
                     enDdO
                  enddo
                  If (groupmember(i,j).lt.0)Then
                     groupmember(i,j)=-groupmember(i,j)
                     fre(i,j)=1.
                     eicg(i)=eicg(i)+eicm(ihit)*fre(i,j)
                  EndIf
               ENDIF
            EndDo
         ENDDO
      endif
c     
      igroup=0
      DO i=1,ngroup
         groupout(i)=.false.
         IF (Eicg(i).gt.egrpmin
     +        .and.Eicg(i)/Eicg(1).gt.0.01
     +        .and.Eicm(groupmember(i,1))/Eicg(i).gt.0.1)THEN
            igroup=igroup+1
            groupout(i)=.true.
            sum_weights=0.0
            xh=0.
            yh=0.
            xh2=0.
            yh2=0.
            Eicg(i)=0.
            Do j=1,nmember(i)
               ihit=groupmember(i,j)
               Eicg(i)=Eicg(i)+eicm(ihit)*fre(i,j)
            EndDo
            if(Eicg(i).le.0)groupout(i)=.false.
            Do j=1,nmember(i)
               ihit=groupmember(i,j)
               if(fre(i,j).gt.0)then
                  de=max(0.,w0+log(eicm(ihit)*fre(i,j)/Eicg(i)))
               else
                  de=0.0
               endif
               sum_weights=sum_weights+de
               xh=xh+de*xhp(ihit)
               yh=yh+de*yhp(ihit)
            EndDo
            xhg(i)=xh/sum_weights
            yhg(i)=yh/sum_weights
c     One more loop for second, third, fourth ... moments
            sum_weights=0.0
            xh=0.
            yh=0.
            xh2=0.
            yh2=0.
            Do j=1,nmember(i)
               ihit=groupmember(i,j)
               if(fre(i,j).gt.0)then
                  de=max(0.,w0+log(eicm(ihit)*fre(i,j)/Eicg(i)))
               else
                  de=0.0
               endif
               sum_weights=sum_weights+de
               xh=xh+de*(xhp(ihit)-xhg(i))*(xhp(ihit)-xhg(i))
               yh=yh+de*(yhp(ihit)-yhg(i))*(yhp(ihit)-yhg(i))
               xh2=xh2+de*(xhp(ihit)-xhg(i))*(xhp(ihit)-xhg(i))*(xhp(ihit)-xhg(i))
               yh2=yh2+de*(yhp(ihit)-yhg(i))*(yhp(ihit)-yhg(i))*(yhp(ihit)-yhg(i))
            EndDo
            if(nmember(i).gt.1)then
               xwg(i)=xh/sum_weights
               ywg(i)=yh/sum_weights
            else
               xwg(i)=xstep/sqrt(12.)
               ywg(i)=ystep/sqrt(12.)
            endif
            m3x(i)=xh2/sum_weights
            m3y(i)=yh2/sum_weights
         ELSE
            groupout(i)=.false.
         ENDIF
      ENDDO
c     First sort clusters (groups of hits) by highest energy
c     Then fill the bos bank
      etot=0.
      do i=1,nhit
         etot=etot+eicm(i)
      enddo
      itest=0
      etotg=0
      call EcGus_sort(ngroup,eicg,sortgroup)
      IF (igroup.gt.0)THEN
         ind = MBANK(IW,'ICHB',0,14,igroup)
         if(ind.gt.0)then
            Do i=1,ngroup
               ig=sortgroup(i)
               If(groupout(ig))Then
                  itest=itest+1
                  indx=ind+(itest-1)*14
                  rw(indx+1)=Eicg(ig)
                  rw(indx+2)=eicm(groupmember(ig,1))
                  rw(indx+3)=Ticg(ig)
                  rw(indx+4)=ticm(groupmember(ig,1),1)
                  rw(indx+5)=xhg(ig)
                  rw(indx+6)=yhg(ig)
                  rw(indx+7)=zhp(groupmember(ig,1))
c     
                  rw(indx+8)=xhp(groupmember(ig,1))
                  rw(indx+9)=yhp(groupmember(ig,1))
c     
                  rw(indx+10)=xwg(ig)
                  rw(indx+11)=ywg(ig)
                  rw(indx+12)=m3x(ig)
                  rw(indx+13)=m3y(ig)
c     
                  iw(indx+14)=nmember(ig)
                  etotg=etotg+Eicg(ig)
               EndIf
            EndDo
         else
            print *,'Could not create ICHB' 
         endif
         if(etotg.gt.etot+0.001)then
c            print *,'Group ',igroup,nhit,etot,etotg-etot
c            do i=1,ngroup
c               ig=sortgroup(i)
c               print *,'Member ',i,ig,nmember(i),eicg(ig)
c               do j=1,nmember(ig)
c                  print *,j,groupmember(ig,j),eicm(groupmember(ig,j)),fre(ig,j)
c               enddo
c            enddo
         endif
      ELSE
         call hcdir('//PAWC/IC',' ')
         call hf1(111,etot,1.)
      ENDIF
c     
      return
      end
