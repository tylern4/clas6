      Subroutine ic_group(ierr)
c     
c     Group hits in the modules 
c     
      IMPLICIT NONE
      SAVE
#include "clasmdl.inc"
#include "icraw.inc"
#include "iccalib.inc"
#include "ichit.inc"
#include "ic.inc"
      integer ierr, sorthits(MaxHits), memb, j1,jm,sk,nmember_scan(maxgroup)
      integer i, si, j, ix, iy, ixgl, iygl, igroup, kg,nmember_old(maxgroup),kg0
      integer idmix(5,2),ioldg,idxy,endgl
      real x,y,xg,yg,xgl,ygl,xh,yh,xh2,yh2,sum_weights,de,dr,wg,etot
      Logical goback,groupscan(maxgroup),elevel,skeep,nomix
c     
c     First sort hits in the modules by decreasing energy  
      call EcGus_sort(nhit,eicm,sorthits)
c     
      call hcdir('//PAWC/IC',' ')
      ngroup=0
      etot=0.
      nmix=0
      dO kg=1,nhit
         sk=sorthits(kg)
         etot=etot+eicm(sk)
         ixgl=ixhit(sk)
         iygl=iyhit(sk)
         skeep=.false.
         if(ngroup.gt.0)then
            do j=1,ngroup
               do j1=nmember_scan(j),nmember_old(j)
                  jm=groupmember(j,j1)
                  ix=ixhit(jm)
                  iy=iyhit(jm)
                  idxy=abs(ix-ixgl)+abs(iy-iygl)
                  if(abs(ix-ixgl).le.1.and.abs(iy-iygl).le.1.and.idxy.gt.0.and.eicm(sk).lt.eicm(jm))then
                     skeep=.true.
                  endif
               enddo
            enddo
         endif
c     Take module with largest energy as a first in the group 
         IF(.not.skeep.and.hitstat(sk).eq.-sk.and.eicm(sk).GT.egrplmin.and.ngroup.lt.maxgroup)THEN
            ngroup=ngroup+1
            hitstat(sk) = ngroup*1000+sk
            Ticg(ngroup)=ticm(sk)
            nmember(ngroup)=1
            groupmember(ngroup,nmember(ngroup))=sk
            fre(ngroup,nmember(ngroup))=1.
            kg0=kg+1
c     
            Do i=kg0,nhit
c     Loop over remaining hits hits
               si=sorthits(i)
               ix=ixhit(si)
               iy=iyhit(si)
               idxy=abs(ix-ixgl)+abs(iy-iygl)
               if(abs(ix-ixgl).le.1.and.abs(iy-iygl).le.1.and.idxy.gt.0)then
                  if(hitstat(si).eq.-si)then
c     If hit is jeicent, include it into the group. For the first 8 jeicent
c     hits no conditions on the distanse, or on the enrgy
                     hitstat(si)=ngroup*1000+si
                     nmember(ngroup)=nmember(ngroup)+1
                     groupmember(ngroup,nmember(ngroup))=si
                     fre(ngroup,nmember(ngroup))=1.
                  elseif(hitstat(si).Gt.0)Then
c     if hit already was included in one of previously defined groups   
                     ioldg=int(hitstat(si))/1000
                     nomix=.true.
                     if(nmix.gt.0)then
                        do j=1,nmix
                           if((idmix(j,1).eq.ioldg.and.idmix(j,2).eq.ngroup).or.
     +                          (idmix(j,2).eq.ioldg.and.idmix(j,1).eq.ngroup))then
c     Do nothing, these 2 groups had common channel before
                              nomix=.false.
                           endif
                        enddo
                     endif
                     if(nmix.eq.0.or.nomix)then
c     First common hit for these 2 groups
                        nmix=nmix+1
                        idmix(nmix,1)=ioldg
                        idmix(nmix,2)=ngroup
                     endif
                     do j=1,nmember(ioldg)
                        if(groupmember(ioldg,j).eq.si)groupmember(ioldg,j)=-si
                     enddo
                     hitstat(si)=-(ngroup*1000+si)
                     nmember(ngroup)=nmember(ngroup)+1
                     groupmember(ngroup,nmember(ngroup))=-si
                     fre(ngroup,nmember(ngroup))=0.
                  endif
               endif
            EndDo
            nmember_scan(ngroup)=2
            nmember_old(ngroup)=nmember(ngroup)
            groupscan(ngroup)=.true.
         enDiF
      enDdO
c     First 9 crystals are grouped, now collecting rest of the cluster 
 111  Continue
      DO j=1,ngroup
         if(.NOT.groupscan(j))goto101
         nmember_old(j)=nmember(j)
         eicg(j)=0.
         xh=0.
         yh=0.
         xh2=0.
         yh2=0.
         sum_weights=0.0
         Do j1=1,nmember_old(j)
            i=abs(groupmember(j,j1))
            eicg(j)=eicg(j)+eicm(i)*fre(j,j1)
         enddo
c
         Do j1=1,nmember_old(j)
            i=abs(groupmember(j,j1))
            if(fre(j,j1).gt.0)then
               de=max(0.,w0+log(eicm(i)*fre(j,j1)/Eicg(j)))
            else
               de=0.
            endif
            sum_weights=sum_weights+de
            xh=xh+de*xhp(i)
            yh=yh+de*yhp(i)
            xh2=xh2+de*xhp(i)*xhp(i)
            yh2=yh2+de*yhp(i)*yhp(i)
         EndDo
         xhg(j)=xh/sum_weights
         yhg(j)=yh/sum_weights
         xwg(j)=sqrt(xh2/sum_weights-xhg(j)*xhg(j))
         ywg(j)=sqrt(yh2/sum_weights-yhg(j)*yhg(j))
         wg=sqrt(ywg(j)**2+xwg(j)**2)
         do j1=nmember_scan(j),nmember_old(j)
            jm=groupmember(j,j1)
            if(jm.gt.0)then
               ixgl=ixhit(jm)
               iygl=iyhit(jm)
               Do i=kg0,nhit
                  si=sorthits(i)
                  x=xhp(si)
                  y=yhp(si)
                  ix=ixhit(si)
                  iy=iyhit(si)
                  idxy=abs(ix-ixgl)+abs(iy-iygl)
                  if(abs(ix-ixgl).le.1.and.abs(iy-iygl).le.1.and.idxy.gt.0)then
                     dr=sqrt((xhg(j)-x)**2+(yhg(j)-y)**2)
                     elevel=eicm(si).gt.eicm(groupmember(j,1))*0.001.and.eicm(si).lt.eicm(jm)
                     if(hitstat(si).eq.-si.and.elevel)then
c     If hit is jeicent, include it into the group. For the first 8 jeicent
c     hits no conditions on the distanse, or on the enrgy
                        hitstat(si)=j*1000+si
                        nmember(j)=nmember(j)+1
                        groupmember(j,nmember(j))=si
                        fre(j,nmember(j))=1.
                        eicg(j)=eicg(j)+eicm(si)*fre(j,j1)
                     elseif((hitstat(si).Gt.0.and.hitstat(si).ne.j*1000+si).and.elevel)Then
c     if hit already was included in one of previously defined groups   
                        ioldg=int(hitstat(si))/1000
                        nomix=.true.
                        if(nmix.gt.0)then
                           do sk=1,nmix
                              if((idmix(sk,1).eq.ioldg.and.idmix(sk,2).eq.j).or.
     +                             (idmix(sk,2).eq.ioldg.and.idmix(sk,1).eq.j))then
c     Do nothing, these 2 groups had common channel before
                                 nomix=.false.
                              endif
                           enddo
                        endif
                        if(nmix.eq.0.or.nomix)then
c     First common hit for these 2 groups
                           nmix=nmix+1
                           idmix(nmix,1)=ioldg
                           idmix(nmix,2)=ngroup
                        endif
c
                        do sk=1,nmember(ioldg)
                           if(groupmember(ioldg,sk).eq.si)then
                              groupmember(ioldg,sk)=-si
C Added two new lines that fix the sharing energy bug Rustam Niyazov 10/14/2008 //
                              eicg(ioldg)=eicg(ioldg)-eicm(si)*fre(ioldg,sk)
                              fre(ioldg,sk)=0.
                           endif                           
                        enddo
                        hitstat(si)=-(j*1000+si)
                        nmember(j)=nmember(j)+1
                        groupmember(j,nmember(j))=-si
                        fre(j,nmember(j))=0.
                     endif
                  endif
               EndDo
            endif
         enddo
 101     Continue
      ENDDO
c     
      goback=.false.
      Do j=1,ngroup
         if(nmember(j).gt.nmember_old(j))then
            nmember_scan(j)=nmember_old(j)+1
            groupscan(j)=.true.
            goback=.true.
         elseif(groupscan(j))then
            groupscan(j)=.false.
            xh=0.
            yh=0.
            xh2=0.
            yh2=0.
            sum_weights=0.0
            eicg(j)=0.
            Do j1=1,nmember_old(j)
               i=abs(groupmember(j,j1))
               eicg(j)=eicg(j)+eicm(i)*fre(j,j1)
            enddo
            igroup=0
            Do j1=1,nmember_old(j)
               i=abs(groupmember(j,j1))
               igroup=igroup+1*int(fre(j,j1))
               if(fre(j,j1).gt.0)then
                  de=max(0.,w0+log(eicm(i)*fre(j,j1)/Eicg(j)))
               else
                  de=0.
               endif
               sum_weights=sum_weights+de
               xh=xh+de*xhp(i)
               yh=yh+de*yhp(i)
               xh2=xh2+de*xhp(i)*xhp(i)
               yh2=yh2+de*yhp(i)*yhp(i)
            EndDo
            xhg(j)=xh/sum_weights
            yhg(j)=yh/sum_weights
            if(igroup.gt.1)then
               xwg(j)=xh2/sum_weights-xhg(j)*xhg(j)
               ywg(j)=yh2/sum_weights-yhg(j)*yhg(j)
            else
               xwg(j)=xstep/sqrt(12.)
               ywg(j)=ystep/sqrt(12.)
            endif
         endif
      enddo
      if(goback)goto 111
      if(ngroup.eq.0)then
         call hf1(112,etot,1.)
      else
         call hf1(111,etot,1.)
      endif
c     
      ierr=ngroup
      return
      end




