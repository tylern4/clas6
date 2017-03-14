      subroutine step_ec1

c_begin_doc

c  Documentation for subroutine step_ec1

c  $Id: step_ec1.F,v 1.3 1998/05/29 16:58:40 holtrop Exp $

c  Purpose:
c  --------
c	stores ec1 hits

c  Author:
c  -------
c     Elliott Wolin, College of William and Mary, 13-apr-95

c  Major revisions:
C    Created:  12-Jan-1994   Serguei Boiarinov
C    Modified:  22-Mar-1995   Harout  Avakian
c  ----------------

c_end_doc


      implicit none
      character*(*) cfile,crevis,cstate,cdate,cautho,crname,crauth
      character*132 crcsid
      parameter (cfile=  '$RCSfile: step_ec1.F,v $')
      parameter (crevis= '$Revision: 1.3 $')
      parameter (cstate= '$State: Exp $')
      parameter (cdate=  '$Date: 1998/05/29 16:58:40 $')
      parameter (cautho= '$Author: holtrop $')
      parameter (crname= 'step_ec1')
      parameter (crauth= 'Elliott Wolin')
      data crcsid/'
     1$Id: step_ec1.F,v 1.3 1998/05/29 16:58:40 holtrop Exp $
     1 '/
      save



c  include files:
c  --------------
#include "gsimpar.inc"
#include "gsimcontrol.inc"
#include "gcbank.inc"
#include "gcsets.inc"
#include "gckine.inc"
#include "gcflag.inc"
#include "gctrak.inc"
#include "gcvolu.inc"
#include "gcnum.inc"
#include "gctmed.inc"
#include "ec1geom.inc"
#include "ec1tmed.inc"
#include "ffpar_ec1.inc"
c_end_inc

c  local variables:
c  ----------------
      integer nl,nsuper,nsub,nstrip,numsv(3),numsa(3)
      integer js,ndet,iali,ihit
      real*4 cloc(3),hitsh(5),hitsa(5),aa,ll,lstrip,elos
      real*4 cloc_(3)
      real*4 temp(3)
C
C translate volume names for all levels from INTEGER to CHARACTER*4
C
      character*4 volnam(15)
      equivalence(names(1),volnam(1))
c_end_var


c  executable code:
c  ----------------

      if(nohits(ec1))return

      if(numed.eq.ec1_med_scint) then
C
C it is first step in this volume
C
         if(inwvol.eq.1) then
C
C get of local coordinates current point
C
            call gmtod(vect(1),cloc(1),1)
c
C get of local coordinates current vertex
C
            call gmtod(vert(1),temp(1),1)
C
C remember of local coordinates
C
            cloc_(1)=cloc(1)
            cloc_(2)=cloc(2)
            cloc_(3)=cloc(3)
C
C clear elos
C
            elos=0.
C
C fill ALIAS
C
            if(volnam(nlevel).eq.'ECSS'.AND.
     &           cloc(3).lt.0.and.temp(3).le.-0.5) then
C
               hitsa(1)=cloc(1)
               hitsa(2)=cloc(2)
               call gmtod(vect(4),hitsa(3),2)
               hitsa(3)=hitsa(3)*vect(7)*1000.
               hitsa(4)=hitsa(4)*vect(7)*1000.
               hitsa(5)=hitsa(5)*vect(7)*1000.
C
C extract IALI for 'PART' (ISET available already)
C
               js=lq(jset-iset)
               ndet=iq(js-1)
               if(ndet.le.0) goto 999
               call glook('PART',iq(js+1),ndet,iali)
               
               if(iali.eq.0) goto 999
C
               numsa(1)=ivert
               numsa(2)=ipart
               numsa(3)=number(nlevel-3)
C
               call gsahit(iset,iali,itra,numsa,hitsa,ihit)
               if(ihit.eq.0) write(6,*) 'LAChits: hit not stored'
            endif
C
            go to 999
         else
C
C it is next step in this volume
C
            elos=elos+destep
C
         endif
C
C it is last step in this volume
C
         if(inwvol.eq.2.or.inwvol.eq.3.or.istop.eq.1.
     &        or.istop.eq.2) then
C
C get of local coordinates
C
            call gmtod(vect(1),cloc(1),1)
C
C find the track center
C
            cloc(1)=(cloc(1)+cloc_(1))/2.
            cloc(2)=(cloc(2)+cloc_(2))/2.
            cloc(3)=(cloc(3)+cloc_(3))/2.
C
C are we in ECSS or in ECSP ?
C
            if(volnam(nlevel).eq.'ECSS') then
               numsv(1)=number(nlevel-3)
               nl=0
               aa=hs1
               ll=hb1
            else
               numsv(1)=number(nlevel-5)
               nl=number(nlevel-1)
               
C each pair of layers (superlayer) have the same length starting from 2
               if(mod(nl,2).gt.0) then
                  ll=hb1+db*(nl+1)
                  aa=hs1+ds*(nl+1)
               else
                  ll=hb1+db*nl
                  aa=hs1+ds*nl
               endif 
            endif
            
C
C nl - number of layer (0,1,2,..,32)
C
            numsv(2)=nl+1
            
C
C numbers of superlayer (0,1,2,..,16) and sublayer (0,1)
C
            nsuper=nl/2
            nsub=nl-nsuper*2
C
C parameters for current scintilator
C short layer:
            if(nsub.eq.0) then
               nstrip=20.*(1.+cloc(1)/ll)+1
C  check
               if(nstrip.lt.1) nstrip=1
               if(nstrip.gt.40) nstrip=40
               lstrip=cloc(2)+aa
            endif
C long layer:
            if(nsub.eq.1) then
               nstrip=12.*(1.+cloc(2)/aa)+1
               lstrip=cloc(1)+ll
c check 
               if(nstrip.lt.1) nstrip=1
               if(nstrip.gt.24) nstrip=24
               
            endif
c
c test lstrip > 0
c
            if(lstrip.lt.0.) then
               lstrip=0.
            endif
C
C store it
C
            numsv(3)=nstrip
            if(nsub.eq.1) then 
               hitsh(1)=cloc(1)
            else
               hitsh(1)=cloc(2)
            endif
            hitsh(2)=cloc(3)
            hitsh(3)=lstrip
C
C ADC and TDC
C
            hitsh(4)=elos*1.E03
            hitsh(5)=tofg*1.E09
C store hits
C ( iset and idet we have now in /GCSETS/, because we are in xxSTEP )
C
            if(elos.gt.0.000001) then
               call gsahit(iset,idet,itra,numsv,hitsh,ihit)
               if(ihit.eq.0) write(6,*) 'LAChits2: hit not stored'
            endif
C
         endif
C
      endif
C**********************************************************************
C
 999  continue
      return
      end

c---------------------------------------------------------------------------------

