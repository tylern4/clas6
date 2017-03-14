      SUBROUTINE HIT_MATCH ( sect )
c
c     _begin_doc
c     RCS ID string
c     $Id: hit_match.F,v 1.7 2008/03/16 04:33:16 fklein Exp $
c     
c     Documentation for subroutine HIT_MATCH
c     
c     Purpose:
c     --------
c     
c     Input Parameters:  sect - integer - Sector number
c     ----------------
c     
c     Output Parameters:  (Name - Type - Meaning)
c     -----------------
c     
c     Other routines:
c     ---------------
c     
c     Notes:
c     ------
c     
c     Author:   S. Stepanyan      Created:  Mon Apr 21 22:33:30 EDT 1997
c     -------
c     
c     Major revisions:
c     ----------------
c     
c     
c     _end_doc
c     
      IMPLICIT NONE
      SAVE
c     
c     _begin_inc
c     include files :
c     ---------------------
c     BOS common block  uncomment the next line for BOS include file
c     #include "bcs.inc"
#include "Ec_general.PAR"
#include "EcFit_general.CMN"
#include "EcCal.CMN"
#include "ec_control.inc"
#include "ecgeom.inc"
#include "ecgeompar.inc"
c     _end_inc
c     
c     _begin_var
c     input/output variables:
c     -----------------------
      INTEGER sect
c     
c     Local pre-defined variables:
c     ---------------------------
c     RCS information: 
      CHARACTER*132  CFILE, CREVIS, CSTATE, CDATE, CAUTHO, CRCSID
      PARAMETER (CFILE = '$RCSfile: hit_match.F,v $')
      PARAMETER (CREVIS = '$Revision: 1.7 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 2008/03/16 04:33:16 $')
      PARAMETER (CAUTHO = '$Author: fklein $')
      DATA CRCSID/   
     1     '$Id: hit_match.F,v 1.7 2008/03/16 04:33:16 fklein Exp $'   
     2     /   
c     Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='HIT_MATCH')
      PARAMETER (CRAUTH='Stepan Stepanyan')
c     
c     Local User defined variables:
c     -----------------------------
      INTEGER NWRITE, il, jl, ih, wht
      INTEGER mlayer, mhit, layer, hit
      REAL pos(6), idiff, jdiff, ci, cj, closer,thick(MAXhits,whole:cover,MAXsectors)
      INTEGER idh, imat
      REAL diff, closest,costh_n_mhit,imhit,jmhit,radm,radp
c     _end_var
c     
c     executable code for routine HIT_MATCH:
c---- 6----------------------------------------------------------------72
c     
      IF (NWRITE .LT. 1) THEN
         NWRITE = NWRITE + 1
         CRMESS='This routine doing primary matching of EC hits'
         CALL RECMES(CRNAME,'I',CRMESS)
      ENDIF
c
c- Assuming that particles trajectory are straight line starting at the origin. 
c
c- First project all hits to the front face of EC in the (i,j) coord.system. 
c
      DO il=whole,cover
        IF ( ecfit_nhit(il,sect).GT.0 ) THEN
          Do ih=1,ecfit_nhit(il,sect)
            pos(1) = EcFit_hit(Ec_x,ih,il,sect)
            pos(2) = EcFit_hit(Ec_y,ih,il,sect)
            pos(3) = EcFit_hit(Ec_z,ih,il,sect)
            pos(4) = pos(1)/sqrt(pos(1)**2+pos(2)**2+pos(3)**2)
            pos(5) = pos(2)/sqrt(pos(1)**2+pos(2)**2+pos(3)**2)
            pos(6) = pos(3)/sqrt(pos(1)**2+pos(2)**2+pos(3)**2)
            costh_n_mhit=pos(4)*n2sect(1,sect)   ! Angle between the master hit
     +               +pos(5)*n2sect(2,sect)      ! and the normal to EC planes.
     +               +pos(6)*n2sect(3,sect)      !
            imhit=EcFit_hit(Ec_i,ih,il,sect)
            jmhit=EcFit_hit(Ec_j,ih,il,sect)
            radm=sqrt(imhit**2+jmhit**2)
cfk  check for imhit=0 and jmhit=0
            IF( radm.LT.1.e-8 ) THEN
               ci=0
               cj=0
            ELSE
               ci=imhit/radm
               cj=jmhit/radm
            ENDIF
            radp=EcBsc_d(il)*tan(acos(costh_n_mhit))
c- Save the coordinates on the face of EC for later use and for CED display.      
            ifec(ih,il,sect) = imhit - radp*ci ! I coordinate   
            jfec(ih,il,sect) = jmhit - radp*cj ! J coordinate 
c- Scale the errors on coordinates I and J. 
            difec(ih,il,sect) = EcFit_hit(Ec_di,ih,il,sect)
     *                        * EcBsc_r/(EcBsc_r+EcBsc_d(il))   
            djfec(ih,il,sect) = EcFit_hit(Ec_dj,ih,il,sect)
     *                        * EcBsc_r/(EcBsc_r+EcBsc_d(il))  
            thick(ih,il,sect) = EcBsc_d(il)/costh_n_mhit
          EndDo
        ENDIF
      ENDDO
c - Initialize counters and areas for matching.
      DO il=whole,cover
         DO jl=whole,cover
            nmatch(il,jl,sect)=0
            DO ih=1,Maxhits
               matched(il,jl,ih,sect)=0
            ENDDO
         ENDDO
      ENDDO
c     
c     - Start matching hits in differen layers.
c     - Assumption is that every inner (or outer) hits should asociate with 
c     - whole hit, but not necesary with outer (or inner). First match inners
c     - with whole.
c     
      mlayer=inner
      layer =whole
      IF ( ecfit_nhit(mlayer,sect).GT.0.and.ecfit_nhit(layer,sect).GT.0) THEN
         Do 20 mhit=1,ecfit_nhit(mlayer,sect)
            if(matched(mlayer,layer,mhit,sect).ne.0)go to 20
c     - 
            closest=ec_match
            closer=closest
            idh = 0
c     -
            Do hit = 1,ecfit_nhit(layer,sect)
               if(matched(layer,mlayer,hit,sect).ne.0) 
     &              closest = amin1(closest,c2match(layer,mlayer,hit,sect)) ! Skip the hit that
               idiff = ( ifec(mhit,mlayer,sect) - ifec(hit,layer,sect) )**2
     /              / ( difec(hit,layer,sect)**2 + difec(mhit,mlayer,sect)**2)
               jdiff = ( jfec(mhit,mlayer,sect) - jfec(hit,layer,sect) )**2
     /              / ( djfec(hit,layer,sect)**2 + djfec(mhit,mlayer,sect)**2)
               diff  = ( idiff + jdiff ) 
               if ( diff.lt.closest ) then
                  closest = diff
                  closer=closest
                  idh = hit
               else
                  closest=closer
               endif
            EndDo
            if(idh.ne.0)then
               if(matched(layer,mlayer,idh,sect).ne.0)then
                  imat=matched(layer,mlayer,idh,sect)
                  idiff = ( ifec(mhit,mlayer,sect) - ifec(imat,mlayer,sect) )**2 
     /                 / ( difec(imat,mlayer,sect)**2 + difec(mhit,mlayer,sect)**2)
                  jdiff = ( jfec(mhit,mlayer,sect) - jfec(imat,mlayer,sect) )**2 
     /                 / ( djfec(imat,mlayer,sect)**2 + djfec(mhit,mlayer,sect)**2)
                  diff  = ( idiff + jdiff )
                  if(diff.lt.closest)then
                     nmatch(mlayer,layer,sect)=nmatch(mlayer,layer,sect)-1
                     matched(mlayer,layer,imat,sect)=0
                  endif
               endif 
               nmatch(mlayer,layer,sect)=nmatch(mlayer,layer,sect)+1
               matched(mlayer,layer,mhit,sect)=idh
               c2match(mlayer,layer,mhit,sect)=closest
               matched(layer,mlayer,idh,sect)=mhit
               c2match(layer,mlayer,idh,sect)=closest
               EcFit_hit(TIME,idh,layer,sect)=EcFit_hit(TIME,mhit,mlayer,sect)
     &              -thick(mhit,mlayer,sect)/speed_of_light 
            endif
 20      Continue
      ENDIF
c     
c     -
c     
      layer = outer
      IF ( ecfit_nhit(mlayer,sect).GT.0.AND.ecfit_nhit(layer,sect).GT.0) THEN
         Do 30 mhit=1,ecfit_nhit(mlayer,sect)
            if(matched(mlayer,layer,mhit,sect).ne.0)go to 30
c     - 
            closest=20.
            closer=closest
            idh = 0
c     
            Do hit = 1,ecfit_nhit(layer,sect)
               if(matched(layer,mlayer,hit,sect).ne.0) 
     &              closest = amin1(closest,c2match(layer,mlayer,hit,sect)) ! Skip the hit that
               idiff = ( ifec(mhit,mlayer,sect) - ifec(hit,layer,sect) )**2
     /              / ( difec(hit,layer,sect)**2 + difec(mhit,mlayer,sect)**2)
               jdiff = ( jfec(mhit,mlayer,sect) - jfec(hit,layer,sect) )**2
     /              / ( djfec(hit,layer,sect)**2 + djfec(mhit,mlayer,sect)**2)
               diff  = ( idiff + jdiff ) 
               if ( diff.lt.closest ) then
                  closest = diff
                  closer=closest
                  idh = hit
               else
                  closest=closer
               endif
            EndDo
            if(idh.ne.0)then
               if(matched(layer,mlayer,idh,sect).ne.0)then
                  imat=matched(layer,mlayer,idh,sect)
                  idiff = ( ifec(mhit,mlayer,sect) - ifec(imat,mlayer,sect) )**2
     /                 / ( difec(imat,mlayer,sect)**2 + difec(mhit,mlayer,sect)**2)
                  jdiff = ( jfec(mhit,mlayer,sect) - jfec(imat,mlayer,sect) )**2
     /                 / ( djfec(imat,mlayer,sect)**2 + djfec(mhit,mlayer,sect)**2)
                  diff  = ( idiff + jdiff ) 
                  if(diff.ge.closest)then
                  else
                     nmatch(mlayer,layer,sect)=nmatch(mlayer,layer,sect)-1
                     matched(mlayer,layer,imat,sect)=0
                     if(matched(layer,whole,idh,sect).ne.0)then
                        nmatch(layer,whole,sect)=nmatch(layer,whole,sect)-1
                        wht=matched(layer,whole,idh,sect)
                        matched(layer,whole,idh,sect)=0
                        matched(whole,layer,wht,sect)=0
                     endif
                  endif
               endif 
               nmatch(mlayer,layer,sect)=nmatch(mlayer,layer,sect)+1
               matched(mlayer,layer,mhit,sect)=idh
               c2match(mlayer,layer,mhit,sect)=closest
               matched(layer,mlayer,idh,sect)=mhit
               c2match(layer,mlayer,idh,sect)=closest
               if(matched(mlayer,whole,mhit,sect).ne.0)then
                  nmatch(layer,whole,sect)=nmatch(layer,whole,sect)+1
                  wht=matched(mlayer,whole,mhit,sect)
                  matched(layer,whole,idh,sect)=wht
                  matched(whole,layer,wht,sect)=idh
                  idiff = (ifec(wht,whole,sect) - ifec(idh,layer,sect) )**2
     /                 / (difec(wht,whole,sect)**2 + difec(idh,layer,sect)**2)
                  jdiff = (jfec(wht,whole,sect) - jfec(idh,layer,sect) )**2
     /                 / (djfec(idh,layer,sect)**2 + djfec(wht,whole,sect)**2)
                  diff  = ( idiff + jdiff ) 
                  c2match(layer,whole,idh,sect)=diff
                  c2match(whole,layer,wht,sect)=diff
                  EcFit_hit(TIME,wht,whole,sect)=(EcFit_hit(TIME,mhit,mlayer,sect)
     &                 -thick(mhit,mlayer,sect)/speed_of_light)
     &                 *EcFit_hit(energy,mhit,mlayer,sect)
     &                 +(EcFit_hit(TIME,idh,layer,sect)
     &                 -thick(idh,layer,sect)/speed_of_light)
     &                 *EcFit_hit(energy,idh,layer,sect)
                  EcFit_hit(TIME,wht,whole,sect)=EcFit_hit(TIME,wht,whole,sect)
     &                 /(EcFit_hit(energy,mhit,mlayer,sect)+EcFit_hit(energy,idh,layer,sect))
               endif
            endif
 30      Continue
      ENDIF
c     
c   These for the preshower detector, layer=cover. Matching will be done only with inner
c
      layer = cover
      IF ( ecfit_nhit(mlayer,sect).GT.0.AND.ecfit_nhit(layer,sect).GT.0) THEN
         Do 70 mhit=1,ecfit_nhit(mlayer,sect)
            if(matched(mlayer,layer,mhit,sect).ne.0)go to 30
c     - 
            closest=20.
            closer=closest
            idh = 0
c     
            Do hit = 1,ecfit_nhit(layer,sect)
               if(matched(layer,mlayer,hit,sect).ne.0) 
     &              closest = amin1(closest,c2match(layer,mlayer,hit,sect)) ! Skip the hit that
               idiff = ( ifec(mhit,mlayer,sect) - ifec(hit,layer,sect) )**2
     /              / ( difec(hit,layer,sect)**2 + difec(mhit,mlayer,sect)**2)
               jdiff = ( jfec(mhit,mlayer,sect) - jfec(hit,layer,sect) )**2
     /              / ( djfec(hit,layer,sect)**2 + djfec(mhit,mlayer,sect)**2)
               diff  = ( idiff + jdiff ) 
               if ( diff.lt.closest ) then
                  closest = diff
                  closer=closest
                  idh = hit
               else
                  closest=closer
               endif
            EndDo
            if(idh.ne.0)then
               if(matched(layer,mlayer,idh,sect).ne.0)then
                  imat=matched(layer,mlayer,idh,sect)
                  nmatch(mlayer,layer,sect)=nmatch(mlayer,layer,sect)-1
                  matched(mlayer,layer,imat,sect)=0
                  if(matched(layer,whole,idh,sect).ne.0)then
                     nmatch(layer,whole,sect)=nmatch(layer,whole,sect)-1
                     wht=matched(layer,whole,idh,sect)
                     matched(layer,whole,idh,sect)=0
                     matched(whole,layer,wht,sect)=0
                  endif
               endif 
               nmatch(mlayer,layer,sect)=nmatch(mlayer,layer,sect)+1
               matched(mlayer,layer,mhit,sect)=idh
               c2match(mlayer,layer,mhit,sect)=closest
               matched(layer,mlayer,idh,sect)=mhit
               c2match(layer,mlayer,idh,sect)=closest
               if(matched(mlayer,whole,mhit,sect).ne.0)then
                  nmatch(layer,whole,sect)=nmatch(layer,whole,sect)+1
                  wht=matched(mlayer,whole,mhit,sect)
                  matched(layer,whole,idh,sect)=wht
                  matched(whole,layer,wht,sect)=idh
                  idiff = (ifec(wht,whole,sect) - ifec(idh,layer,sect) )**2
     /                 / (difec(wht,whole,sect)**2 + difec(idh,layer,sect)**2)
                  jdiff = (jfec(wht,whole,sect) - jfec(idh,layer,sect) )**2
     /                 / (djfec(idh,layer,sect)**2 + djfec(wht,whole,sect)**2)
                  diff  = ( idiff + jdiff ) 
                  c2match(layer,whole,idh,sect)=diff
                  c2match(whole,layer,wht,sect)=diff
                  EcFit_hit(TIME,wht,whole,sect)=(EcFit_hit(TIME,mhit,mlayer,sect)
     &                 -thick(mhit,mlayer,sect)/speed_of_light)
     &                 *EcFit_hit(energy,mhit,mlayer,sect)
     &                 +(EcFit_hit(TIME,idh,layer,sect)
     &                 -thick(idh,layer,sect)/speed_of_light)
     &                 *EcFit_hit(energy,idh,layer,sect)
                  EcFit_hit(TIME,wht,whole,sect)=EcFit_hit(TIME,wht,whole,sect)
     &                 /(EcFit_hit(energy,mhit,mlayer,sect)+EcFit_hit(energy,idh,layer,sect))
               endif
            endif
 70      Continue
      ENDIF
c     
c End of the preshower part
c     
      mlayer=outer
      layer =whole
      IF ( ecfit_nhit(mlayer,sect).GT.0.AND.ecfit_nhit(layer,sect).GT.0) THEN
         Do 40 mhit=1,ecfit_nhit(mlayer,sect)
            If(matched(mlayer,layer,mhit,sect).ne.0) Go To 40
c     - 
            closest=20.
            closer=closest
            idh = 0
c     
            Do 50 hit = 1,ecfit_nhit(layer,sect)
               if(matched(layer,mlayer,hit,sect).ne.0) go to 50
c     
               idiff = ( ifec(mhit,mlayer,sect) - ifec(hit,layer,sect) )**2
     /              / ( difec(hit,layer,sect)**2 + difec(mhit,mlayer,sect)**2)
               jdiff = ( jfec(mhit,mlayer,sect) - jfec(hit,layer,sect) )**2
     /              / ( djfec(hit,layer,sect)**2 + djfec(mhit,mlayer,sect)**2)
               diff  = ( idiff + jdiff ) 
               if ( diff.lt.closest ) then
                  closest = diff
                  closer=closest
                  idh = hit
               else
                  closest=closer
               endif
 50         Continue
            if(idh.ne.0)then
               if(matched(layer,mlayer,idh,sect).ne.0)then
                  imat=matched(layer,mlayer,idh,sect)
                  nmatch(mlayer,layer,sect)=nmatch(mlayer,layer,sect)-1
                  matched(mlayer,layer,imat,sect)=0
               endif 
               nmatch(mlayer,layer,sect)=nmatch(mlayer,layer,sect)+1
               matched(mlayer,layer,mhit,sect)=idh
               c2match(mlayer,layer,mhit,sect)=closest
               matched(layer,mlayer,idh,sect)=mhit
               c2match(layer,mlayer,idh,sect)=closest
               EcFit_hit(TIME,idh,layer,sect)=EcFit_hit(TIME,mhit,mlayer,sect)
     &              -thick(mhit,mlayer,sect)/speed_of_light 
            endif
 40      Continue
      ENDIF
c     
c     -
c     
      IF (nmatch(inner,whole,sect).gt.ecfit_nhit(inner,sect).OR.
     o     nmatch(inner,whole,sect).gt.ecfit_nhit(whole,sect)) THEN
         CRMESS='More INNER - WHOLE matching than hits.'
         CALL RECMES(CRNAME,'I',CRMESS)
      ENDIF
      IF (nmatch(inner,outer,sect).gt.ecfit_nhit(inner,sect).OR.
     o     nmatch(inner,outer,sect).gt.ecfit_nhit(outer,sect)) THEN
         CRMESS='More INNER - OUTER matching than hits.'
         CALL RECMES(CRNAME,'I',CRMESS)
      ENDIF
      IF (nmatch(outer,whole,sect).gt.ecfit_nhit(outer,sect).OR.
     o     nmatch(outer,whole,sect).gt.ecfit_nhit(whole,sect)) THEN
         CRMESS='More OUTER - WHOLE matching than hits.'
         CALL RECMES(CRNAME,'I',CRMESS)
      ENDIF
c     
c     - End of EC internal matching.
c     
      RETURN
      END
c
c------------------------------------------------------------------------------
