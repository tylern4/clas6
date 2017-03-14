      SUBROUTINE TRK_2_EC(itr,npart_ec,npart,sector)
c
c_begin_doc
c  RCS ID string
c  $Id: trk_2_ec.F,v 1.24 2006/06/15 21:06:44 stepanya Exp $
c
c  Documentation for subroutine TRK_2_EC
c
c  Purpose: Finds corresponding hits in EC for given treck.
c  --------
c
c  Input Parameters:  itr      - integer - treck number
c  ----------------   npart_ec - integer - counter for ECST records
c                     npart    - integer - counter for particle record
c                     sector   - treck sector number  
c
c  Output Parameters: npart_ec - integer - In case of success +1
c  -----------------
c
c  Other routines:
c  ---------------
c
c  Notes:
c  ------
c
c  Author:    S. Stepanyan     Created:  Sun Feb 23 16:07:57 EST 1997
c  -------
c
c  Major revisions:
c  ----------------
c  W. Brooks, 27 Jan 1998 - redefine EC times of matched hit     
c
c_end_doc
c
      IMPLICIT NONE
      SAVE
c
c_begin_inc
c  include files :
c  ---------------------
#include "bcs.inc"
#include "evnt_general.inc"
#include "evnt_par.inc"
#include "echits.inc"
#include "particle_out.inc"
#include "ec_inf.inc"
#include "ecgmt.inc"
#include "hbt_planes.inc"
#include "pdgid.inc"
#include "photon_stuff.inc"
c_end_inc
c
c_begin_var
c  input/output variables:
c  -----------------------
      INTEGER itr,sector,npart_ec,npart
c
c  Local pre-defined variables:
c  ---------------------------
c  RCS information: 
      CHARACTER*132  CFILE, CREVIS, CSTATE, CDATE, CAUTHO, CRCSID
      PARAMETER (CFILE = '$RCSfile: trk_2_ec.F,v $')
      PARAMETER (CREVIS = '$Revision: 1.24 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 2006/06/15 21:06:44 $')
      PARAMETER (CAUTHO = '$Author: stepanya $')
      DATA CRCSID/   
     1'$Id: trk_2_ec.F,v 1.24 2006/06/15 21:06:44 stepanya Exp $'   
     2/   
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='TRK_2_EC')
      PARAMETER (CRAUTH='Stepan Stepanyan')
c
c  Local User defined variables:
c  -----------------------------
      INTEGER NWRITE
      REAL diff, xdiff, ydiff, zdiff, closest(whole:outer), closest0
      REAL xpln, ypln, zpln, costh_n_trk, ntime
      REAL ec_cx(whole:outer),ec_cy(whole:outer),ec_cz(whole:outer)
      REAL depth(whole:outer)
      INTEGER ihit, layer, np, nprt
      INTEGER idh(whole:outer)
      LOGICAL El_OK, el_cut
      EXTERNAL el_cut
      REAL pos(6)
c_end_var
c
c  executable code for routine TRK_2_EC:
c----6----------------------------------------------------------------72
c
      IF (NWRITE .LT. 1) THEN
        NWRITE = NWRITE + 1
        CRMESS='This routine doing geometrical matching EC-treck'
        CALL RECMES(CRNAME,'I',CRMESS)
      ENDIF
c - Check if track cross the EC plane
      if(trkp(10,fec_pln,itr).ge.1000..or.trkp(4,fec_pln,itr).le.0)Return
c- Needs to transfer coordinates at treck intersection point with
c- EC plane from "tracking/Bogd." coordinate system to CLAS generic.  
      CALL back_2_clas(itr,fec_pln,sector,pos)
c- x,y and z are the coordinates on the face of first scintillator layer.
c- That not necessary to be the reconstruction plane (planes are 3).
c
      costh_n_trk=pos(4)*n2sect(1,sector)   ! Angle between the treck
     +           +pos(5)*n2sect(2,sector)   ! and the normal to EC planes.
     +           +pos(6)*n2sect(3,sector)   !
c
      closest0 = 0.
      Do layer = whole,outer
        idh(layer) = 0
        closest(layer) = trkec_match   ! Starts here. This is under tcl control.
c        depth(layer) = EcBsc_d(layer)/costh_n_trk
        depth(layer) = Ec_tDepth(layer)/costh_n_trk
        If ( ecfit_nhit(layer,sector).GT.0 )Then   ! For layer=whole this is 
c-                                                 ! not a problem        
c
          xpln = pos(1) + EcBsc_d(layer)*pos(4)/costh_n_trk ! intersection x coordinate
          ypln = pos(2) + EcBsc_d(layer)*pos(5)/costh_n_trk ! intersection y coordinate 
          zpln = pos(3) + EcBsc_d(layer)*pos(6)/costh_n_trk ! intersection z coordinate
c
          DO ihit = 1,ecfit_nhit(layer,sector)
            xdiff = ( xpln - EcFit_hit(Ec_x,ihit,layer,sector) )
            ydiff = ( ypln - EcFit_hit(Ec_y,ihit,layer,sector) )
            zdiff = ( zpln - EcFit_hit(Ec_z,ihit,layer,sector) )
            if ( abs(zdiff).lt.closest(layer) ) then
              closest(layer) = abs(zdiff)
              idh(layer) = ihit 
            endif
          ENDDO
        EndIf
          if(idh(layer).ne.0)then
            ec_cx(layer)  
     ==EcFit_hit(Ec_x,idh(layer),layer,sector)
            ec_cy(layer)  
     ==EcFit_hit(Ec_y,idh(layer),layer,sector)
            ec_cz(layer)
     ==EcFit_hit(Ec_z,idh(layer),layer,sector)
            closest0 = closest0 + closest(layer)
          endif
      EndDo
c
      If ( idh(whole).Ne.0             ! Should be hits in EC as a whole
     &.And.(idh(inner).Ne.0.Or.idh(outer).Ne.0)) Then ! and inner or outer
        npart_ec = npart_ec + 1
        partstec(npart+1) = npart_ec
        ecpart(npart_ec) = npart+1
        ecsector(npart_ec) = sector
        ecstatus(npart_ec) = 0
        Do layer = whole,outer
          echit(layer,npart_ec)=idh(layer)
          partchi2ec(layer,npart+1)= closest(layer)
          ecstatus(npart_ec) = ecstatus(npart_ec) + idh(layer)*100**(layer-1)
        EndDo
        ecr_trk(npart_ec) = trkp(10,fec_pln,itr)+depth(inner)
        ecq(npart_ec) = trkp(9,1,itr)
        ece_tot(npart_ec) = 
     =  ecfit_hit(energy,idh(whole),whole,sector) 
        ectime(npart_ec) = 0.
        ect_in(npart_ec) = 0.
        ect_out(npart_ec) = 0.
        ntime = 0.
        if(idh(inner).ne.0.and.ecfit_hit(energy,idh(inner),inner,sector).gt.0.) then
c- According to the GSIM simualtion EC INNER gives the best position resolution.
           ecx(npart_ec)=ec_cx(inner)
           ecy(npart_ec)=ec_cy(inner)
           ecz(npart_ec)=ec_cz(inner)
           ece_in(npart_ec) = ecfit_hit(energy,idh(inner),inner,sector) 
           if(ecfit_hit(time,idh(inner),inner,sector).gt.0.)then
              ect_in(npart_ec)=ecfit_hit(time,idh(inner),inner,sector)
c     - Average time of EC layers
              ectime(npart_ec) = ectime(npart_ec)
     +             +(ect_in(npart_ec)
     -             -depth(inner)/vlight)*sqrt(ece_in(npart_ec))
              ntime = ntime+sqrt(ece_in(npart_ec))
           endif
           if ( idh(outer).Ne.0.and.ecfit_hit(energy,idh(outer),outer,sector).gt.0.) then
              ece_out(npart_ec) = ecfit_hit(energy,idh(outer),outer,sector)     
c
              if(ecfit_hit(time,idh(outer),outer,sector).gt.0.)then
                 ect_out(npart_ec) = ecfit_hit(time,idh(outer),outer,sector)
                 ectime(npart_ec) = ectime(npart_ec)
     +                +(ect_out(npart_ec)
     -                -depth(outer)/vlight)*sqrt(ece_out(npart_ec))
                 ntime = ntime+sqrt(ece_out(npart_ec))
              endif
           else
              ece_out(npart_ec) = 0.     
           endif
           if(ntime.gt.0.)then
              ectime(npart_ec) = ectime(npart_ec)/ntime
           else
              ectime(npart_ec) = 0.
           endif 
c
        else
c- If there is no INNER hit is matched -> normaly this should not happen, but??
          ecx(npart_ec)=ec_cx(whole)
          ecy(npart_ec)=ec_cy(whole)
          ecz(npart_ec)=ec_cz(whole)
          ece_out(npart_ec) = ecfit_hit(energy,idh(outer),outer,sector)     
          ece_in(npart_ec) = 0.     
c- Average time of matched  EC layers
          if(ecfit_hit(time,idh(outer),outer,sector).gt.0.)then
             ect_out(npart_ec) = ecfit_hit(time,idh(outer),outer,sector)
             ectime(npart_ec) =ect_out(npart_ec)-depth(outer)/vlight
          endif
       endif
       ecid(npart_ec) = 0
c- If track is negative, then look for electron (shower).
c       if ( .NOT.YES_PHOTON ) then 
          El_OK = el_cut(trkp(7,1,itr),
     +              ece_tot(npart_ec),
     +              ece_in(npart_ec)-ece_out(npart_ec),
     +              ec_peak(mom2,idh(whole),Ec_U,whole,sector),
     +              ec_peak(mom2,idh(whole),Ec_V,whole,sector),
     +              ec_peak(mom2,idh(whole),Ec_W,whole,sector),
     +              ec_peak(mom3,idh(whole),Ec_U,whole,sector),
     +              ec_peak(mom3,idh(whole),Ec_V,whole,sector),
     +              ec_peak(mom3,idh(whole),Ec_W,whole,sector))
          if ( El_OK ) then
            ecid(npart_ec) = -electron*ecq(npart_ec)
          endif
c        endif
      EndIf
      RETURN
      END
c
c------------------------------------------------------------------------------




