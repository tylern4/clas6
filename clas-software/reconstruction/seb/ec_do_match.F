      SUBROUTINE EC_DO_MATCH(whit,nprt,ncc,nsc,nec,sect)
c
c_begin_doc
c  RCS ID string
c  $Id: ec_do_match.F,v 1.23 2006/06/15 21:38:02 stepanya Exp $
c
c  Documentation for subroutine EC_DO_MATCH
c
c  Purpose: For EC hit "whit" in the layer WHOLE looking for matching hits  
c           in the INNER and OUTER layers of EC and in CC and SC
c  --------
c
c  Input Parameters:  integer whit - Identifyer for EC hit in layer WHOLE
c  ----------------   integer nprt - counter for found "particles"
c                     integer ncc  - Identifyer of CC hits involved in PID
c                     integer nsc  - Identifyer of SC hits involved in PID
c                     integer nec  - Identifyer of EC hits involved in PID 
c                     integer sect - Sector number   
c
c  Output Parameters: Update values of: 
c  -----------------                   nprt
c                                      ncc
c                                      nsc
c                                      nec
c
c  Other routines:
c  ---------------
c
c  Notes: 
c  ------
c
c  Author:  S. Stepanyan      Created:  Wed Feb 26 14:18:30 EST 1997
c  -------
c
c  Major revisions:
c  ----------------
c     
c
c_end_doc
c
      IMPLICIT NONE
      SAVE
c
c_begin_inc
c  include files :
c  ---------------------
#include "clasmdl.inc"
#include "evnt_general.inc"
#include "evnt_par.inc"
#include "echits.inc" 
#include "particle_out.inc"
#include "dc_inf.inc"
#include "cc_inf.inc"
#include "sc_inf.inc"
#include "ec_inf.inc"
#include "pdgid.inc"
#include "schits.inc"
#include "cchits.inc"
#include "scgeom.inc"
#include "ecgmt.inc"
c_end_inc
c
c_begin_var
c  input/output variables:
c  -----------------------
      INTEGER whit,nprt,ncc,nsc,nec,sect
c
c  Local pre-defined variables:
c  ---------------------------
c  RCS information: 
      CHARACTER*132  CFILE, CREVIS, CSTATE, CDATE, CAUTHO, CRCSID
      PARAMETER (CFILE = '$RCSfile: ec_do_match.F,v $')
      PARAMETER (CREVIS = '$Revision: 1.23 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 2006/06/15 21:38:02 $')
      PARAMETER (CAUTHO = '$Author: stepanya $')
      DATA CRCSID/   
     1'$Id: ec_do_match.F,v 1.23 2006/06/15 21:38:02 stepanya Exp $'   
     2/   
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='EC_DO_MATCH')
      PARAMETER (CRAUTH='Stepan Stepanyan')
c
c  Local User defined variables:
c  -----------------------------
      INTEGER NWRITE
      LOGICAL El_OK, el_cut, ecccphis
      EXTERNAL el_cut
      REAL sc_err
      EXTERNAL sc_err
      REAL pos(6)
      REAL diff, xdiff, ydiff, zdiff
      REAL closest, closest0, ntime
      REAL xpln, ypln, zpln, sc_r, cc_r
      INTEGER hit, layer, ipart, hito
      INTEGER i_sc, i_cc, cchit, schit
      REAL thetrk, phitrk, sign_phi 
      INTEGER signphi, isignphi
      REAL thediff, phich, costh_n_trk
      DATA cc_r /390.0/
      REAL pi
      DATA pi /3.14159265/
c_end_var
c
c  executable code for routine EC_DO_MATCH:
c----6----------------------------------------------------------------72
c
      IF (NWRITE .LT. 1) THEN
        NWRITE = NWRITE + 1
        CRMESS='This routine doing EC hit based matching.'
        CALL RECMES(CRNAME,'I',CRMESS)
      ENDIF
c
c- Assuming that particles trajectory are straight line starting at the origin. 
c
      layer=whole
      hit=whit
      ecstatus(nec+1)=hit
      closest0 = 0.
      closest  = 0
      hito=0
      if(ec_matchid1(hit,layer,sect).gt.0)then
        layer=inner
        hit=ec_matchid1(whit,whole,sect)
        ecstatus(nec+1)= ecstatus(nec+1) + hit*100
        closest0=ec_c2match1(whit,whole,sect)
        closest =closest+1 
      endif
      if(ec_matchid2(hit,layer,sect).gt.0)then
        closest0=closest0+ec_c2match2(whit,whole,sect)
        closest =closest+1
        hito=ec_matchid2(hit,layer,sect) 
        ecstatus(nec+1)= ecstatus(nec+1) + hito*10000
      endif
      pos(1) = EcFit_hit(Ec_x,hit,layer,sect)-tvertx
      pos(2) = EcFit_hit(Ec_y,hit,layer,sect)-tverty
      pos(3) = EcFit_hit(Ec_z,hit,layer,sect)-tvertz
      pos(4) = pos(1)/sqrt(pos(1)**2+pos(2)**2+pos(3)**2)
      pos(5) = pos(2)/sqrt(pos(1)**2+pos(2)**2+pos(3)**2)
      pos(6) = pos(3)/sqrt(pos(1)**2+pos(2)**2+pos(3)**2)
      thetrk = acos ( pos(6) )
      phich = atan2 ( pos(2),pos(1) )
      if(phich.lt.0.)phich=2.*pi+phich
      phitrk=phich
      phitrk = phitrk-float(sect-1)*pi/3.
      if (sect.eq.1.and.phitrk.gt.11.*pi/6.)phitrk = phitrk-2.*pi
      sign_phi = sign (1.,phitrk)
      isignphi = int (sign_phi)
      ectime(nec+1) = 0.
      ect_in(nec+1) = 0.
      ect_out(nec+1) = 0.
      ntime = 0.
c
c- If there is a good hit in the EC, fill the ecst and look for SC&CC hits.
c
      If (layer.Eq.inner.Or.hito.Ne.0) Then  ! Should be hits  
c                                                   ! in inne or outer
        nec = nec + 1
        partstec(nprt+1) = nec
        partchi2ec(whole,nprt+1) = 0.
        partchi2ec(inner,nprt+1) = ec_c2match1(whit,whole,sect)
        partchi2ec(outer,nprt+1) = ec_c2match2(whit,whole,sect)
        partstatus(nprt+1) = -1
        ecpart(nec)=nprt+1
c- Filling ECST.
        ecsector(nec) = sect
        echit(whole,nec) = whit
        echit(inner,nec) = ec_matchid1(whit,whole,sect)
        echit(outer,nec) = ec_matchid2(whit,whole,sect)
        ece_tot(nec) = ecfit_hit(energy,whit,whole,sect) 
c- Trek lenght from the target and direction cosin ->
        ecr_trk(nec) = sqrt(pos(1)**2+pos(2)**2+pos(3)**2) 
        ecx(nec)=EcFit_hit(Ec_x,hit,layer,sect)
        ecy(nec)=EcFit_hit(Ec_y,hit,layer,sect)
        ecz(nec)=EcFit_hit(Ec_z,hit,layer,sect)
        costh_n_trk=pos(4)*n2sect(1,sect) ! Angle between the treck
     +       +pos(5)*n2sect(2,sect) ! and the normal to EC planes.
     +       +pos(6)*n2sect(3,sect) !
        if(layer.eq.inner.and.ecfit_hit(energy,hit,inner,sect).gt.0.) then
c- INNER energy 
           ece_in(nec) = ecfit_hit(energy,hit,inner,sect) 
           if(ecfit_hit(time,hit,inner,sect).gt.0.)then
              ect_in(nec)=ecfit_hit(time,hit,inner,sect)
              ectime(nec) = ectime(nec)
     +              +(ect_in(nec)
     -              -Ec_tdepth(inner)/costh_n_trk/vlight)*ece_in(nec)
c     -              -Ec_tdepth(inner)/costh_n_trk/vlight)*sqrt(ece_in(nec))
              ntime =  ntime+ece_in(nec)
c               ntime = ntime+sqrt(ece_in(nec))
            endif
            if ( hito.Ne.0.and.ecfit_hit(energy,hito,outer,sect).gt.0) then
c- OUTER energy 
               ece_out(nec) = ecfit_hit(energy,hito,outer,sect)     
c- Average time of matched  EC layers
               if(ecfit_hit(time,hito,outer,sect).gt.0.)then
                  ect_out(nec) = ecfit_hit(time,hito,outer,sect)
                  ectime(nec) = ectime(nec)
     +              +(ect_out(nec)
     -              -Ec_tdepth(outer)/costh_n_trk/vlight)*ece_out(nec)
c     -              -Ec_tdepth(outer)/costh_n_trk/vlight)*sqrt(ece_out(nec))
                  ntime = ntime+ece_out(nec)
c                  ntime = ntime+sqrt(ece_out(nec))
               endif
            else
               ece_out(nec) = 0.    
            endif
            if(ntime.gt.0.)then
               ectime(nec) = ectime(nec)/ntime
            else
               ectime(nec) = 0.
            endif 
         elseif(ecfit_hit(energy,hito,outer,sect).gt.0.)then
c     - OUTER energy 
            ece_out(nec) = ecfit_hit(energy,hito,outer,sect)     
            ece_in(nec) = 0.    
            if(ecfit_hit(time,hito,outer,sect).gt.0.)then           
               ect_out(nec) = ecfit_hit(time,hito,outer,sect)
c- Average time of matched  EC layers
               ectime(nec) = ect_out(nec)-EcBsc_d(outer)/costh_n_trk/vlight
            endif
         endif
         ecq(nec) = 0
         ecid(nec) = 0          ! Will be determined by later analyses. 
c     - See if found hit meets shower requirements. This may be not needed.
         El_OK = el_cut(ece_tot(nec)/sampl_frac,
     +           ece_tot(nec),
     +           ece_in(nec)-ece_out(nec),
     +           ec_peak(mom2,whit,Ec_U,whole,sect),
     +              ec_peak(mom2,whit,Ec_V,whole,sect),
     +              ec_peak(mom2,whit,Ec_W,whole,sect),
     +              ec_peak(mom3,whit,Ec_U,whole,sect),
     +              ec_peak(mom3,whit,Ec_V,whole,sect),
     +              ec_peak(mom3,whit,Ec_W,whole,sect))
         if ( El_OK ) ecid(nec) = photon
c  
c- End of EC internal matching.
c
c
c- Look for matching hits in CC
c
        IF ( nrclust.gt.0 ) THEN
        closest = eccc_match   ! Starts here. This is under tcl control.
        cchit = 0
        DO 20 hit=1,nrclust
          If( nrsect(hit).Eq.sect ) Then
            ipart = 1                                 !
            do while ( nprt.ne.0.and.ipart.le.nprt)   !
              i_cc = partstcc(ipart)                  ! Skip hits that 
              ipart = 1 + ipart                       ! were already found.
              if(i_cc.gt.0)then                       !
                if( ccsector(i_cc).eq.sect            !
     &         .and.ccihit(i_cc).eq.hit)    goto 20   !
              endif                                   !
            enddo
c            signphi = isign(1,nrphi(hit))
c            ecccphis = isignphi.eq.signphi
c            if (abs(phitrk).lt.0.035)ecccphis = .true.
            thediff =  ( thetrk - float(nrthet(hit))*pi/180. )
c     /               /  (float(nrdthet(hit))*pi/180. ))**2
            if ( abs(thediff).lt.closest) then
              closest = abs(thediff)
              cchit = hit
            endif
          EndIf
  20    CONTINUE
        If ( cchit.Ne.0 ) Then
c- Fills a CCST.
          ncc = ncc + 1
          partstcc(nprt+1) = ncc
          partchi2cc(nprt+1) = closest
          partstatus(nprt+1) = partstatus(nprt+1) - 1
          ccpart(ncc)=nprt+1
          ccihit(ncc) = cchit
          ccsector(ncc) = sect
          ccr_trk(ncc) = cc_r
          ccnphe(ncc) = nrphe(cchit)
          cctime(ncc) = nrtime(cchit)*0.05
          ccstatus(ncc) = nrsegm(cchit)+(1+nrphi(cchit))*1000
        EndIf
        ENDIF
c  
c- Look for matching hits in SC
c
        sc_r = Sc_rnormal(1,sect) - EcBsc_r - EcBsc_d(layer)
        xpln = pos(1) + sc_r*pos(4)            ! x coordinate
        ypln = pos(2) + sc_r*pos(5)            ! y coordinate 
        zpln = pos(3) + sc_r*pos(6)            ! z coordinate
        closest = ecsc_match      ! Starts here. This is under tcl control.
        schit = 0
        If ( ScFit_Nhits(sect).Gt.0 ) Then
        DO 30 hit = 1, ScFit_Nhits(sect)
          if(ScFit_paddle(sc_GeV,hit,sect).lt.1..or.ScFit_paddle(sc_TIME,hit,sect).lt.1.)GoTo30
            ipart = 1                                 !
            do while ( nprt.ne.0.and.ipart.le.nprt)   !
              i_sc = partstsc(ipart)                  ! Skip hits that 
              ipart = 1 + ipart                       ! were already found.
              if(i_sc.gt.0)then                       !
                if( scsector(i_sc).eq.sect            !
     &         .and.scihit(i_sc).eq.hit)    goto 30   !
              endif                                   !      
            enddo                                     !
          zdiff = ( zpln - ScFit_paddle(sc_z,hit,sect) )
          if ( abs(zdiff).lt.closest ) then
            closest = abs(zdiff)
            schit = hit 
          endif
  30    CONTINUE
        EndIf
c
        If ( schit.Ne.0 ) Then    ! 
          nsc = nsc + 1
          partstsc(nprt+1) = nsc
          partchi2sc(nprt+1) = closest
          partstatus(nprt+1) = partstatus(nprt+1) - 1
c- Fill the SC information structure.
          scpart(nsc)=nprt+1
          scihit(nsc) = schit
          scpdid(nsc) = scfit_hits(schit,sect)
          scsector(nsc) = sect
          scr_trk(nsc) = sqrt(xpln**2+ypln**2+zpln**2)
          sce_dep(nsc) = 
     =                   ScFit_paddle(sc_GeV,schit,sect)
          sctime(nsc)  = 
     =                   ScFit_paddle(sc_TIME,schit,sect)*0.998
          scstatus(nsc)  = 
     =                   scpd_stat(scfit_hits(schit,sect),sect)
        EndIf
      EndIf
      RETURN
      END
c
c----6----------------------------------------------------------------72
c




