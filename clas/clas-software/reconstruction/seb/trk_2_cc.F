      SUBROUTINE TRK_2_CC(itr,npart_cc,npart,sector)
c
c_begin_doc
c  RCS ID string
c  $Id: trk_2_cc.F,v 1.7 2008/06/12 15:05:32 devita Exp $
c
c  Documentation for subroutine TRK_2_CC
c
c  Purpose:
c  --------
c
c  Input Parameters:  itr      - integer - treck number
c  ----------------   npart_cc - integer - counter for CCST records
c                     npart    - integer - counter for particle record
c                     sector   - treck sector number  
c
c  Output Parameters: npart_cc - integer - In case of success +1
c  -----------------
c
c  Other routines:
c  ---------------
c
c  Notes:
c  ------
c
c  Author:  S. Stepanyan    Created:  Sun Feb 23 16:08:42 EST 1997
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
#include "evnt_general.inc"
#include "evnt_par.inc"
#include "echits.inc"
#include "particle_out.inc"
#include "hbt_planes.inc"
#include "cc_inf.inc"
#include "cchits.inc"
c_end_inc
c
c_begin_var
c  input/output variables:
c  -----------------------
      INTEGER itr,sector,npart_cc,npart
c
c  Local pre-defined variables:
c  ---------------------------
c  RCS information: 
      CHARACTER*132  CFILE, CREVIS, CSTATE, CDATE, CAUTHO, CRCSID
      PARAMETER (CFILE = '$RCSfile: trk_2_cc.F,v $')
      PARAMETER (CREVIS = '$Revision: 1.7 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 2008/06/12 15:05:32 $')
      PARAMETER (CAUTHO = '$Author: devita $')
      DATA CRCSID/   
     1'$Id: trk_2_cc.F,v 1.7 2008/06/12 15:05:32 devita Exp $'   
     2/   
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='TRK_2_CC')
      PARAMETER (CRAUTH='Stepan Stepanyan')
c
c  Local User defined variables:
c  -----------------------------
      INTEGER NWRITE
      REAL pi
      DATA pi /3.14159265/
      REAL thetrk, phitrk, sign_phi
      REAL pos(6)
      INTEGER signphi, isignphi, np, nprt
      INTEGER ihit, cchit
      REAL thediff, closest
      LOGICAL dcccphis
c_end_var
c
c  executable code for routine TRK_2_CC:
c----6----------------------------------------------------------------72
c
      IF (NWRITE .LT. 1) THEN
        NWRITE = NWRITE + 1
        CRMESS='This routine doing geometrical matching CC-track'
        CALL RECMES(CRNAME,'I',CRMESS)
      ENDIF
c
c- Move the coordinate system from "trackin/Bogd." to the CLAS generic.
      CALL back_2_clas(itr,cc_pln,sector,pos)
c- For the CC only polar angle has a meaning, azimuthal angle
c- is defined as a left or right from sector mead plane (+1,0,-1) 
      thetrk = acos ( pos(3)/sqrt(pos(1)**2 + pos(2)**2 + pos(3)**2) )
      phitrk = atan2 ( pos(2),pos(1) )
      if(phitrk.lt.0.)phitrk=2.*pi+phitrk
      phitrk=phitrk-(sector-1)*pi/3.
      if(sector.eq.1.and.phitrk.gt.11.*pi/6.)phitrk=phitrk-2.*pi
      sign_phi = sign (1.,phitrk)
      isignphi = int(sign_phi)
c
      closest = trkcc_match**2   ! Starts here. This is under tcl control.
      cchit = 0
      DO ihit=1,nrclust
c        signphi = isign(1,nrphi(ihit))
c        dcccphis=isignphi.eq.signphi
c        if(abs(phitrk).lt.0.05)dcccphis=.true.
        If( nrsect(ihit).Eq.sector ) Then
          thediff = ( thetrk - float(nrthet(ihit))*pi/180. )
c     /            /  (float(nrdthet(ihit))*pi/180. ))**2
          if ( abs(thediff).lt.closest) then
            closest = abs(thediff)
            cchit = ihit
            if(npart_cc.gt.0)then
              do np=1,npart_cc
                if(ccsector(np).eq.sector.and.ccihit(np).eq.cchit)then
                  nprt=ccpart(np)
c                  if(partchi2cc(nprt).gt.abs(thediff))then
c                    partchi2cc(nprt)=-100.
c                    partstcc(nprt)=0
c                  else
c                    cchit=0
c                  endif
                endif
              enddo
            endif
          endif
        EndIf
      ENDDO
      If ( cchit.Ne.0 ) Then
c- Fills a CCST.
        npart_cc = npart_cc + 1
        partstcc(npart+1) = npart_cc
        partchi2cc(npart+1) = closest
        ccpart(npart_cc)=npart+1
        ccihit(npart_cc) = cchit
        ccsector(npart_cc) = sector
        ccr_trk(npart_cc) = trkp(10,cc_pln,itr)
        ccnphe(npart_cc) = nrphe(cchit)
        cctime(npart_cc) = nrtime(cchit)*0.05
        ccstatus(npart_cc) = nrsegm(cchit)+(1+nrphi(cchit))*1000
      EndIf
c
      RETURN
      END
c
c------------------------------------------------------------------------------




