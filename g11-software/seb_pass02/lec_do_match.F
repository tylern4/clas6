      SUBROUTINE LEC_DO_MATCH(ihit,nprt,nsc,nlec,sect)
c
c_begin_doc
c  RCS ID string
c  $Id: lec_do_match.F,v 1.5 2004/06/28 23:36:52 fklein Exp $
c
c  Documentation for subroutine LEC_DO_MATCH
c
c  Purpose: LAC hits that are not matched with tracks matching here with SC.
c  --------
c
c  Input Parameters:  integer ihit - Identifyer for LAC hit 
c  ----------------   integer nprt - counter for found "particles"
c                     integer nsc  - Identifyer of SC hits involved in PID
c                     integer nlec - Identifyer of LAC hits involved in PID 
c                     integer sect - Sector number   
c
c  Output Parameters: Update values of: 
c  -----------------                   nprt
c                                      nsc
c                                      nlec
c
c  Other routines:
c  ---------------
c
c  Notes:
c  ------
c
c  Author:         Created:  Fri Mar  6 16:12:40 EST 1998
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
c BOS common block  uncomment the next line for BOS include file
c#include "bcs.inc"
#include "evnt_general.inc"
#include "evnt_par.inc"
#include "echits.inc" 
#include "lechits.inc" 
#include "particle_out.inc"
#include "sc_inf.inc"
#include "lec_inf.inc"
#include "pdgid.inc"
#include "schits.inc"
#include "scgeom.inc"
c_end_inc
c
c_begin_var
c  input/output variables:
c  -----------------------
      INTEGER ihit,nprt,nsc,nlec,sect
c
c  Local pre-defined variables:
c  ---------------------------
c  RCS information: 
      CHARACTER*132  CFILE, CREVIS, CSTATE, CDATE, CAUTHO, CRCSID
      PARAMETER (CFILE = '$RCSfile: lec_do_match.F,v $')
      PARAMETER (CREVIS = '$Revision: 1.5 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 2004/06/28 23:36:52 $')
      PARAMETER (CAUTHO = '$Author: fklein $')
      DATA CRCSID/   
     1'$Id: lec_do_match.F,v 1.5 2004/06/28 23:36:52 fklein Exp $'   
     2/   
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='LEC_DO_MATCH')
      PARAMETER (CRAUTH='Stepan')
c
c  Local User defined variables:
c  -----------------------------
      INTEGER NWRITE
      REAL pos(6)
      REAL xpln,ypln,zpln,closest,sc_r,zdiff
      INTEGER schit, hit,ipart,i_sc
c_end_var
c
c  executable code for routine LEC_DO_MATCH:
c----6----------------------------------------------------------------72
c
      IF (NWRITE .LT. 1) THEN
        NWRITE = NWRITE + 1
        CRMESS='Doing LAC hit matching with SC hit'
        CALL RECMES(CRNAME,'I',CRMESS)
      ENDIF
      pos(1) = LEc_hit(Ec_x,ihit,1,sect)
      pos(2) = LEc_hit(Ec_y,ihit,1,sect)
      pos(3) = LEc_hit(Ec_z,ihit,1,sect)
      lecr_trk(ihit)=sqrt(pos(1)**2+pos(2)**2+pos(3)**2) 
      if(lecr_trk(ihit).lt.1.) then
         pos(4)=0
         pos(5)=0
         pos(6)=0
      else
         pos(4) = pos(1)/lecr_trk(ihit)
         pos(5) = pos(2)/lecr_trk(ihit)
         pos(6) = pos(3)/lecr_trk(ihit)
       endif
        sc_r = Sc_rnormal(1,sect)
        xpln = pos(1) + sc_r*pos(4)            ! x coordinate
        ypln = pos(2) + sc_r*pos(5)            ! y coordinate 
        zpln = pos(3) + sc_r*pos(6)            ! z coordinate
        closest = lecsc_match      ! Starts here. This is under tcl control.
        schit = 0
        DO 30 hit = 1, ScFit_Nhits(sect)
          if(ScFit_paddle(sc_GeV,hit,sect).lt.1..or.ScFit_paddle(sc_TIME,hit,sect).lt.1.)GoTo30
            ipart = 1                                 !
            do while ( nprt.ne.0.and.ipart.le.nprt)   !
              i_sc = partstec(ipart)                  ! Skip hits that 
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
     =                   ScFit_paddle(sc_TIME,schit,sect)
c          scstatus(nsc)  = 
c     =                   ScFit_HitPsst(schit,sect)
        EndIf
      RETURN
      END
c
c------------------------------------------------------------------------------




