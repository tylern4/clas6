      SUBROUTINE TRK_2_SC(itr,npart_sc,npart,sector)
c
c_begin_doc
c  RCS ID string
c  $Id: trk_2_sc.F,v 1.23 2008/09/10 18:05:21 turonski Exp $
c
c  Documentation for subroutine TRK_2_SC
c
c  Purpose:  Finding a hit that matches with given track.
c  --------  In case of success fills structure recorded as a SCST.
c
c  Input Parameters:  itr      - integer - treck number
c  ----------------   npart_sc - integer - counter for SCST records
c                     npart    - integer - counter for particle record
c                     sector   - treck sector number  
c
c  Output Parameters: npart_sc - integer - In case of success +1
c  -----------------
c
c  Other routines:    Uses sc_err function to get SC hit position errors.
c  ---------------
c
c  Notes:
c  ------
c
c  Author:   S. Stepanyan    Created:  Sun Feb 23 16:08:54 EST 1997
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
#include "hbt_planes.inc"
#include "particle_out.inc"
#include "sc_inf.inc"
#include "schits.inc"
c_end_inc
c
c_begin_var
c  input/output variables:
c  -----------------------
      INTEGER itr,sector,npart_sc,npart
c
c  Local pre-defined variables:
c  ---------------------------
c  RCS information: 
      CHARACTER*132  CFILE, CREVIS, CSTATE, CDATE, CAUTHO, CRCSID
      PARAMETER (CFILE = '$RCSfile: trk_2_sc.F,v $')
      PARAMETER (CREVIS = '$Revision: 1.23 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 2008/09/10 18:05:21 $')
      PARAMETER (CAUTHO = '$Author: turonski $')
      DATA CRCSID/   
     1'$Id: trk_2_sc.F,v 1.23 2008/09/10 18:05:21 turonski Exp $'   
     2/   
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='TRK_2_SC')
      PARAMETER (CRAUTH=' ')
c
c  Local User defined variables:
c  -----------------------------
      INTEGER NWRITE, sclastpd(4:8)
      DATA NWRITE /0/
      DATA sclastpd/0,23,34,42,48/  ! new: /0,23,34,45,57/
      INTEGER ihit, schit, nps, nprt, ipl, sc_pl,status,pddid
      INTEGER i,irun,ievnt,irec
      INTEGER get_sc_version_flag,sc_version
      REAL diff, zdiff, closest, shortest,energ
      REAL sc_err
      EXTERNAL sc_err
      REAL pos(6)

c_end_var
c
c  executable code for routine TRK_2_SC:
c----6----------------------------------------------------------------72
c
  
      IF (NWRITE .LT. 1) THEN
        NWRITE = NWRITE + 1
        CRMESS='This routine matches hits in SC with given treck.'
        CALL RECMES(CRNAME,'I',CRMESS)

        call rernev(irun,ievnt,irec)
        sc_version=get_sc_version_flag(irun)
        write(crmess,'(A,I2,A,I6)')'sc_version_flag=',sc_version,
     &          ' for run',irun
        call recmes(crname,'I',crmess)
        if(sc_version.eq.2) then
           sclastpd(7)=45
           sclastpd(8)=57
        endif
      ENDIF

c- Needs to transform coordinates of treck intersection point with
c- SC plane from "tracking/Bogd." coordinate system to CLAS generic.
      energ=0
      schit = 0 
      shortest = 1000.
      closest = trksc_match  ! Starts here. This is under tcl control.
      sc_pl=0 
      DO ipl=sc_pln,sc_pln+3
        IF(trkp(2,ipl,itr).lt.1000..and.trkp(10,ipl,itr).lt.shortest) then
        shortest = trkp(10,ipl,itr)
        sc_pl=ipl
        ENDIF
      ENDDO  
      IF(sc_pl.gt.0.and.shortest.lt.999.)THEN
        CALL back_2_clas(itr,sc_pl,sector,pos)
c
c- Loop over SC hits in the sector and take the closest 
c- hit to the intersection point. 
c-
      DO ihit = 1, ScFit_Nhits(sector)
        if( ScFit_paddle(sc_GeV,ihit,sector).gt.1. )then
        zdiff = ( pos(3) - ScFit_paddle(sc_z,ihit,sector) )
        diff = zdiff
          if ( abs(diff).lt.closest.or.(abs(pddid-scfit_hits(ihit,sector)).eq.1.and.energ.lt.ScFit_paddle(sc_gev,ihit,sector))) then
            closest = abs(diff)
            schit = ihit
            pddid = scfit_hits(ihit,sector)
            energ=ScFit_paddle(sc_gev,ihit,sector)
            status=scpd_stat(scfit_hits(ihit,sector),sector)
          endif
        endif
      ENDDO
c
      If ( schit.Ne.0 ) Then    ! This should happen alwes.
        if(scfit_hits(schit,sector).le.sclastpd(sc_pl-1).or.
     +     scfit_hits(schit,sector).gt.sclastpd(sc_pl))then
          if(scfit_hits(schit,sector).gt.sclastpd(sc_pl)) sc_pl = sc_pl + 1
          if(scfit_hits(schit,sector).le.sclastpd(sc_pl-1))sc_pl = sc_pl - 1
          CALL back_2_clas(itr,sc_pl,sector,pos)
          closest = abs( pos(3) - ScFit_paddle(sc_z,schit,sector) )
        endif
        npart_sc = npart_sc + 1
        partstsc(npart+1) = npart_sc
        partchi2sc(npart+1) = closest
c- Fill the SC information structure.
        scpart(npart_sc)=npart+1
        scihit(npart_sc) = schit
        scpdid(npart_sc) = scfit_hits(schit,sector)
        scsector(npart_sc) = sector
        sctrk_pln(npart_sc)=sc_pl
        scr_trk(npart_sc) = trkp(10,sc_pl,itr)
        sce_dep(npart_sc) = 
     =             ScFit_paddle(sc_GeV,schit,sector)
C- Factor 0.998 came to make timing for PID the best possibal. 
C- This will be taken out when TOF will be calibrated PMT by PMT from begining. 
C-        sctime(npart_sc)  = 
C-     =                 ScFit_paddle(sc_TIME,schit,sector)*0.998
        sctime(npart_sc)  = 
     =                 ScFit_paddle(sc_TIME,schit,sector)
        scstatus(npart_sc) = status
      EndIf
c
      ENDIF
      RETURN
      END
c
c------------------------------------------------------------------------------




