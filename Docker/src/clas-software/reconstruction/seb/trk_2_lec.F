      SUBROUTINE TRK_2_LEC(itr,npart_lec,npart,sector)
c
c_begin_doc
c  RCS ID string
c  $Id: trk_2_lec.F,v 1.7 2006/06/15 21:36:02 stepanya Exp $
c
c  Documentation for subroutine TRK_2_LEC
c
c  Purpose: For matching track in DC with hit in LAC
c  --------
c
c  Input Parameters:  itr       - integer - treck number
c  ----------------   npart_lec - integer - counter for LECST records
c                     npart     - integer - counter for particle record
c                     sector    - treck sector number  
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
c  Author:  S.Stepanyan       Created:  Fri Mar  6 16:12:57 EST 1998
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
#include "lec_inf.inc"
#include "hbt_planes.inc"
c_end_inc
c
c_begin_var
c  input/output variables:
c  -----------------------
      INTEGER itr,sector,npart_lec,npart
c
c  Local pre-defined variables:
c  ---------------------------
c  RCS information: 
      CHARACTER*132  CFILE, CREVIS, CSTATE, CDATE, CAUTHO, CRCSID
      PARAMETER (CFILE = '$RCSfile: trk_2_lec.F,v $')
      PARAMETER (CREVIS = '$Revision: 1.7 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 2006/06/15 21:36:02 $')
      PARAMETER (CAUTHO = '$Author: stepanya $')
      DATA CRCSID/   
     1'$Id: trk_2_lec.F,v 1.7 2006/06/15 21:36:02 stepanya Exp $'   
     2/   
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='TRK_2_LEC')
      PARAMETER (CRAUTH='Stepan')
c
c  Local User defined variables:
c  -----------------------------
      INTEGER NWRITE
      REAL pos(6)
      REAL zdiff,closest
      INTEGER hit,ihit
c_end_var
c
c  executable code for routine TRK_2_LEC:
c----6----------------------------------------------------------------72
c
      IF (NWRITE .LT. 1) THEN
        NWRITE = NWRITE + 1
        CRMESS='Matching track in DC with hit in LAC'
        CALL RECMES(CRNAME,'I',CRMESS)
      ENDIF
c- Needs to transfer coordinates at treck intersection point with
c- EC plane from "tracking/Bogd." coordinate system to CLAS generic.  
      CALL back_2_clas(itr,lec_pln,sector,pos)
c- x,y and z are the coordinates on the face of first scintillator layer.
c- That not necessary to be the reconstruction plane (planes are 3).
c
      closest = trklec_match   ! Starts here. This is under tcl control.
      ihit=0
      IF(LEC_NHIT(1,sector).GT.0)THEN
        DO hit=1,LEC_NHIT(1,sector)
           if(lec_hit(energy,hit,1,sector).lt.10000.and.lec_hit(energy,hit,1,sector).gt.0.00001)then
              zdiff = pos(3) - lec_hit(ec_z,hit,1,sector)
              If(abs(zdiff).lt.closest)Then
                 closest=abs(zdiff)
                 ihit=hit
              EndIf
           endif
        ENDDO
        If(ihit.gt.0)Then
          npart_lec=npart_lec+1
          partstlec(npart+1) = npart_lec
          partchi2lec(npart+1)= closest
          lecpart(npart_lec)=npart+1
          lechit(1,npart_lec)=ihit
          lece(npart_lec)=lec_hit(energy,ihit,1,sector)
          lecei(npart_lec)=lec_hit(ec_i2,ihit,1,sector)
          lecx(npart_lec)=lec_hit(ec_x,ihit,1,sector)
          lecy(npart_lec)=lec_hit(ec_y,ihit,1,sector)
          lecz(npart_lec)=lec_hit(ec_z,ihit,1,sector)
          lectime(npart_lec)=lec_hit(time,ihit,1,sector)
          lecr_trk(npart_lec)=trkp(10,lec_pln,itr)
          lecsector(npart_lec)=sector
          lecstatus(npart_lec)=1
          lecid(npart_lec)=0
        EndIf
      ENDIF
      RETURN
      END
c
c------------------------------------------------------------------------------




