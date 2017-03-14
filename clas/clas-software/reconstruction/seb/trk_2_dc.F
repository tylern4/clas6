      SUBROUTINE TRK_2_DC(itr,npart_dc,npart,sector)
c
c_begin_doc
c  RCS ID string
c  $Id: trk_2_dc.F,v 1.7 1998/07/01 14:56:35 stepanya Exp $
c
c  Documentation for subroutine TRK_2_DC
c
c  Purpose:
c  --------
c
c  Input Parameters:  itr      - integer - track number
c  ----------------   npart_dc - integer - counter for DCST records
c                     npart    - integer - counter for particle record
c                     sector   - treck sector number  
c
c  Output Parameters: npart_dc - integer - In case of success +1
c  -----------------
c
c  Other routines:
c  ---------------
c
c  Notes:
c  ------
c
c  Author:    S. Stepanyan     Created:  Wed Feb 26 12:33:03 EST 1997
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
#include "echits.inc"
#include "particle_out.inc"
#include "dc_inf.inc"
#include "hbt_planes.inc"
c_end_inc
c
c_begin_var
c  input/output variables:
c  -----------------------
      INTEGER itr,sector,npart_dc,npart
c
c  Local pre-defined variables:
c  ---------------------------
c  RCS information: 
      CHARACTER*132  CFILE, CREVIS, CSTATE, CDATE, CAUTHO, CRCSID
      PARAMETER (CFILE = '$RCSfile: trk_2_dc.F,v $')
      PARAMETER (CREVIS = '$Revision: 1.7 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 1998/07/01 14:56:35 $')
      PARAMETER (CAUTHO = '$Author: stepanya $')
      DATA CRCSID/   
     1'$Id: trk_2_dc.F,v 1.7 1998/07/01 14:56:35 stepanya Exp $'   
     2/   
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='TRK_2_DC')
      PARAMETER (CRAUTH='Stepan Stepanyan')
c
c  Local User defined variables:
c  -----------------------------
      INTEGER NWRITE
      REAL pos(6)
      REAL pi, r2deg
      DATA pi,r2deg /3.14159265,57.29578/
c_end_var
c
c  executable code for routine TRK_2_DC:
c----6----------------------------------------------------------------72
c
      IF (NWRITE .LT. 1) THEN
        NWRITE = NWRITE + 1
        CRMESS='This routine filling dcst structure.'
        CALL RECMES(CRNAME,'I',CRMESS)
      ENDIF
c
c- Add to particle record
          npart_dc = npart_dc + 1
          partstdc(npart) = npart_dc     
          partchi2dc(npart) = trkp(12,vrt_pln,itr)     
c- Stor reconstructed (HB tracking) treck information in the DC structure.
c- Direction cosines
          dccx(npart_dc) = trkp(4,1,itr)
          dccy(npart_dc) = trkp(5,1,itr)
          dccz(npart_dc) = trkp(6,1,itr)
c- Production vertex coordinates x,y and z(cm).
          dcvertx(npart_dc) = trkp(1,1,itr)
          dcverty(npart_dc) = trkp(2,1,itr)
          dcvertz(npart_dc) = trkp(3,1,itr)
c
c- Momentum-p(GeV), charg-q(+1,0,-1), 
          dcp(npart_dc) = trkp(7,vrt_pln,itr)
          dcq(npart_dc) = 1
          if(trkp(9,vrt_pln,itr).lt.0.)dcq(npart_dc) = -1
c- Pointer to part area, 
          dcpart(npart_dc) = npart
c- Sector, track, ch2 and statuse
          dcsector(npart_dc) = sector
          dcitrk(npart_dc) = itr
          dcchi2(npart_dc) = trkp(12,vrt_pln,itr)
          dcstatus(npart_dc) = -1
c- 
      RETURN
      END
c
c------------------------------------------------------------------------------




