      SUBROUTINE FILL_ECHB_NT(ierr)
c
c_begin_doc
c  RCS ID string
c  $Id: fill_echb_nt.F,v 1.3 1997/06/24 17:35:28 qin Exp $
c
c  Documentation for subroutine FILL_ECHB_NT
c
c  Purpose:
c  --------
c
c  Input Parameters: ierr - integer - returnes 1 if there was(were) 
c                     hit(s) in EC
c
c  Output Parameters:  Fill's areas in ECHB_NT common block.
c  -----------------
c
c  Other routines:
c  ---------------
c
c  Notes:
c  ------
c
c  Author:   S. Stepanyan      Created:  Fri Dec 13 00:42:15 EST 1996
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
#include "Ec_general.PAR"
#include "EcFit_general.CMN"
#include "ec_strip.inc"
#include "ec_peak.inc"
#include "ec_control.inc"
#include "EcStrip.CMN"
#include "echb_nt.inc"
c_end_inc
c
c_begin_var
c  input/output variables:
c  -----------------------
      INTEGER ierr
c
c  Local pre-defined variables:
c  ---------------------------
c  RCS information: 
      CHARACTER*132  CFILE, CREVIS, CSTATE, CDATE, CAUTHO, CRCSID
      PARAMETER (CFILE = '$RCSfile: fill_echb_nt.F,v $')
      PARAMETER (CREVIS = '$Revision: 1.3 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 1997/06/24 17:35:28 $')
      PARAMETER (CAUTHO = '$Author: qin $')
      DATA CRCSID/   
     1'$Id: fill_echb_nt.F,v 1.3 1997/06/24 17:35:28 qin Exp $'   
     2/   
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='FILL_ECHB_NT')
      PARAMETER (CRAUTH='whoistheauthor')
c
c  Local User defined variables:
c  -----------------------------
      INTEGER NWRITE
      INTEGER  loop_sect
      INTEGER  loop_layer
      INTEGER  loop_hit
      INTEGER  nhit_sav
      INTEGER  nhits
c_end_var
c
c  executable code for routine FILL_ECHB_NT:
c----6----------------------------------------------------------------72
c
      IF (NWRITE .LT. 1) THEN
        NWRITE = NWRITE + 1
        CRMESS='Filling Ntuple by ECHB'
        CALL RECMES(CRNAME,'I',CRMESS)
      ENDIF
      nhits=0
      Do loop_hit=1, mxhits
        nsecthit(loop_hit)=0
        layer(loop_hit)=0
        nhitid(loop_hit)=0
        nhitl(loop_hit)=0
      EndDo
      loop_hit=0
      DO loop_sect=1,6
        if(ecfit_nhit(whole,loop_sect).gt.0.and.
     &  ecfit_nhit(inner,loop_sect)+ecfit_nhit(outer,loop_sect).gt.0)then
        do loop_layer=whole,outer
          if(ecfit_nhit(loop_layer,loop_sect).gt.0)then
            nhits=ecfit_nhit(loop_layer,loop_sect)+nhits
            if(nhits.le.mxhits) then
            do nhit_sav=1,ecfit_nhit(loop_layer,loop_sect) 
              loop_hit=loop_hit+1
              layer(loop_hit)=loop_layer
              nsecthit(loop_hit)=loop_sect
              nhitl(loop_hit)=ecfit_nhit(loop_layer,loop_sect)
              nhitid(loop_hit)=nhit_sav
              nstrpu(loop_hit)=
     +nstripc(nhit_sav,Ec_u,loop_layer,loop_sect)
              nstrpv(loop_hit)=
     +nstripc(nhit_sav,Ec_v,loop_layer,loop_sect)
              nstrpw(loop_hit)=
     +nstripc(nhit_sav,Ec_w,loop_layer,loop_sect)
              ehit(loop_hit)=
     +ecfit_hit(energy,nhit_sav,loop_layer,loop_sect)
              thit(loop_hit)=
     +ecfit_hit(time,nhit_sav,loop_layer,loop_sect)
              ihit(loop_hit)=
     +ecfit_hit(ec_i,nhit_sav,loop_layer,loop_sect)
              jhit(loop_hit)=
     +ecfit_hit(ec_j,nhit_sav,loop_layer,loop_sect)
              xhit(loop_hit)=
     +ecfit_hit(ec_x,nhit_sav,loop_layer,loop_sect)
              yhit(loop_hit)=
     +ecfit_hit(ec_y,nhit_sav,loop_layer,loop_sect)
              zhit(loop_hit)=
     +ecfit_hit(ec_z,nhit_sav,loop_layer,loop_sect)
              dihit(loop_hit)=
     +ecfit_hit(ec_di,nhit_sav,loop_layer,loop_sect)
              djhit(loop_hit)=
     +ecfit_hit(ec_dj,nhit_sav,loop_layer,loop_sect)
              dxhit(loop_hit)=
     +ecfit_hit(ec_dx,nhit_sav,loop_layer,loop_sect)
              dyhit(loop_hit)=
     +ecfit_hit(ec_dy,nhit_sav,loop_layer,loop_sect)
              dzhit(loop_hit)=
     +ecfit_hit(ec_dz,nhit_sav,loop_layer,loop_sect)
              widths(loop_hit) = 
     +ecfit_hit(width,nhit_sav,loop_layer,loop_sect)
              distu(loop_hit) = 
     +ec_peak(dist,nhit_sav,Ec_U,loop_layer,loop_sect)
              distv(loop_hit) = 
     +ec_peak(dist,nhit_sav,Ec_V,loop_layer,loop_sect)
              distw(loop_hit) = 
     +ec_peak(dist,nhit_sav,Ec_W,loop_layer,loop_sect)
              pathu(loop_hit) = 
     +ppath(nhit_sav,Ec_U,loop_layer,loop_sect)
              pathw(loop_hit) = 
     +ppath(nhit_sav,Ec_W,loop_layer,loop_sect)
              pathv(loop_hit) = 
     +ppath(nhit_sav,Ec_V,loop_layer,loop_sect)
              m2uhit(loop_hit) = 
     +ec_peak(mom2,nhit_sav,Ec_U,loop_layer,loop_sect)
              m2vhit(loop_hit) = 
     +ec_peak(mom2,nhit_sav,Ec_V,loop_layer,loop_sect)
              m2whit(loop_hit) = 
     +ec_peak(mom2,nhit_sav,Ec_W,loop_layer,loop_sect)
              m3uhit(loop_hit) = 
     +ec_peak(mom3,nhit_sav,Ec_U,loop_layer,loop_sect)
              m3vhit(loop_hit) = 
     +ec_peak(mom3,nhit_sav,Ec_V,loop_layer,loop_sect)
              m3whit(loop_hit) = 
     +ec_peak(mom3,nhit_sav,Ec_W,loop_layer,loop_sect)
              m4uhit(loop_hit) = 
     +ec_peak(mom4,nhit_sav,Ec_U,loop_layer,loop_sect)
              m4vhit(loop_hit) = 
     +ec_peak(mom4,nhit_sav,Ec_V,loop_layer,loop_sect)
              m4whit(loop_hit) = 
     +ec_peak(mom4,nhit_sav,Ec_W,loop_layer,loop_sect)
              IF ( loop_layer.eq.whole ) THEN
                  match1(loop_hit) = 
     +matched(loop_layer,inner,nhit_sav,loop_sect)
                  c2match1(loop_hit) = 
     +c2match(loop_layer,inner,nhit_sav,loop_sect)
                  match2(loop_hit) = 
     +matched(loop_layer,outer,nhit_sav,loop_sect)
                  c2match2(loop_hit) = 
     +c2match(loop_layer,outer,nhit_sav,loop_sect)
              ELSE IF ( loop_layer.eq.inner ) THEN
                  match1(loop_hit) = 
     +matched(loop_layer,whole,nhit_sav,loop_sect)
                  c2match1(loop_hit) = 
     +c2match(loop_layer,whole,nhit_sav,loop_sect)
                  match2(loop_hit) = 
     +matched(loop_layer,outer,nhit_sav,loop_sect)
                  c2match2(loop_hit) = 
     +c2match(loop_layer,outer,nhit_sav,loop_sect)
              ELSE IF ( loop_layer.eq.outer ) THEN
                  match1(loop_hit) = 
     +matched(loop_layer,whole,nhit_sav,loop_sect)
                  c2match1(loop_hit) = 
     +c2match(loop_layer,whole,nhit_sav,loop_sect)
                  match2(loop_hit) = 
     +matched(loop_layer,inner,nhit_sav,loop_sect)
                  c2match2(loop_hit) = 
     +c2match(loop_layer,inner,nhit_sav,loop_sect)
              END IF
            enddo
            endif
          endif
C- 
        enddo
        endif
      ENDDO
      nhit=nhits
      if(nhits.gt.0) then
        ierr=0
      endif
99    RETURN
      END
c
c------------------------------------------------------------------------------




