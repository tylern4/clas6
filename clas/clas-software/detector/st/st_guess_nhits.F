      subroutine st_guess_nhits
c
c_begin_doc
c
c  Documentation for subroutine ST_GUESS_NHITS
c
c  Purpose:  Determine whether there is only one hit or more than one hit 
c            in each pair of ST
c  --------
c
c  Input Parameters:
c  ----------------
c
c  Output Parameters:     
c  -----------------
c 
c
c  Called from:
c  ------------
c
c  Other routines:
c  ---------------
c
c  Notes:
c  ------
c
c  Author:   Burin Asavapibhop      Created:  Sat Dec 13 12:00:00 EST 1997
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
#include "wcs.inc"
#include "st_general.PAR"
#include "stgeom.CMN"
#include "sthit.CMN"
#include "sttcl.CMN"
#include "stcal.CMN"
#include "stconvert.CMN"
#include "sttrk.CMN"
c_end_inc
c
c_begin_var
c  input/output variables:
c  -----------------------
c
c
c  Local pre-defined variables:
c  ---------------------------
c  RCS information: 
      CHARACTER*132  CFILE, CREVIS, CSTATE, CDATE, CAUTHO, CRCSID 
      PARAMETER ( CFILE = '$RCSfile: st_guess_nhits.F,v $')
      PARAMETER ( CREVIS = '$Revision: 1.7 $')
      PARAMETER ( CSTATE = '$State: Exp $')
      PARAMETER ( CDATE = '$Date: 2001/11/30 15:53:37 $')
      PARAMETER ( CAUTHO = '$Author: pasyuk $')
      DATA CRCSID/
     1     '$Id: st_guess_nhits.F,v 1.7 2001/11/30 15:53:37 pasyuk Exp $
     3     '/
c     Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='ST_GUESS_NHITS')
      PARAMETER (CRAUTH='B.Asavapibhop')
c     
c  Local User defined variables:
c  -----------------------------
c
      logical OK
      integer i,pair_id
      integer tdc1,tdc2,tdc_p,tdc_m
      real ltot,slope,vprop
      real L_leg, L_nose
      integer ind_stgm, nami_stgm   
      INTEGER NWRITE,MAMIND
*      
c_end_var
c
c  executable code for routine SCCAL_SECTOR:
c----6----------------------------------------------------------------72
c      
      IF (NWRITE .LT. 1) THEN
         NWRITE = NWRITE + 1
         CRMESS='ST_GUESS_NHITS CALLED'
         CALL RECMES(CRNAME,'I',CRMESS)
      ENDIF
      
c     geometry info

      if(nami_stgm.eq.0) nami_stgm=MAMIND(jw,'STG ')
      ind_stgm=jw(nami_stgm)
      
      L_leg=rjw(ind_stgm+15) 
      L_nose=rjw(ind_stgm+16)

      OK=StNHits.gt.0.
      if(.not.OK) then	!nothing to do
c	  CRMESS=' No Hit in Start counter.' 
c	  crmess=crmess//' Event has a bad photon trigger'
c	  call RECMES(CRNAME,'W',CRMESS)
	  return	
      endif
      
		
      do i=1,StNHits
         pair_id=StFoundHit(i)
         tdc1=StFastbus_data(pair_id,St_TDC1)
         tdc2=StFastbus_data(pair_id,St_TDC2)
         
      	 ntrk_hit_st(pair_id,1) = ntrk_hit_leg(pair_id,1) +
     1        ntrk_hit_nose(pair_id,1)
         ntrk_hit_st(pair_id,2) = ntrk_hit_leg(pair_id,2) +
     1        ntrk_hit_nose(pair_id,2)
         ntrk_hit(pair_id) = ntrk_hit_st(pair_id,1)+ntrk_hit_st(pair_id,2)
         
         
c New assignement of nhit_st(pair_id)
c      =1 only tdc1 is present
c      =2 only tdc2 is present
c      =3 both tdc's are there
         
         nhit_st(pair_id) = 0
         if (tdc1 .gt. 0) nhit_st(pair_id) = nhit_st(pair_id) + 1
         if (tdc2 .gt. 0) nhit_st(pair_id) = nhit_st(pair_id) + 2
               	 
      enddo  
      
      return
      end
      
