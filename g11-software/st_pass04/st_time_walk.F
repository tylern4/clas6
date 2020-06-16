	subroutine st_time_walk
c
c_begin_doc
c  RCS ID string
c  $Id: st_time_walk.F,v 1.9 2004/08/04 10:19:13 devita Exp $
c
c  Documentation for subroutine ST_EVNT
c
c  Purpose:  ST supplied TDC calibration routine, called once per event
c  --------
c	Performs time walk corrections.
c
c  Input Parameters:
c  ----------------
c
c  Output Parameters:     
c  -----------------
*-LOGICAL OK		!-	success or fatal error
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
c  Author:   Simon Taylor      Created:  Thu Jun 19 12:00:00 EST 1997
c  -------
c
c  Major revisions:
c  ----------------
c     - handle event that has 2 tracks in one sector :Burin Feb98
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
#include "bcs.inc"
#include "st_general.PAR"
#include "sthit.CMN"
#include "sttcl.CMN"
#include "stcal.CMN"
#include "stconvert.CMN"
#include "sttrk.CMN"
#include "stgeom.CMN"
c_end_inc
c
c_begin_var
c  input/output variables:
c  -----------------------
c
	logical OK
c
c  Local pre-defined variables:
c  ---------------------------
c  RCS information: 
      CHARACTER*132  CFILE, CREVIS, CSTATE, CDATE, CAUTHO, CRCSID 
      PARAMETER ( CFILE = '$RCSfile: st_time_walk.F,v $')
      PARAMETER ( CREVIS = '$Revision: 1.9 $')
      PARAMETER ( CSTATE = '$State: Exp $')
      PARAMETER ( CDATE = '$Date: 2004/08/04 10:19:13 $')
      PARAMETER ( CAUTHO = '$Author: devita $')
      DATA CRCSID/
     1'$Id: st_time_walk.F,v 1.9 2004/08/04 10:19:13 devita Exp $
     3'/
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='St_time_walk')
      PARAMETER (CRAUTH='S.Taylor')
      
      INTEGER NWRITE
c
c  Local User defined variables:
c  -----------------------------
c
	real time_walk
	integer i,pair_id,j
	real t1w0,t1w1,t1w2
	real t2w0,t2w1,t2w2
	real adc1,adc2
*      
c_end_var
c
c----6----------------------------------------------------------------72
c
      IF (NWRITE .LT. 1) THEN
        NWRITE = NWRITE + 1
        CRMESS='ST_TIME_WALK CALLED'
        CALL RECMES(CRNAME,'I',CRMESS)
      ENDIF
 
        OK=StNHits.gt.0.
        if(.not.OK) then        !nothing to do
c         CRMESS=' no hit in start counter. no calibration to perform.'
c         crmess=crmess//' Event has a bad photon trigger'
c         call RECMES(CRNAME,'W',CRMESS)
          return
        endif
 
 
        do i=1,StNHits
c       do i=1,st_max_pair
            pair_id=StFoundHit(i)
c          pair_id=i
 
c perform time-walk correction only if #tracks =< 2 were found in the
C$$$$$$$$$$$$$$$$ now put ALL tracks, even if more than 2 (EAP)
c corresponding sector. If no link between tracking and start
c counter were done, nothing is converted.
c
c----6----------------------------------------------------------------72
c
            if(Ntrk_hit(pair_id).gt.0.and.Nhit_ST(pair_id).gt.0) then
              do j=1,Ntrk_hit(pair_id)
 
                StNconvertedHits(pair_id)=StNconvertedHits(pair_id)+1
                StWhichTrk(StNconvertedHits(pair_id),pair_id)=StFoundTrk(i,j,pair_id)
                StWhichSector(StNconvertedHits(pair_id),pair_id)=
     1            StTrkSector(StWhichTrk(StNconvertedHits(pair_id),pair_id),pair_id)
                StWhichTrkSec(StNconvertedHits(pair_id),StWhichSector(StNconvertedHits(pair_id),pair_id))
     1            = StTrkNumSec(j,StWhichSector(StNconvertedHits(pair_id),pair_id))
                StWhichTrkSec(StNconvertedHits(pair_id),StWhichSector(StNconvertedHits(pair_id),pair_id))
     1            = StTrkNumSec(j,StWhichSector(StNconvertedHits(pair_id),pair_id))
                StWhichTrkLength(StNconvertedHits(pair_id),pair_id)=
     1            StTrkLength(StWhichTrk(StNconvertedHits(pair_id),pair_id),pair_id)
              enddo
            endif
 
        enddo
 
 
        return
        end



	subroutine stn_time_walk

      IMPLICIT NONE
      SAVE
c
c_begin_inc
c  include files :
c  ---------------------
c BOS common block  uncomment the next line for BOS include file
#include "bcs.inc"
#include "st_general.PAR"
#include "sthit.CMN"
#include "sttcl.CMN"
#include "stcal.CMN"
#include "stconvert.CMN"
#include "sttrk.CMN"
#include "stgeom.CMN"
c_end_inc
c
c_begin_var
c  input/output variables:
c  -----------------------
c
	logical OK
c
c  Local pre-defined variables:
c  ---------------------------
c  RCS information: 
      CHARACTER*132  CFILE, CREVIS, CSTATE, CDATE, CAUTHO, CRCSID 
      PARAMETER ( CFILE = '$RCSfile: st_time_walk.F,v $')
      PARAMETER ( CREVIS = '$Revision: 1.9 $')
      PARAMETER ( CSTATE = '$State: Exp $')
      PARAMETER ( CDATE = '$Date: 2004/08/04 10:19:13 $')
      PARAMETER ( CAUTHO = '$Author: devita $')
      DATA CRCSID/
     1'$Id: st_time_walk.F,v 1.9 2004/08/04 10:19:13 devita Exp $
     3'/
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='St_time_walk')
      PARAMETER (CRAUTH='S.Taylor')
      
      INTEGER NWRITE
c
c  Local User defined variables:
c  -----------------------------
c
	real time_walk
	integer i,pair_id,j
	real t1w0,t1w1,t1w2
	real t2w0,t2w1,t2w2
	real adc1,adc2
*      
c_end_var
c
c----6----------------------------------------------------------------72
c
      IF (NWRITE .LT. 1) THEN
        NWRITE = NWRITE + 1
        CRMESS='ST_TIME_WALK CALLED'
        CALL RECMES(CRNAME,'I',CRMESS)
      ENDIF
      
	OK=StNHits.gt.0.
	if(.not.OK) then	!nothing to do
c	  CRMESS=' no hit in start counter. no calibration to perform.' 
c	  crmess=crmess//' Event has a bad photon trigger'
c	  call RECMES(CRNAME,'W',CRMESS)
	  return	
	endif


	do i=1,StNHits
c	do i=1,st_max_pair
	    pair_id=StFoundHit(i)
c	   pair_id=i

c perform time-walk correction only if #tracks =< 2 were found in the 
C$$$$$$$$$$$$$$$$ now put ALL tracks, even if more than 2 (EAP)
c corresponding sector. If no link between tracking and start 
c counter were done, nothing is converted.
c
c----6----------------------------------------------------------------72
c
	    if(Ntrk_hit(pair_id).gt.0.and.Nhit_ST(pair_id).gt.0) then
              do j=1,Ntrk_hit(pair_id)
	
     	        StNconvertedHits(pair_id)=StNconvertedHits(pair_id)+1
	        StWhichTrk(StNconvertedHits(pair_id),pair_id)=StFoundTrk(i,j,pair_id)
	        StWhichSector(StNconvertedHits(pair_id),pair_id)=
     1	          StTrkSector(StWhichTrk(StNconvertedHits(pair_id),pair_id),pair_id)
     	        StWhichTrkSec(StNconvertedHits(pair_id),pair_id)
     1	          = StTrkNumSec(j,pair_id)
     	        StWhichTrkLength(StNconvertedHits(pair_id),pair_id)=
     1	          StTrkLength(StWhichTrk(StNconvertedHits(pair_id),pair_id),pair_id)
		StWhereHit(StNconvertedHits(pair_id),pair_id)= 
     1            StTrkWhere(StWhichTrk(StNconvertedHits(pair_id),pair_id),pair_id)
	      enddo
	    endif

	enddo
	

	return
	end

