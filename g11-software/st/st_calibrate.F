	subroutine st_calibrate(OK)
c
c_begin_doc
c  RCS ID string
c  $Id: st_calibrate.F,v 1.18 2004/10/26 16:57:24 devita Exp $
c
c  Documentation for subroutine ST_EVNT
c
c  Purpose:  ST supplied TDC calibration routine, called once per event
c  --------
c	Converts time data from TDC channels into (raw) ns 
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
      PARAMETER ( CFILE = '$RCSfile: st_calibrate.F,v $')
      PARAMETER ( CREVIS = '$Revision: 1.18 $')
      PARAMETER ( CSTATE = '$State: Exp $')
      PARAMETER ( CDATE = '$Date: 2004/10/26 16:57:24 $')
      PARAMETER ( CAUTHO = '$Author: devita $')
      DATA CRCSID/
     1'$Id: st_calibrate.F,v 1.18 2004/10/26 16:57:24 devita Exp $
     3'/
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='St_calibrate')
      PARAMETER (CRAUTH='S.Taylor')
      
      INTEGER NWRITE
c
c  Local User defined variables:
c  -----------------------------
c
	real time_walk ! Time_walk function, defined below.
c
	integer i,pair_id,j
	real t1,t1c0,t1c1,t1c2
	real t2,t2c0,t2c1,t2c2
	real t1w0,t1w1,t1w2,t2w0,t2w1,t2w2
	real a1,a2
	real ped1,ped2
*      
c_end_var
c
c  executable code for routine SCCAL_SECTOR:
c----6----------------------------------------------------------------72
c
      IF (NWRITE .LT. 1) THEN
        NWRITE = NWRITE + 1
        CRMESS='ST_CALIBRATE CALLED'
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
	    pair_id=StFoundHit(i)
c
c	    copy to local variables the calibration constants
c
	    t1c0= StCal_values(C0,side1,pair_id)
	    t1c1= StCal_values(C1,side1,pair_id)
	    t1c2= StCal_values(C2,side1,pair_id)
	    t2c0= StCal_values(C0,side2,pair_id)
	    t2c1= StCal_values(C1,side2,pair_id)
	    t2c2= StCal_values(C2,side2,pair_id)	   
	    ped1= StCal_values(ped,side1,pair_id)
	    ped2= StCal_values(ped,side2,pair_id)	    
	    t1w0= StCal_values(W0,side1,pair_id)
	    t1w1= StCal_values(W1,side1,pair_id)
	    t1w2= StCal_values(W2,side1,pair_id)
	    t2w0= StCal_values(W0,side2,pair_id)
	    t2w1= StCal_values(W1,side2,pair_id)
	    t2w2= StCal_values(W2,side2,pair_id)
c   
c	    convert ADC
c
	    a1=float(StFastbus_data(pair_id,St_ADC1))-ped1    !I*4=>R*4
  	    a2=float(StFastbus_data(pair_id,St_ADC2))-ped2    !I*4=>R*4
c
	    if(a1.lt.0) a1=0
	    if(a2.lt.0) a2=0
	    StCorrected_data(pair_id,St_ADC1)=a1
c     &              a1/StCal_values(M0,side1,pair_id)
	    StCorrected_data(pair_id,St_ADC2)=a2
c     &              a2/StCal_values(M0,side2,pair_id)
c	    
c	    convert TDC
c
	    t1=float(StFastbus_data(pair_id,St_TDC1))    !I*4=>R*4
	    t2=float(StFastbus_data(pair_id,St_TDC2))    !I*4=>R*4
	    StCorrected_data(pair_id,St_TDC1)=t1c0+t1c1*t1+t1c2*t1**2
     &              + time_walk(a1,t1w0,t1w1,t1w2)
	    StCorrected_data(pair_id,St_TDC2)=t2c0+t2c1*t2+t2c2*t2**2
     &              + time_walk(a2,t2w0,t2w1,t2w2)
	    StStatus(pair_id)=TIME_WALKED
	 enddo

	return
	end






	subroutine stn_calibrate(OK)
c
c_begin_doc
c  RCS ID string
c  $Id: st_calibrate.F,v 1.18 2004/10/26 16:57:24 devita Exp $
c
c  Documentation for subroutine ST_EVNT
c
c  Purpose:  ST supplied TDC calibration routine, called once per event
c  --------
c	Converts time data from TDC channels into (raw) ns 
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
      PARAMETER ( CFILE = '$RCSfile: st_calibrate.F,v $')
      PARAMETER ( CREVIS = '$Revision: 1.18 $')
      PARAMETER ( CSTATE = '$State: Exp $')
      PARAMETER ( CDATE = '$Date: 2004/10/26 16:57:24 $')
      PARAMETER ( CAUTHO = '$Author: devita $')
      DATA CRCSID/
     1'$Id: st_calibrate.F,v 1.18 2004/10/26 16:57:24 devita Exp $
     3'/
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='St_calibrate')
      PARAMETER (CRAUTH='S.Taylor')
      
      INTEGER NWRITE
c
c  Local User defined variables:
c  -----------------------------
c
	real time_walk_stn ! Time_walk function, defined below.
c
	integer i,pair_id,j,nerror
	real t1,t1c0,t1c1,t1c2
	real t2,t2c0,t2c1,t2c2
	real t1w0,t1w1,t1w2,t2w0,t2w1,t2w2,adc_max
	real a1,a2
	real ped1,ped2
	data nerror/0/
*      
c_end_var
c
c  executable code for routine SCCAL_SECTOR:
c----6----------------------------------------------------------------72
c
      IF (NWRITE .LT. 1) THEN
        NWRITE = NWRITE + 1
        CRMESS='STN_CALIBRATE CALLED'
        CALL RECMES(CRNAME,'I',CRMESS)
      ENDIF
      
	OK=StNHits.gt.0.
	if(.not.OK.AND.nerror.eq.0) then	!nothing to do
	  CRMESS=' no hit in start counter. no calibration to perform.' 
	  call RECMES(CRNAME,'W',CRMESS)
	  nerror=1
	  return	
	endif

	do i=1,StNHits
	    pair_id=StFoundHit(i)

c
c	    copy to local variables the calibration constants
c
	    t1c0= StnCal_values(C0,pair_id)
	    t1c1= StnCal_values(C1,pair_id)
	    t1c2= StnCal_values(C2,pair_id)
	    ped1= StnCal_values(ped,pair_id)
	    t1w0= StnCal_values(W0,pair_id)
	    t1w1= StnCal_values(W1,pair_id)
	    t1w2= StnCal_values(W2,pair_id)
	    adc_max= StnCal_values(ADCMAX,pair_id)
c   
c	    convert ADC
c
	    a1=float(StFastbus_data(pair_id,St_ADC1))-ped1    !I*4=>R*4
c
	    if(a1.lt.0) a1=0
	    StCorrected_data(pair_id,St_ADC1)=a1/StnCal_values(m0,pair_id)
c	    
c	    convert TDC
c
	    t1=-float(StFastbus_data(pair_id,St_TDC1))    !I*4=>R*4
	    StCorrected_data(pair_id,St_TDC1)=t1c0+t1c1*t1+t1c2*t1**2
     &              + time_walk_stn(a1,t1w0,t1w1,t1w2,adc_max)
	    StStatus(pair_id)=TIME_WALKED
	 enddo

	return
	end



c-------------------------------------------------------------------------
c Function for calculating the time-walk correction.
c-------------------------------------------------------------------------
	Real function time_walk (x,w0,w1,w2)
c
c      Time-walk correction function
c
	real x, w0,w1,w2,xnorm, thresh, walk_max, xmax
c
c      20 mv Threshold converted to channels (20*1.77)
c
	data thresh, adc_max /35., 600./
c
	xnorm = x /thresh
	
	if (x.eq.0) then
	   time_walk=0
	   return
	endif
	
	if (xnorm .lt. w2) then
	   time_walk = w0/xnorm**w1
	else
	   time_walk = w0*(1+w1)/w2**w1 - w0*w1*xnorm/w2**(w1+1)
	endif
c       
c       compute function relative to maximum ADC value adc_max
c       
	xmax = adc_max/thresh
	if (xmax .lt. w2) then
	   walk_max = w0/xmax**w1
	else
	   walk_max = w0*(1+w1)/w2**w1 - w0*w1*xmax/w2**(w1+1)
	endif
c
	time_walk = walk_max-time_walk
	return
	end


c-------------------------------------------------------------------------
c New Function for calculating the time-walk correction.
c-------------------------------------------------------------------------
	Real function time_walk_stn (x,w0,w1,w2,adc_max)
c
c      Time-walk correction function
c
	implicit none
	real x, w0,w1,w2,adc_max
	real thresh,adc_allmax
c
	data thresh/50./
        data adc_allmax/4000./
c
	if(x.gt.0.AND.x.le.adc_allmax) then
	   continue
	else
	  time_walk_stn=0     ! MC data or 0 ADC
	  return
	endif 
	
c	if(x.gt.thresh.AND.x.le.adc_max) then
c	   time_walk_stn=(w0+w1/(x-w2))	  
c	elseif(x.gt.0.AND.x.le.thresh) then
c	   time_walk_stn=(w0+w1/(thresh-w2))
c	elseif(x.gt.adc_max) then
c	   time_walk_stn=(w0+w1/(adc_max-w2))
c	else
c	   time_walk_stn=0
c	endif

	if(x.gt.thresh) then
	   time_walk_stn=(w0+w1/(x-w2))	  
	elseif(x.gt.0.AND.x.le.thresh) then
	   time_walk_stn=(w0+w1/(thresh-w2))
	else
	   time_walk_stn=0
	endif

	return
	end



