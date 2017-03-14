      SUBROUTINE stitm_fetch_to_BOS(OK)
c
c_begin_doc
c  RCS ID string
c  $Id: stitm_fetch_to_BOS.F,v 1.4 2004/05/25 21:00:31 devita Exp $
c
c  Documentation for subroutine stitm_fetch_to_BOS
c
c  Purpose: 
c  --------
c
c  Calling Sequence:
c  ----------------
c
c  Input Parameters:  NONE
c  ----------------
c
c  Output Parameters:  NONE
c  -----------------
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
c  Author:   Francois ROUDOT      Created:  Thu Jun 19 12:00:00 EST 1997
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
#include "bcs.inc"
#include "st_general.PAR"
#include "sthit.CMN"
#include "stconvert.CMN"
#include "sttrk.CMN"
c_end_inc
c
c_begin_var
c  input/output variables:
c  -----------------------
      logical ok
c      
c
c  Local pre-defined variables:
c  ---------------------------
c  RCS information: 
      CHARACTER*(*)  CFILE, CREVIS, CSTATE, CDATE, CAUTHO 
      PARAMETER (CFILE=  '$RCSfile: stitm_fetch_to_BOS.F,v $')
      PARAMETER (CREVIS= '$Revision: 1.4 $')
      PARAMETER (CSTATE= '$State: Exp $')
      PARAMETER (CDATE=  '$Date: 2004/05/25 21:00:31 $')
      PARAMETER (CAUTHO= '$Author: devita $')
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*132  CRMESS
      PARAMETER (CRNAME='stitm_fetch_to_BOS')
      PARAMETER (CRAUTH='Francois Roudot')
c
c  Local st defined variables:
c  ----------------------------
c
	integer idx,i,pair_id
	integer MBANK	!- BOS routine
	logical dump 
	integer StNhitDump
	integer StDumpPair_id(3)
c
c_end_var
c
c  executable code for routine stitm_fetch_to_BOS:
c  ------------------------------------
c
c	decide whether or not it is needed to dump partially analyzed data.
c	The criterium is : more than one track in one coupled-paddle
c
c      dump=.FALSE.
c      StNhitDump=0
      
     
c      if(StNHits.gt.0) then
c        do i=1,StNHits
c	   pair_id=StFoundHit(i)
c	   if(StTrknumber(pair_id).gt.1.) then
c	     dump=.TRUE.          
c	     StNhitDump=StNhitDump+1
c	     StDumpPair_id(i)=pair_id
c	   endif
c        enddo
c      endif
      
      if(StNHits.le.0) then    !nothing to do
         write(6,*)'Nothing found?'
        return
      else		    !Only now open the bank
         idx = MBANK(iw,StStI_BOS_name,1,StStI_BOS_size,StNHits)
         OK= idx.GT.0
         If(.NOT.OK) Then
            CRMESS= ' '
            WRITE(CRMESS,'(a)') 'unable to open BOS bank "'//
     &                              StStI_BOS_name//'"'
            call RECMES(CRNAME,'F',CRMESS)
            RETURN
         endIf
         if(StType.eq.0) then
            do i=1,StNhits
               pair_id=StFoundHit(i)
               idx=idx+1
               iw(idx)=pair_id
               idx=idx+1
               iw(idx)=StStatus(pair_id)
               idx=idx+1
               rw(idx)=StCorrected_data(pair_id,St_TDC1)
               idx=idx+1
               rw(idx)=StCorrected_data(pair_id,St_ADC1)
               idx=idx+1
               rw(idx)=StCorrected_data(pair_id,St_TDC2)
               idx=idx+1
               rw(idx)=StCorrected_data(pair_id,St_ADC2)
            enddo
         elseif(StType.eq.1) then
            do i=1,StNhits
               idx=idx+1
               iw(idx)=StSecHit(i)*100+(StFoundHit(i)-(StSecHit(i)-1)*4)*10
               idx=idx+1
               iw(idx)=StStatus(StFoundHit(i))
               idx=idx+1
               rw(idx)=StCorrected_data(StFoundHit(i),St_TDC1)
               idx=idx+1
               rw(idx)=StCorrected_data(StFoundHit(i),St_ADC1)
               idx=idx+1
               rw(idx)=0.
               idx=idx+1
               rw(idx)=0.
            enddo
         endif
      endIf
      
      RETURN
      END
c
c------------------------------------------------------------------------------




