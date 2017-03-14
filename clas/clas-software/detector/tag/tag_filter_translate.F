	subroutine tag_filter_translate(OK)
c
c_begin_doc
c  RCS ID string
c  $Id: tag_filter_translate.F,v 1.1 1998/10/20 17:06:08 anciant Exp $
c
c  Documentation for subroutine tag_filter_translate
c
c  Purpose: TAGGER supplied routined for filtering hits / L/R T coincidence, E counter time window
c  --------  this particular routines also translate hits in the TAGT banks using the trnaslation
c            table loaded in include/tagcal.CMN in the array TTranslateTo(122) in the common TagTTransTable
c
c  translation for T left are stored in index 1->61 
c  translation for T right are stored in index 62->122
c
c  translation to T left are numbered 1->61
c  translation to T right are numbered 62->122
c  erasing one hit is done by translating it to 0
c
c  The alorithm is: 
c    hits in TAGT left i   i = 1 -> 61 : 
c        if TTranslateTo(i) < 61   they are transposed in T left (TTranslateTo(i))
c        if TTranslateTo(i) > 61   they are transposed in T right (TTranslateTo(i) - 61)
c    hits in TAGT right i  i=1 -> 61
c        if TTranslateTo(i+61) < 61   they are transposed in T left (TTranslateTo(i+61))
c        if TTranslateTo(i+61) > 61   they are transposed in T right (TTranslateTo(i+61) - 61)
c
c    this scheme allows to handle in a simple way left/right inversion.
c    The neutral translation table is the one with TTranslateTo(i) = i
c
c
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
c#include "bcs.inc"
#include "tag_general.PAR"
#include "event.CMN"
#include "tagcal.CMN"
c_end_inc
c
c_begin_var
c  input/output variables:
c  -----------------------
c
c  Local pre-defined variables:
c  ---------------------------
c  RCS information: 
      CHARACTER*(*)  CFILE, CREVIS, CSTATE, CDATE, CAUTHO 
      PARAMETER (CFILE=  '$RCSfile: tag_filter_translate.F,v $')
      PARAMETER (CREVIS= '$Revision: 1.1 $')
      PARAMETER (CSTATE= '$State: Exp $')
      PARAMETER (CDATE=  '$Date: 1998/10/20 17:06:08 $')
      PARAMETER (CAUTHO= '$Author: anciant $')
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='tag_filter_out')
      PARAMETER (CRAUTH='Francois Roudot')
c
c  Local tag defined variables:
c  ----------------------------
      logical ok
	integer i
        integer tmp_tdc(0:122)
	
c_end_var
c
c  executable code for routine tag_filter_out:
c  -------------------------------------
	
	crmess = ' '

      ok=.true.


c
c ----- Find the number of E counters in a reasonable time window
c 
	NbGoodE = 0

	do i=1,Tag_E_NbHits
 	  if (ec_val(i).gt.E_TDC_MIN.and.ec_val(i).lt.E_TDC_MAX.and.
     +    ec_id(i).ge.1.and.ec_id(i).le.384) then ! we filter
 		NbGoodE = NbGoodE + 1 ! We count the number of good E
		Good_E_Id(NbGoodE) = ec_id(i)
		Good_E_val(NbGoodE) = -ECchWidth * float(ec_val(i)) /1000.- posEpeak(ec_id(i)) ! and convert 
	  endif
	enddo
	
	if(NbGoodE.lt.MAXEC) Good_E_Id(NbGoodE + 1) = 0

c
c ----- Find the number of coincidence between T right and T left
c

c initialise temporary array

        do i=0,122
          tmp_tdc(i) = 0
        enddo
c
c translate usefull hits
c

	do i=1,Tag_T_NbHits
	  if (tc_id(i).ge.1.and.tc_id(i).le.61) then ! Nothing we can do when bad T id !!!
            if (tc_val_left(i).gt.1.and.tc_val_left(i).lt.4094) ! only translate usefull hits.
     +          tmp_tdc( TTranslateTo( tc_id(i) ) ) = tc_val_left(i)
            if (tc_val_right(i).gt.1.and.tc_val_right(i).lt.4094) ! only translate usefull hits.
     +          tmp_tdc( TTranslateTo( tc_id(i) + 61 ) ) = tc_val_right(i)
          endif
        enddo

c
c Get Good L/R coincidence and convert to nanoseconde.
c
    
	NbGoodT = 0
	do i=1,61
          if (tmp_tdc(i).ne.0.and.tmp_tdc(i + 61).ne.0) then ! good hits left & right
	    NbGoodT = NbGoodT + 1
	    Good_T_Id(NbGoodT) = i
	    Good_T_L_val(NbGoodT) = TCchWidthL(i) * float(tmp_tdc(i)) /1000.
     +					 - posTpeakL(i)  ! width given in ps
	    Good_T_R_val(NbGoodT) = TCchWidthR(i) * float(tmp_tdc(i + 61)) /1000.
     +					- posTpeakR(i)   ! width given in ps
          endif
	enddo
	
	if(NbGoodT.lt.MAXTC) Good_T_Id(NbGoodT + 1) = 0
			
      RETURN
      END
c
c------------------------------------------------------------------------------



 
