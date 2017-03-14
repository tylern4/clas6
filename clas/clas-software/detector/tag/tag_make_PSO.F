      SUBROUTINE TAG_MAKE_PSO(PS_event)
c
c_begin_doc
c  RCS ID string
c  $Id: tag_make_PSO.F,v 1.4 1998/04/22 00:40:03 anciant Exp $
c
c  Documentation for subroutine TAG_MAKE_PSO
c
c  Purpose:  Pair Spectrometer event main processing routine 
c            that eventually creates the output bank PSO
c  --------
c
c  Input Parameters:  NONE
c  ----------------
c
c  Output Parameters: NONE
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
c  Author:   Francois Roudot      Created:  Thu Jun 19 12:00:00 EST 1997
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
c#include "tag_general.PAR"
#include "tagtcl.CMN"
#include "tagwarnings.CMN"
c_end_inc
c
c_begin_var
c  input/output variables:
c  -----------------------
      logical PS_event ! returns if good PS event.

c
c  Local pre-defined variables:
c  ---------------------------
c  RCS information: 
      CHARACTER*132  CFILE, CREVIS, CSTATE, CDATE, CAUTHO, CRCSID 
      PARAMETER ( CFILE = '$RCSfile: tag_make_PSO.F,v $')
      PARAMETER ( CREVIS = '$Revision: 1.4 $')
      PARAMETER ( CSTATE = '$State: Exp $')
      PARAMETER ( CDATE = '$Date: 1998/04/22 00:40:03 $')
      PARAMETER ( CAUTHO = '$Author: anciant $')
      DATA CRCSID/
     1'$Id: tag_make_PSO.F,v 1.4 1998/04/22 00:40:03 anciant Exp $
     3'/
c  Module information:
      CHARACTER*132  CRNAME, CRAUTH
      PARAMETER (CRNAME='TAG_MAKE_PCO')
      PARAMETER (CRAUTH='Francois Roudot')
c
c  Local Tag defined variables:
c  ---------------------------
       integer nwrite
       character*132 crmess
       logical OK,PS_match

c_end_var
c
c  executable code for routine TAG_MAKE_PSO:
c  -----------------------------------------
c
       crmess = '  '
       
       IF (NWRITE .LT. 1) THEN
         NWRITE = NWRITE + 1
         if (Tagger_PS_TDC.eq.1) then
            crmess = 'processing of the first Pair spectrometer event - TDC hit is required'
         else
            crmess = 'processing of the first Pair spectrometer event - TDC hit is NOT required'
         endif
         CALL RECMES(CRNAME,'I',CRMESS)
       ENDIF

c
c      check availability and store in common raw 
c      data contained in DSPS BOS bank    
c       
       call tag_read_PS_BOS(OK,PS_event,Tagger_PS_TDC)
       if(.not.OK) then
            wtrpsb = wtrpsb + 1
            if (mod(wtrpsb-1,Tagger_warning).eq.0) then
             write(crmess,1) wtrpsb
1            format ('problems while reading PS data from BOS bank occured ',I5,' times')
             CALL RECMES(CRNAME,'I',CRMESS)
            endif
	 return
       endif
c
c      If information is available :
c

      if (PS_event) then 

c
c      perform PS tdc to time, adc to energy deposit conversions
c             
	 call tag_PS_process(OK)
         if(.not.OK) then
            wtpsp = wtpsp + 1
            if (mod(wtpsp-1,Tagger_warning).eq.0) then
             write(crmess,2) wtpsp
2            format ('Errors while processing Pair Spectrometer occured ',I5,' times')
             CALL RECMES(CRNAME,'I',CRMESS)
            endif
	   return
         endif
c
c      perform PS T time matching :
c

       call tag_PS_T_match(PS_match)

c
c      perform PS further analysis
c             
        call tag_PS_analysis(OK)

c
c      put DSD control results in BOS bank when information is available
c
        call tag_PS_fetch_to_BOS(OK)

c
c  
c
       endif ! (PS_event)
       
       PS_event = (PS_event.and.PS_match)

       RETURN
       END
c
c----------------------------------------------------------------------





