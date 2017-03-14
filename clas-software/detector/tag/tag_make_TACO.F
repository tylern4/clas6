      SUBROUTINE TAG_MAKE_TACO(TAC_event)

c
c_begin_doc
c  RCS ID string
c  $Id: tag_make_TACO.F,v 1.4 1998/04/22 00:40:03 anciant Exp $
c
c  Documentation for subroutine TAG_MAKE_TACO
c
c  Purpose:  Total absorption counter events main processing routine 
c            that eventually creates the output bank TACO
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
       logical TAC_event
c
c  Local pre-defined variables:
c  ---------------------------
c  RCS information: 
      CHARACTER*132  CFILE, CREVIS, CSTATE, CDATE, CAUTHO, CRCSID 
      PARAMETER ( CFILE = '$RCSfile: tag_make_TACO.F,v $')
      PARAMETER ( CREVIS = '$Revision: 1.4 $')
      PARAMETER ( CSTATE = '$State: Exp $')
      PARAMETER ( CDATE = '$Date: 1998/04/22 00:40:03 $')
      PARAMETER ( CAUTHO = '$Author: anciant $')
      DATA CRCSID/
     1'$Id: tag_make_TACO.F,v 1.4 1998/04/22 00:40:03 anciant Exp $
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
       logical OK,TAC_match

c_end_var
c
c  executable code for routine TAG_MAKE_TACO:
c  -----------------------------------------
c
       crmess = '  '
       
       IF (NWRITE .LT. 1) THEN
         NWRITE = NWRITE + 1
         if (Tagger_TAC_TDC.eq.1) then
           crmess = 'processing of the first Total Absorption event - TDC hit required'
         else
           crmess = 'processing of the first Total Absorption event - TDC hit is NOT required'           
         endif
         CALL RECMES(CRNAME,'I',CRMESS)
       ENDIF

c
c      check availability and store in common raw 
c      data contained in DSTC BOS bank    
c       
       call tag_read_TAC_BOS(OK,TAC_event,Tagger_TAC_TDC)
       if(.not.OK) then
            wtrtcb = wtrtcb + 1
            if (mod(wtrtcb-1,Tagger_warning).eq.0) then
             write(crmess,1) wtrtcb
1            format ('problems while reading TAC data from BOS bank occured ',I5,' times')
             CALL RECMES(CRNAME,'I',CRMESS)
            endif
	 return
       endif

c
c      If information is available :
c
      if (TAC_event) then 

c
c      perform TAC analysis
c             
	 call tag_TAC_process(OK)
         if(.not.OK) then
            wttcp = wttcp + 1
            if (mod(wttcp-1,Tagger_warning).eq.0) then
             write(crmess,2) wttcp
2            format ('Errors while processing TAC occured ',I5,' times')
             CALL RECMES(CRNAME,'I',CRMESS)
            endif
	   return
         endif

c
c      perform TAC T time matching :
c
       call tag_TAC_T_match(TAC_match)

c
c      put DSD control results in BOS bank when information is available
c
       call tag_TAC_fetch_to_BOS(ok)

c
c  
c
       endif ! (TAC_event)

       TAC_event = (TAC_event.and.TAC_match)
       
       RETURN
       END
c
c----------------------------------------------------------------------





