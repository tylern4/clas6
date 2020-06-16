      SUBROUTINE TAG_MAKE_PCO(PC_event)
c
c_begin_doc
c  RCS ID string
c  $Id: tag_make_PCO.F,v 1.4 1998/04/22 00:40:02 anciant Exp $
c
c  Documentation for subroutine TAG_MAKE_PCO
c
c  Purpose:  Pair Counter event main processing routine 
c            that eventually creates the output bank PCO
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
#include "tag_general.PAR"
#include "tagtcl.CMN"
#include "tagwarnings.CMN"
c_end_inc
c
c_begin_var
c  input/output variables:
c  -----------------------
       logical PC_event  ! returns if good PC event.
c
c  Local pre-defined variables:
c  ---------------------------
c  RCS information: 
      CHARACTER*132  CFILE, CREVIS, CSTATE, CDATE, CAUTHO, CRCSID 
      PARAMETER ( CFILE = '$RCSfile: tag_make_PCO.F,v $')
      PARAMETER ( CREVIS = '$Revision: 1.4 $')
      PARAMETER ( CSTATE = '$State: Exp $')
      PARAMETER ( CDATE = '$Date: 1998/04/22 00:40:02 $')
      PARAMETER ( CAUTHO = '$Author: anciant $')
      DATA CRCSID/
     1'$Id: tag_make_PCO.F,v 1.4 1998/04/22 00:40:02 anciant Exp $
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
       logical OK,PC_match

c_end_var
c
c  executable code for routine TAG_MAKE_PCO:
c  -----------------------------------------
c
       crmess = '  '
       
       IF (NWRITE .LT. 1) THEN
         NWRITE = NWRITE + 1
         if (Tagger_PC_TDC.eq.1) then
           crmess = 'processing of the first Pair counter event - TDC hit is required'
         else
           crmess = 'processing of the first Pair counter event - TDC hit is NOT required'
         endif
         CALL RECMES(CRNAME,'I',CRMESS)
       ENDIF

c
c      check availability and store in common raw 
c      data contained in DSPC BOS bank    
c       
       call tag_read_PC_BOS(OK,PC_event,Tagger_PC_TDC)
       if(.not.OK) then
            wtrpcb = wtrpcb + 1
            if (mod(wtrpcb-1,Tagger_warning).eq.0) then
             write(crmess,1) wtrpcb
1            format ('problems while reading PC data from BOS bank occured ',I5,' times')
             CALL RECMES(CRNAME,'I',CRMESS)
            endif
	 return
       endif
c
c      If information is available :
c
      if (PC_event) then 

c
c      perform PC analysis
c             
	 call tag_PC_process(OK)
         if(.not.OK) then
            wtpcp = wtpcp + 1
            if (mod(wtpcp-1,Tagger_warning).eq.0) then
             write(crmess,2) wtpcp
2            format ('Errors while processing Pair Counter occured ',I5,' times')
             CALL RECMES(CRNAME,'I',CRMESS)
            endif
           return
         endif

c
c      perform PC T time matching :
c
       call tag_PC_T_match(PC_match)

c
c      put DSD control results in BOS bank when information is available
c
       call tag_PC_fetch_to_BOS(ok)

c
c  
c
       endif ! (PC_event)

       PC_event = (PC_event.and.PC_match)
       
       RETURN
       END
c
c----------------------------------------------------------------------





