      SUBROUTINE TAG_EVNT
c
c_begin_doc
c  RCS ID string
c  $Id: tag_evnt.F,v 1.16 2001/06/26 16:31:06 pasyuk Exp $
c
c  Documentation for subroutine TAG_EVNT
c
c  Purpose:  TAGGER supplied EVENT processing routine, called once per event
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
c
c  Local pre-defined variables:
c  ---------------------------
c  RCS information: 
      CHARACTER*132  CFILE, CREVIS, CSTATE, CDATE, CAUTHO, CRCSID 
      PARAMETER ( CFILE = '$RCSfile: tag_evnt.F,v $')
      PARAMETER ( CREVIS = '$Revision: 1.16 $')
      PARAMETER ( CSTATE = '$State: Exp $')
      PARAMETER ( CDATE = '$Date: 2001/06/26 16:31:06 $')
      PARAMETER ( CAUTHO = '$Author: pasyuk $')
      DATA CRCSID/
     1'$Id: tag_evnt.F,v 1.16 2001/06/26 16:31:06 pasyuk Exp $
     3'/
c  Module information:
      CHARACTER*132  CRNAME, CRAUTH
      PARAMETER (CRNAME='TAG_EVNT')
      PARAMETER (CRAUTH='Francois Roudot')
c
c  Local Tag defined variables:
c  ---------------------------
       integer nwrite
       character*132 crmess
       logical OK,PC_event,PS_event,TAC_event
       logical TAGT_event
       integer RawPattern

c_end_var
c
c  executable code for routine TAG_EVNT:
c  -------------------------------------
c
       crmess = '  '
       
       IF (NWRITE .LT. 1) THEN
         NWRITE = NWRITE + 1
            crmess = 'processing of the first tagger event '
         CALL RECMES(CRNAME,'I',CRMESS)
       ENDIF

c
c ---- Convert raw Coda Format bank to CLAS bank format
c
c
c --- if Tagger_NO_E =1 we don't need to convert raw to TAGE bank
c
       RawPattern = Tagger_Raw_Format ! We don't want to overide the Tcl switch here
       if (Tagger_NO_E.eq.1) RawPattern = iand(RawPattern,B'1111111111111101')

       if(RawPattern.ne.0) then
	 call tag_Raw_2_BOS(OK,RawPattern)
         if(.not.OK) then
           wtr2b = wtr2b + 1
           if (mod(wtr2b-1,Tagger_warning).eq.0) then
             write(crmess,1) wtr2b
1            format ('Not being able to convert CODA banks to physics BOS bank for TAGGER occured ',I5,' times')
             CALL RECMES(CRNAME,'I',CRMESS)
           endif
	   return
         endif
       endif

c
c  --- Tagger processing generates the TAGR and TAGI banks:
c

      call tag_make_TAGRI(OK,TAGT_event)

c
c     PC, PS and TAC processing generates the PCO, PSO and TACO banks
c

c      call tag_make_PCO(PC_event)

      call tag_make_PSO(PS_event)

      call tag_make_TACO(TAC_event)

cccccccccccccccccccccc debug routines ccccccccccccccccccccccccccccccc
c
c      DSD debug routine
c
	if (Tagger_DSD_debug.ne.0) call tag_DSD_debug

c
c     call to the Debug - event.CMN dump routine
c

	if (TAGT_event.and.Tagger_debug_begin.le.Tagger_debug_end) then
          call tag_debug(Tagger_debug_begin,Tagger_debug_end)
	endif

       
       RETURN
       END
c
c----------------------------------------------------------------------





