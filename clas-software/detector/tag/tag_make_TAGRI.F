      SUBROUTINE TAG_MAKE_TAGRI(OK,TAGT_event)
c
c_begin_doc
c  RCS ID string
c  $Id: tag_make_TAGRI.F,v 1.6 2006/06/02 14:53:21 pasyuk Exp $
c
c  Documentation for subroutine TAG_MAKE_TAGR
c
c  Purpose:  TAGGER main routine for the generation of TAGR
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
#include "tagRunControl.CMN"
#include "tagwarnings.CMN"

c_end_inc
c
c_begin_var
c  input/output variables:
c  -----------------------
       logical TAGT_event,ok


c
c  Local pre-defined variables:
c  ---------------------------
c  RCS information: 
      CHARACTER*132  CFILE, CREVIS, CSTATE, CDATE, CAUTHO, CRCSID 
      PARAMETER ( CFILE = '$RCSfile: tag_make_TAGRI.F,v $')
      PARAMETER ( CREVIS = '$Revision: 1.6 $')
      PARAMETER ( CSTATE = '$State: Exp $')
      PARAMETER ( CDATE = '$Date: 2006/06/02 14:53:21 $')
      PARAMETER ( CAUTHO = '$Author: pasyuk $')
      DATA CRCSID/
     1'$Id: tag_make_TAGRI.F,v 1.6 2006/06/02 14:53:21 pasyuk Exp $
     3'/
c  Module information:
      CHARACTER*132  CRNAME, CRAUTH
      PARAMETER (CRNAME='TAG_MAKE_TAGRI')
      PARAMETER (CRAUTH='Eric Anciant')
c
c  Local Tag defined variables:
c  ---------------------------
       integer nwrite
       character*132 crmess
       logical TAGT_bank,TAGE_bank,TAGE_event

c_end_var
c
c  executable code for routine TAG_EVNT:
c  -------------------------------------
c
       crmess = '  '
       
       IF (NWRITE .LT. 1) THEN
         NWRITE = NWRITE + 1
         crmess = 'First generation of TAGR & TAGI Tagger output banks'
         CALL RECMES(CRNAME,'I',CRMESS)
         if (Tagger_NO_E.eq.1) then
            crmess = ' E''s ARE NOT REQUIRED'
            CALL RECMES(CRNAME,'I',CRMESS)
         endif
       ENDIF
c
c      reset to zero internal arrays
c    
   
       call tag_reset_arrays

c
c      store in common raw data contained in TAGT, TAGE BOS bank
c       
       call tag_read_from_BOS(OK,TAGT_bank,TAGE_bank,TAGT_event,TAGE_event)
       if(.not.OK) then
         wtrfb = wtrfb + 1
         if (mod(wtrfb-1,Tagger_warning).eq.0) then
           write(crmess,1) wtrfb
1          format ('Not being able to read tagger data from BOS bank occured ',I5,' times')
           CALL RECMES(CRNAME,'W',CRMESS)
         endif
       endif
c
c     perform the tagger analysis : finds candidates
c
       if(use_tgtlr.eq.0) then
c     this is for single hit T-counters.
          
          if (TAGT_event) then
             call tag_analysis(OK,TAGE_bank,TAGE_event,Tagger_NO_E)
             if(.not.OK) then
                wta = wta + 1
                if (mod(wta-1,Tagger_warning).eq.0) then
                   write(crmess,2) wta
 2                 format ('Problems during tagger analysis occured ',I5,' times')
                   CALL RECMES(CRNAME,'I',CRMESS)
                endif
             endif
          endif
          
c     
c     Generates informations for the TAGR Bank
c     
          
          call tag_process_TAGR(OK,TAGT_event,TAGE_event)
          if(.not.OK) then
             wtpt = wtpt + 1
             if (mod(wtpt-1,Tagger_warning).eq.0) then
                write(crmess,3) wtpt
 3              format ('Problems during TAGR hits generation occured ',I5,' times')
                CALL RECMES(CRNAME,'I',CRMESS)
             endif
          endif
                    
c     
c     Intermediate analysis results in TAGI output BOS bank
c     
          
          call tag_Int_fetch_to_BOS(OK,TAGT_event,TAGE_event)
          ok=.true.
          if(.not.OK) then
             wtiftb = wtiftb + 1
             if (mod(wtiftb-1,Tagger_warning).eq.0) then
                write(crmess,4) wtiftb
 4              format ('Not being abble to write intermediate results TAGI Tagger bank occured ',I5,' times')
                CALL RECMES(CRNAME,'I',CRMESS)
             endif
          endif
                    
c
c     Put final result in TAGR BOS bank
c     
          call tag_fetch_to_BOS(OK,TAGT_event,TAGE_event)
          ok=.true.
          if(.not.OK) then
             wtftb = wtftb + 1
             if (mod(wtftb-1,Tagger_warning).eq.0) then
                write(crmess,5) wtftb
 5              format ('Not being abble to write Tagger output bank occured ',I5,' times')
                CALL RECMES(CRNAME,'I',CRMESS)
             endif
          endif
c     end single hit
       ELSE
c     this is for pipeline TDC
          if (TAGT_event) then       
             call tag_filter_out(OK)
             call tag_etm(OK)              
          endif    
       endif
       
       RETURN
       END
c
c----------------------------------------------------------------------





