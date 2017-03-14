      SUBROUTINE TAG_ANALYSIS(OK,TAGE_bank,TAGE_event,NO_E_required)
c
c_begin_doc
c  RCS ID string
c  $Id: tag_analysis.F,v 1.17 2005/09/19 18:31:32 pasyuk Exp $
c
c  Documentation for subroutine TAG_ANALYSIS
c
c  Purpose:  TAGGER supplied analysis routine, called once per event
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
#include "tagtcl.CMN"
#include "tagwarnings.CMN"
#include "tagRunControl.CMN"
c_end_inc
c
c_begin_var
c  input/output variables:
c  -----------------------
c
	logical OK,TAGE_bank,TAGE_event
        integer NO_E_required
c
c  Local pre-defined variables:
c  ---------------------------
c  RCS information: 
      CHARACTER*132  CFILE, CREVIS, CSTATE, CDATE, CAUTHO, CRCSID 
      PARAMETER ( CFILE = '$RCSfile: tag_analysis.F,v $')
      PARAMETER ( CREVIS = '$Revision: 1.17 $')
      PARAMETER ( CSTATE = '$State: Exp $')
      PARAMETER ( CDATE = '$Date: 2005/09/19 18:31:32 $')
      PARAMETER ( CAUTHO = '$Author: pasyuk $')
      DATA CRCSID/
     1'$Id: tag_analysis.F,v 1.17 2005/09/19 18:31:32 pasyuk Exp $
     3'/
c  Module information:
      CHARACTER*132  CRNAME, CRAUTH
      PARAMETER (CRNAME='TAG_ANALYSIS')
      PARAMETER (CRAUTH='Francois Roudot')
c
c  Local Tag defined variables:
c  ---------------------------
       integer nwrite
       character*132 crmess
c_end_var
c
c  executable code for routine TAG_ANALYSIS:
c  -------------------------------------
c
	crmess='  '
       
	IF (NWRITE .LT. 1) THEN
          NWRITE = NWRITE + 1
          if (TAGE_bank) then
              crmess='First tagger event reconstruction TAGE is present - this message written once'
          else
              crmess='First tagger event reconstruction TAGE is NOT present - this message written once'
          endif
          CALL RECMES(CRNAME,'I',CRMESS)
        ENDIF

      ok=.true.
       
c
c     Filter out all bad formed hits, dead channel hits...
c
      if (Tagger_NO_LR.eq.0) then
        if (tagger_do_t_trans(1).ne.0) then
           call tag_filter_translate(OK)
        else
           call tag_filter_out(OK)
        endif
      else
        if (tagger_do_t_trans(1).ne.0) then
           call tag_filter_translate_NOLR(OK)
        else
           call tag_filter_out_NOLR(OK)
        endif
      endif


        if(.not.OK) then
           wtfo = wtfo + 1
           if (mod(wtfo-1,Tagger_warning).eq.0) then
             write(crmess,1) wtfo
1            format ('Problems during Tagger filtering occured ',I5,' times')
             CALL RECMES(CRNAME,'I',CRMESS)
           endif
        endif

c
c ---- do T counters regardless of anything else (need it for gflux)

         call tag_T_norm_analysis(ok)

c
c     Perform geometric matching between E & T counters
c

          call tag_E_T_geom_match(OK,NO_E_required)
          if(.not.OK) then
           wtetgm = wtetgm + 1
           if (mod(wtetgm-1,Tagger_warning).eq.0) then
             write(crmess,2) wtetgm
2            format ('Problems during E-T geometric matching occured ',I5,' times')
             CALL RECMES(CRNAME,'I',CRMESS)
           endif
          endif   

c
c     Perform time coincidence between E & T counters
c

	  call tag_E_T_time_match(OK,NO_E_required)
          if(.not.OK) then
           wtettm = wtettm + 1
           if (mod(wtettm-1,Tagger_warning).eq.0) then
             write(crmess,3) wtettm
3            format ('Problems during E-T time coincidence occured ',I5,' times')
             CALL RECMES(CRNAME,'I',CRMESS)
           endif
          endif

c
c     Perform binning of T's and E's
c

	  call tag_E_T_binning(OK)
          if(.not.OK) then
           wtetb = wtetb + 1
           if (mod(wtetb-1,Tagger_warning).eq.0) then
             write(crmess,4) wtetb
4            format ('Problems during E & T binning occured ',I5,' times')
             CALL RECMES(CRNAME,'I',CRMESS)
           endif
          endif

c
c     Perform time correction to T counter, using the RF
c

	call tag_RF_T_correction(OK)	
        if(.not.OK) then
            wtrtc = wtrtc + 1
            if (mod(wtrtc-1,Tagger_warning).eq.0) then
             write(crmess,6) wtrtc
6            format ('Problems during RF - T time correction occured ',I5,' times')
             CALL RECMES(CRNAME,'I',CRMESS)
            endif
        endif
        		
	RETURN
      END
c
c----------------------------------------------------------------------

