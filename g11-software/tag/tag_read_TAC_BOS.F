        subroutine tag_read_TAC_BOS(OK,TAC_event,Need_TDC)
c
c_begin_doc
c  RCS ID string
c  $Id: tag_read_TAC_BOS.F,v 1.6 1999/03/09 18:48:51 boiarino Exp $
c
c  Documentation for subroutine tag_read_TAC_BOS
c
c  Purpose: 
c  --------
c
c       Reads raw data from DSTC BOS bank and fills the DSD
c       common.
c
c  Input Parameters:  (Name - Type - Meaning)
c  ----------------
c
c  Output Parameters:  (Name - Type - Meaning)
c  -----------------
c
c  Other routines:
c  ---------------
c
c  Notes:	Created: march 1998 
c  ------
c
c  Author:      E. Anciant
c  -------
c
c  Major revisions:
c  ----------------    
c
c_end_doc
c
      IMPLICIT NONE
      SAVE
c
c_begin_inc
c  include files :
c  ---------------------
* 
c BOS common block  uncomment the next line for BOS include file
*      
#include "bcs.inc"
#include "tag_general.PAR"
#include "event.CMN"
#include "tagwarnings.CMN"
#include "tagtcl.CMN"
*
c_end_inc
c
c_begin_var
c  input/output variables:
c  -----------------------
*
*-OUTPUT
      LOGICAL OK                !success or failure
      LOGICAL TAC_event         ! returns if there are DSD BOS bank in the event?
      integer Need_TDC                  ! do we need a TDC hit to consider there is an event ?
c
c  Local pre-defined variables:
c  ---------------------------
c  RCS information: 
      CHARACTER*132  CFILE, CREVIS, CSTATE, CDATE, CAUTHO, CRCSID
      PARAMETER (CFILE = '$rcsRCSfile$')
      PARAMETER (CREVIS = '$rcsRevision$')
      PARAMETER (CSTATE = '$rcsState$')
      PARAMETER (CDATE = '$rcsDate$')
      PARAMETER (CAUTHO = '$rcsAuthor$')
      DATA CRCSID/   
     1'$Id: tag_read_TAC_BOS.F,v 1.6 1999/03/09 18:48:51 boiarino Exp $'   
     2/   
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='tag_read_TAC_BOS')
      PARAMETER (CRAUTH=' ')
c
c  Local User defined variables:
c  -----------------------------
c        
        integer size,size4
        
	integer idtac
	integer MLINK 
c_end_var
c
c
c  executable code for routine tag_read_TAC_BOS:
c----6----------------------------------------------------------------72
*

	idtac = MLINK(iw,DSTCData_BOS_name,0)

	OK=.true.

        IF(idtac.le.0) then
          TAC_event = .false.
          RETURN
        ENDIF

	size4=IW(idtac)                      !size in [I*4] words
        size= 2*size4                       !size in [I*2] words

	if (size.ne.0) then 	! if there is a TAC event
*
*-BOS only "knows" fullwords; odd number of half words were rounded up
*-(the remaining halfword is wasted)
          IF( MOD(size-1,DSTCData_BOS_size).EQ.0 ) THEN
            size= size-1                      !BOS was forced to round up
          ENDIF
        
          OK= size.GE.DSTCData_BOS_size .and. MOD(size,DSTCData_BOS_size).EQ.0

          IF(.NOT.OK) THEN
            witrtcb = witrtcb + 1
            if (mod(witrtcb-1,Tagger_warning).eq.0) then
             write(crmess,1) DSTCData_BOS_name,witrtcb
1            format (a4,' :malformed bank occured ',I5,' times')
             CALL RECMES(CRNAME,'I',CRMESS)
            endif
           RETURN
          ENDIF

          tac_id = iw16(2*idtac + 1)            ! <----- in the common DSDraw
          tac_tdc = iw16(2*idtac + 2)	        ! <----- in the common DSDraw
          tac_adc_lt = iw16(2*idtac + 3)	! <----- in the common DSDraw
          tac_adc_rt = iw16(2*idtac + 4)	! <----- in the common DSDraw
          tac_adc_lb = iw16(2*idtac + 5)	! <----- in the common DSDraw
          tac_adc_rb = iw16(2*idtac + 6)	! <----- in the common DSDraw
          tac_adc_sum1 = iw16(2*idtac + 7)	! <----- in the common DSDraw
          tac_adc_sum2 = iw16(2*idtac + 8)	! <----- in the common DSDraw
          tac_adc_sum3 = iw16(2*idtac + 9)	! <----- in the common DSDraw

          if (Need_TDC.eq.1) then
            TAC_event = (tac_tdc.ne.0)
          else
            TAC_event = .true.
          endif

        else ! if the bank is empty.
          TAC_event = .false.
	endif	    		 

	return
	end		
