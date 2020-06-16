        subroutine tag_read_PC_BOS(OK,PC_event,Need_TDC)
c
c_begin_doc
c  RCS ID string
c  $Id: tag_read_PC_BOS.F,v 1.5 1999/03/09 18:48:50 boiarino Exp $
c
c  Documentation for subroutine tag_read_PC_BOS
c
c  Purpose: 
c  --------
c
c       Reads raw data from DSPC BOS bank and fills the DSD
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
c  Notes:	Created: 5 fev 1998 
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
      LOGICAL PC_event          ! returns if there are PC BOS bank in the event?
      integer Need_TDC          ! Do we accept events without TDC s ?
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
     1'$Id: tag_read_PC_BOS.F,v 1.5 1999/03/09 18:48:50 boiarino Exp $'   
     2/   
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='tag_read_PC_BOS')
      PARAMETER (CRAUTH=' ')
c
c  Local User defined variables:
c  -----------------------------
c        
        integer size,size4
        
	integer idpc
	integer MLINK 
c_end_var
c
c
c  executable code for routine tag_read_DSD_BOS:
c----6----------------------------------------------------------------72
*

      idpc  = MLINK(iw,DSPCData_BOS_name,0)

      OK=.true.

        IF(idpc.le.0) then ! empty bank
          PC_event = .false.
          RETURN
        ENDIF

        size4=IW(idpc)                      !size in [I*4] words
        size= 2*size4                       !size in [I*2] words

        if (size.eq.0) then 	!  there is nothing in the bank 
          PC_event = .false.
        else ! there is something
c
c-BOS only "knows" fullwords; odd number of half words were rounded up
c-(the remaining halfword is wasted)
c
          IF( MOD(size-1,DSPCData_BOS_size).EQ.0 ) THEN
            size= size-1                      !BOS was forced to round up
          ENDIF
        
          OK= size.GE.DSPCData_BOS_size .and. MOD(size,DSPCData_BOS_size).EQ.0

          IF(.NOT.OK) THEN
            witrpcb = witrpcb + 1
            if (mod(witrpcb-1,Tagger_warning).eq.0) then
             write(crmess,1) DSPCData_BOS_name,witrpcb
1            format (a4,' :malformed bank occured ',I5,' times')
             CALL RECMES(CRNAME,'I',CRMESS)
            endif
           RETURN
          ENDIF
	  pc_tdc = iw16(2*idpc + 2)	! <----- in the common DSDraw
	  pc_adc_mn = iw16(2*idpc + 3)	! <----- in the common DSDraw
	  pc_adc_lt = iw16(2*idpc + 4)	! <----- in the common DSDraw
	  pc_adc_rb = iw16(2*idpc + 5)	! <----- in the common DSDraw
	  pc_adc_lb = iw16(2*idpc + 6)	! <----- in the common DSDraw
	  pc_adc_rt = iw16(2*idpc + 7)	! <----- in the common DSDraw
	  pc_adc_ve = iw16(2*idpc + 8)	! <----- in the common DSDraw
          
          if (Need_TDC.eq.1) then
            PC_event = (pc_tdc.ne.0)
          else
            PC_event = .true.
          endif

	endif

	return
	end		
