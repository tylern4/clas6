        subroutine tag_read_PS_BOS(OK,PS_event,Need_TDC)
c
c_begin_doc
c  RCS ID string
c  $Id: tag_read_PS_BOS.F,v 1.7 1999/05/20 14:06:32 anciant Exp $
c
c  Documentation for subroutine tag_read_PS_BOS
c
c  Purpose: 
c  --------
c
c       Reads raw data from DSPS BOS bank and fills the DSD
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
      LOGICAL PS_event          ! returns if there are DSD BOS bank in the event?
      integer Need_TDC          ! do we need a hit in the TDC to consider there is an event ?
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
     1'$Id: tag_read_PS_BOS.F,v 1.7 1999/05/20 14:06:32 anciant Exp $'   
     2/   
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='tag_read_PS_BOS')
      PARAMETER (CRAUTH=' ')
c
c  Local User defined variables:
c  -----------------------------
c        
        integer size,size4
        
	integer idps
	integer i,ii,j,PS_TDC_hit
	integer MLINK 
c_end_var
c
c
c  executable code for routine tag_read_PS_BOS:
c----6----------------------------------------------------------------72
*
	idps  = MLINK(iw,DSPSData_BOS_name,0)

	OK=.true.

        IF(idps.le.0) then
          PS_event = .false.
          RETURN
        ENDIF

        size4=IW(idps)                      !size in [I*4] words
        size= 2*size4                       !size in [I*2] words
	
	
	if (size.eq.0) then 	! there is nothing
          PS_event = .false.
        else
*
*-BOS only "knows" fullwords; odd number of half words were rounded up
*-(the remaining halfword is wasted)
          IF( MOD(size-1,DSPSData_BOS_size).EQ.0 ) THEN
            size= size-1                      !BOS was forced to round up
          ENDIF
        
          OK= size.GE.DSPSData_BOS_size .and. MOD(size,DSPSData_BOS_size).EQ.0

          IF(.NOT.OK) THEN
            witrpsb = witrpsb + 1
            if (mod(witrpsb-1,Tagger_warning).eq.0) then
             write(crmess,1) DSPSData_BOS_name,witrpsb
1            format (a4,' :malformed bank occured ',I5,' times')
             CALL RECMES(CRNAME,'I',CRMESS)
            endif
           RETURN
          ENDIF

	  Nb_Hits_PS = size/DSPSData_BOS_size
          PS_TDC_hit = 0
          ii = 0
          if (Need_TDC.eq.1) then
             do i = 1,Nb_Hits_PS
                j=(i-1)*DSPSData_BOS_size
                if (iw16(2*idps + 1 +j).ne.0) then
                   if (iw16(2*idps + 2 +J).ne.0) then ! (is there a TDC hit ?)
                      ii = ii + 1
                      ps_id( ii )  = iw16(2*idps + 1 +j) ! <----- in the common DSDraw
                      ps_tdc( ii ) = iw16(2*idps + 2 +J) ! <----- in the common DSDraw
                      ps_adc( ii ) = iw16(2*idps + 3 +J) ! <----- in the common DSDraw
                   endif
                else
                   call recmes(crname,'W','Wrong PS Id')
                endif
             enddo
          else
             do i = 1,Nb_Hits_PS
                j=(i-1)*DSPSData_BOS_size
                if (iw16(2*idps + 1 +j).ne.0) then              
                   ii = ii + 1
                   ps_id( ii )  = iw16(2*idps + 1 +j) ! <----- in the common DSDraw
                   ps_tdc( ii ) = iw16(2*idps + 2 +J) ! <----- in the common DSDraw
                   ps_adc( ii ) = iw16(2*idps + 3 +J) ! <----- in the common DSDraw
                   PS_TDC_hit = PS_TDC_hit + ps_tdc( ii )
                else
                   call recmes(crname,'W','Wrong PS Id')
                endif
             enddo
          endif
          
          Nb_Hits_PS = ii
          PS_event = (ii.gt.0)
          do i = (Nb_Hits_PS + 1),8 ! safety
             ps_id( i )  = 0    ! <----- in the common DSDraw
             ps_tdc( i ) = 0    ! <----- in the common DSDraw
             ps_adc( i ) = 0    ! <----- in the common DSDraw
          enddo
       endif
       
       return
       end		
