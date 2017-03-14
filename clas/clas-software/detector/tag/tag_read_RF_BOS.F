        subroutine tag_read_RF_BOS(OK)
c
c_begin_doc
c  RCS ID string
c  $Id: tag_read_RF_BOS.F,v 1.8 1999/03/09 18:48:51 boiarino Exp $
c
c  Documentation for subroutine tag_read_RF_BOS
c
c  Purpose: 
c  --------
c
c       Reads the RF BOS bank to fill the RF common in event.CMN
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
c  Notes:
c  ------
c
c  Author:          Created:  Sat jun 11 14:07:15 EST 1997
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
*
c_end_inc
c
c_begin_var
c  input/output variables:
c  -----------------------
*
*-OUTPUT
      LOGICAL OK                !success or failure
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
     1'$Id: tag_read_RF_BOS.F,v 1.8 1999/03/09 18:48:51 boiarino Exp $'   
     2/   
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='tag_read_RF_BOS')
      PARAMETER (CRAUTH=' ')
c
c  Local User defined variables:
c  -----------------------------
c                
	integer indRF, indTRGS
	integer MLINK
        integer size 
c_end_var
c
c
c  executable code for routine tag_read_RF_BOS:
c----6----------------------------------------------------------------72
*       
	indRF = MLINK(iw,'RF  ',0)
        indTRGS=MLINK(iw,'TRGS',0)

        ok = (indrf.ne.0)

	if (ok) then
	 size = IW(indRF)
	 if (size.ne.0) then 
	  goodrf = RW(indRF + 1)
	  timedrf1 = RW(indRF + 2)
	  timedrf2 = RW(indRF + 3)
         else
          CRMESS= 'RF bank empty'
          call RECMES(CRNAME,'W',CRMESS)
          ok=.false.
         endif
        elseif (indTRGS.eq.0) then
         CRMESS= 'RF bank corrupted or absent'
         call RECMES(CRNAME,'W',CRMESS)
	endif

	return
	end		



