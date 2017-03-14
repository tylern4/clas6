      SUBROUTINE tag_fetch_to_BOS(OK,TAGT_event,TAGE_event)
c
c_begin_doc
c  RCS ID string
c  $Id: tag_fetch_to_BOS.F,v 1.9 1999/03/09 18:48:48 boiarino Exp $
c
c  Documentation for subroutine tag_fetch_to_BOS
c
c  Purpose: 
c  --------
c
c  Calling Sequence:
c  ----------------
c
c  Input Parameters:  NONE
c  ----------------
c
c  Output Parameters:  NONE
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
c  Author:   Francois ROUDOT      Created:  Thu Jun 19 12:00:00 EST 1997
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
#include "bcs.inc"
#include "tag_general.PAR"
#include "event.CMN"
#include "tagcal.CMN"
c_end_inc
c
c_begin_var
c  input/output variables:
c  -----------------------

      LOGICAL OK,TAGT_event,TAGE_event
c      
c
c  Local pre-defined variables:
c  ---------------------------
c  RCS information: 
      CHARACTER*(*)  CFILE, CREVIS, CSTATE, CDATE, CAUTHO 
      PARAMETER (CFILE=  '$RCSfile: tag_fetch_to_BOS.F,v $')
      PARAMETER (CREVIS= '$Revision: 1.9 $')
      PARAMETER (CSTATE= '$State: Exp $')
      PARAMETER (CDATE=  '$Date: 1999/03/09 18:48:48 $')
      PARAMETER (CAUTHO= '$Author: boiarino $')
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='tag_fetch_to_BOS')
      PARAMETER (CRAUTH='Francois Roudot')
c
c  Local st defined variables:
c  ----------------------------
      integer MBANK,ind,i,j
c_end_var
c
c  executable code for routine tag_fetch_to_BOS:
c  ---------------------------------------------
c
      ok=.true.
c      

      if (TAGT_event) then
	ind = MBANK(iw,TagR_BOS_name,1,TagR_BOS_size,Nb_TAGR_hits)
        do i = 1, Nb_TAGR_hits
	  j = (i-1) * TagR_BOS_size
          RW(ind+j+1) = PhotonEnergy(i)
          RW(ind+j+2) = TimeEvent(i)
          RW(ind+j+3) = TimeEventcor(i)
	  IW(ind+j+4) = HitStatus(i)
	  IW(ind+j+5) = TAGR_TID(i)
	  IW(ind+j+6) = TAGR_EID(i)
        enddo
      else ! (TAGT_event)
	ind = MBANK(iw,TagR_BOS_name,1,TagR_BOS_size,0)
      endif

      RETURN
      END
c
c------------------------------------------------------------------------------




