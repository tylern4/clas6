	subroutine tag_PC_T_Match(PC_match)
c
c_begin_doc
c  RCS ID string
c  $Id: tag_PC_T_match.F,v 1.2 1999/03/12 14:40:14 anciant Exp $
c
c  Documentation for subroutine tag_PC_T_match
c
c  Purpose: TAGGER supplied routined for normalisation
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
c  Author:   Eric Anciant : created jan, 1998
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
#include "event.CMN"
#include "tagcal.CMN"     
c_end_inc
c
c_begin_var
c  input/output variables:
c  -----------------------

       LOGICAL PC_match ! returns if there is a PC - T match
c
c  Local pre-defined variables:
c  ---------------------------
c  RCS information: 
      CHARACTER*(*)  CFILE, CREVIS, CSTATE, CDATE, CAUTHO 
      PARAMETER (CFILE=  '$RCSfile: tag_PC_T_match.F,v $')
      PARAMETER (CREVIS= '$Revision: 1.2 $')
      PARAMETER (CSTATE= '$State: Exp $')
      PARAMETER (CDATE=  '$Date: 1999/03/12 14:40:14 $')
      PARAMETER (CAUTHO= '$Author: anciant $')
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='tag_E_T_time_match')
      PARAMETER (CRAUTH='Francois Roudot')
c
c  Local tag defined variables:
c  ----------------------------
      real abs
      real windowPC,diff
      integer i

c_end_var
c
c  executable code for routine tag_PC_T_match
c  -------------------------------------------
c
	crmess = ' '

        pcTid = 0
        windowPC = TAG_DSD_window

	do i=1,nb_bin_Ts
          diff = abs(Mean_T_val(i,1) + tag2tof - pc_time)
	  if (diff.lt.windowPC) then 
		pcTid = bin_T_Id(i)
                windowPC = diff      ! this trick to get the closest T if several whithin TAG_DSD_window
          endif
       enddo

      PC_match = (pcTid.ne.0)

      RETURN
      END
c
c------------------------------------------------------------------------------



 
