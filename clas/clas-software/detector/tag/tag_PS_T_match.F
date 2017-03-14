	subroutine tag_PS_T_match(PS_match)
c
c_begin_doc
c  RCS ID string
c  $Id: tag_PS_T_match.F,v 1.2 1999/03/12 14:40:14 anciant Exp $
c
c  Documentation for subroutine tag_PS_T_match
c
c  Purpose: PS T time match
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
c  Author:   Eric Anciant : created march 1998
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

       LOGICAL PS_match ! returns if there is a match
c
c  Local pre-defined variables:
c  ---------------------------
c  RCS information: 
      CHARACTER*(*)  CFILE, CREVIS, CSTATE, CDATE, CAUTHO 
      PARAMETER (CFILE=  '$RCSfile: tag_PS_T_match.F,v $')
      PARAMETER (CREVIS= '$Revision: 1.2 $')
      PARAMETER (CSTATE= '$State: Exp $')
      PARAMETER (CDATE=  '$Date: 1999/03/12 14:40:14 $')
      PARAMETER (CAUTHO= '$Author: anciant $')
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='tag_PS_T_match')
      PARAMETER (CRAUTH='Francois Roudot')
c
c  Local tag defined variables:
c  ----------------------------
      real abs
      real windowPS(8),diff
      integer i,k

c_end_var
c
c  executable code for routine tag_PS_T_match:
c  -------------------------------------------
c
	crmess = ' '

	do i=1,8
          psTid(i) = 0
          windowPS(i) = TAG_DSD_window
        enddo

        PS_match = .false.

        do k=1,Nb_Hits_PS
	  do i=1,nb_bin_Ts
          diff = abs(Mean_T_val(i,1) + tag2tof - ps_scint_time(k) )
	  if (diff.lt.windowPS(k)) then 
		psTid(k) = bin_T_Id(i)
                windowPS(k) = diff      ! this trick to get the closest T if several whithin TAG_DSD_window
          endif
          enddo
        PS_match = (PS_match.or.psTid(k).ne.0)
        enddo

      RETURN
      END
c
c------------------------------------------------------------------------------



 
