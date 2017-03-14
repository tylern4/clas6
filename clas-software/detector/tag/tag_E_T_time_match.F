	subroutine tag_E_T_time_match(OK,NO_E_required)
c
c_begin_doc
c  RCS ID string
c  $Id: tag_E_T_time_match.F,v 1.6 1998/09/16 08:10:39 anciant Exp $
c
c  Documentation for subroutine tag_E_T_time_match
c
c  Purpose: TAGGER supplied routined for geometric matching between E & T counters
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
c#include "bcs.inc"
#include "tag_general.PAR"
#include "event.CMN"
#include "tagcal.CMN"
c_end_inc
c
c_begin_var
c  input/output variables:
c  -----------------------
c
c  Local pre-defined variables:
c  ---------------------------
c  RCS information: 
      CHARACTER*(*)  CFILE, CREVIS, CSTATE, CDATE, CAUTHO 
      PARAMETER (CFILE=  '$RCSfile: tag_E_T_time_match.F,v $')
      PARAMETER (CREVIS= '$Revision: 1.6 $')
      PARAMETER (CSTATE= '$State: Exp $')
      PARAMETER (CDATE=  '$Date: 1998/09/16 08:10:39 $')
      PARAMETER (CAUTHO= '$Author: anciant $')
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='tag_E_T_time_match')
      PARAMETER (CRAUTH='Francois Roudot')
c
c  Local tag defined variables:
c  ----------------------------
      logical ok,flag
      integer i,j,nb_coinc_etemp,NO_E_required
      real deltaET

c_end_var
c
c  executable code for routine tag_E_T_time_match:
c  -------------------------------------
c
      crmess = ' '
      ok=.true.
     

	nb_coinc_hits = 0
	nb_coinc_ts = 0

      if (NO_E_required.eq.0) then ! normal analysis, when the E is out of coincidence, it is removed.

	do i=1,Nb_matched_Ts
	nb_coinc_etemp = 0 ! we got to check if there are E remaining at the end.
	  do j=1,NB_matched_Es(i)
	  deltaET = abs(Matched_T_L_Val(i)/2. + Matched_T_R_Val(i)/2. - Matched_E_val(i,j))
	  if (deltaET.lt.(ET_window/2.)) then
	 	if (nb_coinc_etemp.eq.0) then ! if this is the first E in coinc for this T
	  		nb_coinc_ts = nb_coinc_ts + 1 ! we got one coincidence at least.
			coinc_T_Id(nb_coinc_ts) = Matched_T_Id(i) ! we can store the values of the T.
			coinc_T_L_val(nb_coinc_ts) = Matched_T_L_Val(i)
			coinc_T_R_Val(nb_coinc_ts) = Matched_T_R_Val(i)
		endif
	  	nb_coinc_hits = nb_coinc_hits + 1 ! new hit
	  	nb_coinc_etemp = nb_coinc_etemp + 1
	  	coinc_E_Id(nb_coinc_ts,nb_coinc_etemp) = Matched_E_Id(i,j)
	  	coinc_E_val(nb_coinc_ts,nb_coinc_etemp) = Matched_E_val(i,j)
	  endif		
	  enddo

	if (nb_coinc_etemp.ne.0) NB_coinc_Es(nb_coinc_ts) = nb_coinc_etemp
	enddo

      else ! (NO_E_required.eq.0)
c now if all E's for one T are out of coincidence, this T becomes a T with no E's
c Therefore, we keep all T's whatever happens to the E's:

        nb_coinc_ts = Nb_matched_Ts ! we keep all T's whatever happens to the E's
	do i=1,Nb_coinc_Ts
	  coinc_T_Id(i) = Matched_T_Id(i)  ! we just copy T's properties.
	  coinc_T_L_val(i) = Matched_T_L_Val(i) 
	  coinc_T_R_Val(i) = Matched_T_R_Val(i)
	  nb_coinc_etemp = 0 ! counter for E's in time coincidence.
	  do j=1,NB_matched_Es(i)
	    deltaET = abs(Matched_T_L_Val(i)/2. + Matched_T_R_Val(i)/2. - Matched_E_val(i,j))
	    if (deltaET.lt.(ET_window/2.)) then
	      nb_coinc_hits = nb_coinc_hits + 1 ! New hit.
	      nb_coinc_etemp = nb_coinc_etemp + 1 
	      coinc_E_Id(i,nb_coinc_etemp) = Matched_E_Id(i,j)
	      coinc_E_val(i,nb_coinc_etemp) = Matched_E_val(i,j)
	    endif
	  enddo
          NB_coinc_Es(i) = nb_coinc_etemp
          if (nb_coinc_etemp.eq.0) nb_coinc_hits = nb_coinc_hits + 1 ! there is a hit, although no E's.
	enddo
      endif

	RETURN
      END
c
c------------------------------------------------------------------------------



 
