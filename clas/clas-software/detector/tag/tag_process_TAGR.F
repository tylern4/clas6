	subroutine tag_process_TAGR(OK,TAGT_event,TAGE_event)
c
c_begin_doc
c  RCS ID string
c  $Id: tag_process_TAGR.F,v 1.11 2007/05/28 19:17:27 hovanes Exp $
c
c  Documentation for subroutine tag_process_TAGR
c
c  Purpose: TAGGER supplied routined for forming the TAGR bank 
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
c  temporary definition:
c
c                bit 1  (1)  : left T present
c                bit 2  (2)  : right T present
c                bit 3  (4)  : ecounter present
c                bit 4  (8)  : more than one hit in the Event.
c                bit 5  (16) : 2nd hit associated with a Triple T coincidence
c                             i.e. 3 adjacent T's were hit, the 2 first of them are
c                             gathered into an even T bin, the last one is recorded as an
c                             extra hit in an odd T bin. This second hit is therefore flagged.
c                             THIS SECOND HIT SHOULD BE IGNORED, because very likely there was 
c                             only one electron there.
ccccccccccccccccccc  this status bit also used to ignore second hit(T20)  in T18,20 combination
c                    which if they are in time is just a single hit. 
c
c                bit 6  (32) : extra hits in the E binning in the following cases:
c                              - More than 2 adjacent E's fired: the 3rd (sometimes 4th) E 
c                              is (are) reconstructed as extra hit(s), while they are likely to come
c                              from the same SINGLE electrons, therefore, those extra hits
c                              are flagged and should be ignored.
c                              - One E fired more than once inside the coincidence window with
c                              the T counter. It can be either electronic reflections, or
c                              2 electrons. In any case, only one hit is recorded in the T counter,
c                              so no usefull information can be extracted from this second hit,
c                              and it should be therefore ignored.
c                        in both cases, THOSE HITS SHOULD BE IGNORED.
c
c                bit 7  (64) : more than one E-binned hits are associated with one T counter hit.
c                             There is therefore an uncertainty about the photon energy.
c                             Those cases should be analysed with special care.
c                bit 8 (256) : overflow
c
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
#include "tagRunControl.CMN"
#include "event.CMN"
#include "tagcal.CMN"
#include "tagwarnings.CMN"
#include "tagtcl.CMN"
c_end_inc
c
c_begin_var
c  input/output variables:
c  -----------------------

      LOGICAL OK
      logical TAGT_event,TAGE_event
      integer tag_nbc_bin ! external function
c
c  Local pre-defined variables:
c  ---------------------------
c  RCS information: 
      CHARACTER*(*)  CFILE, CREVIS, CSTATE, CDATE, CAUTHO 
      PARAMETER (CFILE=  '$RCSfile: tag_process_TAGR.F,v $')
      PARAMETER (CREVIS= '$Revision: 1.11 $')
      PARAMETER (CSTATE= '$State: Exp $')
      PARAMETER (CDATE=  '$Date: 2007/05/28 19:17:27 $')
      PARAMETER (CAUTHO= '$Author: hovanes $')
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='tag_PROCESS_TAGR.F')
      PARAMETER (CRAUTH='Eric Anciant')
c
c  Local tag defined variables:
c  ----------------------------
      integer i,j,EventStatus,TCStatus(MAXTC),NBacceptedE

c_end_var
c
c  executable code for routine tag_process_TAGR
c  --------------------------------------------
c
	crmess = ' '


	Nb_TAGR_hits = 0
        EventStatus = 0 ! overall event flags contains more than one hit flag, and overflow flag.
                        ! and also the left / right T flags, because this is mandatory at this point

c        TCStatus(i) contain Tcounter flag: triple T coincidence, and several E's associated with it.


	if (TAGT_event) then
	   eventstatus=3	! bit pattern is left/right at least.
	   do i=1,nb_bin_Ts
c       
c       TC flags:
c       
	      TCStatus(i) = 0
c------------------------------------------------------
c		 if (bin_T_id(i).eq.38) then 
c		    bin_T_id(i) = 37
c		 endif
c-------------------------------------------------------
	      if (i.gt.1) then
c       now lot's look if T18 and T20 are involved here, they require special treatment
c       Lets check then if there is T19,20 coincidence and reassign bin from 38 to 37
		 if(bin_T_Id(i).eq.39)then 
		    if (bin_T_Id(i-1).eq.bin_T_Id(i)-3) then
c       first check if there is tripple coincidence T18,19,20
		       if (abs(T_time(i-1,1)-T_time(i,1)).lt.(ADJ_T_COINC/2.)) then
			  TCStatus(i) = 16 
		       endif
c       tripple T coincidence 18-20
		    else if(bin_T_Id(i-1).eq.bin_T_Id(i)-4) then
c       then check if there is T18,20 coincidence
		       if (abs(T_time(i-1,1)-T_time(i,1)).lt.(ADJ_T_COINC/2.)) then
			  TCStatus(i) = 16 
c       18,20 coincidence, ignore 20 		  
		       endif
		    endif
		 endif
c-------------------------------------------------------
c       none of the above: standard procedure to check for tripple coincidence
		 if (bin_T_Id(i-1).eq.bin_T_Id(i)-3) then
		    if (abs(T_time(i-1,1)-T_time(i,1)).lt.(ADJ_T_COINC/2.)) then
		       TCStatus(i) = 16 
		    endif
c       tripple T coincidence
		 endif
	      endif
c-------------------------------------------------------       
	      NBacceptedE = NB_bin_Es(i)
c       
c       first case, there are E hits:
c       
	      do j=1,NB_bin_Es(i)
		 if (EventStatus.lt.256) then ! if we are not overflowed...
		    if (Nb_TAGR_hits.ge.MAXTAGR) then ! we check first if we are not getting overflowed !
		       witpt = witpt + 1
		       if (mod(witpt-1,Tagger_warning).eq.0) then
			  write(crmess,10) Nb_TAGR_hits,MAXTAGR,witpt
 10			  format (I4,' hits reconstructed in the Tagger overflowing the hits array  size',I5,
     1		       ' occured ',I5,' times')
			  CALL RECMES(CRNAME,'W',CRMESS)
		       endif
		       Eventstatus = Eventstatus + 256 ! we just got overflowed !!
		    else	! we are not overflowed !!
		       Nb_TAGR_hits = Nb_TAGR_hits + 1
		       photonEnergy(NB_TAGR_hits) = Eaverage(bin_E_Id(i,j)) * Beam_energy(1) / 1000. ! to have the energy in Gev
		       TimeEvent(NB_TAGR_hits) = Mean_T_val(i,1) + tag2tof
		       TimeEventcor(NB_TAGR_hits) = T_time(i,1) + tag2tof
		       TAGR_TID(NB_TAGR_hits) = bin_T_Id(i)
		       TAGR_EID(NB_TAGR_hits) = bin_E_Id(i,j)
c       
c       Now we construct the hit flag:
c       
		       HitStatus(NB_TAGR_hits) = 4 ! First of all, there are E's in coincidence
		       if (j.gt.1) then
			  if (bin_E_Id(i,j)-bin_E_Id(i,j-1).le.4) then
				! multiple hit in one E, or 3 adjacent E's, or 4 adjacent E's
			     if (bin_E_Id(i,j)-bin_E_Id(i,j-1).le.3.or.tag_nbc_bin(bin_E_Id(i,j)).eq.2) then
				! if the difference is 4 and the bins are odd this is 2 non-adjacent E's
				! (but diff=4 and the bins are even, this is 4 adjacents E's)
				HitStatus(NB_TAGR_hits) = HitStatus(NB_TAGR_hits) + 32 ! hit flag
				NBacceptedE = NBacceptedE - 1 ! for the T flag.
			     endif
			  endif
		       endif
		    endif	! Nb_TAGR_hits within limits
		 endif		! (EventStatus.lt.256) (overflow)
	      enddo		! j loop
c       
c       TC flags again.
c       
	      if (NBacceptedE.gt.1) TCStatus(i) = TCStatus(i) + 64 ! more than one E hit associated with this T counter.

c       
c       second case: there are no E hits...
c       
	      if (NB_bin_Es(i).eq.0) then
		 if (EventStatus.lt.256) then ! if we are not overflowed 
		    if (Nb_TAGR_hits.ge.MAXTAGR) then ! are we getting overflowed ?
		       witpt = witpt + 1 ! yes, problem...
		       if (mod(witpt-1,Tagger_warning).eq.0) then
			  write(crmess,10) Nb_TAGR_hits,MAXTAGR,witpt
			  CALL RECMES(CRNAME,'W',CRMESS)
		       endif
		       Eventstatus = Eventstatus + 256 
		    else	! we are not overflowed !
		       Nb_TAGR_hits = Nb_TAGR_hits + 1
		       photonEnergy(NB_TAGR_hits) = 0. ! Not defined.
		       TimeEvent(NB_TAGR_hits) = Mean_T_val(i,1) + tag2tof
		       TimeEventcor(NB_TAGR_hits) = T_time(i,1) + tag2tof
		       TAGR_TID(NB_TAGR_hits) = bin_T_Id(i)
		       TAGR_EID(NB_TAGR_hits) = 0
		       HitStatus(NB_TAGR_hits) = 0 ! hit flag is no E
		    endif	! (Nb_TAGR_hits.ge.MAXTAGR)
		 endif		! (EventStatus.lt.256)
	      endif		! (NB_bin_Es(i).eq.0)

	   enddo		! i (T loop )


	   if (Nb_TAGR_hits.gt.1) Eventstatus = Eventstatus + 8 ! if more than one hit
c       
c       generates HitStatus which is the combinaison of hit status, T counter status, and Event status:
c       
	   Nb_TAGR_hits = 0
	   do i=1,nb_bin_Ts
	      do j=1,NB_bin_Es(i)
		 Nb_TAGR_hits = Nb_TAGR_hits + 1
		 HitStatus(Nb_TAGR_hits) = HitStatus(Nb_TAGR_hits) + TCStatus(i) + EventStatus
	      enddo
	      if (NB_bin_Es(i).eq.0) then
		 Nb_TAGR_hits = Nb_TAGR_hits + 1
		 HitStatus(Nb_TAGR_hits) = HitStatus(Nb_TAGR_hits) + TCStatus(i) + EventStatus
	      endif
	   enddo

	endif			! (TAGT_event)

      RETURN
      END
c
c------------------------------------------------------------------------------



 
