	subroutine tag_E_T_geom_match(OK,NO_E_required)
c
c_begin_doc
c  RCS ID string
c  $Id: tag_E_T_geom_match.F,v 1.5 1998/09/16 08:10:39 anciant Exp $
c
c  Documentation for subroutine tag_E_T_geom_match
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
      PARAMETER (CFILE=  '$RCSfile: tag_E_T_geom_match.F,v $')
      PARAMETER (CREVIS= '$Revision: 1.5 $')
      PARAMETER (CSTATE= '$State: Exp $')
      PARAMETER (CDATE=  '$Date: 1998/09/16 08:10:39 $')
      PARAMETER (CAUTHO= '$Author: anciant $')
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='tag_E_T_geom_match')
      PARAMETER (CRAUTH='Francois Roudot')
c
c  Local tag defined variables:
c  ----------------------------
      logical ok
      	logical flag
        integer NO_E_required,NOEHit
	integer i,j,k
	integer nbhits, nbt, nbe
	integer etcoinc(2,61),tconly(61),econly(61)	
	integer eid_t(61,MAXECTC)
	integer ec_tc(61),nbe_t(61),ec_tc_id(61,MAXECTC+1)
	
c_end_var
c
c  executable code for routine tag_E_T_geom_match:
c  -------------------------------------
	crmess = ' '
	flag = .FALSE.
      ok=.true.
    
c
c ----- reset
c
	do i=1,61
	  nbe_t(i) = 0 ! how many E's are in coincidence with the i-th T
	  ec_tc(i) = 0 ! 
	  tconly(i) = 0 !
	  econly(i) = 0 !
	enddo
c
c ----- We fill the array ec_tc for coincidences with the TC
c
	do i=1,NbGoodE ! loop over E's
 	  do j=tmin(Good_E_Id(i)),tmax(Good_E_Id(i)) ! for all T's that can be in coincidence iwht this E
	    if (nbe_t(j).lt.MAXECTC) then ! first check that the corresponding T doesn't have already too many E's
                                          ! in coincidence wiht itself
	  	nbe_t(j) = nbe_t(j) + 1 ! There is one more E that could be in coincidence with this T
	  	eid_t(j,nbe_t(j)) = i ! the Nbe_t(j)-th E in coincidence with the j-th T is stored in the
                                      ! in the i-th element of the arrays Good_E_Id, a.s.o...
	    endif
 	  enddo	
	enddo
c
c now we have an array nbe_t(j=1:61) that contains how many E's could be in coincidence with the jth T 
c further more, for each of those E's tagged by the indices j, i=1:nbe_t(j), the array
c eid_t(j,i) contains where informations of this E are stored in the arrays Good_E_Id &
c
c below we are going to sort those arrays to only keep what we could call "T candidates for a coincidence
c with E's" that is those T's for which there is at least one geometric matched E.
c

	
	j=0
	do i=1,61
	if (nbe_t(i).ne.0) then
	  j = j +1 ! we count the number of candidates
	  ec_tc(j)=i ! we store them
	  do k=1,nbe_t(i)
	    ec_tc_id(j,k)=eid_t(i,k) ! And the associated E's
	  enddo
	  if (nbe_t(i).lt.MAXECTC) ec_tc_id(j,nbe_t(i) + 1) = 0 ! the end of the array of associated E's  
                                                                ! is marked by a zero.
	endif
	enddo
	if (j.lt.61) ec_tc(j+1)=0 ! the end of the array ec_tc is also marked by a zero.
	
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c	print *,'============ T candidates from Es ======================'
c	j=1
c	do while(ec_tc(j).ne.0)
c	print *,'T candidate :',ec_tc(j)
c	i =1
c	do while(ec_tc_id(j,i).ne.0)
c		print *,'        <== (',ec_tc_id(j,i),') = E',Good_E_Id(ec_tc_id(j,i)),
c     +		'(',Good_E_val(ec_tc_id(j,i)),')'
c		i = i +1
c	enddo
c	j = j + 1
c	enddo
	
c
c ----- get coincidences between Good T's and T candidates from E
c

        call tag_coincidence(61,Good_T_Id,ec_tc,etcoinc,tconly,econly)
c
c the previous routine compares the values in the arrays Good_T_Id and ec_tc
c and sort them in the following, if a value is both in Good_T_Id and ec_tc,
c its index in Good_T_Id is put in etcoinc(1,i), and its index in ec_tc is put in etcoinc(2,i)
c if this value is only present in Good_T_ID, its index is put in tconly
c if this value is only present in ec_tc, its index is put in econly
c
       


c	
c ----- Now we fill TagETmatch array
c

         if (NO_E_required.eq.0) then ! for speed purpose, split the case when E are always required (default)

           nbt = 0
           nbhits = 0
           i=1
           do while(etcoinc(1,i).ne.0.and.i.le.61.and.nbhits.le.Tag_max_hit) ! remaining hits in coincidence.
	     nbt = nbt + 1 ! one more T's 
             Matched_T_Id(nbt) = Good_T_Id(etcoinc(1,i)) 
             Matched_T_L_val(nbt) = Good_T_L_val(etcoinc(1,i))
             Matched_T_R_Val(nbt) = Good_T_R_val(etcoinc(1,i))
             j=1
             nbe = 0
	     do while (j.le.MAXECTC.and.ec_tc_id(etcoinc(2,i),j).ne.0) ! Store the corresponding E's
	       nbe = nbe + 1 ! one more E for this given T
	       nbhits = nbhits + 1 ! one more hit
	       Matched_E_Id(nbt,nbe)  = Good_E_Id(ec_tc_id(etcoinc(2,i),j))
	       Matched_E_val(nbt,nbe) = Good_E_val(ec_tc_id(etcoinc(2,i),j))
	       j = j + 1
	     enddo
	   NB_matched_Es(nbt) = nbe
	   i = i + 1
	   enddo
	 
           NB_matched_Ts = nbt
           Nb_matched_Hits = nbhits	

        else ! here we don't require a E for a good coincidence.

          nbt = 0
          nbhits = 0
          i=1
          k=1
          do while(etcoinc(1,i).ne.0.and.i.le.61.and.nbhits.le.Tag_max_hit) ! E_T coincidence hits available.
	     nbt = nbt + 1 ! one more T's - whatever
c
c Let's check if the next hit to consider is really a T-E coincidence or a T alone:
c
            NoEHit = 0 ! by default next hit is a E-T coincidence
            if (k.le.61.and.tconly(k).ne.0) then ! is there a NO-E hit available 
              if (Good_T_Id(tconly(k)).lt.Good_T_Id(etcoinc(1,i))) NoEHit = 1
                      ! for this NO-E hit is the T id lower ?
            endif

            if (NoEHit.eq.0) then ! if the next is with a E coincidence, same algorithm than before:
              Matched_T_Id(nbt) = Good_T_Id(etcoinc(1,i)) 
              Matched_T_L_val(nbt) = Good_T_L_val(etcoinc(1,i))
              Matched_T_R_Val(nbt) = Good_T_R_val(etcoinc(1,i))

              j=1
	      nbe = 0
	      do while (j.le.MAXECTC.and.ec_tc_id(etcoinc(2,i),j).ne.0) ! Store the corresponding E's
	        nbe = nbe + 1 ! one more E for this given T
	        nbhits = nbhits + 1 ! one more hit
	        Matched_E_Id(nbt,nbe)  = Good_E_Id(ec_tc_id(etcoinc(2,i),j))
	        Matched_E_val(nbt,nbe) = Good_E_val(ec_tc_id(etcoinc(2,i),j))
	        j = j + 1
	      enddo
	      NB_matched_Es(nbt) = nbe
	      i = i + 1
            else ! (NoEHit.eq.0) => special alorithm if no E
              Matched_T_Id(nbt) = Good_T_Id(tconly(k)) 
              Matched_T_L_val(nbt) = Good_T_L_val(tconly(k))
              Matched_T_R_Val(nbt) = Good_T_R_val(tconly(k))
	      NB_matched_Es(nbt) = 0 ! no corresponding E's
	      nbhits = nbhits + 1 ! one more hit
              k = k + 1
            endif
	  enddo
c
c We must finish to fill with possible hits with no E's:
c
          do while (k.le.61.and.tconly(k).ne.0) ! some  more NO-E hits available :
            nbt = nbt + 1
            Matched_T_Id(nbt) = Good_T_Id(tconly(k)) 
            Matched_T_L_val(nbt) = Good_T_L_val(tconly(k))
            Matched_T_R_Val(nbt) = Good_T_R_val(tconly(k))
	    NB_matched_Es(nbt) = 0 ! no corresponding E's
	    nbhits = nbhits + 1 ! one more hit
            k = k + 1
          enddo
	 
	  NB_matched_Ts = nbt
          Nb_matched_Hits = nbhits	
        
        endif ! (NO_E_required.eq.0)
	
c	if (flag) call tag_debug(1,2)
			
      RETURN
      END
      
