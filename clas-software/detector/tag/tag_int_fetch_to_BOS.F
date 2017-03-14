      SUBROUTINE tag_Int_fetch_to_BOS(OK,TAGT_event,TAGE_event)
c
c_begin_doc
c  RCS ID string
c  $Id: tag_int_fetch_to_BOS.F,v 1.14 1999/03/09 18:48:49 boiarino Exp $
c
c  Documentation for subroutine tag_int_fetch_to_BOS
c
c  Purpose:  Put in TAGI BOS bank the matching analysis results
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
#include "tagtcl.CMN"
#include "tagwarnings.CMN"
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
      PARAMETER (CFILE=  '$RCSfile: tag_int_fetch_to_BOS.F,v $')
      PARAMETER (CREVIS= '$Revision: 1.14 $')
      PARAMETER (CSTATE= '$State: Exp $')
      PARAMETER (CDATE=  '$Date: 1999/03/09 18:48:49 $')
      PARAMETER (CAUTHO= '$Author: boiarino $')
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='tag_int_fetch_to_BOS')
      PARAMETER (CRAUTH='Francois Roudot')
c
c  Local st defined variables:
c  ----------------------------
      integer MBANK
      external MBANK
      integer ind,i,j,k,nbhits,NB_entries
      integer tag_nbc_bin
      real MOR_time
c_end_var
c
c  executable code for routine tag_int_fetch_to_BOS:
c  ------------------------------------
c
	crmess = ' '
c

        if (.not.TAGT_event) then
          ind = MBANK(iw,TagIData_BOS_name,1,TagIData_BOS_size,0)
        else
          NB_entries = Nb_bin_Hits
          ind =
     &     MBANK(iw,TagIData_BOS_name,1,TagIData_BOS_size,NB_entries)
          nbhits = 0
          do i=1,Nb_bin_Ts
c
c first case, there are E's in coincidence:
c
          RF_T_diff(i,1) = Mean_T_val(i,1) - T_time(i,1)
          if (tag_nbc_bin(bin_T_Id(i)).eq.2)  RF_T_diff(i,2) = Mean_T_val(i,2) - T_time(i,2)

              do k=1,NB_bin_Es(i)
              nbhits = nbhits + 1
              j = (nbhits-1)*TagIData_BOS_size
              IW(ind+j+1) = bin_T_Id(i)
              RW(ind+j+2) = bin_T_L_val(i,1)
              RW(ind+j+3) = bin_T_R_val(i,1)
              IW(ind+j+4) = bin_E_Id(i,k)
              RW(ind+j+5) = bin_E_val(i,k,1)
              RW(ind+j+6) = Mean_T_val(i,1)
              RW(ind+j+7) = RF_T_diff(i,1)
              RW(ind+j+8) = 0 
              if (tag_nbc_bin(bin_T_Id(i)).eq.2) RW(ind+j+8) = Mean_T_val(i,1) - Mean_T_val(i,2)     
              enddo
c
c second case there are no E in coincidence:
c
             if (NB_bin_Es(i).eq.0) then
              nbhits = nbhits + 1
              j = (nbhits-1)*TagIData_BOS_size
              IW(ind+j+1) = bin_T_Id(i)
              RW(ind+j+2) = bin_T_L_val(i,1)
              RW(ind+j+3) = bin_T_R_val(i,1)
              IW(ind+j+4) = 0
              RW(ind+j+5) = 0
              RW(ind+j+6) = Mean_T_val(i,1)
              RW(ind+j+7) = RF_T_diff(i,1)
              RW(ind+j+8) = 0 
              if (tag_nbc_bin(bin_T_Id(i)).eq.2) RW(ind+j+8) = Mean_T_val(i,1) - Mean_T_val(i,2)
             endif

          enddo ! i (T counter loop)
        endif ! (TAGT_event)
	
	OK = .true.
        if (nbhits.ne.Nb_bin_Hits) then
            witiftb = witiftb + 1
            if (mod(witiftb-1,Tagger_warning).eq.0) then
              write(crmess,2) nbhits,Nb_bin_Hits
2             format ('Mismatch ',I4,
     +          ' events written to the TAGI Bank and ',I4,'events')
             CALL RECMES(CRNAME,'I',CRMESS)
             write(crmess,5) witiftb
5            format ('this problem occured ',I5,' times')
             CALL RECMES(CRNAME,'I',CRMESS)
            endif
        endif

      RETURN
      END
c
c------------------------------------------------------------------------------




