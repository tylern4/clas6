	subroutine st_reset_arrays
c
c_begin_doc
c  RCS ID string
c  $Id: st_reset_arrays.F,v 1.6 2004/06/22 00:16:24 devita Exp $
c
c  Documentation for subroutine st_reset_arrays
c
c  Purpose: 
c  --------
c	Initializes all elements of ST internal arrays to zero.
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
c
#include "st_general.PAR"
#include "sthit.CMN"
#include "sttrk.CMN"
#include "stconvert.CMN"
#include "stfit.CMN"
c_end_inc
c
c_begin_var
c  input/output variables:
c  -----------------------
c      
c
c  Local pre-defined variables:
c  ---------------------------
c  RCS information: 
      CHARACTER*(*)  CFILE, CREVIS, CSTATE, CDATE, CAUTHO 
      PARAMETER (CFILE=  '$RCSfile: st_reset_arrays.F,v $')
      PARAMETER (CREVIS= '$Revision: 1.6 $')
      PARAMETER (CSTATE= '$State: Exp $')
      PARAMETER (CDATE=  '$Date: 2004/06/22 00:16:24 $')
      PARAMETER (CAUTHO= '$Author: devita $')
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='st_reset_arrays')
      PARAMETER (CRAUTH='Francois Roudot')
c
c  Local st defined variables:
c  ----------------------------
      integer i,j,k
c_end_var
c
c  executable code for routine st_reset_arrays:
c  ------------------------------------
c
      crmess = '  '
      

      do i=1,St_max_pair
	  do j=1,4
	    StFastbus_data(i,j)=0
	    StCorrected_data(i,j)=0.
	  enddo
	  do j=1,2 
	    Ntrk_hit_ST(i,j)=0
	    ntrk_hit_leg(i,j)=0
	    ntrk_hit_nose(i,j)=0
	  enddo
	  StSecHit(i)=0
	  StFoundHit(i)=0
	  StTrknumber(i)=0
	  StStatus(i)=0
	  Nhit_ST(i)=0
	  Ntrk_hit(i)=0
	  iside_ST(i)=0
	  do k=1,St_max_hit
	    l_st(k,i)=0.
	    StTrkSector(k,i)=0
	    StTrkLength(k,i)=0.
	    StWhich_pairid(k)=0
	    StWhichTrk(k,i)=0
	    StWhichSector(k,i)=0
	    StWhichTrkLength(k,i)=0
	    StFit_pos_indep_time(k)=0.
	    StFit_sector(k)=0
	    StFit_length(k)=0.
	    StNconvertedHits(i)=0
	  enddo  
      enddo

      StNHits=0
      StNfit = 0
      
      return
      end
c
c
c------------------------------------------------------------------------------
