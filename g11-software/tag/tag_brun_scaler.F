	subroutine tag_brun_scaler(OK)
c
c_begin_doc
c  RCS ID string
c  $Id: tag_brun_scaler.F,v 1.1 1998/04/17 13:26:06 auger Exp $
c
c  Documentation for subroutine tag_brun_scaler
c
c  Purpose: creates permanent(over the run) scaler BOS banks used to cumulate scaler events.
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
c  Called from: tag_brun.F
c  ------------
c
c  Other routines:
c  ---------------
c
c  Notes:
c  ------
c
c  Author:   Eric Anciant : created feb, 1998
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
#include "wcs.inc"
c_end_inc
c
c_begin_var
c  input/output variables:
c  -----------------------
c
	logical ok
c
c  Local pre-defined variables:
c  ---------------------------
c  RCS information: 
      CHARACTER*(*)  CFILE, CREVIS, CSTATE, CDATE, CAUTHO 
      PARAMETER (CFILE=  '$RCSfile: tag_brun_scaler.F,v $')
      PARAMETER (CREVIS= '$Revision: 1.1 $')
      PARAMETER (CSTATE= '$State: Exp $')
      PARAMETER (CDATE=  '$Date: 1998/04/17 13:26:06 $')
      PARAMETER (CAUTHO= '$Author: auger $')
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='tag_brun_scaler')
      PARAMETER (CRAUTH='Francois Roudot')
c
c  Local tag defined variables:
c  ----------------------------
      integer ind, MBANK, MLINK, i

c_end_var
c
c  executable code for routine tag_brun_scaler
c  --------------------------------------------------
c
	crmess = ' '

        ind = MBANK(JW,'RTSL',0,64,1)
	ok = ind.ne.0
	if (.not.ok) then
	  crmess = ' NOT able to create permanent RTSL BOS bank'
	  call recmes(CRNAME,'F',CRMESS)
	  return
	endif
c
c       fill it with zero just to be sure
c
        ind = MLINK(JW,'RTSL',0)

        do i=1,64
         JW(ind+i) = 0
        enddo
c
c
        ind = MBANK(JW,'G1SL',0,64,1)
	ok = ind.ne.0
	if (.not.ok) then
	  crmess = ' NOT able to create permanent G1SL BOS bank'
	  call recmes(CRNAME,'F',crmess)
	  return
	endif
c
c       fill it with zero just to be sure
c
        ind = MLINK(JW,'G1SL',0)

        do i=1,64
         JW(ind+i) = 0
        enddo
c
c
        ind = MBANK(JW,'G2SL',0,64,1)
	ok = ind.ne.0
	if (.not.ok) then
	  crmess = ' NOT able to create permanent G2SL BOS bank'
	  call recmes(CRNAME,'F',crmess)
	  return
	endif
c
c       fill it with zero just to be sure
c
        ind = MLINK(JW,'G2SL',0)

        do i=1,64
         JW(ind+i) = 0
        enddo
c
c
        ind = MBANK(JW,'G3SL',0,64,1)
	ok = ind.ne.0
	if (.not.ok) then
	  crmess = ' NOT able to create permanent G3SL BOS bank'
	  call recmes(CRNAME,'F',crmess)
	  return
	endif
c
c       fill it with zero just to be sure
c
        ind = MLINK(JW,'G3SL',0)

        do i=1,64
         JW(ind+i) = 0
        enddo
c
c
        ind = MBANK(JW,'G4SL',0,64,1)
	ok = ind.ne.0
	if (.not.ok) then
	  crmess = ' NOT able to create permanent G4SL BOS bank'
	  call recmes(CRNAME,'F',crmess)
	  return
	endif
c
c       fill it with zero just to be sure
c
        ind = MLINK(JW,'G4SL',0)

        do i=1,64
         JW(ind+i) = 0
        enddo
c
c
	
      RETURN
      END
c
c------------------------------------------------------------------------------



 
