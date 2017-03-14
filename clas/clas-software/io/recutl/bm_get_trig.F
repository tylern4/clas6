	SUBROUTINE BM_GET_TRIG( Q_TOT_OUT, CURRENT_OUT, 
     &	                        TL_TU_OUT, iBk )
c
c_begin_doc
c  RCS ID string
c  $Id: bm_get_trig.F,v 1.2 1997/10/23 20:46:28 hovanes Exp $
c
c  Documentation for subroutine BM_GET_TRIG
c
c  Purpose:  Return beam pars  for iBk special event 
c            before current special event 
c
c  Input Parameters:  iBk
c  ----------------
c
c  Output Parameters: Q_TOT_OUT, CURRENT_OUT, TL_TU_OUT
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
c  Author:   Hovanes Egiyan      Created:  Fri Sep 26 14:01:23 EST 1997
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
c BOS common block  uncomment the next line for BOS include file
#include "bcs.inc"
c_end_inc
c
c_begin_var
c  input/output variables:
c  -----------------------
c
c  Local pre-defined variables:
c  ---------------------------
c  RCS information: 
      CHARACTER*132  CFILE, CREVIS, CSTATE, CDATE, CAUTHO, CRCSID 
      PARAMETER ( CFILE = '$RCSfile: bm_get_trig.F,v $')
      PARAMETER ( CREVIS = '$Revision: 1.2 $')
      PARAMETER ( CSTATE = '$State: Exp $')
      PARAMETER ( CDATE = '$Date: 1997/10/23 20:46:28 $')
      PARAMETER ( CAUTHO = '$Author: hovanes $')
      DATA CRCSID/
     1'$Id: bm_get_trig.F,v 1.2 1997/10/23 20:46:28 hovanes Exp $
     3'/
c  Module information:
      CHARACTER*132  CRNAME, CRAUTH
      PARAMETER (CRNAME='BM_GET_TRIG')
      PARAMETER (CRAUTH='Hovanes Egiyan')
c
C-
#include "bm_pars.inc"   
C-
      real Q_TOT_OUT, CURRENT_OUT, TL_TU_OUT
      integer iBk 
C-
      integer ix_c
C-
      character*100 CRMESS      
C-
c_end_var
c
c  executable code for routine BM_GET_TRIG:
c  ------------------------------------- 
C-
       if ( iBk .LT. ix ) then
        ix_c = ix - iBk
       else
        if (iBk .GE. ixMax .OR. (.NOT. LOOP) ) then
         CRMESS = 'TOO OLD INFO REQUESTED.  SKIPPING'
         call RECMES( CRNAME, 'W', CRMESS )
         return
        endif
        ix_c = ixMax - ( iBk - ix )
       endif
C-
       Q_TOT_OUT = Q_TOT(ix_c)
       CURRENT_OUT = CURRENT(ix_c)
       TL_TU_OUT =  TL_TU(ix_c)
C-
      RETURN
      END
c
c----------------------------------------------------------------------




