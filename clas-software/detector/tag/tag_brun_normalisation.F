	subroutine tag_brun_normalisation
c
c_begin_doc
c  RCS ID string
c  $Id: tag_brun_normalisation.F,v 1.1 1998/02/19 10:13:47 anciant Exp $
c
c  Documentation for subroutine tag_brun_normalisation
c
c  Purpose: Begin of run normalisation routine (resets normalisation arrays)
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
c#include "bcs.inc"
#include "normalisation.CMN"
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
      PARAMETER (CFILE=  '$RCSfile: tag_brun_normalisation.F,v $')
      PARAMETER (CREVIS= '$Revision: 1.1 $')
      PARAMETER (CSTATE= '$State: Exp $')
      PARAMETER (CDATE=  '$Date: 1998/02/19 10:13:47 $')
      PARAMETER (CAUTHO= '$Author: anciant $')
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='tag_E_T_time_match')
      PARAMETER (CRAUTH='Francois Roudot')
c
c  Local tag defined variables:
c  ----------------------------
      integer i,j

c_end_var
c
c  executable code for routine tag_brun_normalisation
c  --------------------------------------------------
c
	crmess = ' '

       numberofevents = 0

       do i=1,767
       do j=1,MAXTCECMAP
        hitraw(i,j) = 0
        hitST(i,j) = 0
        hitSTXtra(i,j) = 0
        hitPC(i,j) = 0
        hitPCXtra(i,j) = 0
        hitPS(i,j) = 0
        hitPSXtra(i,j) = 0
        hitTAC(i,j) = 0
        hitTACXtra(i,j) = 0
       enddo
       enddo

       do i=1,61
         scalerT(i) = 0
         scalerPC(i) = 0
         scalerPCdelayed(i) = 0
         scalerPS(i) = 0
         scalerPSdelayed(i) = 0
         scalerTAC(i) = 0
       enddo
         
      RETURN
      END
c
c------------------------------------------------------------------------------



 
