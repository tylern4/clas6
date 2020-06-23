      SUBROUTINE TAG_FILL_ERUN(do_hist,do_tagr_nt,do_tagirf_nt,do_dsd_nt)
c
c_begin_doc
c  RCS ID string
c  $Id: tag_fill_erun.F,v 1.1 1998/05/30 21:13:35 anciant Exp $
c
c  Documentation for subroutine TAG_FILL_ERUN
c
c  Purpose:
c  --------
c
c  Input Parameters:  (Name - Type - Meaning)
c  ----------------
c
c  Output Parameters:  (Name - Type - Meaning)
c  -----------------
c
c  Other routines:
c  ---------------
c
c  Notes:
c  ------
c
c  Author:   Eric Anciant      Created:  may 15th 1998
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
#include "wcs.inc"
#include "tag_general.PAR"

c_end_inc
c
c_begin_var
c  input/output variables:
c  -----------------------
c
      logical do_hist,do_tagr_nt,do_tagirf_nt,do_dsd_nt

c  Local pre-defined variables:
c  ---------------------------
c  RCS information: 
      CHARACTER*132  CFILE, CREVIS, CSTATE, CDATE, CAUTHO, CRCSID
      PARAMETER (CFILE = '$RCSfile: tag_fill_erun.F,v $')
      PARAMETER (CREVIS = '$Revision: 1.1 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 1998/05/30 21:13:35 $')
      PARAMETER (CAUTHO = '$Author: anciant $')
      DATA CRCSID/   
     1'$Id: tag_fill_erun.F,v 1.1 1998/05/30 21:13:35 anciant Exp $'   
     2/   
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='TAG_FILL_ERUN')
      PARAMETER (CRAUTH='Eric Anciant')
c
c  Local User defined variables:
c  -----------------------------
      INTEGER NWRITE,i
      integer MLINK ,ind

c_end_var
c
c  executable code for routine TAG_FILL_ERUN:
c----6----------------------------------------------------------------72
c
      IF (NWRITE .LT. 1) THEN
        NWRITE = NWRITE + 1
        CRMESS='Filling Tagger Histograms for end of run (scalers)'
        CALL RECMES(CRNAME,'I',CRMESS)
      ENDIF


       if (do_hist) then
c  First change PAWC directories
         call hcdir('//PAWC/TAG',' ')

c  Do your HFILLS below this line
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c ---- get the RTSL (raw Tcounter scaler bank):

         ind = MLINK(JW,'RTSL',0)
	 if(ind.eq.0) return
         do i=1,64
           call hf1(105,float(i),float(JW(ind+i)))
         enddo

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c ---- get the G1SL (gated Tcounter scaler bank - set 1 odd):
   
         ind = MLINK(JW,'G1SL',0)
	 if(ind.eq.0) return
         do i=1,64
           call hf1(106,float(i),float(JW(ind+i)))
         enddo

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c ---- get the G2SL (gated Tcounter scaler bank - set 1 even):
   
         ind = MLINK(JW,'G2SL',0)
	 if(ind.eq.0) return
         do i=1,64
           call hf1(107,float(i),float(JW(ind+i)))
         enddo

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c ---- get the G3SL (gated Tcounter scaler bank - set 2 odd):
   
         ind = MLINK(JW,'G3SL',0)
	 if(ind.eq.0) return
         do i=1,64
           call hf1(108,float(i),float(JW(ind+i)))
         enddo

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c ---- get the G4SL (gated Tcounter scaler bank - set 2 even):
   
         ind = MLINK(JW,'G4SL',0)
	 if(ind.eq.0) return
         do i=1,64
           call hf1(109,float(i),float(JW(ind+i)))
         enddo

       endif


c  return to the top directory and exit
      call hcdir('//PAWC',' ')

      RETURN
      END
c
c------------------------------------------------------------------------------




