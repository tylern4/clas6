      SUBROUTINE TAG_FILL_BRUN(do_hist,do_tagr_nt,do_tagirf_nt,do_dsd_nt)
c
c_begin_doc
c  RCS ID string
c  $Id: tag_fill_brun.F,v 1.3 2008/03/16 04:33:10 fklein Exp $
c
c  Documentation for subroutine TAG_FILL_BRUN
c
c  Purpose: Reset the scaler histo to 0 at the beginning of each RUN
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
      PARAMETER (CFILE = '$RCSfile: tag_fill_brun.F,v $')
      PARAMETER (CREVIS = '$Revision: 1.3 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 2008/03/16 04:33:10 $')
      PARAMETER (CAUTHO = '$Author: fklein $')
      DATA CRCSID/   
     1'$Id: tag_fill_brun.F,v 1.3 2008/03/16 04:33:10 fklein Exp $'   
     2/   
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='TAG_FILL_BRUN')
      PARAMETER (CRAUTH='Eric Anciant')
c
c  Local User defined variables:
c  -----------------------------
      INTEGER NWRITE
      LOGICAL hexist

c_end_var
c
c  executable code for routine TAG_FILL_ERUN:
c----6----------------------------------------------------------------72
c
      IF (NWRITE .LT. 1) THEN
        NWRITE = NWRITE + 1
        CRMESS='Resetting Tagger scaler Histograms for begin of run'
        CALL RECMES(CRNAME,'I',CRMESS)
      ENDIF
c  First change PAWC directories
      call hcdir('//PAWC/TAG',' ')

c  Do your HFILLS below this line


       if (do_hist) then
          if(hexist(105)) call hreset(105,' ')
          if(hexist(106)) call hreset(106,' ')
          if(hexist(107)) call hreset(107,' ')
          if(hexist(108)) call hreset(108,' ')
          if(hexist(109)) call hreset(109,' ')
       endif


c  return to the top directory and exit
      call hcdir('//PAWC',' ')

      RETURN
      END
c
c------------------------------------------------------------------------------




