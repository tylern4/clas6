	integer function tag_unbin(bin)
c
c_begin_doc
c  RCS ID string
c  $Id: tag_unbin.F,v 1.1 1998/02/11 18:34:17 anciant Exp $
c
c  Documentation for function tag_unbin
c
c  Purpose: TAGGER supplied routine for un-binning E & T counters
c
c  --------
c
c  Calling Sequence:
c  ----------------
c
c  Input Parameters:  bin #
c  ----------------
c
c  Output Parameters: channel # of the first counter
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
c  Author:   Eric ANCIANT 		created:  Thu Jun 19 12:00:00 EST 1997
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
c_end_inc
c
c_begin_var
c  input/output variables:
c  -----------------------
      integer bin
c
c  Local pre-defined variables:
c  ---------------------------
c  RCS information: 
      CHARACTER*(*)  CFILE, CREVIS, CSTATE, CDATE, CAUTHO 
      PARAMETER (CFILE=  '$RCSfile: tag_unbin.F,v $')
      PARAMETER (CREVIS= '$Revision: 1.1 $')
      PARAMETER (CSTATE= '$State: Exp $')
      PARAMETER (CDATE=  '$Date: 1998/02/11 18:34:17 $')
      PARAMETER (CAUTHO= '$Author: anciant $')
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='tag_unbin')
      PARAMETER (CRAUTH='Eric ANCIANT')
c
c  Local tag defined variables:
c  ----------------------------

c_end_var
c
c  executable code for routine tag_bin:
c  -------------------------------------

      tag_unbin = (bin + 1)/2
      return
      end                             
 
c
c----------------------------------------------------------------------
