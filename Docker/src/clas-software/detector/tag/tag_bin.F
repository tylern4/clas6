	integer function tag_bin(channel,nbhit)
c
c_begin_doc
c  RCS ID string
c  $Id: tag_bin.F,v 1.1 1998/02/11 18:34:16 anciant Exp $
c
c  Documentation for function tag_bin
c
c  Purpose: TAGGER supplied routine for binning E & T counters
c			(single hit, or adjacent counter)
c  --------
c
c  Calling Sequence:
c  ----------------
c
c  Input Parameters:  channel # of the 1st counter & number of counter hit (1 or 2)
c  ----------------
c
c  Output Parameters:  function = corresponding bin
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
      integer channel,nbhit
c
c  Local pre-defined variables:
c  ---------------------------
c  RCS information: 
      CHARACTER*(*)  CFILE, CREVIS, CSTATE, CDATE, CAUTHO 
      PARAMETER (CFILE=  '$RCSfile: tag_bin.F,v $')
      PARAMETER (CREVIS= '$Revision: 1.1 $')
      PARAMETER (CSTATE= '$State: Exp $')
      PARAMETER (CDATE=  '$Date: 1998/02/11 18:34:16 $')
      PARAMETER (CAUTHO= '$Author: anciant $')
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='tag_E_T_geom_match')
      PARAMETER (CRAUTH='Eric ANCIANT')
c
c  Local tag defined variables:
c  ----------------------------

c_end_var
c
c  executable code for routine tag_bin:
c  -------------------------------------

      tag_bin = 2 * channel - 2 + nbhit
      return
      end                             
 
c
c----------------------------------------------------------------------
