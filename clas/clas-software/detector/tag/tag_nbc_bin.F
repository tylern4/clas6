	integer function tag_nbc_bin(bin)
c
c_begin_doc
c  RCS ID string
c  $Id: tag_nbc_bin.F,v 1.1 1998/02/11 18:34:17 anciant Exp $
c
c  Documentation for function tag_nbc_bin
c
c  Purpose: TAGGER supplied routine giving the number of counter (nbc) hit for one bin
c		practically parity function: even bin => 2
c					     odd bin => 1
c
c  --------
c
c  Calling Sequence:
c  ----------------
c
c  Input Parameters:  bin #
c  ----------------
c
c  Output Parameters: number of counter (nbc)
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
c  Author:   Eric ANCIANT 		created: feb 11 1998
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
      PARAMETER (CFILE=  '$RCSfile: tag_nbc_bin.F,v $')
      PARAMETER (CREVIS= '$Revision: 1.1 $')
      PARAMETER (CSTATE= '$State: Exp $')
      PARAMETER (CDATE=  '$Date: 1998/02/11 18:34:17 $')
      PARAMETER (CAUTHO= '$Author: anciant $')
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='tag_nbc_bin')
      PARAMETER (CRAUTH='Eric ANCIANT')
c
c  Local tag defined variables:
c  ----------------------------

c_end_var
c
c  executable code for routine tag_bin:
c  -------------------------------------

      tag_nbc_bin = bin/2*2-bin + 2

      return
      end                             
 
c
c----------------------------------------------------------------------
