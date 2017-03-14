	real function tag_deltaE(ebin)
c
c_begin_doc
c  RCS ID string
c  $Id: tag_deltae.F,v 1.2 1998/06/25 15:23:04 anciant Exp $
c
c  Documentation for function tag_deltae
c
c  Purpose: TAGGER supplied routine for getting the delta E of a given E bin
c
c  --------
c
c  Calling Sequence:
c  ----------------
c
c  Input Parameters:  E counter bin
c
c  Output Parameters:  function = delta E
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
c  Author:   Eric ANCIANT 		created:  jun 24, 1998
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
#include "tagcal.CMN"
#include "tagRunControl.CMN"
c_end_inc
c
c_begin_var
c  input/output variables:
c  -----------------------
      integer ebin
c
c  Local pre-defined variables:
c  ---------------------------
c  RCS information: 
      CHARACTER*(*)  CFILE, CREVIS, CSTATE, CDATE, CAUTHO 
      PARAMETER (CFILE=  '$RCSfile: tag_deltae.F,v $')
      PARAMETER (CREVIS= '$Revision: 1.2 $')
      PARAMETER (CSTATE= '$State: Exp $')
      PARAMETER (CDATE=  '$Date: 1998/06/25 15:23:04 $')
      PARAMETER (CAUTHO= '$Author: anciant $')
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='tag_deltaE')
      PARAMETER (CRAUTH='Eric ANCIANT')
c
c  Local tag defined variables:
c  ----------------------------

c_end_var
c
c  executable code for routine tag_bin:
c  -------------------------------------

      tag_deltae = Edeviation(ebin) * Beam_energy(1) / 1000. ! to have the energy in Gev
      return
      end                             
 
c
c----------------------------------------------------------------------
