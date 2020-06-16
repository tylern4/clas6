      SUBROUTINE IC_TCL_INIT
c
c_begin_doc
c  RCS ID string
c  $Id: ic_tcl_init.F,v 1.5 2004/06/29 16:19:47 clasdvcs Exp $
c
c  Documentation for subroutine USER_TCL_INIT
c
c  Purpose:  routine for initializing IC tcl
c  --------
c
c  Input Parameters:  none
c  ----------------
c
c  Output Parameters:  none
c  -----------------
c
c  Other routines:
c  ---------------
c
c  Notes:
c  ------
c
c  Author:   Arne Freyberger      Created:  Fri Dec  1 17:04:06 EST 1995
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
#include "ictcl.inc"
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
      PARAMETER (CFILE = '$rcsRCSfile$')
      PARAMETER (CREVIS = '$rcsRevision$')
      PARAMETER (CSTATE = '$rcsState$')
      PARAMETER (CDATE = '$rcsDate$')
      PARAMETER (CAUTHO = '$rcsAuthor$')
      DATA CRCSID/   
     1'$Id: ic_tcl_init.F,v 1.5 2004/06/29 16:19:47 clasdvcs Exp $'   
     2/   
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='IC_TCL_INIT')
      PARAMETER (CRAUTH='Stepan Stepanyan')
c
c  Local User defined variables:
c  -----------------------------
      INTEGER NWRITE, i, ix, iy
      INTEGER ITCL_INT, ITCL_STRING, ITCL_INT_READ, LSTRING, ITCL_DOUBLE
      DATA ITCL_INT, ITCL_DOUBLE/1,2/
c_end_var
c
c  executable code for routine USER_TCL_INIT:
c----6----------------------------------------------------------------72
      ehitmin=0.005
      call tcl_linkvar('ehitmin',ehitmin,itcl_double)
      egrplmin=0.05
      call tcl_linkvar('egrplmin',egrplmin,itcl_double)
      egrpmin=0.05
      call tcl_linkvar('egrpmin',egrpmin,itcl_double)
      rdist=1.9
      call tcl_linkvar('rdist',rdist,itcl_double)
      w0=3.1
      call tcl_linkvar('w0',w0,itcl_double)
c
      ihalf=23
      call tcl_linkvar('ihalf',ihalf,itcl_int)
      iswap=0
      call tcl_linkvar('iswap',iswap,itcl_int)

      RETURN
      END
c
c------------------------------------------------------------------------------




