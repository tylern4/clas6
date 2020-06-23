      SUBROUTINE EID_TCL_INIT
c
c_begin_doc
c  RCS ID string
c  $Id: eid_tcl_init.F,v 1.2 2000/12/13 22:38:22 fklein Exp $
c
c  Documentation for subroutine CC_TCL_INIT
c
c  Purpose:   Initialize tcl variables for Fast electron
c  ========   identification programm
c
c  Input Parameters:  NONE
c  ----------------
c
c  Output Parameters: Tcl variable defined.
c  -----------------
c
c  Author:   Alexander Vlassov    Created:  Mon Nov  4 12:11:53 EST 1996
c  -------
c
c_end_doc
c
      IMPLICIT NONE
      SAVE
c
c_begin_inc
c  include files :
c  ---------------------
#include "eidcontr.inc"
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
      PARAMETER (CFILE = '$RCSfile: eid_tcl_init.F,v $')
      PARAMETER (CREVIS = '$Revision: 1.2 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 2000/12/13 22:38:22 $')
      PARAMETER (CAUTHO = '$Author: fklein $')
      DATA CRCSID/   
     1'$Id: eid_tcl_init.F,v 1.2 2000/12/13 22:38:22 fklein Exp $'   
     2/   
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='EID_TCL_INIT')
      PARAMETER (CRAUTH='Alexander Vlassov')
c
c  Local User defined variables:
c  -----------------------------
      integer itype
c_end_var
c
c  executable code for routine EID_TCL_INIT:
c----6----------------------------------------------------------------72
c
      CRMESS='EID_init_TCL was called !!!'
      CALL RECMES(CRNAME,'I',CRMESS)
c
c     =========================================
      itype = 1   ! Integer read and write
      call tcl_linkvar('CCecthr',CCecthr,itype)
      call tcl_linkvar('CCscthr',CCscthr,itype)
      call tcl_linkvar('CCscecy',CCscecy,itype)
      call tcl_linkvar('CCscecz',CCscecz,itype)
      call tcl_linkvar('CCicoor',CCicoor,itype)
      call tcl_linkvar('CCscpn' ,CCscpn ,itype)
c     =========================================
c
      RETURN
      END
c
c-----------------------------------------------------------------------




