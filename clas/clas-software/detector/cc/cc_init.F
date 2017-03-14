      SUBROUTINE CC_INIT
c
c_begin_doc
c  RCS ID string
c  $Id: cc_init.F,v 1.5 2004/02/12 22:04:35 vlassov Exp $
c
c  Documentation for subroutine CC_INIT
c
c  Purpose:   Initialization for CC package, reading the calibration file
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
c  Author:   Alexander Vlassov    Created:  Fri Jun 21 17:55:08 EDT 1996
c  -------
c
c  Major revisions:
c
c   A. Vlassov Feb 12, 2004
c Skip the calibration reading from a file,
c should be in cc_brun program.
c  ------------------------------------------
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
#include "cccalb.inc"
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
      PARAMETER (CFILE = '$RCSfile: cc_init.F,v $')
      PARAMETER (CREVIS = '$Revision: 1.5 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 2004/02/12 22:04:35 $')
      PARAMETER (CAUTHO = '$Author: vlassov $')
      DATA CRCSID/   
     1'$Id: cc_init.F,v 1.5 2004/02/12 22:04:35 vlassov Exp $'   
     2/   
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='CC_INIT')
      PARAMETER (CRAUTH='Alexander Vlassov')
c
c  Local User defined variables:
c  -----------------------------
      character*80 filename
      INTEGER i,LUNC,ires,isect
c
c_end_var
c
c  executable code for routine CC_INIT:
c----6----------------------------------------------------------------72
c
      CRMESS='CC_INIT is called **** !'
c
      CALL RECMES(CRNAME,'I',CRMESS)
      CALL SET_LOG(.TRUE.)
c
c  Register my CC package with job database....
c
      CALL REVINI('CCRC',CREVIS,CDATE)
c
c     Reading calibration constants is in cc_brun.
c
c
      CRMESS='CC_INIT ends !'
      CALL RECMES(CRNAME,'I',CRMESS)
c
      RETURN
      END
c
c-----------------------------------------------------------------------


