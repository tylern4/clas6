      SUBROUTINE EC1TCL_INIT
c
c_begin_doc
c  Documentation for subroutine EC1TCL_INIT
c
c  Purpose:  initialize runtime control variables for LAC package
c  --------
c
c  Input Parameters: none
c  ----------------
c
c  Output Parameters: none
c  -----------------
c_end_doc
c
      IMPLICIT NONE
      SAVE
c  RCS information: 
      CHARACTER*132  CFILE, CREVIS, CSTATE, CDATE, CAUTHO, CRCSID
      PARAMETER (CFILE = '$rcsRCSfile$')
      PARAMETER (CREVIS = '$rcsRevision$')
      PARAMETER (CSTATE = '$rcsState$')
      PARAMETER (CDATE = '$rcsDate$')
      PARAMETER (CAUTHO = '$rcsAuthor$')
      DATA CRCSID/   
     1'$Id: ec1tcl_init.F,v 1.4 1998/04/02 20:14:26 manak Exp $'   
     2/   
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='EC1_TCL_INIT')
      PARAMETER (CRAUTH='Arne Freyberger')
c
c_begin_inc
c  include files :
c  ---------------------
c BOS common block  uncomment the next line for BOS include file
#include "ec1_control.inc"
c_end_inc
c
c_begin_var
c  input/output variables:
c  -----------------------
c
c  -----------------------------
       INTEGER NWRITE
       integer intcl_double,intcl_int
       data intcl_double/2/
       data intcl_int/1/
*
*
*********************** end: Ec1_controls.DTE ****************************
c_end_var
c
c  executable code for routine EC1TCL_INIT:
c----6----------------------------------------------------------------72
c
      IF (NWRITE .LT. 1) THEN
        NWRITE = NWRITE + 1
        CRMESS='EC1_INIT_TCL initialization'
        CALL RECMES(CRNAME,'I',CRMESS)
      ENDIF
c
c	set lac defaults
c       
      call ec1_set_def()

c 
      call tcl_linkvar('ec1_reco_type',
     1      ec1_reco_type,intcl_int)
      call tcl_linkvar('ec1Maxclust',
     1      ec1Maxclust,intcl_int)
      call tcl_linkvar('ec1_calib',
     1      ec1_calib,intcl_int)
c
c        doubles
c
      call tcl_linkvar('ec1_threshold',
     1      ec1_threshold,intcl_double)
      call tcl_linkvar('ec1_timeresol',
     1      ec1_timeresol,intcl_double)
      call tcl_linkvar('ec1stenmin_in',
     1      ec1stenmin_in,intcl_double)
       call tcl_linkvar('ec1stenmin_out',
     1      ec1stenmin_out,intcl_double)
c
c
      RETURN
      END
c
c------------------------------------------------------------------------------




