      SUBROUTINE EC_FILL
c
c_begin_doc
c  RCS ID string
c  $Id: ec_fill.F,v 1.4 1999/06/09 19:35:32 stepanya Exp $
c
c  Documentation for subroutine EC_FILL
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
c  Author:   Francois Roudot      Created:  Thu May 29 10:00:00 EDT 1997
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
#include "histtcl.inc"
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
      PARAMETER (CFILE = '$RCSfile: ec_fill.F,v $')
      PARAMETER (CREVIS = '$Revision: 1.4 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 1999/06/09 19:35:32 $')
      PARAMETER (CAUTHO = '$Author: stepanya $')
      DATA CRCSID/   
     1'$Id: ec_fill.F,v 1.4 1999/06/09 19:35:32 stepanya Exp $'   
     2/   
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='EC_FILL')
      PARAMETER (CRAUTH='Francois Roudot')
c
c  Local User defined variables:
c  -----------------------------
      INTEGER NWRITE
      INTEGER ierr
c_end_var
c
c  executable code for routine EC_FILL:
c----6----------------------------------------------------------------72
c
      IF (NWRITE .LT. 1) THEN
        NWRITE = NWRITE + 1
        CRMESS='Filling EGN Histograms, this message written once'
        CALL RECMES(CRNAME,'I',CRMESS)
      ENDIF

c  First change PAWC directories
      call hcdir('//PAWC/EC',' ')
c Fill histograms or N-tupls
      IF(LFEC_HIST)THEN
        CALL FILL_EC_HIST
      ENDIF
      IF(LECHB_NT_DO)THEN
        ierr=1
        CALL FILL_ECHB_NT(ierr)
        If(ierr.eq.0)CALL hfnt(14)
      ENDIF
c  return to the top directory and exit
      call hcdir('//PAWC',' ')

      RETURN
      END
c
c------------------------------------------------------------------------------




