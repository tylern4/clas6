      SUBROUTINE USER_FILL
c
c_begin_doc
c  RCS ID string
c  $Id: user_fill.F,v 1.4 2004/05/01 14:55:13 stepanya Exp $
c
c  Documentation for subroutine USER_FILL
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
c#include "bcs.inc"
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
      PARAMETER (CFILE = '$RCSfile: user_fill.F,v $')
      PARAMETER (CREVIS = '$Revision: 1.4 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 2004/05/01 14:55:13 $')
      PARAMETER (CAUTHO = '$Author: stepanya $')
      DATA CRCSID/   
     1'$Id: user_fill.F,v 1.4 2004/05/01 14:55:13 stepanya Exp $'   
     2/   
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='USER_FILL')
      PARAMETER (CRAUTH='Francois Roudot')
c
c  Local User defined variables:
c  -----------------------------
      INTEGER NWRITE,ierr
c_end_var
c
c  executable code for routine USER_FILL:
c----6----------------------------------------------------------------72
c
      IF (NWRITE .LT. 1) THEN
        NWRITE = NWRITE + 1
        CRMESS='Filling USER Histograms, this message written once'
        CALL RECMES(CRNAME,'I',CRMESS)
      ENDIF
c  First change PAWC directories
      call hcdir('//PAWC/USER',' ')

c  Do your HFILLS below this line


c  return to the top directory and exit
      if(lusr1_h_do)then
         call hcdir('//PAWC',' ')
         call hcdir('//LUN1',' ')
         CALL FILL_TAGR_NT(ierr)
         call fill_msd_nt(ierr)
         ierr=0
         call fill_epics_nt(ierr)
         IF(ierr.gt.0)CALL hfnt(77)
      endif
      RETURN
      END
c
c------------------------------------------------------------------------------




