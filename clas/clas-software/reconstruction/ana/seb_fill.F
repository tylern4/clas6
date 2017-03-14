      SUBROUTINE SEB_FILL
c
c_begin_doc
c  RCS ID string
c  $Id: seb_fill.F,v 1.7 2008/03/16 04:33:10 fklein Exp $
c
c  Documentation for subroutine SEB_FILL
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
      PARAMETER (CFILE = '$RCSfile: seb_fill.F,v $')
      PARAMETER (CREVIS = '$Revision: 1.7 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 2008/03/16 04:33:10 $')
      PARAMETER (CAUTHO = '$Author: fklein $')
      DATA CRCSID/   
     1'$Id: seb_fill.F,v 1.7 2008/03/16 04:33:10 fklein Exp $'   
     2/   
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='SEB_FILL')
      PARAMETER (CRAUTH='Francois Roudot')
c
c  Local User defined variables:
c  -----------------------------
      INTEGER NWRITE /0/
      INTEGER ierr
c_end_var
c
c  executable code for routine SEB_FILL:
c----6----------------------------------------------------------------72
c
      IF (NWRITE .LT. 1) THEN
        NWRITE = NWRITE + 1
        CRMESS='Filling SEB Histograms, this message written once'
        CALL RECMES(CRNAME,'I',CRMESS)
      ENDIF

      IF(LSEB_HIST)THEN
c  First change PAWC directories
         call hcdir('//PAWC/SEB',' ')

c  Do your HFILLS below this line
         CALL FILL_PART_HIST
      ENDIF
      IF(.NOT.LALL_NT_DO.AND.LSEB_NT_DO)THEN
         call hcdir('//PAWC',' ')
         call hcdir('//LUN1',' ')
         ierr=1
         CALL FILL_SEB_NT(ierr)
         If(ierr.eq.0)CALL hfnt(15)
      ENDIF

c  return to the top directory and exit

      RETURN
      END
c
c------------------------------------------------------------------------------




