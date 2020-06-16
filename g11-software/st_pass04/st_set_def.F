      SUBROUTINE ST_SET_DEF
c
c_begin_doc
c  RCS ID string
c  $Id: st_set_def.F,v 1.2 1998/05/04 13:32:09 burin Exp $
c
c  Documentation for subroutine ST_SET_DEF
c
c  Purpose: set default TCL var. for ST
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
c  Author:   Burin Asavapibhop      Created:  Sat Feb 21 12:03:57 EST 1998
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
#include "sttcl.CMN"
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
      PARAMETER (CFILE = '$RCSfile: st_set_def.F,v $')
      PARAMETER (CREVIS = '$Revision: 1.2 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 1998/05/04 13:32:09 $')
      PARAMETER (CAUTHO = '$Author: burin $')
      DATA CRCSID/   
     1'$Id: st_set_def.F,v 1.2 1998/05/04 13:32:09 burin Exp $'   
     2/   
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='ST_SET_DEF')
      PARAMETER (CRAUTH='Burin Asavapibhop')
c
c  Local User defined variables:
c  -----------------------------
      INTEGER NWRITE
c_end_var
c
c  executable code for routine ST_SET_DEF:
c----6----------------------------------------------------------------72
c
      IF (NWRITE .LT. 1) THEN
        NWRITE = NWRITE + 1
        CRMESS='Setting default tcl for ST'
        CALL RECMES(CRNAME,'I',CRMESS)
      ENDIF
      
c
c      st_time_resolution=0.5    ! not used so far Feb98
c
      RETURN
      END
c
c------------------------------------------------------------------------------




