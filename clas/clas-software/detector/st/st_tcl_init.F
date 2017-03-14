      SUBROUTINE ST_TCL_INIT
c
c_begin_doc
c  RCS ID string
c  $Id: st_tcl_init.F,v 1.7 2008/03/16 04:33:24 fklein Exp $
c
c  Documentation for subroutine ST_TCL_INIT
c
c  Purpose:  ST routine for initializing runtime variables
c  --------
c
c  Input Parameters:  none
c  ----------------
c
c  Output Parameters:  
c  -----------------
c
c  Other routines:
c  ---------------
c
c  Notes:
c  ------
c
c  Author:   Francois Roudot      Created:  Thu Jun 19 12:00:00 EST 1997
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
c
#include "sttcl.CMN"
c_end_inc
c
c_begin_var
c  input/output variables:
c  -----------------------
c
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
     1'$Id: st_tcl_init.F,v 1.7 2008/03/16 04:33:24 fklein Exp $'   
     2/   
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='ST_TCL_INIT')
      PARAMETER (CRAUTH='Francois Roudot')
c
c  Local st defined variables:
c  ----------------------------
      INTEGER NWRITE

      INTEGER ITCL_INT, ITCL_STRING, ITCL_FLOAT, ITCL_INT_READ
      DATA ITCL_INT, ITCL_FLOAT, ITCL_STRING, ITCL_INT_READ/1,2,4,129/
c_end_var
c
c  executable code for routine STTCL_INIT:
c----------------------------------------
c
      crmess = '  '
      IF (NWRITE .LT. 1) THEN
        NWRITE = NWRITE + 1
        CRMESS='Start counter TCL init routine'
        CALL RECMES(CRNAME,'I',CRMESS)
      endif
      
      call st_set_def
c
c
      call tcl_linkvar('st_time_resolution',st_time_resolution,itcl_float)
      st_type=-1
      call tcl_linkvar('st_type',st_type,itcl_int)
c

      RETURN
      END
c
c------------------------------------------------------------------------------




