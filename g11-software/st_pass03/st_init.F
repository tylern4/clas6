      SUBROUTINE ST_INIT
c
c_begin_doc
c  RCS ID string
c  $Id: st_init.F,v 1.6 2001/08/08 15:40:56 liji Exp $
c
c  Documentation for subroutine ST_INIT
c
c  Purpose:  ST routine called ONCE at initialization time
c  --------  
c
c  Input Parameters: NONE
c  ----------------
c
c  Output Parameters: NONE
c  -----------------
c
c  Other routines:
c  ---------------
c
c  Notes:
c  ------
c
c  Author:   Francois Roudot      Created:  Thu Jun 19 12:00:00 EDT 1997
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
      PARAMETER ( CFILE = '$RCSfile: st_init.F,v $')
      PARAMETER ( CREVIS = '$Revision: 1.6 $')
      PARAMETER ( CSTATE = '$State: Exp $')
      PARAMETER ( CDATE = '$Date: 2001/08/08 15:40:56 $')
      PARAMETER ( CAUTHO = '$Author: liji $')
      DATA CRCSID/
     1'$Id: st_init.F,v 1.6 2001/08/08 15:40:56 liji Exp $
     3'/
c  Module information:
      CHARACTER*132  CRNAME, CRAUTH
      CHARACTER*132  CRMESS
      PARAMETER (CRNAME='ST_INIT')
      PARAMETER (CRAUTH='Francois Roudot')
c
c  Local St defined variables:
c  -----------------------------
        INTEGER NWRITE
c_end_var
c
c  executable code for routine ST_INIT:
c  -------------------------------------
      IF (NWRITE .LT. 1) THEN
        NWRITE = NWRITE + 1
        CRMESS='This is the start counter init routine'
        CALL RECMES(CRNAME,'I',CRMESS)
      ENDIF

c  Register my ST package with job database....

      CALL REVINI('ST  ',CREVIS,CDATE)
c
c     set some start counter specific geometric quantities
c
c      call stgeom_initialize(irun)
c
      RETURN
      END
c
c------------------------------------------------------------------------------





