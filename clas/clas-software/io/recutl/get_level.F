      SUBROUTINE GET_LEVEL(LINFO,LWARN,LDEBUG,LERROR,LFATAL)
c
c_begin_doc
c  RCS ID string
c  $Id: get_level.F,v 1.1.1.1 1997/03/28 20:58:23 marki Exp $
c
c  Documentation for subroutine GET_LEVEL
c
c  Purpose: returns the logicals for level control of the messaging
c  --------
c
c  Input Parameters:  NONE
c  ----------------
c
c  OUTPUT Parameters:  LINFO  - L - TRUE route INFO messages
c  ----------------   LWARN  - L - TRUE route WARNING messages
c                     LDEBUG - L - TRUE route DEBUG messages
c                     LERROR - L - TRUE route ERROR messages
c                     LFATAL - L - TRUE route FATAL messages
c
c  Other routines:
c  ---------------
c
c  Notes:
c  ------
c
c  Author:   Arne Freyberger      Created:  Thu May  4 16:33:21 EDT 1995
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
      CHARACTER*(132)  CFILE, CREVIS, CSTATE, CDATE, CAUTHO, CRCSID
      PARAMETER (CFILE = '$RCSfile: get_level.F,v $')
      PARAMETER (CREVIS = '$Revision: 1.1.1.1 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 1997/03/28 20:58:23 $')
      PARAMETER (CAUTHO = '$Author: marki $')
      DATA CRCSID/   
     1'$Id: get_level.F,v 1.1.1.1 1997/03/28 20:58:23 marki Exp $'   
     2/   
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
c      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='GET_LEVEL')
      PARAMETER (CRAUTH='Arne Freyberger')
c
c  Local User defined variables:
c  -----------------------------
      LOGICAL LINFO, LWARN, LDEBUG, LERROR, LFATAL, LALL(5)
      INTEGER ISET
      DATA ISET/1/
c_end_var
c
c  executable code for routine GET_LEVEL:
c----6----------------------------------------------------------------72
c
      CALL KEEP_LEVEL(LALL,ISET)
      LINFO = LALL(1)
      LWARN = LALL(2)
      LDEBUG = LALL(3)
      LERROR = LALL(4)
      LFATAL = LALL(5)
c
      RETURN
      END
c
c------------------------------------------------------------------------------




