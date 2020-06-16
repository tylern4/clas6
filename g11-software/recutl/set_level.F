      SUBROUTINE SET_LEVEL(LINFO,LWARN,LDEBUG,LERROR,LFATAL)
c
c_begin_doc
c  RCS ID string
c  $Id: set_level.F,v 1.1.1.1 1997/03/28 20:58:34 marki Exp $
c
c  Documentation for subroutine SET_LEVEL
c
c  Purpose:  Sets the level of messaging
c  --------
c
c  Input Parameters:  LINFO  - L - TRUE route INFO messages
c  ----------------   LWARN  - L - TRUE route WARNING messages
c                     LDEBUG - L - TRUE route DEBUG messages
c                     LERROR - L - TRUE route ERROR messages
c                     LFATAL - L - TRUE route FATAL messages
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
c  Author:   Arne Freyberger      Created:  Thu May  4 16:33:18 EDT 1995
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
      PARAMETER (CFILE = '$RCSfile: set_level.F,v $')
      PARAMETER (CREVIS = '$Revision: 1.1.1.1 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 1997/03/28 20:58:34 $')
      PARAMETER (CAUTHO = '$Author: marki $')
      DATA CRCSID/   
     1'$Id: set_level.F,v 1.1.1.1 1997/03/28 20:58:34 marki Exp $'   
     2/   
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
c      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='SET_LEVEL')
      PARAMETER (CRAUTH='Arne Freyberger')
c
c  Local User defined variables:
c  -----------------------------
      LOGICAL LINFO, LWARN, LDEBUG, LERROR, LFATAL, LALL(5)
      INTEGER ISET
      DATA ISET/-1/
c_end_var
c
c  executable code for routine SET_LEVEL:
c----6----------------------------------------------------------------72
c
      LALL(1) = LINFO
      LALL(2) = LWARN
      LALL(3) = LDEBUG
      LALL(4) = LERROR
      LALL(5) = LFATAL
c
      CALL KEEP_LEVEL(LALL,ISET)
      RETURN
      END
c
c------------------------------------------------------------------------------




