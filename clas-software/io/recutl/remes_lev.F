      SUBROUTINE REMES_LEV(LINFO,LWARN)
c
c_begin_doc
c  RCS ID string
c  $Id: remes_lev.F,v 1.1.1.1 1997/03/28 20:58:31 marki Exp $
c
c  Documentation for subroutine REMES_LEV
c
c  Purpose:  Allows the user to turn OFF/on informational and warning messages
c  --------  
c
c  Calling Sequence:
c  ----------------
c
c                     input for first call/output otherwise
c  Input Parameters:  (LINFO - L - TRUE enable informational messages)
c  ----------------                FALSE DISABLE informational 
c                      LWARN - L - TRUE enable warning messages
c                                  FALSE disable warning messages
c  Output Parameters: (LINFO - L - TRUE enable informational messages)
c  ----------------                FALSE DISABLE informational 
c                      LWARN - L - TRUE enable warning messages
c                                  FALSE disable warning messages
c
c  Called from:
c  ------------
c
c  Other routines:
c  ---------------
c
c  Notes:
c  ------
c
c  Author:   Arne Freyberger      Created:  Wed Mar 22 15:37:03 EST 1995
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
      PARAMETER (CFILE=  '$RCSfile: remes_lev.F,v $')
      PARAMETER (CREVIS= '$Revision: 1.1.1.1 $')
      PARAMETER (CSTATE= '$State: Exp $')
      PARAMETER (CDATE=  '$Date: 1997/03/28 20:58:31 $')
      PARAMETER (CAUTHO= '$Author: marki $')
      DATA CRCSID/
     1'$Id: remes_lev.F,v 1.1.1.1 1997/03/28 20:58:31 marki Exp $'
     3/
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
c      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='REMES_LEV')
      PARAMETER (CRAUTH='Arne Freyberger')
c
c  Local User defined variables:
c  -----------------------------
      LOGICAL LINFO, LWARN, LINIT, LINFO_KEEP, LWARN_KEEP
      DATA LINIT/.TRUE./
c_end_var
c
c  executable code for routine REMES_LEV:
c  -------------------------------------
c
      IF (LINIT) THEN
       LINFO_KEEP = LINFO
       LWARN_KEEP = LWARN
       LINIT = .FALSE.
      ELSE
       LINFO = LINFO_KEEP
       LWARN = LWARN_KEEP
      ENDIF
      RETURN
      END
c
c------------------------------------------------------------------------------




