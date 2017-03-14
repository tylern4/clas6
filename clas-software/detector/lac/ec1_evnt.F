      SUBROUTINE EC1_EVNT
c
c_begin_doc
c  RCS ID string
c  $Id: ec1_evnt.F,v 1.1.1.1 1997/03/28 20:57:58 marki Exp $
c
c  Documentation for subroutine EC1_EVNT
c
c  Purpose: perform the event by event reconstruction of the LAC
c  --------
c
c  Input Parameters: none
c  ----------------
c
c  Output Parameters:  none---> BOS bank filled
c  -----------------
c
c  Other routines:
c  ---------------
c
c  Notes:
c  ------
c
c  Author:   Arne Freyberger      Created:  Tue Nov 28 15:41:35 EST 1995
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
      PARAMETER (CFILE = '$RCSfile: ec1_evnt.F,v $')
      PARAMETER (CREVIS = '$Revision: 1.1.1.1 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 1997/03/28 20:57:58 $')
      PARAMETER (CAUTHO = '$Author: marki $')
      DATA CRCSID/   
     1'$Id: ec1_evnt.F,v 1.1.1.1 1997/03/28 20:57:58 marki Exp $'   
     2/   
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='EC1_EVNT')
      PARAMETER (CRAUTH='Arne Freyberger')
c
c  Local User defined variables:
c  -----------------------------
      LOGICAL LOK
c_end_var
c
c  executable code for routine EC1_EVNT:
c----6----------------------------------------------------------------72
c
      CALL EC1_BOS_IO(LOK)
      RETURN
      END
c
c------------------------------------------------------------------------------




