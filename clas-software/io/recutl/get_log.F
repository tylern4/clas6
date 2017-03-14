      SUBROUTINE GET_LOG(LOUT)
c
c_begin_doc
c  RCS ID string
c  $Id: get_log.F,v 1.1.1.1 1997/03/28 20:58:23 marki Exp $
c
c  Documentation for subroutine GET_LOG
c
c  Purpose:
c  --------
c
c  Input Parameters: NONE
c  ----------------      
c
c  Output Parameters:   LOUT - L - COUT = .TRUE. logging enabled
c  ----------------                       .FALSE. logging disabled
c
c  Other routines:
c  ---------------
c
c  Notes:
c  ------
c
c  Author:   Arne Freyberger      Created:  Fri May  5 14:47:53 EDT 1995
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
      PARAMETER (CFILE = '$RCSfile: get_log.F,v $')
      PARAMETER (CREVIS = '$Revision: 1.1.1.1 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 1997/03/28 20:58:23 $')
      PARAMETER (CAUTHO = '$Author: marki $')
      DATA CRCSID/   
     1'$Id: get_log.F,v 1.1.1.1 1997/03/28 20:58:23 marki Exp $'   
     2/   
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
c      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='GET_LOG')
      PARAMETER (CRAUTH='Arne Freyberger')
c
c  Local User defined variables:
c  -----------------------------
      LOGICAL  LOUT
c_end_var
c
c  executable code for routine GET_LOG:
c----6----------------------------------------------------------------72
c
      CALL KEEP_LOG(LOUT,1)
      RETURN
      END
c
c------------------------------------------------------------------------------




