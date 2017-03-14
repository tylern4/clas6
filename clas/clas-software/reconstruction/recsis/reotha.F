      SUBROUTINE REOTHA
c
c_begin_doc
c  RCS ID string
c  $Id: reotha.F,v 1.2 2003/10/23 22:36:10 stepanya Exp $
c
c  Documentation for subroutine REOTHA
c
c  Purpose:
c  --------
c
c  Calling Sequence:
c  ----------------
c
c  Input Parameters:  (Name - Type - Meaning)
c  ----------------
c
c  Output Parameters:  (Name - Type - Meaning)
c  -----------------
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
c  Author:   Arne Freyberger      Created:  Tue Mar 14 16:26:20 EST 1995
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
      PARAMETER ( CFILE = '$RCSfile: reotha.F,v $')
      PARAMETER ( CREVIS = '$Revision: 1.2 $')
      PARAMETER ( CSTATE = '$State: Exp $')
      PARAMETER ( CDATE = '$Date: 2003/10/23 22:36:10 $')
      PARAMETER ( CAUTHO = '$Author: stepanya $')
      DATA CRCSID/
     1'
     2$Id: reotha.F,v 1.2 2003/10/23 22:36:10 stepanya Exp $
     3'/
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
c      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='REOTHA')
      PARAMETER (CRAUTH='Arne Freyberger')
      integer err(10)
c
c  Local User defined variables:
c  -----------------------------
c_end_var
c
c  executable code for routine REOTHA:
c  -------------------------------------
c
      CALL USER_XTRA(err)
      RETURN
      END
c
c------------------------------------------------------------------------------




