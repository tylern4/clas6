      SUBROUTINE RFATAL
c
c_begin_doc
c  RCS ID string
c  $Id: rfatal.F,v 1.1 1997/03/28 20:58:17 marki Exp $
c
c  Documentation for subroutine RFATAL
c
c  Purpose:  Stop processing after an orderly shutting down
c  --------
c
c  Calling Sequence:
c  ----------------
c
c  Input Parameters:  NONE
c  ----------------
c
c  Output Parameters:  NONE:
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
c  Author:   Arne Freyberger      Created:  Wed Mar 15 14:31:20 EST 1995
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
      PARAMETER ( CFILE = '$RCSfile: rfatal.F,v $')
      PARAMETER ( CREVIS = '$Revision: 1.1 $')
      PARAMETER ( CSTATE = '$State: Exp $')
      PARAMETER ( CDATE = '$Date: 1997/03/28 20:58:17 $')
      PARAMETER ( CAUTHO = '$Author: marki $')
      DATA CRCSID/
     1'$Id: rfatal.F,v 1.1 1997/03/28 20:58:17 marki Exp $
     3'/
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
c      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='RFATAL')
      PARAMETER (CRAUTH='Arne Freyberger')
c
c  Local User defined variables:
c  -----------------------------
c_end_var
c
c  executable code for routine RFATAL:
c  -------------------------------------
c
      CALL RESTAT
c
c
      CALL FCLOS
c
      CALL RECLOS
c
      STOP
      END
c
c------------------------------------------------------------------------------




