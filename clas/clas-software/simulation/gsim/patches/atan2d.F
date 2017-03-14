       REAL FUNCTION ATAN2D(Y,X)
c
c_begin_doc
c  RCS ID string
c  $Id: atan2d.F,v 1.1.1.1 1997/03/28 20:58:04 marki Exp $
c
c  Documentation for subroutine ATAN2D
c
c  Purpose:
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
c  Author:   arne       Created:  Thu Jan 11 21:14:45 EST 1996
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
      PARAMETER (CFILE = '$rcsRCSfile$')
      PARAMETER (CREVIS = '$rcsRevision$')
      PARAMETER (CSTATE = '$rcsState$')
      PARAMETER (CDATE = '$rcsDate$')
      PARAMETER (CAUTHO = '$rcsAuthor$')
      DATA CRCSID/   
     1'$Id: atan2d.F,v 1.1.1.1 1997/03/28 20:58:04 marki Exp $'   
     2/   
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='ATAN2D')
      PARAMETER (CRAUTH='arne ')
c
c  Local User defined variables:
c  -----------------------------
      REAL Y,X
c_end_var
c
c  executable code for routine ATAN2D:
c----6----------------------------------------------------------------72
c
      ATAN2D = 180.*ATAN2(Y,X)/3.1415927
      RETURN
      END
c
c------------------------------------------------------------------------------




