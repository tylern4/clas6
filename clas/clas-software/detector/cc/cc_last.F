      SUBROUTINE CC_LAST
c
c_begin_doc
c  RCS ID string
c  $Id: cc_last.F,v 1.2 1997/05/06 21:18:23 vlassov Exp $
c
c  Documentation for subroutine CC_LAST
c
c  Purpose:  Nothing is to be done for the end of run...
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
c  Author:   Alexander Vlassov    Created:  Fri Jun 21 18:30:34 EDT 1996
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
      PARAMETER (CFILE = '$RCSfile: cc_last.F,v $')
      PARAMETER (CREVIS = '$Revision: 1.2 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 1997/05/06 21:18:23 $')
      PARAMETER (CAUTHO = '$Author: vlassov $')
      DATA CRCSID/   
     1'$Id: cc_last.F,v 1.2 1997/05/06 21:18:23 vlassov Exp $'   
     2/   
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='CC_LAST')
      PARAMETER (CRAUTH='Alexander Vlassov')
c
c  Local User defined variables:
c  -----------------------------
c_end_var
c
c  executable code for routine CC_LAST:
c----6----------------------------------------------------------------72
c
      CRMESS = 'CC_LAST was called !'
      CALL RECMES(CRNAME,'I',CRMESS)
c
      RETURN
      END
c
c-----------------------------------------------------------------------




