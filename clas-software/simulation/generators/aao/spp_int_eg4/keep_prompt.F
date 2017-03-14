      SUBROUTINE KEEP_PROMPT(cprompt,itog)
c
c_begin_doc
c  RCS ID string
c  $Id: keep_prompt.F,v 1.1.1.1 1997/03/28 20:58:26 marki Exp $
c
c  Documentation for subroutine KEEP_PROMPT
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
c  Author:   Arne Freyberger      Created:  Fri Mar  1 14:59:57 EST 1996
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
      PARAMETER (CFILE = '$RCSfile: keep_prompt.F,v $')
      PARAMETER (CREVIS = '$Revision: 1.1.1.1 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 1997/03/28 20:58:26 $')
      PARAMETER (CAUTHO = '$Author: marki $')
      DATA CRCSID/   
     1'$Id: keep_prompt.F,v 1.1.1.1 1997/03/28 20:58:26 marki Exp $'   
     2/   
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
c      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='KEEP_PROMPT')
      PARAMETER (CRAUTH='Arne Freyberger')
c
c  Local User defined variables:
c  -----------------------------
      INTEGER ITOG
      CHARACTER*132 CPROMPT, CSAVE
c_end_var
c
c  executable code for routine KEEP_PROMPT:
c----6----------------------------------------------------------------72
c
      if (itog .gt. 0) then
       cprompt = csave
      else
       csave = cprompt
      endif
      RETURN
      END
c
c------------------------------------------------------------------------------




