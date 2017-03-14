      SUBROUTINE GET_PROMPT(CPROMPT)
c
c_begin_doc
c  RCS ID string
c  $Id: get_prompt.F,v 1.1.1.1 1997/03/28 20:58:24 marki Exp $
c
c  Documentation for subroutine GET_PROMPT
c
c  Purpose: RETURNS THE STORED PROMPT
c  --------
c
c  Input Parameters: none
c  ----------------
c
c  Output Parameters:  CPROMPT-C*(*)- the character string of the prompt
c  -----------------
c
c  Other routines:
c  ---------------
c
c  Notes:
c  ------
c
c  Author:   Arne Freyberger      Created:  Fri Mar  1 14:59:44 EST 1996
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
      PARAMETER (CFILE = '$RCSfile: get_prompt.F,v $')
      PARAMETER (CREVIS = '$Revision: 1.1.1.1 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 1997/03/28 20:58:24 $')
      PARAMETER (CAUTHO = '$Author: marki $')
      DATA CRCSID/   
     1'$Id: get_prompt.F,v 1.1.1.1 1997/03/28 20:58:24 marki Exp $'   
     2/   
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
c      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='GET_PROMPT')
      PARAMETER (CRAUTH='Arne Freyberger')
c
c  Local User defined variables:
c  -----------------------------
      character*(*) cprompt
c_end_var
c
c  executable code for routine GET_PROMPT:
c----6----------------------------------------------------------------72
c
      call keep_prompt(cprompt,1)
      RETURN
      END
c
c------------------------------------------------------------------------------




