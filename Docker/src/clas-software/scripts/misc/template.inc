c      include file template
c
c_begin_doc
c  RCS ID string
c  $Id: template.inc,v 1.1.1.1 1997/04/07 15:27:44 freyberg Exp $
c
c  Documentation for include file template
c
c  Purpose:
c  --------
c
c  Used in routines:
c  ----------------
c
c  Notes:
c  ------
c
c  Author:   John Doe      Created:  day-mon-xx
c  -------
c
c  Major revisions:
c  ----------------
c
c_end_doc
c
c
c  Remember NO NESTED INCLUDES
c
c  COMMON BLOCK NAME should be the same as include file name!
c
c_begin_var
c  variables for include file template:
c  -----------------------------------
c  RCS information: 
      CHARACTER*(132)  CItemplate
nocap      DATA CItemplate/
nocap     1'$Id: template.inc,v 1.1.1.1 1997/04/07 15:27:44 freyberg Exp $'
nocap     2/
nocapc----6----------------------------------------------------------------72
c  
c  Variables contained in template common block
c  --------------------------------------------
c
c_end_var
c
       COMMON/template/
c----6----------------------------------------------------------------72




