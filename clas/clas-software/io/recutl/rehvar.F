      SUBROUTINE REHVAR
c
c_begin_doc
c  RCS ID string
c  $Id: rehvar.F,v 1.1.1.1 1997/03/28 20:58:30 marki Exp $
c
c  Documentation for subroutine REHVAR
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
c  Author:   Arne Freyberger      Created:  Thu Sep 12 21:50:41 EDT 1996
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
#include "rhist.inc"
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
      PARAMETER (CFILE = '$RCSfile: rehvar.F,v $')
      PARAMETER (CREVIS = '$Revision: 1.1.1.1 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 1997/03/28 20:58:30 $')
      PARAMETER (CAUTHO = '$Author: marki $')
      DATA CRCSID/   
     1'$Id: rehvar.F,v 1.1.1.1 1997/03/28 20:58:30 marki Exp $'   
     2/   
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
c      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='REHVAR')
      PARAMETER (CRAUTH='Arne Freyberger')
c
c  Local User defined variables:
c  -----------------------------
      integer icdeci
      character*132 word
      external word, icdeci

      integer ib, it
      character*132 c132
c_end_var
c
c  executable code for routine REHVAR:
c----6----------------------------------------------------------------72
c
      call wordsep('.')

      do ib = 1, nfill_stack
       if (ndim(ib) .eq. 0) then
        c132  = word(hist_x(ib),1)
        xbname(nhist(ib)) = c132(1:4)
        c132 = word(hist_x(ib),2)
        xrec(nhist(ib)) =  icdeci(word(hist_x(ib),2),1,132)
        call getcol(word(hist_x(ib),3),xbname(nhist(ib)),xcol(nhist(ib)))
        call getfmt(xbname(nhist(ib)), xcol(nhist(ib)), xfmt(nhist(ib)))

        c132  = word(hist_y(ib),1)
        ybname(nhist(ib)) = c132(1:4)
        c132 = word(hist_y(ib),2)
        yrec(nhist(ib)) =  icdeci(word(hist_y(ib),2),1,132)
        call getcol(word(hist_y(ib),3),ybname(nhist(ib)),ycol(nhist(ib)))
        call getfmt(ybname(nhist(ib)), ycol(nhist(ib)), yfmt(nhist(ib)))

       else
        ndimtup(nhist(ib)) = ndim(ib)
        
        do it = 1, ndim(ib)
         c132 = word(ctclvar(ib,it),1)
         cbnktup(nhist(ib),it) = c132(1:4)
         c132 = word(ctclvar(ib,it),2)
         xrectup(nhist(ib),it) = icdeci(c132,1,132)
         call getcol(word(ctclvar(ib,it),3),  cbnktup(nhist(ib),it), 
     1               xcoltup(nhist(ib),it))
         call getfmt(cbnktup(nhist(ib),it), 
     1               xcoltup(nhist(ib),it),
     2               xfmttup(nhist(ib),it))

        enddo
       endif
      enddo

c  reset TCL counter
      nfill_stack = 0

      RETURN
      END
c
c------------------------------------------------------------------------------




