      SUBROUTINE REHBK
c
c_begin_doc
c  RCS ID string
c  $Id: rehbk.F,v 1.1.1.1 1997/03/28 20:58:29 marki Exp $
c
c  Documentation for subroutine REHBK
c
c  Purpose:  allows interactive booking of histograms
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
c  Author:   Arne Freyberger      Created:  Thu Sep 12 19:56:09 EDT 1996
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
      PARAMETER (CFILE = '$RCSfile: rehbk.F,v $')
      PARAMETER (CREVIS = '$Revision: 1.1.1.1 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 1997/03/28 20:58:29 $')
      PARAMETER (CAUTHO = '$Author: marki $')
      DATA CRCSID/   
     1'$Id: rehbk.F,v 1.1.1.1 1997/03/28 20:58:29 marki Exp $'   
     2/   
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='REHBK')
      PARAMETER (CRAUTH='Arne Freyberger')
c
c  Local User defined variables:
c  -----------------------------
      character*80 pres_dir
      character*8 ctag(25)
      integer ib, it
c_end_var
c
c  executable code for routine REHBK:
c----6----------------------------------------------------------------72
c  first store the correct directory
      call hcdir(PRES_DIR,'R')
c
      do ib = 1,nbook_stack
       nhist_max = max(nhist(ib), nhist_max)
       idhist(nhist(ib)) = hist_id(ib)
       dirhist(nhist(ib)) = hist_dir(ib)

       call hcdir(hist_dir(ib),' ')
       if (.not. lntup(ib)) then
        if (nychan(ib) .eq. 0) then
         call hbook1(hist_id(ib), hist_tit(ib), nxchan(ib), 
     1             sngl(xmin(ib)), sngl(xmax(ib)),0.0)       
        else
         call hbook2(hist_id(ib), hist_tit(ib), 
     1               nxchan(ib), sngl(xmin(ib)), sngl(xmax(ib)),
     1               nychan(ib), sngl(ymin(ib)), sngl(ymax(ib)), 0.0)
        endif
       else
        do it = 1, nxchan(ib)
         ctag(it) = cntup(ib,it)
        enddo
        call hbookn(hist_id(ib),hist_tit(ib),nxchan(ib),
     1              ' ',200000,ctag)
       endif
 
      enddo

c  move back to entering directory
       call hcdir(pres_dir,' ')
      
c  reset TCL counter
       nbook_stack = 0

      RETURN
      END
c
c------------------------------------------------------------------------------




