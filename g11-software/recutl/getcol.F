      SUBROUTINE GETCOL(cname,bname,icol)
c
c_begin_doc
c  RCS ID string
c  $Id: getcol.F,v 1.1.1.1 1997/03/28 20:58:24 marki Exp $
c
c  Documentation for subroutine GETCOL
c
c  Purpose:  given bank name BNAME, and column discriptor CNAME returns the
c  --------  FIRST column index that match column discriptor
c
c  Input Parameters:  cname : c*8
c  ----------------   bname : c*4
c
c  Output Parameters:  icol : integer
c  -----------------
c
c  Other routines:
c  ---------------
c
c  Notes:
c  ------
c
c  Author:   Arne Freyberger      Created:  Mon Sep 16 15:08:27 EDT 1996
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
#include "bnkfmt.inc"
c_end_inc
c
c_begin_var
c  input/output variables:
c  -----------------------
      character*8 cname
      character*4 bname
      integer icol
c
c  Local pre-defined variables:
c  ---------------------------
c  RCS information: 
      CHARACTER*132  CFILE, CREVIS, CSTATE, CDATE, CAUTHO, CRCSID
      PARAMETER (CFILE = '$RCSfile: getcol.F,v $')
      PARAMETER (CREVIS = '$Revision: 1.1.1.1 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 1997/03/28 20:58:24 $')
      PARAMETER (CAUTHO = '$Author: marki $')
      DATA CRCSID/   
     1'$Id: getcol.F,v 1.1.1.1 1997/03/28 20:58:24 marki Exp $'   
     2/   
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='GETCOL')
      PARAMETER (CRAUTH='Arne Freyberger')
c
c  Local User defined variables:
c  -----------------------------
      logical lok
      integer ncol_ddl, i
      character*256 cfmt
      character*8 cdisc(100)

c_end_var
c
c  executable code for routine GETCOL:
c----6----------------------------------------------------------------72
c
      CALL BNKTST(BNAME, LOK, NCOL_DDL, CFMT, CDISC)
      icol = 0
      do i = 1, ncol_ddl
        if (cdisc(i) .eq. cname) then
          icol = i
          return
        endif
      enddo
      RETURN
      END
c
c------------------------------------------------------------------------------




