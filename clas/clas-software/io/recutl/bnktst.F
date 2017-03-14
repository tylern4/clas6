      SUBROUTINE BNKTST(BNAME,LFMT,NCOL,CFMT,CELEMENTS)
c
c_begin_doc
c  RCS ID string
c  $Id: bnktst.F,v 1.1.1.1 1997/03/28 20:58:21 marki Exp $
c
c  Documentation for subroutine BNKTST
c
c  Purpose: given bank name BNAME returns LFMT  TRUE if the bank has
c  -------- been formatted, NCOL = number of columns in FORMAT and
c           CFMT is a character string containing the format field
c
c  Input Parameters:  BNAME: c4 : bank name
c  ----------------
c
c  Output Parameters: LFMT: logical : Returned TRUE if bank has 
c  -----------------                  been formatted, returns FALSE
c                                     if bank not formatted.
c                     NCOL: integer : number of columns in format, 0
c                                     if unformatted.
c                     CFMT: c132    : character string of format field
c                                     returned blank if unformatted.
c                     CELEMENTS(100): c8 : discription of the variables
c
c  Notes:  relies on the BNKFMT include file which is filled by TXT2BOS
c  ------
c
c  Author:   Arne Freyberger      Created:  Tue Jul 30 14:05:20 EDT 1996
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
c
c  Local pre-defined variables:
c  ---------------------------
c  RCS information: 
      CHARACTER*132  CFILE, CREVIS, CSTATE, CDATE, CAUTHO, CRCSID
      PARAMETER (CFILE = '$RCSfile: bnktst.F,v $')
      PARAMETER (CREVIS = '$Revision: 1.1.1.1 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 1997/03/28 20:58:21 $')
      PARAMETER (CAUTHO = '$Author: marki $')
      DATA CRCSID/   
     1'$Id: bnktst.F,v 1.1.1.1 1997/03/28 20:58:21 marki Exp $'   
     2/   
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='BNKTST')
      PARAMETER (CRAUTH='Arne Freyberger')
c
c  Local User defined variables:
c  -----------------------------
      character*4 bname
      character*8 celements(max_elements)
      character*132 cfmt
      logical lfmt
      integer ncol, i, j
c_end_var
c
c  executable code for routine BNKTST:
c----6----------------------------------------------------------------72
c
      cfmt = ' '
      lfmt = .false.
      ncol = 0
         do 100 i=1,icreate
            if (create_name(i) .ne. bname) goto 100
            lfmt = .true.
            ncol = ncreate_col(i)
            cfmt = create_format(i)
            do 90 j = 1, ncol
              celements(j) = create_elements(i,j)
 90         continue
            goto 101
 100     continue
 101     continue
      RETURN
      END
c
c------------------------------------------------------------------------------




