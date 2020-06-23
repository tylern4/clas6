      SUBROUTINE GETFMT(cbname,icol,ifmt)
c
c_begin_doc
c  RCS ID string
c  $Id: getfmt.F,v 1.1.1.1 1997/03/28 20:58:24 marki Exp $
c
c  Documentation for subroutine GETFMT
c
c  Purpose:  returns the format for COLUMN icol in bank CBAME
c  --------
c
c  Input Parameters:  cbname : c4 : BOS BANK name
c  ----------------   icol   : i  : column in bos bank
c
c  Output Parameters:  ifmt : i : format for column 1=Integer
c  -----------------                                3 = real
c                                                   2 = integer*2
c
c  Other routines:
c  ---------------
c
c  Notes:
c  ------
c
c  Author:   Arne Freyberger      Created:  Fri Sep 13 14:22:09 EDT 1996
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
      PARAMETER (CFILE = '$RCSfile: getfmt.F,v $')
      PARAMETER (CREVIS = '$Revision: 1.1.1.1 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 1997/03/28 20:58:24 $')
      PARAMETER (CAUTHO = '$Author: marki $')
      DATA CRCSID/   
     1'$Id: getfmt.F,v 1.1.1.1 1997/03/28 20:58:24 marki Exp $'   
     2/   
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='GETFMT')
      PARAMETER (CRAUTH='Arne Freyberger')
c
c  Local User defined variables:
c  -----------------------------
      character*4 cbname
      INTEGER icol, ifmt

      logical lok
      integer ncol_ddl
      character*256 cfmt
      character*8 cdisc(100)
      character*1 cone
c_end_var
c
c  executable code for routine GETFMT:
c----6----------------------------------------------------------------72
c
      CALL BNKTST(CBNAME, LOK, NCOL_DDL, CFMT, CDISC)
      ifmt = 0
      IF (.NOT. LOK)  RETURN
c  get the format for the NCOL column

          IF (CFMT(1:3) .EQ. 'B16') THEN
           ifmt = 2
          ELSEIF (CFMT(1:3) .EQ. 'B32') THEN
           ifmt = 1
          ELSE
           CONE = CFMT((ICOL-1)*2+1:(ICOL-1)*2+1)
           IF (CONE .EQ. 'I') THEN
            ifmt = 1
           ELSEIF (CONE .EQ. 'F') THEN
            ifmt = 3
           ENDIF
          ENDIF
      RETURN
      END
c
c------------------------------------------------------------------------------




