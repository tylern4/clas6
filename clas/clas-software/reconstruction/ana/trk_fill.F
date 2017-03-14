      SUBROUTINE TRK_FILL
c
c_begin_doc
c  RCS ID string
c  $Id: trk_fill.F,v 1.3 1998/04/03 22:56:11 jyun Exp $
c
c  Documentation for subroutine TRK_FILL
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
c  Author:   Francois Roudot      Created:  Thu May 29 10:00:00 EDT 1997
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
      PARAMETER (CFILE = '$RCSfile: trk_fill.F,v $')
      PARAMETER (CREVIS = '$Revision: 1.3 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 1998/04/03 22:56:11 $')
      PARAMETER (CAUTHO = '$Author: jyun $')
      DATA CRCSID/   
     1'$Id: trk_fill.F,v 1.3 1998/04/03 22:56:11 jyun Exp $'   
     2/   
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='TRK_FILL')
      PARAMETER (CRAUTH='Francois Roudot')
c
c  Local User defined variables:
c  -----------------------------
      INTEGER NWRITE
c_end_var
c
c  executable code for routine TRK_FILL:
c----6----------------------------------------------------------------72
c
      IF (NWRITE .LT. 1) THEN
        NWRITE = NWRITE + 1
        CRMESS='Filling TRK Histograms, this message written once'
        CALL RECMES(CRNAME,'I',CRMESS)
      ENDIF
c  First change PAWC directories
      call hcdir('//PAWC/TRK',' ')

c  Do your HFILLS below this line
      call trk_fill_raw
      call trk_fill_hbt
      call trk_fill_tbt


c  return to the top directory and exit
      call hcdir('//PAWC',' ')

      RETURN
      END
c
c------------------------------------------------------------------------------




