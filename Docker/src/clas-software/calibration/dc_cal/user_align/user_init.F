      SUBROUTINE USER_INIT
c
c_begin_doc
c  RCS ID string
c  $Id: user_init.F,v 1.2 2002/05/27 23:17:23 sam Exp $
c
c  Documentation for subroutine USER_INIT
c
c  Purpose:  USER routine called ONCE at initialization time
c  --------  This routine upon check out from RCS is a dummy routine
c            examples of how to open an HBOOK file are given, if the
c            user wishes to use HBOOK calls, s/he should uncomment lines
c            denoted with chbook.   That is perform a global delete
c            of the occurence of `chbook'
c
c            The user must also edit the subroutine
c            user_last.F to insure the HBOOK file is closed properly.
c            No guarantees implied....
c
c  Input Parameters: NONE
c  ----------------
c
c  Output Parameters: NONE
c  -----------------
c
c  Other routines:
c  ---------------
c
c  Notes:
c  ------
c
c  Author:   Arne Freyberger      Created:  Thu Apr 13 16:27:07 EDT 1995
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
#include "clas_offsets.inc"
#include "bcs.inc"
#include "wcs.inc"
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
      PARAMETER ( CFILE = '$RCSfile: user_init.F,v $')
      PARAMETER ( CREVIS = '$Revision: 1.2 $')
      PARAMETER ( CSTATE = '$State: Exp $')
      PARAMETER ( CDATE = '$Date: 2002/05/27 23:17:23 $')
      PARAMETER ( CAUTHO = '$Author: sam $')
      DATA CRCSID/
     1'$Id: user_init.F,v 1.2 2002/05/27 23:17:23 sam Exp $
     3'/
c  Module information:
      CHARACTER*132  CRNAME, CRAUTH
      CHARACTER*132  CRMESS
      PARAMETER (CRNAME='USER_INIT')
      PARAMETER (CRAUTH='Arne Freyberger')
c
c  Local User defined variables:
c  -----------------------------
        INTEGER NWRITE

c_end_var
c
c  executable code for routine USER_INIT:


c  -------------------------------------
      CALL SET_LEVEL(.true.,.true.,.true.,.true.,.true.)
      IF (NWRITE .LT. 1) THEN
        NWRITE = NWRITE + 1
        CRMESS='This is a DUMMY routine, this message written once'
        CALL RECMES(CRNAME,'I',CRMESS)
      ENDIF
c
c
c  First toggle on log file, you can set this to false if your tired
c  of all the REnnnn.LOG files.
c
      CALL SET_LOG(.TRUE.)
c
c  Register my USER package with job database....
c
      CALL REVINI('USER',CREVIS,CDATE)
C
c
c below is an example of how to add and use subdirectories
c you are garuanteed to be safe if you place all your histograms
c into your own uniquely named subdirectory.
c 
c
c Do the HMDIR stuff twice to declare the subdirectory in memory and disk
c
      call hmdir('ALIGN','S')
      call hcdir('//PAWC',' ')
      call hmdir('ALIGN','S')
      call hcdir('//PAWC/ALIGN',' ')
c
      call hcdir('//LUN1/ALIGN',' ')
      call align_book
c

c  always changed back to top directory
c
      call hcdir('//PAWC',' ')
c
      RETURN
      END
c
c------------------------------------------------------------------------------





