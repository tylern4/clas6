      SUBROUTINE CLASDD_STOP
c
c_begin_doc
c  RCS ID string
c  $Id: clasdd_stop.F,v 1.2 1997/07/08 15:56:44 qin Exp $
c
c  Documentation for subroutine CLASDD_STOP
c
c  Purpose: This is a dummy routine to replace the DD software
c  -------- used for the online data acquisition at JLAB. The
c           online DD software is written solely for Sun
c           platforms so it is not included in the standard
c           distribution of RECSIS.
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
c  Author:         Created:  Mon Jun 30 10:52:14 EDT 1997
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
      PARAMETER (CFILE = '$RCSfile: clasdd_stop.F,v $')
      PARAMETER (CREVIS = '$Revision: 1.2 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 1997/07/08 15:56:44 $')
      PARAMETER (CAUTHO = '$Author: qin $')
      DATA CRCSID/   
     1'$Id: clasdd_stop.F,v 1.2 1997/07/08 15:56:44 qin Exp $'   
     2/   
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='CLASDD_STOP')
      PARAMETER (CRAUTH='whoistheauther')
c
c  Local User defined variables:
c  -----------------------------
      INTEGER NWRITE
c_end_var
c
c  executable code for routine CLASDD_STOP:
c----6----------------------------------------------------------------72
c
      IF (NWRITE .LT. 1) THEN
        NWRITE = NWRITE + 1
        CRMESS='You have called a dummy CLAS DD routine. It appears you'
        CALL RECMES(CRNAME,'I',CRMESS)
        CRMESS='are trying to use the online DD ring without compiling'
        CALL RECMES(CRNAME,'I',CRMESS)
        CRMESS='RECSIS with the "ONLINE=yes" option. If you really do '
        CALL RECMES(CRNAME,'I',CRMESS)
        CRMESS='not want to use the DD ring, then set the tcl variable'
        CALL RECMES(CRNAME,'I',CRMESS)
        CRMESS='READ_DD_RING to zero in your inititalization file.'
        CALL RECMES(CRNAME,'I',CRMESS)
      ENDIF
      RETURN
      END
c
c------------------------------------------------------------------------------




