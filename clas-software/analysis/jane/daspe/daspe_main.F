      PROGRAM DASPE_MAIN
c
c_begin_doc
c  RCS ID string
c  $Id: daspe_main.F 1.1 1996/12/11 20:41:20 stepanya Exp $
c
c  Documentation for subroutine DASPE_MAIN
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
c  Author: S.Stepanyan  Created:  Wed Jun 23 15:40:59 EST 1996
c  -------
c
c  Major revisions:
c  ----------------
c     
c
c_end_doc`
c
      IMPLICIT NONE
      SAVE
c
c_begin_inc
c  include files :
c  ---------------------
#include "inpar.inc"
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
      PARAMETER (CFILE = '$RCSfile: daspe_main.F $')
      PARAMETER (CREVIS = '$Revision: 1.1 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 1996/12/11 20:41:20 $')
      PARAMETER (CAUTHO = '$Author: stepanya $')
      DATA CRCSID/   
     1'$Id: daspe_main.F 1.1 1996/12/11 20:41:20 stepanya Exp $'   
     2/   
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='DASPE_MAIN')
      PARAMETER (CRAUTH='S.Stepanyan')
c
c  Local User defined variables:
c  -----------------------------
       INTEGER ierr
c_end_var
c
c  executable code for routine DASPE_MAIN:
c----6----------------------------------------------------------------72
c  Initialisation of interrupt procedure
       CALL assign_sig
c  End of initialisation of interrupt procedure
       CALL daspe_init(ierr)
       IF( fitopt.EQ.'F' .OR. fitopt.EQ.'f' .OR. 
     +     fitopt.EQ.'MF' .OR. fitopt.EQ.'mf' ) THEN
          CALL DASPE_FIT
       ELSEIF( fitopt.EQ.'G' .OR. fitopt.EQ.'g' ) THEN
          CALL DASPE_EVGen
c        CALL total
       ELSEIF( fitopt.EQ.'M' .OR. fitopt.EQ.'m' ) THEN
          call mult_ana
c
       ENDIF
       END
c
c------------------------------------------------------------------------------




