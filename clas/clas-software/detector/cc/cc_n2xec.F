      SUBROUTINE CC_N2XEC(ANCC,rxec,rdxec)
c
c_begin_doc
c  RCS ID string
c  $Id: cc_n2xec.F,v 1.1.1.1 1997/03/28 20:49:48 marki Exp $
c
c  Documentation for subroutine CC_N2XEC
c
c  Purpose:   Estimates the value of x-coordinate in EC for electrons
c  --------
c
c  Input Parameters:  (Name - Type - Meaning)
c  ----------------
c                     ANCC  (R*4) Mean CC segment #
c
c  Output Parameters:  (Name - Type - Meaning)
c  -----------------
c                     rxec - (R*4) Estimated x-coord in EC (in cm)
c                    rdxec - (R*4) Estimated error of x (cm)
c
c  Other routines:
c  ---------------
c
c  Notes:
c  ------
c
c  Author:   Alexander Vlassov      Created:  Tue Aug 27 12:31:34 EDT 1996
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
c_end_inc
c
c_begin_var
c  input/output variables:
c  -----------------------
      real*4 ANCC, rxec,rdxec 
c
c  Local pre-defined variables:
c  ---------------------------
c  RCS information: 
      CHARACTER*132  CFILE, CREVIS, CSTATE, CDATE, CAUTHO, CRCSID
      PARAMETER (CFILE = '$RCSfile: cc_n2xec.F,v $')
      PARAMETER (CREVIS = '$Revision: 1.1.1.1 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 1997/03/28 20:49:48 $')
      PARAMETER (CAUTHO = '$Author: marki $')
      DATA CRCSID/   
     1'$Id: cc_n2xec.F,v 1.1.1.1 1997/03/28 20:49:48 marki Exp $'   
     2/   
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='CC_N2XEC')
      PARAMETER (CRAUTH='Alexander Vlassov')
c
c  Local User defined variables:
c  -----------------------------
      INTEGER NWRITE
      real derr
      parameter(derr=20.0)
c_end_var
c
c  executable code for routine CC_N2XEC:
c----6----------------------------------------------------------------72
c
      IF (NWRITE .LT. 1) THEN
        NWRITE = NWRITE + 1
        CRMESS='CC_N2XEC was called !!'
        CALL RECMES(CRNAME,'I',CRMESS)
      ENDIF
c
c ==========>   1 step approach !
c
      rdxec = derr
      rxec = 17.778 * ANCC + 41.111
c
c ==========>   1 step approach !
c
      RETURN
      END
c
c-----------------------------------------------------------------------




