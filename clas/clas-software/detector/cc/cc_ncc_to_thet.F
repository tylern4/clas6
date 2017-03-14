      SUBROUTINE CC_NCC_TO_THET(ANCC,Ee,theta,dtheta)
c
c_begin_doc
c  RCS ID string
c  $Id: cc_ncc_to_thet.F,v 1.1.1.1 1997/03/28 20:49:48 marki Exp $
c
c  Documentation for subroutine CC_NCC_TO_THET
c
c  Purpose:
c  --------
c
c  Input Parameters:  (Name - Type - Meaning)
c  ----------------
c                     ANCC - (R*4) Mean CC segment #
c                     Ee   - (R*4) Particle momentum
c  ----------------
c
c  Output Parameters:  (Name - Type - Meaning)
c  -----------------
c                     theta - (R*4) Estimated angle Theta in degrees
c                    dtheta - (R*4) Theta error in degrees
c  -----------------
c
c  Other routines:
c  ---------------
c
c  Notes:
c  ------
c
c  Author:   Alexander Vlassov    Created:  Sun Jun 30 17:35:35 EDT 1996
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
c_end_inc
c
c_begin_var
c  input/output variables:
c  -----------------------
      real*4 ANCC,Ee,theta,dtheta
c
c  Local pre-defined variables:
c  ---------------------------
c  RCS information: 
      CHARACTER*132  CFILE, CREVIS, CSTATE, CDATE, CAUTHO, CRCSID
      PARAMETER (CFILE = '$RCSfile: cc_ncc_to_thet.F,v $')
      PARAMETER (CREVIS = '$Revision: 1.1.1.1 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 1997/03/28 20:49:48 $')
      PARAMETER (CAUTHO = '$Author: marki $')
      DATA CRCSID/   
     1'$Id: cc_ncc_to_thet.F,v 1.1.1.1 1997/03/28 20:49:48 marki Exp $'   
     2/   
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='CC_NCC_TO_THET')
      PARAMETER (CRAUTH='Alexander Vlassov')
c
c  Local User defined variables:
c  -----------------------------
      INTEGER NWRITE
      real shift,dshift
      parameter(shift=4.0)
      parameter(dshift=3.0)
c_end_var
c
c  executable code for routine CC_NCC_TO_THET:
c----6----------------------------------------------------------------72
c
      IF (NWRITE .LT. 1) THEN
        NWRITE = NWRITE + 1
        CRMESS='CC_NCC_TO_THET is called !!!'
        CALL RECMES(CRNAME,'I',CRMESS)
      ENDIF
c
c ==========>   0 step approach !
c
      theta = 6.566 + 1.414*ANCC + 0.03395*ANCC*ANCC
     &        + shift
      dtheta = dshift
c
c ==========>   0 step approach !
c
c
      RETURN
      END
c
c-----------------------------------------------------------------------





