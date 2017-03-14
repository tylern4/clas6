      SUBROUTINE CC_NCC_TO_IEC(ANCC,Ee,riec,rdiec)
c
c_begin_doc
c  RCS ID string
c  $Id: cc_ncc_to_iec.F,v 1.1.1.1 1997/03/28 20:49:48 marki Exp $
c
c  Documentation for subroutine CC_NCC_TO_IEC
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
c                     riec - (R*4) Estimated i-coord in EC (in cm)
c                    rdiec - (R*4) Estimated error of i (cm)
c  -----------------
c
c  Other routines:
c  ---------------
c
c  Notes:
c  ------
c
c  Author:   Alexander Vlassov    Created:  Sun Jun 30 19:18:23 EDT 1996
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
      real*4 ANCC,Ee,riec,rdiec
c
c  Local pre-defined variables:
c  ---------------------------
c  RCS information: 
      CHARACTER*132  CFILE, CREVIS, CSTATE, CDATE, CAUTHO, CRCSID
      PARAMETER (CFILE = '$RCSfile: cc_ncc_to_iec.F,v $')
      PARAMETER (CREVIS = '$Revision: 1.1.1.1 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 1997/03/28 20:49:48 $')
      PARAMETER (CAUTHO = '$Author: marki $')
      DATA CRCSID/   
     1'$Id: cc_ncc_to_iec.F,v 1.1.1.1 1997/03/28 20:49:48 marki Exp $'   
     2/   
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='CC_NCC_TO_IEC')
      PARAMETER (CRAUTH='Alexander Vlassov')
c
c  Local User defined variables:
c  -----------------------------
      INTEGER NWRITE
      real derr
      parameter(derr=25.0)
c_end_var
c
c  executable code for routine CC_NCC_TO_IEC:
c----6----------------------------------------------------------------72
c
      IF (NWRITE .LT. 1) THEN
        NWRITE = NWRITE + 1
        CRMESS='CC_NCC_TO_IEC is called !!!'
        CALL RECMES(CRNAME,'I',CRMESS)
      ENDIF
c
c ==========>   1 step approach !
c
      rdiec = derr
      riec = 19.167 * ANCC - 184.583
c
c ==========>   1 step approach !
c
      RETURN
      END
c
c-----------------------------------------------------------------------




