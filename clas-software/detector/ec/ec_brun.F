      SUBROUTINE EC_BRUN(IRUN)
c
c_begin_doc
c  RCS ID string
c  $Id: ec_brun.F,v 1.3 1998/01/02 21:32:50 manak Exp $
c
c  Documentation for subroutine EC_BRUN.F
c
c  Purpose: Initialize EC package
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
c  Author:   Cole Smith      Created:  Thu Aug 22 12:36:39 EDT 1996
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
c
c  Local pre-defined variables:
c  ---------------------------
c  RCS information: 
      CHARACTER*132  CFILE, CREVIS, CSTATE, CDATE, CAUTHO, CRCSID
      PARAMETER (CFILE = '$RCSfile: ec_brun.F,v $')
      PARAMETER (CREVIS = '$Revision: 1.3 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 1998/01/02 21:32:50 $')
      PARAMETER (CAUTHO = '$Author: manak $')
      DATA CRCSID/   
     1'$Id: ec_brun.F,v 1.3 1998/01/02 21:32:50 manak Exp $'   
     2/   
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='EC_BRUN.F')
      PARAMETER (CRAUTH='Cole Smith')
c
c  input variables:
c  -----------------------------
      INTEGER irun
c
c_end_var
c
c  executable code for routine EC_BRUN :
c  ------------------------------------

      CALL RECMES(crname,'I','... Filling Electromagnetic Calorimeter Calibration Banks')

c Get and load BOS banks into common arrays

      CALL ec_getcal_adc(irun)
      CALL ec_getcal_tdc(irun)
      CALL ec_getcal_atten(irun)
      CALL ec_getcal_geom(irun)  
      RETURN
      END
c
c------------------------------------------------------------------------------




