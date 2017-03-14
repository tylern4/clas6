      SUBROUTINE EC1_BRUN(irun)
c
c_begin_doc
c  RCS ID string
c  $Id: ec1_brun.F,v 1.3 1998/08/01 18:26:39 staylor Exp $
c
c  Documentation for subroutine ec1_BRUN
c
c  --------
c
c  Calling Sequence:
c  ----------------
c
c  Input Parameters:  NONE
c  ----------------
c
c  Output Parameters:  NONE
c  -----------------
c
c  Called from:
c  ------------
c
c  Other routines:
c  ---------------
c
c  Notes:
c  ------
c
c  Author:   Arne Freyberger      Created:  Wed Mar 15 14:01:12 EST 1995
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
#include "ec1_control.inc"
#include "ec1_calibr.inc"
#include "ec1_pedstl.inc"
c_end_inc
c
c_begin_var
c  input/output variables:
c  -----------------------
      integer irun
c
c  Local pre-defined variables:
c  ---------------------------
c  RCS information: 
      CHARACTER*(*)  CFILE, CREVIS, CSTATE, CDATE, CAUTHO 
      PARAMETER (CFILE=  '$RCSfile: ec1_brun.F,v $')
      PARAMETER (CREVIS= '$Revision: 1.3 $')
      PARAMETER (CSTATE= '$State: Exp $')
      PARAMETER (CDATE=  '$Date: 1998/08/01 18:26:39 $')
      PARAMETER (CAUTHO= '$Author: staylor $')
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='EC1_BRUN')
      PARAMETER (CRAUTH='Arne Freyberger')
c
c  Local User defined variables:
c  -----------------------------
      INTEGER NWRITE,CURR_TIME
c_end_var
c
c  executable code for routine USER_BRUN:
c  -------------------------------------
c
      IF (NWRITE .LT. 1) THEN
        NWRITE = NWRITE + 1
        CRMESS='Reading the calibration tables'
        CALL RECMES(CRNAME,'I',CRMESS)
      ENDIF
      CALL RECMES(crname,'I','... Fill LAC  Calibration/Geometry Banks')
C
C            Read the pedestal/calibration data  Ec1_calib=0 use a constant for all
*                      ,1-data file,2-MAP-manager
C
      if(ec1_calib.eq.2) then
         call ec1calibmap(irun)
      else 
         call ec1filecalib      ! fortran call to read the file ec1calib.dat
         ec1_calupdt=1          ! call 1 time to fill common
      endif
         call ec1filegeom(irun)      ! fortran call to read the file ec1geom.dat
C
      RETURN
      END
c
c------------------------------------------------------------------------------




