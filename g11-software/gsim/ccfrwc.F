CDECK  ID>, CCFRWC.
      SUBROUTINE CCFRWC(ICH,UWC,VWC,R0,D0)
c
c_begin_doc
c  RCS ID string
c  $Id: ccfrwc.F,v 1.2 1997/04/24 15:38:08 holtrop Exp $
c
c  Documentation for subroutine CCFRWC(ICH,UWC,VWC,R0,D0)
c
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Reference system change
C-                         from CC R.S. to WC R.S.
C-
C-   Inputs  : ICH - The CC and WC number.
C-             UWC - Point in WC system.
C-             VWC - Direction in WC system
c-
C-   Outputs :
c-             R0  - Point coordinates in CC system.
c-             D0  - Direction in CC system.
C-   Controls:
C-
C-   Created    27-APR-1994   Alexander V. Vlassov
C-   Modified   18-AUG-1995   Alexander V. Vlassov
C-
C----------------------------------------------------------------------
c
c_end_doc
c
      IMPLICIT NONE
      SAVE
c
c_begin_inc
c  include files :
c  ---------------------
C----------------------------------------------------------------------
#include "ccp.inc"
#include "gconsp.inc"
C----------------------------------------------------------------------
c_end_inc
c
c_begin_var
c  input/output variables:
c  -----------------------
      integer ICH
      real R0(3),D0(3),UWC(3),VWC(3)
c
c
c  Local pre-defined variables:
c  ---------------------------
C----------------------------------------------------------------------
c rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rsc rcs rcs rcs
c rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rsc rcs rcs rcs
c                                                                         rcs
c  RCS information:                                                       rcs
c                                                                         rcs
      CHARACTER*(132)  CFILE, CREVIS, CSTATE, CDATE, CAUTHO, CRCSID
      PARAMETER (CFILE = '$RCSfile: ccfrwc.F,v $') 
      PARAMETER (CREVIS = '$Revision: 1.2 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 1997/04/24 15:38:08 $')
      PARAMETER (CAUTHO = '$Author: holtrop $')
      DATA CRCSID/ 
     1'$Id: ccfrwc.F,v 1.2 1997/04/24 15:38:08 holtrop Exp $'
     2/ 
c  Module information: 
      CHARACTER*(*)  CRNAME, CRAUTH
      PARAMETER (CRNAME='ccfrwc')                                   
      PARAMETER (CRAUTH='Alexander Vlassov')
c                                                                         rcs
c rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rsc rcs rcs rcs
c rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rsc rcs rcs rcs
c
      real teta, tm(3), pm(3), sh(3)
      data pm /90., 90., 0./
c
c_end_var
c
c  executable code for routine CCFRWC
c------------------------------------------------------------------------------
c
      teta  = raddeg*csa0(ICH)    ! in degrees
c
      sh(1) = -sw0(3,ICH)
      sh(2) = -sw0(1,ICH)*cos(csa0(ICH)) + sw0(2,ICH)*sin(csa0(ICH))
      sh(3) = -sw0(1,ICH)*sin(csa0(ICH)) - sw0(2,ICH)*cos(csa0(ICH))
c
      tm(1) = 90. - teta
      tm(2) = -teta
      tm(3) = 90.
c
c   From WC to CC Reference System :
c
      call CCREFS(UWC,VWC,tm,pm,sh,R0,D0)
c
  999 RETURN
      END
