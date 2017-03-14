CDECK  ID>, CCTOWC.
      SUBROUTINE CCTOWC(ICH,R0,D0,UWC,VWC)
c
c_begin_doc
c  RCS ID string
c  $Id: cctowc.F,v 1.2 1997/04/24 15:38:12 holtrop Exp $
c
c  Documentation for subroutine CCTOWC(ICH,R0,D0,UWC,VWC)
c
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Reference system change
C-                         from CC R.S. to WC R.S.
C-
C-   Inputs  : ICH - The CC and WC number.
c-             R0  - Point coordinates in CC system.
c-             D0  - Direction in CC system.
c-
C-   Outputs : UWC - Point in WC system.
C-             VWC - Direction in WC system
C-   Controls:
C-
C-   Created    27-APR-1994   Alexander V. Vlassov
C-   Modified   21-AUG-1995   Alexander V. Vlassov
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
c  Local pre-defined variables:
c  ---------------------------
C----------------------------------------------------------------------
c rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rsc rcs rcs rcs
c rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rsc rcs rcs rcs
c                                                                         rcs
c  RCS information:                                                       rcs
c                                                                         rcs
      CHARACTER*(132)  CFILE, CREVIS, CSTATE, CDATE, CAUTHO, CRCSID
      PARAMETER (CFILE = '$RCSfile: cctowc.F,v $') 
      PARAMETER (CREVIS = '$Revision: 1.2 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 1997/04/24 15:38:12 $')
      PARAMETER (CAUTHO = '$Author: holtrop $')
      DATA CRCSID/ 
     1'$Id: cctowc.F,v 1.2 1997/04/24 15:38:12 holtrop Exp $'
     2/ 
c  Module information: 
      CHARACTER*(*)  CRNAME, CRAUTH
      PARAMETER (CRNAME='cctowc')                                   
      PARAMETER (CRAUTH='Alexander Vlassov')
c                                                                         rcs
c rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rsc rcs rcs rcs
c rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rsc rcs rcs rcs
c
c  Local User defined variables:
c  -----------------------------
c
      real teta, tm(3), pm(3), sh(3)
      data tm /0., 90., 90./
c_end_var
c
c  executable code for routine CCTOWC
c------------------------------------------------------------------------------
c
c   TO WC Reference System :
c
      teta = raddeg*csa0(ICH)
      call ucopy(sw0(1,ICH),sh,3)
c
      pm(1) =  0.
      pm(2) = -teta
      pm(3) = 90. - teta
c
      call CCREFS(R0,D0,tm,pm,sh,uwc,vwc)
C-
  999 RETURN
      END
