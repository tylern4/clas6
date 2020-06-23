CDECK  ID>, CC1TO2.
      SUBROUTINE CC1TO2( NCC1, r0, dir0, NCC2, r, dir)
c_begin_doc
c  RCS ID string
c  $Id: cc1to2.F,v 1.2 1997/04/24 15:38:04 holtrop Exp $
c
c  Documentation for subroutine  CC1TO2
c
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :  Transformation from NCC1 R.S to NCC2 R.S.
C-
C-   Inputs  : NCC1 - number of initial CC Reference System
C-               r0 - point coordinates
C-             dir0 - direction
C-             NCC2 - number of final CC Reference System
C-
C-   Outputs :    r - point coordinates in the new R.S.
C-              dir - direction in the new R.S.
C-   Controls:
C-
C-   Created   1-MAY-1994   Alexander V. Vlassov
C-   Modified   18-AUG-1995   Alexander V. Vlassov
C-
C----------------------------------------------------------------------
c_end_doc
c
      IMPLICIT NONE
      SAVE
c
c_begin_inc
c  include files :
c  ---------------------
#include "ccp.inc"
#include "gconsp.inc"
c  ---------------------
c_end_inc
c
c_begin_var
c  input/output variables:
c  -----------------------
c
      INTEGER NCC1,NCC2
      REAL r0(3),dir0(3),r,dir
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
      PARAMETER (CFILE = '$RCSfile: cc1to2.F,v $') 
      PARAMETER (CREVIS = '$Revision: 1.2 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 1997/04/24 15:38:04 $')
      PARAMETER (CAUTHO = '$Author: holtrop $')
      DATA CRCSID/ 
     1'$Id: cc1to2.F,v 1.2 1997/04/24 15:38:04 holtrop Exp $'
     2/ 
c  Module information: 
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='cc1to2')                                   
      PARAMETER (CRAUTH='Alexander Vlassov')
c                                                                         rcs
c rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rsc rcs rcs rcs
c rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rsc rcs rcs rcs
c
c  Local User defined variables:
c  -----------------------------
c
      real teta,tm(3),pm(3),Z(3)
c
      data pm / 0., 90., 90./
      data Z  / 0.,  0.,  0./
c
c_end_var
c
c  executable code for routine CC1TO2:
c  ===================================
c
      if(NCC1.le.0 .or. NCC1.gt.18) go to 10
      if(NCC2.le.0 .or. NCC2.gt.18) go to 10
c
      if(NCC1.eq.NCC2) then
c
        call ucopy(r0,r,3)
        call ucopy(dir0,dir,3)
c
      else
c
        teta = th(NCC2) - th(NCC1)
c
        tm(1) = 90.0
        tm(2) = 90.0 - teta
        tm(3) = - teta
c
        call CCREFS(r0,dir0,tm,pm,Z,r,dir)
c
      end if
      return
c
   10 continue   !  Error in parameters !
c
      CRMESS = 'WRONG numbers of CC !'
      call recmes(CRNAME,'E',CRMESS)
C------------------------------------------------------------
C
  999 RETURN
      END
