C+DECK, CCTURNX.
      SUBROUTINE CCTURNX(ANGLE,X,P_IN,P_FN)
c
c_begin_doc
c  RCS ID string
c  $Id: ccturnx.F,v 1.2 1997/04/24 15:38:13 holtrop Exp $
c
c  Documentation for subroutine CCTURNX(ANGLE,X,P_IN,P_FN)
c
c     =====================================
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Calculation of new mirror parameters
C-            after the turn on ANGLE (in DEG) around point X(2)
C-
C-   Inputs  : ANGLE, X(2), P_IN(5)
C-   Outputs : P_FN(5)
C-   Controls: 
C-
C-   Created    01-JUN-1995   Alexander V. Vlassov
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
c
#include "gconsp.inc"
C
c_end_inc
c
c_begin_var
c  input/output variables:
c  -----------------------
c
      real angle,x(2)
      double precision p_in(5), p_fn(5)
c
c  Local pre-defined variables:
c  ---------------------------
C---------------------------------------------------------------------
c rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rsc rcs rcs rcs
c rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rsc rcs rcs rcs
c                                                                         rcs
c  RCS information:                                                       rcs
c                                                                         rcs
      CHARACTER*132  CFILE, CREVIS, CSTATE, CDATE, CAUTHO, CRCSID
      PARAMETER (CFILE = '$RCSfile: ccturnx.F,v $') 
      PARAMETER (CREVIS = '$Revision: 1.2 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 1997/04/24 15:38:13 $')
      PARAMETER (CAUTHO = '$Author: holtrop $')
      DATA CRCSID/ 
     1'$Id: ccturnx.F,v 1.2 1997/04/24 15:38:13 holtrop Exp $'
     2/ 
c  Module information: 
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='ccturnx')                      
      PARAMETER (CRAUTH='Alexander Vlassov')
c                                                                         rcs
c rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rsc rcs rcs rcs
c rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rsc rcs rcs rcs
c
c  Local User defined variables:
c  -----------------------------
c
      real xt(2)
      double precision p_t1(5), p_t2(5), c, s 
c_end_var
c
c  executable code for routine CCTURNX
c------------------------------------------------------------------------------
C
C     SHIFT - 
C
      xt(1) = -x(1)
      xt(2) = -x(2)
      call CCSHFT(xt,p_in,p_t1)
C
C     TURN on ANGLE 
C
      c = cos(DEGRAD*angle)
      s = sin(DEGRAD*angle)
C
      p_t2(1) = p_t1(1)*c*c + p_t1(2)*s*s - p_t1(3)*s*c
      p_t2(2) = p_t1(1)*s*s + p_t1(2)*c*c + p_t1(3)*s*c
      p_t2(3) = 2.*(p_t1(1) - p_t1(2))*s*c + p_t1(3)*(c*c - s*s)
      p_t2(4) = p_t1(4)*c - p_t1(5)*s
      p_t2(5) = p_t1(5)*c + p_t1(4)*s
C
C     SHIFT + 
C
      call CCSHFT(x,p_t2,p_fn)
C
  999 RETURN
      END
