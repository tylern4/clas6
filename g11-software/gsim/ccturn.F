CDECK  ID>, CCTURN.
      SUBROUTINE CCTURN(ANGLE,P_IN,P_FN)
c
c_begin_doc
c  RCS ID string
c  $Id: ccturn.F,v 1.2 1997/04/24 15:38:13 holtrop Exp $
c
c  Documentation for subroutine CCTURN(ANGLE,P_IN,P_FN)
c
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Calculation of new mirror parameters after
C-                         the turn on angle ANGLE around (0,0,0) point
C-
C-   Inputs  : ANGLE, P_IN(5)
C-   Outputs : P_FN(5)
C-   Controls:
C-
C-   Created    15-MAR-1994   Alexander V. Vlassov
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
c_end_inc
c
c_begin_var
c  input/output variables:
c  -----------------------
c
      real angle
      double precision p_in(5), p_fn(5)
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
      PARAMETER (CFILE = '$RCSfile: ccturn.F,v $') 
      PARAMETER (CREVIS = '$Revision: 1.2 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 1997/04/24 15:38:13 $')
      PARAMETER (CAUTHO = '$Author: holtrop $')
      DATA CRCSID/ 
     1'$Id: ccturn.F,v 1.2 1997/04/24 15:38:13 holtrop Exp $'
     2/ 
c  Module information: 
      CHARACTER*(*)  CRNAME, CRAUTH
      PARAMETER (CRNAME='ccturn')                                   
      PARAMETER (CRAUTH='Alexander Vlassov')
c                                                                         rcs
c rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rsc rcs rcs rcs
c rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rsc rcs rcs rcs
c
C----------------------------------------------------------------------
c
c  Local User defined variables:
c  -----------------------------
      double precision a,c,s
C
c_end_var
c
c  executable code for routine CCTURN
c------------------------------------------------------------------------------
      a = angle
      c = cos(a)
      s = sin(a)
C
      p_fn(1) = p_in(1)*c*c + p_in(2)*s*s - p_in(3)*s*c
      p_fn(2) = p_in(1)*s*s + p_in(2)*c*c + p_in(3)*s*c
      p_fn(3) = 2.*(p_in(1) - p_in(2))*s*c + p_in(3)*(c*c - s*s)
      p_fn(4) = p_in(4)*c - p_in(5)*s
      p_fn(5) = p_in(5)*c + p_in(4)*s
C
  999 RETURN
      END
