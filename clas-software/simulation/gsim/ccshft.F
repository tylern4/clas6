      SUBROUTINE CCSHFT(X,P_IN,P_FN)
c
c_begin_doc
c  RCS ID string
c  $Id: ccshft.F,v 1.2 1997/04/24 15:38:11 holtrop Exp $
c
c  Documentation for subroutine CCSHFT(X,P_IN,P_FN)
c
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Calculation of new mirror parameters
C-                         after the SHIFT on vector X
C-
C-   Inputs  : X(2), P_IN(5)
C-   Outputs : P_FN(5)
C-   Controls: 
C-
C-   Created    22-MAR-1994   Alexander V. Vlassov
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
c_end_inc
c
c_begin_var
c  input/output variables:
c  -----------------------
c
      real x(2)
      double precision p_in(5),p_fn(5)
c
c  Local pre-defined variables:
c  ---------------------------
C----------------------------------------------------------------------
c rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rsc rcs rcs rcs
c rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rsc rcs rcs rcs
c                                                                         rcs
c  RCS information:                                                       rcs
c                                                                         rcs
      CHARACTER*132  CFILE, CREVIS, CSTATE, CDATE, CAUTHO, CRCSID
      PARAMETER (CFILE = '$RCSfile: ccshft.F,v $') 
      PARAMETER (CREVIS = '$Revision: 1.2 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 1997/04/24 15:38:11 $')
      PARAMETER (CAUTHO = '$Author: holtrop $')
      DATA CRCSID/ 
     1'$Id: ccshft.F,v 1.2 1997/04/24 15:38:11 holtrop Exp $'
     2/ 
c  Module information: 
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='ccshft')
      PARAMETER (CRAUTH='Alexander Vlassov')
c                                                                         rcs
c rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rsc rcs rcs rcs
c rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rsc rcs rcs rcs
c
c  Local User defined variables:
c  -----------------------------
      double precision d
c_end_var
c
c  executable code for routine CCSHFT
c------------------------------------------------------------------------------
C
      d = p_in(1)*x(1)*x(1) + p_in(2)*x(2)*x(2) + p_in(3)*x(1)*x(2)
     &  - p_in(4)*x(1) - p_in(5)*x(2) + 1.0
C     
      if(d.ne.0.) then
C 
        p_fn(1) = p_in(1)/d 
        p_fn(2) = p_in(2)/d
        p_fn(3) = p_in(3)/d
        p_fn(4) = (p_in(4) - 2.0*p_in(1)*x(1) - p_in(3)*x(2))/d
        p_fn(5) = (p_in(5) - 2.0*p_in(2)*x(2) - p_in(3)*x(1))/d
C
      else
        call ucopy(p_in,p_fn,5)
      end if
  999 RETURN
      END
c
