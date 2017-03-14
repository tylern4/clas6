CDECK  ID>, CCDPLN.
      SUBROUTINE CCDPLN(X0,P,DX)
c
c_begin_doc
c  RCS ID string
c  $Id: ccdpln.F,v 1.2 1997/04/24 15:38:07 holtrop Exp $
c
c  Documentation for subroutine CCDPLN
c
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Calculation of the distance between point
C-       and plane
C-
C-   Inputs  : X0 - coordinates of the  point
C-             P - Plane parameters in form
C-             P(1)*x + P(2)*y + P(3)*z + 1. = 0.
C-
C-   Outputs : DX - distance from Point to PLANE (P) (with sign)
C-   Controls:
C-
C-   Created    13-APR-1994   Alexander V. Vlassov
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
      real X0(3),P(3),DX
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
      PARAMETER (CFILE = '$RCSfile: ccdpln.F,v $') 
      PARAMETER (CREVIS = '$Revision: 1.2 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 1997/04/24 15:38:07 $')
      PARAMETER (CAUTHO = '$Author: holtrop $')
      DATA CRCSID/ 
     1'$Id: ccdpln.F,v 1.2 1997/04/24 15:38:07 holtrop Exp $'
     2/ 
c  Module information: 
      CHARACTER*(*)  CRNAME, CRAUTH
      PARAMETER (CRNAME='ccdpln')                                   
      PARAMETER (CRAUTH='Alexander Vlassov')
c                                                                         rcs
c rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rsc rcs rcs rcs
c rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rsc rcs rcs rcs
c
c  Local User defined variables:
c  -----------------------------
      double precision a,b,un
      integer i
      data un/1.0d000/
c_end_var
c
c  executable code for routine CCDPLN
c------------------------------------------------------------------------------
C
      a = 0.
      b = 0.
      do i = 1,3
        a = a + un*P(i)* P(i)
        b = b + un*P(i)*X0(i)
      end do
c
      a  = (b+un)/sqrt(a)
      DX = a
c
  999 RETURN
      END
