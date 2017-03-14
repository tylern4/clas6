CDECK  ID>, CCDLIN.
      SUBROUTINE CCDLIN(X,D,X0,DX)
c
c_begin_doc
c  RCS ID string
c  $Id: ccdlin.F,v 1.2 1997/04/24 15:38:06 holtrop Exp $
c
c  Documentation for subroutine  CCDLIN
c
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Calculation of the distance between point
C-       and stright line.
C-
C-   Inputs  : X - coordinates of the initial point on the line
C-             D - cosx, cosy, cosz direction of the line
C-             X0 - Point
C-
C-   Outputs : DX - distance from Point to stright line (X,D)
C-   Controls:
C-
C-   Created    13-APR-1994   Alexander V. Vlassov
C-   Modified   18-AUG-1995   Alexander V. Vlassov
C-
c
c_end_doc
c
C----------------------------------------------------------------------
      IMPLICIT NONE
      SAVE
C----------------------------------------------------------------------
c_begin_inc
c  include files :
c  ---------------------
c_end_inc
c
c_begin_var
c  input/output variables:
c  -----------------------
c
      real X(3),D(3),X0(3),DX
c
c  Local pre-defined variables:
c  ---------------------------
c rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rsc rcs rcs rcs
c rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rsc rcs rcs rcs
c                                                                         rcs
c  RCS information:                                                       rcs
c                                                                         rcs
      CHARACTER*(132)  CFILE, CREVIS, CSTATE, CDATE, CAUTHO, CRCSID
      PARAMETER (CFILE = '$RCSfile: ccdlin.F,v $') 
      PARAMETER (CREVIS = '$Revision: 1.2 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 1997/04/24 15:38:06 $')
      PARAMETER (CAUTHO = '$Author: holtrop $')
      DATA CRCSID/ 
     1'$Id: ccdlin.F,v 1.2 1997/04/24 15:38:06 holtrop Exp $'
     2/ 
c  Module information: 
      CHARACTER*(*)  CRNAME, CRAUTH
      PARAMETER (CRNAME='ccdlin')                                   
      PARAMETER (CRAUTH='Alexander Vlassov')
c                                                                         rcs
c rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rsc rcs rcs rcs
c rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rsc rcs rcs rcs
c
c  Local User defined variables:
c  -----------------------------
      double precision a,b(3),y(3),un
      integer i
      data un/1.0d000/
c
c  executable code for routine CCDLIN
c------------------------------------------------------------------------------
c
      a = 0.
      do i = 1,3
        b(i) = X(i) - X0(i)
        a = a + un*d(i)*d(i)
      end do
 
      y(1) = b(3)*D(2) - b(2)*D(3)
      y(2) = b(1)*D(3) - b(3)*D(1)
      y(3) = b(2)*D(1) - b(1)*D(2)
 
      a = ( y(1)*y(1) + y(2)*y(2) + y(3)*y(3) )/ a
      a = sqrt(a)
 
      DX = a
 
  999 RETURN
      END
