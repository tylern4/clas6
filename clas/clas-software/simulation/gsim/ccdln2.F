CDECK  ID>, CCDLN2.
      SUBROUTINE CCDLN2(X0,X1,X2,DX)
c
c_begin_doc
c  RCS ID string
c  $Id: ccdln2.F,v 1.2 1997/04/24 15:38:07 holtrop Exp $
c
c  Documentation for subroutine CCDLN2
c
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Calculation of the distance between point
C-                         and stright line, defined by two points.
C-
C-   Inputs  : X0 - Point coordinates
C-             X1,X2 - coordinates of the  points on the line
C-
C-   Outputs : DX - distance from Point X0 to stright line (X1,X2)
C-
C-   Comments: Used Subroutine CCDLIN
C-
C-   Created     7-JUL-1994   Alexander V. Vlassov
C-   Modified   18-AUG-1995   Alexander V. Vlassov
C-
c
c_end_doc
c
C----------------------------------------------------------------------
      IMPLICIT NONE
      SAVE
C----------------------------------------------------------------------
c
c_begin_inc
c  include files :
c  ---------------------
c_end_inc
c
c_begin_var
c
c  input/output variables:
c  -----------------------
C
      real X0(3),X1(3),X2(3),DX
C
c  Local pre-defined variables:
c  ---------------------------
c rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rsc rcs rcs rcs
c rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rsc rcs rcs rcs
c                                                                         rcs
c  RCS information:                                                       rcs
c                                                                         rcs
      CHARACTER*(132)  CFILE, CREVIS, CSTATE, CDATE, CAUTHO, CRCSID
      PARAMETER (CFILE = '$RCSfile: ccdln2.F,v $') 
      PARAMETER (CREVIS = '$Revision: 1.2 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 1997/04/24 15:38:07 $')
      PARAMETER (CAUTHO = '$Author: holtrop $')
      DATA CRCSID/ 
     1'$Id: ccdln2.F,v 1.2 1997/04/24 15:38:07 holtrop Exp $'
     2/ 
c  Module information: 
      CHARACTER*(*)  CRNAME, CRAUTH
      PARAMETER (CRNAME='ccdln2')                                   
      PARAMETER (CRAUTH='Alexander Vlassov')
c                                                                         rcs
c rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rsc rcs rcs rcs
c rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rsc rcs rcs rcs
c
c  Local User defined variables:
c  -----------------------------
C
      real DIR(3)
      double precision a,d(3),un
      integer i
      data un/1.0d000/
C
c_end_var
c
c  executable code for routine  CCDLN2
c------------------------------------------------------------------------------
      a = 0.
      do i = 1,3
        d(i) = X2(i) - X1(i)
        a = a + un*d(i)*d(i)
      end do
c
      do i = 1,3
        d(i)   = d(i)/a
        DIR(i) = d(i)
      end do
c
      call CCDLIN(X1,DIR,X0,DX)
c
      return
      END
