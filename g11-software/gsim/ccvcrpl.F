CDECK  ID>, CCVCRPL.
      SUBROUTINE CCVCRPL(R0,DIR,P,IRES,S,R)
c
c_begin_doc
c  RCS ID string
c  $Id: ccvcrpl.F,v 1.2 1997/04/24 15:38:13 holtrop Exp $
c
c  Documentation for subroutine CCVCRPL(R0,DIR,P,IRES,S,R)
c
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : crossing of the stright line(R0,d)
C-                         with a plane
C-
C-   Inputs  :   R0(3) - initial point of line
C-               D (3) - vector direction: r = R0 + s*D
C-               P (3) - plane parameters:
C-               P(1)*x + P(2)*y + P(3)*z + 1 = 0
C-
C-   Outputs :   IRES =  0 - no cross with the plane.
C-                       1 - cross in positive direction
C-                      -1 - cross in negative direction
C-               S    =  Distance to the cross point
C-               R(3) =  Cross point coordinates.
C-
C-   Controls:
C-
C-   Created    14-APR-1994   Alexander V. Vlassov
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
c_end_inc
c
c_begin_var
c  input/output variables:
c  -----------------------
c
      INTEGER IRES
      REAL r0(3),dir(3),P(3),R(3),S
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
      PARAMETER (CFILE = '$RCSfile: ccvcrpl.F,v $') 
      PARAMETER (CREVIS = '$Revision: 1.2 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 1997/04/24 15:38:13 $')
      PARAMETER (CAUTHO = '$Author: holtrop $')
      DATA CRCSID/ 
     1'$Id: ccvcrpl.F,v 1.2 1997/04/24 15:38:13 holtrop Exp $'
     2/ 
c  Module information: 
      CHARACTER*(*)  CRNAME, CRAUTH
c      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='ccvcrpl')                                   
      PARAMETER (CRAUTH='Alexander Vlassov')
c                                                                         rcs
c rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rsc rcs rcs rcs
c rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rsc rcs rcs rcs
c
c  Local User defined variables:
c  -----------------------------
c
      double precision un,a,b,t,c,D(3)
      real vsmall
      integer i
c
      data un/1.0d000/
      data vsmall /0.000001/
c
c_end_var
c
c  executable code for routine CCVCRPL
c------------------------------------------------------------------------------
      a = 0.
      b = 0.
      S = 0.
      c = 0.
c
      do i = 1,3
        c = c + un*dir(i)*dir(i)
      end do
      c = dsqrt(c)
c
      do i = 1,3
        d(i) = un*dir(i)/c
      end do
c
      do i = 1,3
        a = a + un*P(i)*d(i)
        b = b + un*P(i)*r0(i)
      end do
c
      b = b + un
c
      if(abs(b).le.vsmall) then  ! point on the plane
        do i = 1,3
          r(i) = R0(i)
        end do
        IRES = 1
      else
        if(abs(a).le.vsmall) then
          do i = 1,3
            r(i) = 0.
          end do
          IRES = 0
        else
          t = -b/a
          do i = 1,3
            R(i) = t*d(i) + r0(i)
          end do
          IRES = 1
          S = t
          if(t.lt.0.) IRES = -1
        end if
      end if
c
  999 RETURN
      END
