CDECK  ID>, CCROSE.
      SUBROUTINE CCROSE(r0,dir,P,IRES,S,R)
c
c_begin_doc
c  RCS ID string
c  $Id: ccrose.F,v 1.2 1997/04/24 15:38:11 holtrop Exp $
c
c  Documentation for subroutine CCROSE(r0,dir,P,IRES,S,R)
c
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Seaching the crossing of Stright Line
C-              with the elliptic mirror surface in positive direction
c-    *****  note, that we  use only one cross with biggest Y ****
C-
C-   Inputs: r0(3)  - starting point on the line
C-           dir(3) - direction cosines of the line
C-                    _   _    _
C-           so, that R = r0 + d*S is the line equation
C-           Direction of line is defined by t > 0.
C-
C-           P(5)  - Parameters of surface :
C-           P(1)*x**2 + P(2)*y**2 + P(3)*x*y + P(4)*x + P(5)*y + 1 = 0
C-
C-  Outputs: IRES  - 0 NO crosses with the surface.
C-                 - 1 ONE cross point at POSITIVE direction.
C-           S     - Distance to the mirror
C-           R(3)  - coordinates of the cross point in
C-                   positive direction.
C-
C-   Created    28-JUL-1993   Mikhail V. KOSSOV
C-   Modified   20-APR-1994   Alexander V. Vlassov
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
      REAL r0(3),dir(3),P(5),S,R(3)
      INTEGER IRES
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
      PARAMETER (CFILE = '$RCSfile: ccrose.F,v $') 
      PARAMETER (CREVIS = '$Revision: 1.2 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 1997/04/24 15:38:11 $')
      PARAMETER (CAUTHO = '$Author: holtrop $')
      DATA CRCSID/ 
     1'$Id: ccrose.F,v 1.2 1997/04/24 15:38:11 holtrop Exp $'
     2/ 
c  Module information: 
      CHARACTER*(*)  CRNAME, CRAUTH
      PARAMETER (CRNAME='ccrose')                                   
      PARAMETER (CRAUTH='Alexander Vlassov')
c                                                                         rcs
c rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rsc rcs rcs rcs
c rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rsc rcs rcs rcs
c
c
c  Local User defined variables:
c  -----------------------------
c
      double precision un,a,b,c,d,t
      integer i
c
      data un/1.0d000/
c_end_var
c
c  executable code for routine CCROSE
c------------------------------------------------------------------------------
c
      a = un*P(1)*dir(1)*dir(1) + un*P(2)*dir(2)*dir(2)
     &  + un*P(3)*dir(1)*dir(2)
      b = un*2.*P(1)*dir(1)* r0(1) + un*2.*P(2)*dir(2)* r0(2) +
     &    un*   P(3)*dir(1)* r0(2) + un*   P(3)*dir(2)* r0(1) +
     &    un*   P(4)*dir(1)        + un*   P(5)*dir(2)
      c = un*P(1)*r0(1)*r0(1) + un*P(2)*r0(2)*r0(2) +
     &    un*P(3)*r0(1)*r0(2) + un*P(4)*r0(1) + un*P(5)*r0(2) + un
c
c     Now t is a root of ( a*t*t + b*t + c = 0 )
c
      d = b*b - a*c*4.
c
      if(d.le.0) then
        go to 10                  !  NO cross point
c ***   case of d = 0 is equivalent of d<0 for us !
c
      else
c
        t = (-b - sqrt(d))/(a*2.)
        c = (-b + sqrt(d))/(a*2.)
c
        a = un*r0(2) + t*dir(2)
        b = un*r0(2) + c*dir(2)
c
        if(t.lt.0. .and. c.lt.0.) then
          go to 10                ! NO cross point in positive direction
c
        else if(t.lt.0. .and. c.ge.0.) then
          if(b.lt.a) go to 10     ! Wrong cross point
          t = c
c
        else if(c.lt.0. .and. t.ge.0.) then
          if(a.lt.b) go to 10     ! Wrong cross point
c
        else
          if(c.gt.t) then
            if(a.gt.b) go to 10   ! Wrong cross point
            t = c
c
          else
            if(b.gt.a) go to 10   ! Wrong cross point
c
          end if
        end if
      end if
c
c ====================  CROSS point
c
      IRES = 1
      S    = t
      do i = 1,3
        R(i) = r0(i) + t*dir(i)
      end do
      return
c ====================  NO cross point
   10 continue
c
      IRES = 0
      S    = 0.
      do i = 1,3
        R(i) = 0.
      end do
c ====================
  999 RETURN
      END
