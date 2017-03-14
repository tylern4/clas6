      SUBROUTINE CCWCMCR(r0,dir,P,IRES,S1,R1,S2,R2)
c
c_begin_doc
c  RCS ID string
c  $Id: ccwcmcr.F,v 1.2 1997/04/24 15:38:15 holtrop Exp $
c
c  Documentation for subroutine CCWCMCR
c
C-   Purpose and Methods : Seaching the crossing of Stright Line
C-                         with 3D, 2nd order CYLINDRICAL surface
C-
C-   Inputs: r0(3)  - starting point on the line in the WC R.S.
C-           dir(3) - direction cosines of the line in the WC R.S.
C-                    _   _    _
C-           so, that R = r0 + d*S is the line equation
C-           Direction of line is defined by S > 0.
C-
C-           P(3)  - Parameters of cylinder :
C-           P(1)*x**2 + P(2)*y**2 + 1 = 0
C-
C-  Outputs: IRES  - 0 NO crosses with the surface.
C-                 - 1 ONE cross point at POSITIVE direction.
C-                 - 2 TWO cross points at positive direction.
C-           S1    - Distance to the nearest cross point
C-           R1(3) - coordinates of the nearest cross point in
C-                   positive direction.
C-           S2    - Distance to the other cross point
C-           R2(3) - coordinates of the other cross point in
C-                   positive direction.
C-
C-   Created  06-OCT-1996   Alexander V. Vlassov
C-
c  Author:   Alexander Vlassov      Created:  Sun Oct  6 11:22:54 EDT 1996
c  -------
c
c  Major revisions:
c  ----------------     
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
C
      REAL r0(3),dir(3),P(3),S1,S2,R1(3),R2(3)
      INTEGER IRES
c
c  Local pre-defined variables:
c  ---------------------------
c  RCS information: 
      CHARACTER*132  CFILE, CREVIS, CSTATE, CDATE, CAUTHO, CRCSID
      PARAMETER (CFILE = '$RCSfile: ccwcmcr.F,v $')
      PARAMETER (CREVIS = '$Revision: 1.2 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 1997/04/24 15:38:15 $')
      PARAMETER (CAUTHO = '$Author: holtrop $')
      DATA CRCSID/   
     1'$Id: ccwcmcr.F,v 1.2 1997/04/24 15:38:15 holtrop Exp $'   
     2/   
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      PARAMETER (CRNAME='CCWCMCR')
      PARAMETER (CRAUTH='Alexander Vlassov')
c
c  Local User defined variables:
c  -----------------------------
c
      double precision un,a,b,c,d,t,vsmall
      integer i
c
      data vsmall/1.0d-04/
      data un/1.0d000/
c
c_end_var
c
c  executable code for routine CCWCMCR:
c----6----------------------------------------------------------------72
c
c
      a = 0.
      b = 0.
      c = 0.
c
      do i = 1,3
        a = a + un*P(i)*dir(i)*dir(i)
        b = b + un*P(i)*dir(i)* r0(i)
        c = c + un*P(i)* r0(i)* r0(i)
      end do
      c = c + un
c
c     Now t is a root of ( a*t*t + 2*b*t + c = 0 )
c
      d = b*b - a*c

c
      if(d.le.0.) then
        go to 10               !  NO cross point
c ***   case of d = 0 is equivalent of d<0 for us !
      else
        t = (-b - sqrt(d))/a
        c = (-b + sqrt(d))/a
c
        if(t.lt.vsmall .and. c.lt.vsmall ) then
          go to 10             ! NO cross point
        else if(t.lt.vsmall .and. c.ge.vsmall ) then
          t = c
          IRES = 1
        else if(c.lt.vsmall .and. t.ge.vsmall ) then
          IRES = 1
        else
          IRES = 2
          if(c.le.t) then
            S2  = t
            do i = 1,3
              R2(i) = r0(i) + t*dir(i)
            end do
            t = c
          else
            S2  = c
            do i = 1,3
              R2(i) = r0(i) + c*dir(i)
            end do
          end if
        end if
      end if
c
c ====================  CROSS point
c
      S1   = t
      do i = 1,3
        R1(i) = r0(i) + t*dir(i)
      end do
      return
c ====================  NO cross point
   10 continue
c
      IRES = 0
      S1    = 0.
      do i = 1,3
        R1(i) = 0.
      end do
c ====================
      RETURN
      END
c
c------------------------------------------------------------------------------




