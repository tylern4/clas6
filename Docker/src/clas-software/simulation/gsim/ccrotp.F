CDECK  ID>, CCROTP.
      SUBROUTINE CCROTP(x0,x1,x2,t1,t2,t3,p1,p2,p3)
c
c_begin_doc
c  RCS ID string
c  $Id: ccrotp.F,v 1.2 1997/04/24 15:38:11 holtrop Exp $
c
c  Documentation for subroutine CCROTP(x0,x1,x2,t1,t2,t3,p1,p2,p3)
c
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :  Calculation of the angles teta and phi for
C-                          Reference System, defined by 3 points
C-
C-   Inputs  :  x0 (3) - Center of R.S.
C-              x1 (3) - Defines X axes
C-              x2 (3) - Defines Y axes
C-
C-   Outputs :  t1,t2,t3 - angles TETA  (in DEGREES)
C-              p1,p2,p3 - angles PHI   (in DEGREES)
C-
C-   Comments:  [x1,x0] and [x2,x0] should be perpendicular optionally
C-              If not, then subroutine gives diagnostics and make
C-              ortogonalization, assume that X - direction is correct.
C-              Input parameters are not changed.
C-
C-   Created    06-JUL-1994   Alexander V. Vlassov
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
      real x0(3), x1(3), x2(3), t1, t2, t3, p1, p2, p3
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
      PARAMETER (CFILE = '$RCSfile: ccrotp.F,v $') 
      PARAMETER (CREVIS = '$Revision: 1.2 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 1997/04/24 15:38:11 $')
      PARAMETER (CAUTHO = '$Author: holtrop $')
      DATA CRCSID/ 
     1'$Id: ccrotp.F,v 1.2 1997/04/24 15:38:11 holtrop Exp $'
     2/ 
c  Module information: 
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='ccrotp')                                   
      PARAMETER (CRAUTH='Alexander Vlassov')
c                                                                         rcs
c rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rsc rcs rcs rcs
c rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rsc rcs rcs rcs
c
C-----------------------------------------------------------------------
c
c  Local User defined variables:
c  -----------------------------
c
      integer j
      DOUBLE PRECISION RADDEG,un,Z,SINMIN
      DOUBLE PRECISION V1(3),V2(3),V3(3),a1,a2,a3
      real vsmall
      data vsmall /0.000001/
c
      PARAMETER (RADDEG=57.2957795130823209)
      PARAMETER (un=1.D0,Z=0.D0)
      DATA SINMIN/1.00D-5/
c
c_end_var
c
c  executable code for routine CCROTP
C--------------------------------------------------------------
c
C-    Normalization
c
      a1 = Z
      a2 = Z
c
      do j = 1,3
        V1(j) = x1(j) - x0(j)
        V2(j) = x2(j) - x0(j)
        a1 = a1 + un*V1(j)*V1(j)
        a2 = a2 + un*V2(j)*V2(j)
      end do
c
      do j = 1,3
        V1(j) = V1(j)/sqrt(a1)
        V2(j) = V2(j)/sqrt(a2)
      end do
c
c   Ortogonality test
c
      a1 = Z
      do j = 1,3
        a1 = a1 + V1(j)*V2(j)
      end do
c
      if(abs(a1).gt.SINMIN) then
c     ==========================>  Non-ortogonal vectors !
c
        CRMESS = 'Wrong INITIAL data'
        call recmes(CRNAME,'W',CRMESS)
        CRMESS = ' *** CCROTP *** Non-ortogonal vectors!'
        call recmes(CRNAME,'W',CRMESS)
        write(CRMESS,*) ' ***   V1   ***',V1
        call recmes(CRNAME,'N',CRMESS)
        write(CRMESS,*) ' ***   V2   ***',V2
        call recmes(CRNAME,'N',CRMESS)
        CRMESS = ' Vectors will be modified to be ortogonal!'
        call recmes(CRNAME,'W',CRMESS)
c
C.     =====     Assume V1 to be correct    =====
c
        a2 = Z
        do j = 1,3
          V2(j) = V2(j) - V1(j)*a1
          a2 = a2 + un*V2(j)*V2(j)
        end do
c
        do j = 1,3
          V2(j) = V2(j)/sqrt(a2)
        end do
c
      end if
c
c     Making V3
c
      V3(1) = V1(2)*V2(3) - V1(3)*v2(2)
      V3(2) = V1(3)*V2(1) - V1(1)*v2(3)
      V3(3) = V1(1)*V2(2) - V1(2)*v2(1)
      a1 = Z + V3(1)*V3(1) + V3(2)*v3(2) + V3(3)*v3(3)
c
      do j = 1,3
        V3(j) = V3(j)/sqrt(a1)
      end do
c
c   Now define angles:
c
      a1 = acos(V1(3))
      a2 = acos(V2(3))
      a3 = acos(V3(3))
c
      if(V1(2).gt.0.) then
        p1 =  RADDEG*acos(V1(1)/sqrt(V1(1)*V1(1) + V1(2)*V1(2)))
      else if(V1(2).lt.0.) then
        p1 = -RADDEG*acos(V1(1)/sqrt(V1(1)*V1(1) + V1(2)*V1(2)))
      else if(abs(V1(2)).lt.vsmall) then
        p1 = 0.
        if(V1(1).lt.0.) p1 = 180.
      end if
c
      if(V2(2).gt.0.) then
        p2 =  RADDEG*acos(V2(1)/sqrt(V2(1)*V2(1) + V2(2)*V2(2)))
      else if(V2(2).lt.0.) then
        p2 = -RADDEG*acos(V2(1)/sqrt(V2(1)*V2(1) + V2(2)*V2(2)))
      else if(abs(V2(2)).lt.vsmall) then
        p2 = 0.
        if(V2(1).lt.0.) p2 = 180.
      end if
c
      if(V3(2).gt.0.) then
        p3 =  RADDEG*acos(V3(1)/sqrt(V3(1)*V3(1) + V3(2)*V3(2)))
      else if(V3(2).lt.0.) then
        p3 = -RADDEG*acos(V3(1)/sqrt(V3(1)*V3(1) + V3(2)*V3(2)))
      else if(abs(V3(2)).lt.vsmall) then
        p3 = 0.
        if(V3(1).lt.0.) p3 = 180.
      end if
c
      t1 = RADDEG*a1
      t2 = RADDEG*a2
      t3 = RADDEG*a3
c
      return
      end
