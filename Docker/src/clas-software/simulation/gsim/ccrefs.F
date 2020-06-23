CDECK  ID>, CCREFS.
      SUBROUTINE CCREFS(r0,dir0,t0,ph0,sh,r,dir)
c
c_begin_doc
c  RCS ID string
c  $Id: ccrefs.F,v 1.2 1997/04/24 15:38:11 holtrop Exp $
c
c  Documentation for subroutine CCREFS(r0,dir0,t0,ph0,sh,r,dir)
c
C--------------------------------------------------------------
C-
C-   Purpose and Methods : Reference system change
C-
C-   Inputs  : r0(3)   - point coordinates in "OLD" system
C-             dir0(3) - vector direction  in "OLD" system
C-             t0(i)   - polar angle "TETA"  of "NEW" i-axis
C-             (angle between "NEW" i-axis and "OLD" Z-axis)
C-             ph0(i)  - azimutal angle "PHI" of "NEW" i-axis
C-             (angle between (X,Y) plane projection of "NEW" i-axis
C-              and "OLD" X-axis)
C-         ****   All angles in degrees  ****
C-
C-             sh(3) - Coordinates of the origin of "NEW" system in "OLD" one
C-
C-   Outputs : r(3)   - point coordinates in "NEW" system
C-             dir(3) - vector direction  in "NEW" system
C-   Controls:
C-
C-   Created    15-APR-1994   Alexander V. Vlassov
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
      REAL r0(3),dir0(3),t0(3),ph0(3),sh(3),r(3),dir(3)
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
      PARAMETER (CFILE = '$RCSfile: ccrefs.F,v $') 
      PARAMETER (CREVIS = '$Revision: 1.2 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 1997/04/24 15:38:11 $')
      PARAMETER (CAUTHO = '$Author: holtrop $')
      DATA CRCSID/ 
     1'$Id: ccrefs.F,v 1.2 1997/04/24 15:38:11 holtrop Exp $'
     2/ 
c  Module information: 
      CHARACTER*(*)  CRNAME, CRAUTH
      PARAMETER (CRNAME='ccrefs')                                   
      PARAMETER (CRAUTH='Alexander Vlassov')
c                                                                         rcs
c rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rsc rcs rcs rcs
c rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rsc rcs rcs rcs
c
c  Local User defined variables:
c  -----------------------------
      REAL told(3),phold(3)
c
      double precision un,a,b,c,a1,b1,c1,degrad,
     &  t(3,3),tet(3),phi(3)
      integer i
      PARAMETER (DEGRAD=0.0174532925199432958)
c
      data un/1.0d000/
c
c_end_var
c
c  executable code for routine CCREFS
c------------------------------------------------------------------------------
      do i = 1,3
        if( t0(i) .ne.  told(i) ) go to 1
        if(ph0(i) .ne. phold(i) ) go to 1
      end do
c
      go to 10
c
    1 continue
c
      do i = 1,3
        tet(i) = degrad* t0(i)
        phi(i) = degrad*ph0(i)
      end do
c
c  ============  Matrix   ==============
c
      do i = 1,3
        t(1,i) = sin(tet(i))*cos(phi(i))
        t(2,i) = sin(tet(i))*sin(phi(i))
        t(3,i) = cos(tet(i))
      end do
c
c  ============  Matrix   ==============
c
   10 continue
c     ======== Calculation
c
      a = 0.
      b = 0.
      c = 0.
c
      a1 = 0.
      b1 = 0.
      c1 = 0.
c
      do i = 1,3
        a = a + t(i,1)*(r0(i)-sh(i))
        b = b + t(i,2)*(r0(i)-sh(i))
        c = c + t(i,3)*(r0(i)-sh(i))
c
        a1 = a1 + t(i,1)*dir0(i)
        b1 = b1 + t(i,2)*dir0(i)
        c1 = c1 + t(i,3)*dir0(i)
c
      end do
c
c =======  point  ========
c
      r(1) = a
      r(2) = b
      r(3) = c
c
c =======  direction  ======
c
      dir(1) = a1
      dir(2) = b1
      dir(3) = c1
c
c  ===============> Now remember used t0 and ph0
c
      call ucopy( t0, told,3)
      call ucopy(ph0,phold,3)
c
  999 RETURN
      END
