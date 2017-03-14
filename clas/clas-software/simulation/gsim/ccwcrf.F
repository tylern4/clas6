CDECK  ID>, CCWCRF.
      SUBROUTINE CCWCRF(r0,D0,P,dir)
c
c_begin_doc
c  RCS ID string
c  $Id: ccwcrf.F,v 1.2 1997/04/24 15:38:16 holtrop Exp $
c
c  Documentation for subroutine CCWCRF(r0,D0,P,dir)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Reflection off 3D, 2nd order ELIPZOID surface
C-                         in Ellipzoid Reference System.
C-
C-
C-   Inputs  : r0(3)  - Reflection point
C-             D0(3)  - Initial vector
C-             P(3)   - Parameters of ellips in it's coordinate SYSTEM :
C-                      Ellipzoid is described as
C-                      P(1)*x**2 + P(2)*y**2 + P(3)*z**2 + 1 = 0
C-
C-   Outputs : dir(3) - New unit vector
C-
C-   Created    21-APR-1994   Alexander V. Vlassov
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
      REAL r0(3),D0(3),P(3),dir(3)
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
      PARAMETER (CFILE = '$RCSfile: ccwcrf.F,v $') 
      PARAMETER (CREVIS = '$Revision: 1.2 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 1997/04/24 15:38:16 $')
      PARAMETER (CAUTHO = '$Author: holtrop $')
      DATA CRCSID/ 
     1'$Id: ccwcrf.F,v 1.2 1997/04/24 15:38:16 holtrop Exp $'
     2/ 
c  Module information: 
      CHARACTER*(*)  CRNAME, CRAUTH
      PARAMETER (CRNAME='ccwcrf')                                   
      PARAMETER (CRAUTH='Alexander Vlassov')
c                                                                         rcs
c rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rsc rcs rcs rcs
c rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rsc rcs rcs rcs
c
c  Local User defined variables:
c  -----------------------------
c
      REAL pl(3)
      integer i
c_end_var
c
c  executable code for routine CCWCRF
c------------------------------------------------------------------------------
c
      do i = 1,3
        pl(i) = P(i)*r0(i)
      end do
c
      call CCREFLPL(D0,pl,dir)
c
      RETURN
      END
