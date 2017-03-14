CDECK  ID>, CCREFLOC.
      SUBROUTINE CCREFLOC(r0,D0,P,dir)
c
c_begin_doc
c  RCS ID string
c  $Id: ccrefloc.F,v 1.2 1997/04/24 15:38:10 holtrop Exp $
c
c  Documentation for subroutine CCREFLOC(r0,D0,P,dir)
C----------------********-----------------------------------------------
C-
C-   Purpose and Methods : Reflaction off second order curve in point r0
C-                         Attached plane parameters are calculated and
C-                         Then subroutine CCREFLPL is used.
C-
c_
C-   Inputs  : r0(3) - point of reflection
C-             D0(3) - vector in initial ray direction
C-             P(5)  - parameters of second order surface to reflect off
C-                       p1*x2+p2*y2+p3*xy+p4*x+p5*y+1=0
C-
C-   Outputs : dir(3) - new unit vector of ray directon after reflection
C-
C-   Created    28-JUN-1993   Mikhail V. KOSSOV
C-   Modified   22-APR-1994   Alexander V. Vlassov
C-   Modified   18-AUG-1995   Alexander V. Vlassov
C-----------------------------------------------------------------------
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
      REAL r0(3),D0(3),P(5),dir(3)
c
c  Local pre-defined variables:
c  ---------------------------
C-----------------------------------------------------------------------
c rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rsc rcs rcs rcs
c rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rsc rcs rcs rcs
c                                                                         rcs
c  RCS information:                                                       rcs
c                                                                         rcs
      CHARACTER*(132)  CFILE, CREVIS, CSTATE, CDATE, CAUTHO, CRCSID
      PARAMETER (CFILE = '$RCSfile: ccrefloc.F,v $') 
      PARAMETER (CREVIS = '$Revision: 1.2 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 1997/04/24 15:38:10 $')
      PARAMETER (CAUTHO = '$Author: holtrop $')
      DATA CRCSID/ 
     1'$Id: ccrefloc.F,v 1.2 1997/04/24 15:38:10 holtrop Exp $'
     2/ 
c  Module information: 
      CHARACTER*(*)  CRNAME, CRAUTH
      PARAMETER (CRNAME='ccrefloc')                                   
      PARAMETER (CRAUTH='Alexander Vlassov')
c                                                                         rcs
c rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rsc rcs rcs rcs
c rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rsc rcs rcs rcs
c
c  Local User defined variables:
c  -----------------------------
c
      REAL pl(3)
      double precision un,a,b,c
c
      data un/1.0d000/
c_end_var
c
c  executable code for routine CCREFLOC
c------------------------------------------------------------------------------
c
      a = un*2. + un*r0(1)*P(4) + un*r0(2)*P(5)
      b = (un*2.*P(1)*r0(1) + un*P(3)*r0(2) + un*P(4))/a
      c = (un*P(3)*r0(1) + un*2.*P(2)*r0(2) + un*P(5))/a
c
      pl(1) = b
      pl(2) = c
      pl(3) = 0.
c
      call CCREFLPL(D0,pl,dir)
c
      RETURN
      END
