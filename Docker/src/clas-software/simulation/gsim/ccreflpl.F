CDECK  ID>, CCREFLPL.
      SUBROUTINE CCREFLPL(D0,P,dir)
c
c_begin_doc
c  RCS ID string
c  $Id: ccreflpl.F,v 1.2 1997/04/24 15:38:10 holtrop Exp $
c
c  Documentation for subroutine CCREFLPL(D0,P,dir)
c
C----------------********-----------------------------------------------
C-
C-   Purpose and Methods : 3D Reflection off the plane P.
C-
C-   Inputs  : D0   - vector in initial ray direction
C-             P(3) - Plane parameters: P(1)*x + P(2)*y + P(3)*z + 1 = 0
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
C
      REAL D0(3),dir(3),P(3)
c
c  Local pre-defined variables:
c  ---------------------------
c rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rsc rcs rcs rcs
c rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rsc rcs rcs rcs
c                                                                         rcs
c  RCS information:                                                       rcs
c                                                                         rcs
      CHARACTER*(132)  CFILE, CREVIS, CSTATE, CDATE, CAUTHO, CRCSID
      PARAMETER (CFILE = '$RCSfile: ccreflpl.F,v $') 
      PARAMETER (CREVIS = '$Revision: 1.2 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 1997/04/24 15:38:10 $')
      PARAMETER (CAUTHO = '$Author: holtrop $')
      DATA CRCSID/ 
     1'$Id: ccreflpl.F,v 1.2 1997/04/24 15:38:10 holtrop Exp $'
     2/ 
c  Module information: 
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='ccreflpl')                                   
      PARAMETER (CRAUTH='Alexander Vlassov')
c                                                                         rcs
c rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rsc rcs rcs rcs
c rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rsc rcs rcs rcs
c
c  Local User defined variables:
c  -----------------------------
      real vsmall
c
      double precision un,a,b,c,t
      integer i
c
      data vsmall/1.0E-16/
      data un/1.0d000/
c
c_end_var
c
c  executable code for routine CCREFLPL
c------------------------------------------------------------------------------
      a = 0.
      b = 0.
      c = 0.
c
      do i = 1,3
        a = a + un* P(i)* P(i)
        b = b + un* P(i)*D0(i)
        c = c + un*D0(i)*D0(i)
      end do
c
      c = sqrt(c)
c
      if(a.lt.vsmall) then
c     ====================  *** ERROR ***
c
        CRMESS = 'WRONG plane parameters !'
        call recmes(CRNAME,'E',CRMESS)
C
      end if
c
      do i = 1,3
c
        t      = (un*D0(i) - un*2.*b*P(i)/a)/c
        dir(i) = t
c
      end do
c
  999 RETURN
      END
