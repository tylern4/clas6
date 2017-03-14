      SUBROUTINE CCFRSG(ncc,x_in,x_fn)
C----------------------------------------------------------------------
c_begin_doc
c  RCS ID string
c  $Id: ccfrsg.F,v 1.2 1997/04/24 15:38:08 holtrop Exp $
c
c  Documentation for subroutine CCFRSG(ncc,x_in,x_fn)
c
CB)-----------------------------------------(begin of long description)
C-
C-   Purpose and Methods : To transfer point x_in in Sector  R.S.
C-                         to CC segment R.S. (x_fn)
C-
C-   Inputs  :  ncc     -  Segment #
C-              x_in(3) -  Point coordinates in the Sector  R.S.
C
C-              /CCP/ is used to extract CC parameters
C-                  
C-   Outputs :  x_fn(3) -  Point coordinates in the CC  Segment R.S.
C-   Comments:  x_in and x_fn could be the same 
C-
C-   Created  14-AUG-1995   Alexander V. Vlassov
C-
C----------------------------------------------------------------------
c
c_end_doc
c
      IMPLICIT NONE
      SAVE
c
c_begin_inc
c
#include "ccp.inc"
c  ---------------------
c_end_inc
c
c_begin_var
c  input/output variables:
c  -----------------------
c
      integer ncc
      real x_in(3),x_fn(3)
c
c
c  Local pre-defined variables:
c  ---------------------------
C
c rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rsc rcs rcs rcs
c rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rsc rcs rcs rcs
c                                                                         rcs
c  RCS information:                                                       rcs
c                                                                         rcs
      CHARACTER*132  CFILE, CREVIS, CSTATE, CDATE, CAUTHO, CRCSID
      PARAMETER (CFILE = '$RCSfile: ccfrsg.F,v $') 
      PARAMETER (CREVIS = '$Revision: 1.2 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 1997/04/24 15:38:08 $')
      PARAMETER (CAUTHO = '$Author: holtrop $')
      DATA CRCSID/ 
     1'$Id: ccfrsg.F,v 1.2 1997/04/24 15:38:08 holtrop Exp $'
     2/ 
c  Module information: 
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='ccfrsg')                                   
      PARAMETER (CRAUTH='Alexander Vlassov')
c                                                                         rcs
c rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rsc rcs rcs rcs
c rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rsc rcs rcs rcs
c
CD)----------------------------------(declare common blocks, variables)
C
c  Local User defined variables:
c  -----------------------------
C  External references:
C-----------------------------------------------------------------------
c
      integer i,j
      DOUBLE PRECISION T(3,3),DEGRAD
      DOUBLE PRECISION tet,a(3),un
c
      DATA un /1.0d000/
      PARAMETER (DEGRAD=0.0174532925199432958)
c
C----------------------------------------------------------------------
c_end_var
c
c  executable code for routine CCFRSG
c------------------------------------------------------------------------------
C
      if(ncc.le.0.or.ncc.gt.18) then
        write(CRMESS,*) 'Wrong CC number! (1-18):',ncc
        call recmes(CRNAME,'W',CRMESS)
      end if
c
C----------------------------------------------------------------------
C
      tet = DEGRAD*th(ncc)
C
      T(1,1) = 0.
      T(2,1) = un
      T(3,1) = 0.
c
      T(1,2) = sin(tet)
      T(2,2) = 0.
      T(3,2) = cos(tet)
c
      T(1,3) = cos(tet)
      T(2,3) = 0.
      T(3,3) = -sin(tet)
c
      do j = 1,3
c
        a(j) = 0.
        do i = 1,3
          a(j) = a(j) + T(i,j)*x_in(i)
        end do
      end do
c
      do j = 1,3
        x_fn(j) = a(j)
      end do
c
      RETURN
      END
