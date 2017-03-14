
CDECK  ID>, CCWCFRS.
      SUBROUTINE CCWCFRS(ncc,x_in,x_fn)
c
c_begin_doc
c  RCS ID string
c  $Id: ccwcfrs.F,v 1.2 1997/04/24 15:38:15 holtrop Exp $
c
c  Documentation for subroutine CCWCFRS(ncc,x_in,x_fn)
c
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : To transfer point x_in in Shifted WC R.S.
C-                         to WC  R.S. (x_fn)
C-
C-   Inputs  :  ncc     -  Segment #
C-              x_in(3) -  Point coordinates in the SWC R.S.
C-              /CCP/ is used to extract CC parameters
C-
C-   Outputs :  x_fn(3) -  Point coordinates in the WC  R.S.
C-   Comments:  x_in and x_fn could be the same
C-
C-   Created    23-JUN-1994   Alexander V. Vlassov
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
#include "ccp.inc"
c
c_end_inc
c
c_begin_var
c  input/output variables:
c  -----------------------
c
      integer ncc
      real x_in(3),x_fn(3)
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
      PARAMETER (CFILE = '$RCSfile: ccwcfrs.F,v $') 
      PARAMETER (CREVIS = '$Revision: 1.2 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 1997/04/24 15:38:15 $')
      PARAMETER (CAUTHO = '$Author: holtrop $')
      DATA CRCSID/ 
     1'$Id: ccwcfrs.F,v 1.2 1997/04/24 15:38:15 holtrop Exp $'
     2/ 
c  Module information: 
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='ccwcfrs')                                   
      PARAMETER (CRAUTH='Alexander Vlassov')
c                                                                         rcs
c rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rsc rcs rcs rcs
c rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rsc rcs rcs rcs
c
c
c  Local User defined variables:
c  -----------------------------
c
      integer j
      DOUBLE PRECISION DEGRAD
      DOUBLE PRECISION tet,a(3)
c
      PARAMETER (DEGRAD=0.0174532925199432958)
c
c_end_var
c
c  executable code for routine CCWCFRS
c-----------------------------------------------------------------------------
c
      if(ncc.le.0.or.ncc.gt.18) then
        write(CRMESS,*) 'Wrong CC number! (1-18):',ncc
        call recmes(CRNAME,'E',CRMESS)
      end if
c
C----------------------------------------------------------------------
C
      tet = DEGRAD*wcang(ncc)
c
      a(1) = cos(tet)*x_in(1) - sin(tet)*x_in(2)
      a(2) = sin(tet)*x_in(1) + cos(tet)*x_in(2)
      a(3) = x_in(3)
c
      do j = 1,3
        x_fn(j) = a(j)
      end do
c
      RETURN
      END
