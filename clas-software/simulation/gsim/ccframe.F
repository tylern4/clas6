CDECK  ID>, CCFRAME.
      SUBROUTINE CCFRAME(nch,sxy,x0,y0)
c
c_begin_doc
c  RCS ID string
c  $Id: ccframe.F,v 1.2 1997/04/24 15:38:08 holtrop Exp $
c
c  Documentation for subroutine CCFRAME(nch,sxy,x0,y0)
c
C-----------------------------------------------------------------------
C-
C-   Purpose and Methods : CC Frame drawing
C-
C-   Inputs  : nch - CC number
C-             sxy - scale factor
C-             x0,y0 - shift for picture
C-
C-   Outputs : picture of frame (HIGZ)
C-   Controls:
C-
C-   Created    28-JUL-1993   Mikhail V. KOSSOV
C-   Modified   06-MAR-1994   Alexander V. Vlassov
C-   Modified   29-APR-1994   Alexander V. Vlassov
C-   Modified   18-AUG-1995   Alexander V. Vlassov
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
C-----------------------------------------------------------------------
#include "ccp.inc"
#include "ccfrmp.inc"
C-----------------------------------------------------------------------
c_end_inc
c
c_begin_var
c  input/output variables:
c  -----------------------
c
      integer nch
      real sxy,x0,y0
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
      PARAMETER (CFILE = '$RCSfile: ccframe.F,v $') 
      PARAMETER (CREVIS = '$Revision: 1.2 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 1997/04/24 15:38:08 $')
      PARAMETER (CAUTHO = '$Author: holtrop $')
      DATA CRCSID/ 
     1'$Id: ccframe.F,v 1.2 1997/04/24 15:38:08 holtrop Exp $'
     2/ 
c  Module information: 
      CHARACTER*(*)  CRNAME, CRAUTH
      PARAMETER (CRNAME='ccframe')                                   
      PARAMETER (CRAUTH='Alexander Vlassov')
c                                                                         rcs
c rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rsc rcs rcs rcs
c rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rsc rcs rcs rcs
c
c  Local User defined variables:
c  -----------------------------
c
      integer j,k
      real x1(mirrn),x2(mirrn),y(mirrn)
c
c_end_var
c
c  executable code for routine CCFRAME
c-----------------------------------------------------------------------
*      call ISPLCI(1)
      if(mod(nch,4).eq.0) then
        call ISPLCI(1)
      else if(mod(nch,4).eq.1) then
        call ISPLCI(2)
      else if(mod(nch,4).eq.2) then
        call ISPLCI(3)
      else if(mod(nch,4).eq.3) then
        call ISPLCI(4)
      end if
      call isln(1)
C-----------------------------------------------------------------------
c
      do j = 1,mirrn
c
        x1(j) =  xell(j,nch)*sxy + x0
        x2(j) = -xell(j,nch)*sxy + x0
        y (j) =  yell(j,nch)*sxy + y0
c
      end do
c
      CALL IPL(mirrn,x1,y)   ! elliptical mirror
      CALL IPL(mirrn,x2,y)   ! elliptical mirror
c ===============================================
c
      do j = 1,mirrn
c
        x1(j) =  xhyp(j,nch)*sxy + x0
        x2(j) = -xhyp(j,nch)*sxy + x0
        y (j) =  yhyp(j,nch)*sxy + y0
c
      end do
c
      CALL IPL(mirrn,x1,y)   ! hyperbolic mirror
      CALL IPL(mirrn,x2,y)   ! hyperbolic mirror
c ===============================================
c
      do j = 1,nwcon
c
        x1(j) =  xw1(j,nch)*sxy + x0
        x2(j) = -xw1(j,nch)*sxy + x0
        y (j) =  yw1(j,nch)*sxy + y0
c
      end do
c
      CALL IPL(nwcon,x1,y)   ! WC surface
      CALL IPL(nwcon,x2,y)   ! WC surface
c ========================================
c
      do j = 1,nwcon
c
        x1(j) =  xw2(j,nch)*sxy + x0
        x2(j) = -xw2(j,nch)*sxy + x0
        y (j) =  yw2(j,nch)*sxy + y0
c
      end do
c
      CALL IPL(nwcon,x1,y)   ! WC surface
      CALL IPL(nwcon,x2,y)   ! WC surface
c ========================================
c
C --- Straight walls
c
      x1(1) =  x21(nch)*sxy + x0
      x2(1) = -x21(nch)*sxy + x0
      y (1) =  yhyp(mirrn,nch)*sxy + y0
 
      x1(2) =  x22(nch)*sxy + x0
      x2(2) = -x22(nch)*sxy + x0
      y (2) =  y22(nch)*sxy + y0
 
      x1(3) =  x11(nch)*sxy + x0
      x2(3) = -x11(nch)*sxy + x0
      y (3) =  y11(nch)*sxy + y0
 
      x1(4) =  x12(nch)*sxy + x0
      x2(4) = -x12(nch)*sxy + x0
      y (4) =  yell(mirrn,nch)*sxy + y0
c
      CALL IPL(4,x1,y)  ! Coil plane, WC window, PMT wall.
      CALL IPL(4,x2,y)  ! Coil plane, WC window, PMT wall.
c
C --- Photocathod
c
      x1(1) =  xp1(nch)*sxy + x0
      x2(1) = -xp1(nch)*sxy + x0
      y (1) =  yp1(nch)*sxy + y0
 
      x1(2) =  xp2(nch)*sxy + x0
      x2(2) = -xp2(nch)*sxy + x0
      y (2) =  yp2(nch)*sxy + y0
 
      call ISLWSC (3.)
      call IPL(2,x1,y)      !  PMT
      call IPL(2,x2,y)      !  PMT
      call ISLWSC (1.)
c
c --- Middle plane
c
      x1(1) = x0
      y (1) = yhyp(1,nch)*sxy + y0
 
      x1(2) = x0
      y (2) = yell(1,nch)*sxy + y0
c
      call isln(2)
      CALL IPL(2,x1,y)     ! Middle plane
      call isln(1)
c
C --- Magnetic shield walls
c =========================================
c
      call ISPLCI(3)          ! Color
c
      do k = 1,2           ! Wall #
c
        do j = 1,4         ! Point #
c
          x1(j) =  xcms(j,k,nch)*sxy + x0
          x2(j) = -xcms(j,k,nch)*sxy + x0
          y (j) =  ycms(j,k,nch)*sxy + y0
c
        end do
c
        x1(5) =  xcms(1,k,nch)*sxy + x0
        x2(5) = -xcms(1,k,nch)*sxy + x0
        y (5) =  ycms(1,k,nch)*sxy + y0
c
      CALL IPL(5,x1,y)
      CALL IPL(5,x2,y)
c
      end do
c
      x1(1) =  xcms(4,3,nch)*sxy + x0
      x2(1) = -xcms(4,3,nch)*sxy + x0
      y (1) =  ycms(4,3,nch)*sxy + y0
c
      x1(2) =  xcms(3,3,nch)*sxy + x0
      x2(2) = -xcms(3,3,nch)*sxy + x0
      y (2) =  ycms(3,3,nch)*sxy + y0
c
      x1(3) =  xcms(4,4,nch)*sxy + x0
      x2(3) = -xcms(4,4,nch)*sxy + x0
      y (3) =  ycms(4,4,nch)*sxy + y0
c
      CALL IPL(3,x1,y)
      CALL IPL(3,x2,y)
c
c =========================================
      call ISPLCI(1)          ! Color
c
C --- End of mirror walls creation ----*************************
c
      RETURN
      END
