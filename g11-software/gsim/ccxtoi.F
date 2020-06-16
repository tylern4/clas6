CDECK  ID>, CCXTOI.
      SUBROUTINE CCXTOI(nw,ix)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Make integer array ix for storing
C-                         into the bank
C-
C-   Inputs  :  /ccph/
C-   Outputs :  nw - number of words in array ix
C-              ix - integer array
C-   Controls:
C-
C-   Created  12-MAY-1994   Alexander V. Vlassov
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
c rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rsc rcs rcs rcs
c rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rsc rcs rcs rcs
c                                                                         rcs
c  RCS information:                                                       rcs
c                                                                         rcs
      CHARACTER*(132)  CFILE, CREVIS, CSTATE, CDATE, CAUTHO, CRCSID
      PARAMETER (CFILE = '$RCSfile: ccxtoi.F,v $') 
      PARAMETER (CREVIS = '$Revision: 1.2 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 1997/04/24 15:38:16 $')
      PARAMETER (CAUTHO = '$Author: holtrop $')
      DATA CRCSID/ 
     1'$Id: ccxtoi.F,v 1.2 1997/04/24 15:38:16 holtrop Exp $'
     2/ 
c  Module information: 
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='ccxtoi')                                   
      PARAMETER (CRAUTH='Alexander Vlassov')
c                                                                         rcs
c rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rsc rcs rcs rcs
c rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rsc rcs rcs rcs
c
C----------------------------------------------------------------------
#include "ccph.inc"
C----------------------------------------------------------------------
      integer nw,ix(*)
C
      integer i,j,k
c
C**************************************************************
c
      nw = 1
c
      ix(2) = jtr
      ix(3) = ig
      ix(4) = jtro
      ix(5) = igo
      nw    = nw + 4
c
      call ucopy(ihnc,ix(nw+1),ig)
      nw = nw + ig
c
      call ucopy(ihsc,ix(nw+1),ig)
      nw = nw + ig
c
      call ucopy(nref,ix(nw+1),ig)
      nw = nw + ig
c
      call ucopy(njtr,ix(nw+1),ig)
      nw = nw + ig
c
      do i = 1,ig
        ix(nw+i) = 1000.*tdc(i) + 0.5
      end do
      nw = nw + ig
c
      call ucopy(nstp,ix(nw+1),jtr)
      nw = nw + jtr
c
      call ucopy(nvsc,ix(nw+1),jtr)
      nw = nw + jtr
c
      call ucopy(nvch,ix(nw+1),jtr)
      nw = nw + jtr
c
      call ucopy(nvtype,ix(nw+1),jtr)
      nw = nw + jtr
c
      do i = 1,jtr
        ix(nw+i) = 1000.*vphi(i) + 0.5
      end do
      nw = nw + jtr
c
      do i = 1,jtr
        ix(nw+i) = 1000.*vteta(i) + 0.5
      end do
      nw = nw + jtr
c
      do i = 1,jtr
        ix(nw+i) = 1000.*vlen(i) + 0.5
      end do
      nw = nw + jtr
c
      do i = 1,ig
        k = nref(i)
        call ucopy(ncl(1,i),ix(nw+1),k)
        nw = nw+k
      end do
c
      do i = 1,ig
        k = nref(i)
        do j = 1,k
          ix(nw+1) = 1000.*rl(1,j,i) + sign(0.5,rl(1,j,i))
          ix(nw+2) = 1000.*rl(2,j,i) + sign(0.5,rl(2,j,i))
          ix(nw+3) = 1000.*rl(3,j,i) + sign(0.5,rl(3,j,i))
          nw = nw + 3
        end do
      end do
c
      do i = 1,jtr
        k = nstp(i)
        do j = 1,k
          ix(nw+1) = 1000.*rtr(1,j,i) + sign(0.5,rtr(1,j,i))
          ix(nw+2) = 1000.*rtr(2,j,i) + sign(0.5,rtr(2,j,i))
          ix(nw+3) = 1000.*rtr(3,j,i) + sign(0.5,rtr(3,j,i))
          nw = nw + 3
        end do
      end do
c
      do i = 1,jtr
        k = nstp(i)
        do j = 1,k
          ix(nw+j) = 1000.*vpmom(j,i) + 0.5
        end do
        nw = nw + k
      end do
c
c -------------------------------------------------------------
      ix(1) = nw
c =============================================================
c
  999 RETURN
      END
