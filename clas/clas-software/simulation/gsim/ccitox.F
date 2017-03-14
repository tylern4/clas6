CDECK  ID>, CCITOX.
      SUBROUTINE CCITOX(nw,ix)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Fill /ccph/ from the integer array ix
C-
C-
C-   Inputs  :  nw - number of words in array ix
C-              ix - integer array
C-
C-   Outputs :  /ccph/
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
      PARAMETER (CFILE = '$RCSfile: ccitox.F,v $') 
      PARAMETER (CREVIS = '$Revision: 1.2 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 1997/04/24 15:38:09 $')
      PARAMETER (CAUTHO = '$Author: holtrop $')
      DATA CRCSID/ 
     1'$Id: ccitox.F,v 1.2 1997/04/24 15:38:09 holtrop Exp $'
     2/ 
c  Module information: 
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='ccitox')                                   
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
      integer i,j,k,n
c
C**************************************************************
c
      if(ix(1).ne.nw) then
        CRMESS = 'First word of the array should be equal to nw'
        call recmes(CRNAME,'E',CRMESS)
      end if
      n = 1
c
      jtr  = ix(2)
      ig   = ix(3)
      jtro = ix(4)
      igo  = ix(5)
      n    = n + 4
c
      call ucopy(ix(n+1),ihnc,ig)
      n = n + ig
c
      call ucopy(ix(n+1),ihsc,ig)
      n = n + ig
c
      call ucopy(ix(n+1),nref,ig)
      n = n + ig
c
      call ucopy(ix(n+1),njtr,ig)
      n = n + ig
c
      do i = 1,ig
        tdc(i) = 0.001*ix(n+i)
      end do
      n = n + ig
c
      call ucopy(ix(n+1),nstp,jtr)
      n = n + jtr
c
      call ucopy(ix(n+1),nvsc,jtr)
      n = n + jtr
c
      call ucopy(ix(n+1),nvch,jtr)
      n = n + jtr
c
      call ucopy(ix(n+1),nvtype,jtr)
      n = n + jtr
c
      do i = 1,jtr
        vphi(i) = 0.001*ix(n+i)
      end do
      n = n + jtr
c
      do i = 1,jtr
        vteta(i) = 0.001*ix(n+i)
      end do
      n = n + jtr
c
      do i = 1,jtr
        vlen(i) = 0.001*ix(n+i)
      end do
      n = n + jtr
c
      do i = 1,ig
        k = nref(i)
        call ucopy(ix(n+1),ncl(1,i),k)
        n = n + k
      end do
c
      do i = 1,ig
        k = nref(i)
        do j = 1,k
          rl(1,j,i) = 0.001*ix(n+1)
          rl(2,j,i) = 0.001*ix(n+2)
          rl(3,j,i) = 0.001*ix(n+3)
          n = n + 3
        end do
      end do
c
      do i = 1,jtr
        k = nstp(i)
        do j = 1,k
          rtr(1,j,i) = 0.001*ix(n+1)
          rtr(2,j,i) = 0.001*ix(n+2)
          rtr(3,j,i) = 0.001*ix(n+3)
          n = n + 3
        end do
      end do
c
      do i = 1,jtr
        k = nstp(i)
        do j = 1,k
          vpmom(j,i) = 0.001*ix(n+j)
        end do
        n = n + k
      end do
c
c -------------------------------------------------------------
      if(n.ne.nw) then
        write(CRMESS,*) 'Wrong length of array:',nw,n 
        call recmes(CRNAME,'E',CRMESS)
      end if
c =============================================================
c
  999 RETURN
      END
