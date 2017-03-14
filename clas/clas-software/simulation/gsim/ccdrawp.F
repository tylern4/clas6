CDECK  ID>, CCDRAWP.
      SUBROUTINE CCDRAWP
c
c_begin_doc
c  RCS ID string
c  $Id: ccdrawp.F,v 1.2 1997/04/24 15:38:08 holtrop Exp $
c
c  Documentation for subroutine CCDRAWP
c
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : To show the CC - section with
c-                         secondary ray tracing
C-              Without KUIP - MENU
C-
C-   Inputs  : scale factor sxy and (x0,y0) of shift (KUIP) (in data statement)
C-   Outputs : picture
C-   Controls:
C-
C-   Created    15-FEB-1994   Alexander V. Vlassov
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
C
#include "ccp.inc"
#include "ccph.inc"
C
c_end_inc
c
c_begin_var
c  input/output variables:
c  -----------------------
c
c  Local pre-defined variables:
c  ---------------------------
c
c rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rsc rcs rcs rcs
c rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rsc rcs rcs rcs
c                                                                         rcs
c  RCS information:                                                       rcs
c                                                                         rcs
      CHARACTER*(132)  CFILE, CREVIS, CSTATE, CDATE, CAUTHO, CRCSID
      PARAMETER (CFILE = '$RCSfile: ccdrawp.F,v $') 
      PARAMETER (CREVIS = '$Revision: 1.2 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 1997/04/24 15:38:08 $')
      PARAMETER (CAUTHO = '$Author: holtrop $')
      DATA CRCSID/ 
     1'$Id: ccdrawp.F,v 1.2 1997/04/24 15:38:08 holtrop Exp $'
     2/ 
c  Module information: 
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='ccdrawp')                                   
      PARAMETER (CRAUTH='Alexander Vlassov')
c                                                                         rcs
c rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rsc rcs rcs rcs
c rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rsc rcs rcs rcs
c
c  Local User defined variables:
c  -----------------------------
c
      INTEGER i,j,k,kwch,lwch(18),il
      integer nch,nreg
      integer ibgcolor,ired,iblue,igreen,nhit
c
      real x(99),y(99)
      real sxy,x0,y0
c
      data ibgcolor,ired,iblue,igreen /0,2,4,3/
      data sxy,x0,y0 /0.12, 0.2, -42.0/
c
c  executable code for routine CCDRAWP
C-----------------------------------------------------------------------
C
      write(CRMESS,*) ' CCdrawP :',sxy,x0,y0
      call recmes(CRNAME,'I',CRMESS)
C
      CALL ISPLCI(ibgcolor)
      CALL ISPMCI(ibgcolor)
 
      call igset('*',0.)
      call hplnew
      call isfaci(ibgcolor)
      call isfais(1)
      call igbox(0.,1.,0.,1.)  ! clear screen
      call isln(1)             ! LINE type
      CALL ISPLCI(1)           ! LINE color
c
      kwch = 0
      nreg = 0
      call ISLWSC (1.)
c
      write(CRMESS,*) 'TOTAL number of photons = ',igo
      call recmes(CRNAME,'I',CRMESS)
C
      DO i=1,igo          ! cycle on the photon's  number
c
        k = nref(i)
        if(k.ge.99) k=99
        if(k.ge.2) then
          do 1 j = 1,k
            x(j)=rl(1,j,i)*sxy+x0
            y(j)=rl(2,j,i)*sxy+y0
c
            nch = ncl(j,i)
            if(kwch.ge.1) then
              do il = 1,kwch
                if(nch.eq.lwch(il)) go to 1
              end do
              CALL CCFRAME(nch,sxy,x0,y0)  !  new nch
              kwch = kwch + 1
              lwch(kwch) = nch
              write(CRMESS,*) 'NEW nch = ',nch
              call recmes(CRNAME,'I',CRMESS)
C
            else
              CALL CCFRAME(nch,sxy,x0,y0)
              kwch = kwch + 1
              lwch(kwch) = nch
              write(CRMESS,*) 'INITIAL nch = ',nch
              call recmes(CRNAME,'I',CRMESS)
            end if
c
    1     continue
 
          if(ihnc(i).eq.0) then
            nhit = 0
            CALL ISPLCI(iblue)  ! no  hit
          else
            nhit = 1
            nreg = nreg + 1
            CALL ISPLCI(ired)   ! hit PMT
          end if
          call ISLWSC (1.)
          CALL IPL(k,x,y)
c
        else
          write(CRMESS,*) ' WRONG number of points: i,k=',i,k
          call recmes(CRNAME,'W',CRMESS)
          write(CRMESS,*) 'Array x:',(rl(1,il,i),il = 1,5)
          call recmes(CRNAME,'W',CRMESS)
          write(CRMESS,*) 'Array y:',(rl(2,il,i),il = 1,5)
          call recmes(CRNAME,'W',CRMESS)
        end if
 
      END DO
C
      write(CRMESS,*) 'REGISTERED number of photons = ',nreg
      call recmes(CRNAME,'I',CRMESS)
C
C ************************************************
C
      DO i=1,jtro         ! cycle on the track's  number
c
        k = nstp(i)
        if(k.ge.99) k=99
        if(k.ge.2) then
          do j = 1,k
            x(j)=abs(rtr(1,j,i))*sxy+x0
            y(j)=rtr(2,j,i)*sxy+y0
          end do
 
          call ISLWSC (4.)
          call ISPLCI(igreen)
          CALL IPL(k,x,y)
          call ISLWSC (1.)
        else
          write(CRMESS,*) ' WRONG number of points: i,k=',i,k
          call recmes(CRNAME,'W',CRMESS)
          write(CRMESS,*) 'Array x:',(rl(1,il,i),il = 1,5)
          call recmes(CRNAME,'W',CRMESS)
          write(CRMESS,*) 'Array y:',(rl(2,il,i),il = 1,5)
          call recmes(CRNAME,'W',CRMESS)
        end if
      END DO
C
      RETURN
      END
