      SUBROUTINE fsgen_output(ierr,ievent,Fiducial)
c  ---------------
c
c  Notes:
c  ------
c
c  Author:    S.Stepanyan     Created:  Tue Mar 25 14:32:25 EST 1997
c  -------
c
c  Major revisions:
c  ----------------
c     
      IMPLICIT NONE
#include "parm1.inc"
#include "parm2.inc"
#include "parm3.inc"
#include "user_par.inc"
#include "bcs.inc"
#include "lujet.inc"
#include "leptou.inc"
      INTEGER indx,ind,ievent
      INTEGER j,ierr,j1,i
      INTEGER mvertex
      INTEGER status
      REAL    plu
      REAL    x_pos,xy(2)
      REAL    y_pos
      REAL    z_pos
      REAL    r_c
      REAL    phi_spher
      REAL    getrand, theta, phi
      real    speedoflight, pp,tprint
      real v1(300,3)
      parameter (speedoflight=29.9792)   ! cm/ns
      INTEGER nbank, iid
      INTEGER boswrite
      EXTERNAL nbank,getrand
      EXTERNAL boswrite,user_fc
      LOGICAL Fiducial,user_fc
C ********************************************************
C       Define the production vertex
C ********************************************************
cstep
c         do j=1,line
c            if(k(j,2).eq.111)then
c               if(p(j,3)/plu(j,8).gt.0.98)return
c            endif
c         enddo
cstep
       if (tzmin.ge.tzmax) then
         z_pos=tzmin
       else
         z_pos=tzmin+getrand(line)*(tzmax-tzmin)
       endif
       if(txy.gt.0)then
          call rnorml(xy,2,getrand)
          x_pos=xy(1)*txy
          y_pos=xy(2)*txy
       else
         x_pos=0.0
         y_pos=0.0
       endif
       do j=1,line
          v1(j,1) = v(j,1)/10.0 + x_pos
          v1(j,2) = v(j,2)/10.0 + y_pos
          v1(j,3) = v(j,3)/10.0 + z_pos 
       enddo
c Check for fiducial particles
       Fiducial=.true.
       do j=1,line
          if(k(j,1).lt.10)then
             do i=1,nfc
                if(pidfc(i).eq.k(j,2).and.fiducial)then
                   theta=plu(j,14)
                   phi=plu(j,16)
                   iid=pidfc(i)
                   pp=plu(j,8)
                   if(ffc(i).gt.0)then
                      if(theta.lt.thfcm(i).or.theta.gt.thfcx(i))
     +                     Fiducial=.false.
                      if(phi.lt.phfcm(i).or.phi.gt.phfcx(i))
     +                     Fiducial=.false.
                      if(plu(j,8).lt.pfcm(i).or.plu(j,8).gt.pfcx(i))
     +                     Fiducial=.false.
                   elseif(ffc(i).lt.0)then
                      Fiducial=user_fc(iid,pp,theta,phi)
                      if(iid.eq.2112.and..not.fiducial)then
                         ierr=-1000
                         return
                      endif
                   endif
                endif
             enddo
          endif
       enddo
c
       if(txout.eq.0)then
          write(51,*)ievent,line,w2,q2,u
          do j=1,line
             WRITE(51,1002) k(j,1),k(j,2),k(j,3),p(j,1),p(j,2),p(j,3),
     +              p(j,4),p(j,5),v(j,1),v(j,2),v(j,3)
 1002        format (3i6,8e12.3)
          enddo
       endif
       if(bout.ne.0)return
C********************************************************
C       Write to the output event file
C ********************************************************
       ind=nbank('HEAD',0,8,1)
       iw(ind+1)=1
       iw(ind+5)=-4
       iw(ind+2)=RunNumber
       iw(ind+3)=ievent
       iw(ind+7)=15
       iw(ind+6)=0
       iw(ind+8)=0
       ind=nbank('MCTK',0,11,line) 
       if(ind.ne.0)then
          j1=0
         do j=1,line
            j1=j1+1
            indx=ind+(j-1)*11
            if(plu(j,8).gt.0.0)then
               rw(indx+1)=p(j,1)/plu(j,8) 
               rw(indx+2)=p(j,2)/plu(j,8) 
               rw(indx+3)=p(j,3)/plu(j,8)
            else
               rw(indx+1)=0.0 
               rw(indx+2)=0.0 
               rw(indx+3)=0.0
            endif 
            rw(indx+4)=plu(j,8)
            rw(indx+5)=p(j,5)
            rw(indx+6)=plu(j,6)
            iw(indx+7)=k(j,2)   ! PID
            iw(indx+8)=k(j,1)   ! Status
            if(.not.Fiducial.and.k(j,1).lt.10)iw(indx+8)=11
            iw(indx+9) = j      ! Beg. Vertex. 
            if(k(j,1).gt.10)then
               iw(indx+10)=1    ! End Vertex.
            else
               iw(indx+10)=0         
            endif
            if(j.eq.1.and.k(j,2).eq.22)then
               iw(indx+9)=0
               iw(indx+10)=1   
            endif
            iw(indx+11)=k(j,3)  ! parent track
         enddo
      endif
      ind=nbank('MCVX',0,5,line)
      j1=0
      if(ind.ne.0)then
         do j=1,line    
            j1=j1+1
            indx=ind+(j-1)*5
            rw(indx+1)=v1(j,1) 
            rw(indx+2)=v1(j,2) 
            rw(indx+3)=v1(j,3)
            rw(indx+4)=0
            iw(indx+5)=0
         enddo
      endif
C
c  check to see if we are doing photon stuff. If so, make tagr bank.
c  We need to do it knowing the z_pos of the vertex.
      if (Q2MAX .Eq. 0..and.Q2MIN.EQ.0.) then
c     call fsgen_fill_tagger(ebeam,qv,-z_pos/speedoflight,ierr)
      endif

C
      ierr=bosWrite(jout,iw,'HEADMCTKMCVXTAGR')
      call bdrop(iw,'HEADMCTKMCVXTAGR')
      call bgarb(iw)
      RETURN
      END


