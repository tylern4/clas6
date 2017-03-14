      SUBROUTINE Ec_ijk_xyz(di,dj,dk,dx,dy,dz,sector,iterr)
*--------------------------------------------------------------------------
* purpose and method: transfer from ijk to xyz
* 
* inputs   : di      - I
*          : dj      - J
*          : dk      - K
*          : sector 
* outputs  : dx      - x
*          : dy      - y
*          : dz      - z coordinate
* created 27-June-1995   Zhujun Li from Bogdan rrot
*
*----------------------------------------------------------------------------
      IMPLICIT NONE
      SAVE
c rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rsc rcs rcs rcs
c rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rsc rcs rcs rcs
c                                                                         rcs
c  RCS information:                                                       rcs
c                                                                         rcs
      CHARACTER*132  CFILE, CREVIS, CSTATE, CDATE, CAUTHO, CRCSID
      PARAMETER (CFILE = '$RCSfile: ec_ijk_xyz.F,v $') 
      PARAMETER (CREVIS = '$Revision: 1.2 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 1997/05/01 19:48:25 $')
      PARAMETER (CAUTHO = '$Author: freyberg $')
      DATA CRCSID/ 
     1'$Id: ec_ijk_xyz.F,v 1.2 1997/05/01 19:48:25 freyberg Exp $'
     2/ 
c  Module information: 
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='ec_ijk_xyz')                                   
      PARAMETER (CRAUTH='Zhujun Li')
c                                                                         rcs
c rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rsc rcs rcs rcs
c rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rsc rcs rcs rcs
c
c
#include "Ec_general.PAR"
#include "ecgeompar.inc"
#include "ecgeom.inc"
C* Input parameters      
      real di,dj,dk,dx,dy,dz
      integer sector,iterr
C* Local parameters
      integer i,j 
      real p(3),pv(3),rot(3,3)
*
      rot(1,1)=cos(EcBsc_a*d2rad)*cos(EcBsc_phi(sector)*d2rad)
      rot(1,2)=-sin(EcBsc_phi(sector)*d2rad)
      rot(1,3)=sin(EcBsc_a*d2rad)*cos(EcBsc_phi(sector)*d2rad)
      rot(2,1)=cos(EcBsc_a*d2rad)*sin(EcBsc_phi(sector)*d2rad)
      rot(2,2)=cos(EcBsc_phi(sector)*d2rad)
      rot(2,3)=sin(EcBsc_a*d2rad)*sin(EcBsc_phi(sector)*d2rad)
      rot(3,1)=-sin(EcBsc_a*d2rad)
      rot(3,2)=0.
      rot(3,3)=cos(EcBsc_a*d2rad)
*
      p(1)=di
      p(2)=dj
      p(3)=dk
*
      do j=1,3
         pv(j)=0.
      enddo
      do j=1,3
        do i=1,3
          pv(j)=pv(j)+rot(j,i)*p(i)
        enddo
      enddo
      IF ( iterr.EQ.1 ) THEN
        dx=pv(1)+EcBsc_x0a(sector)
        dy=pv(2)+EcBsc_y0a(sector)
        dz=pv(3)+EcBsc_z0a(sector)
      ELSEIF( iterr.EQ.2 ) THEN
        dx=abs(pv(1))
        dy=abs(pv(2))
        dz=abs(pv(3))
      ENDIF
*
      RETURN
      END
*
