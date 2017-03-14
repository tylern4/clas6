      SUBROUTINE EC_GETCAL_GEOM(IRUN)
c
c_begin_doc
c  RCS ID string
c  $Id: ec_getcal_geom.F,v 1.11 2006/06/15 14:48:53 stepanya Exp $
c
c  Documentation for subroutine EC_GETCAL_GEOM.F
c
c  Purpose:
c  --------
c
c  Input Parameters:  (Name - Type - Meaning)
c  ----------------
c
c  Output Parameters:  (Name - Type - Meaning)
c  -----------------
c
c  Other routines:
c  ---------------
c
c  Notes:
c  ------
c
c  Author:   Lee Smith      Created:  Thu Aug 22 13:46:42 EDT 1996
c  -------
c
c  Major revisions:
c  ----------------
c     
c
c_end_doc
c
      IMPLICIT NONE
      SAVE
c
c_begin_inc
c  include files :
#include "Ec_general.PAR"
#include "ecgeom.inc"
#include "ecgeompar.inc"
#include "ec_control.inc"
c  ---------------------
c BOS common block  uncomment the next line for BOS include file
c#include "bcs.inc"
#include "wcs.inc"
c_end_inc
c
c_begin_var
c  input/output variables:
c  -----------------------
c
c  Local pre-defined variables:
c  ---------------------------
c  RCS information: 
      CHARACTER*132  CFILE, CREVIS, CSTATE, CDATE, CAUTHO, CRCSID
      PARAMETER (CFILE = '$RCSfile: ec_getcal_geom.F,v $')
      PARAMETER (CREVIS = '$Revision: 1.11 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 2006/06/15 14:48:53 $')
      PARAMETER (CAUTHO = '$Author: stepanya $')
      DATA CRCSID/   
     1'$Id: ec_getcal_geom.F,v 1.11 2006/06/15 14:48:53 stepanya Exp $'   
     2/   
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='EC_GETCAL_GEOM.F')
      PARAMETER (CRAUTH='Lee Smith')
c
c  Local User defined variables:
c  -----------------------------
      INTEGER NWRITE
      integer irun
      integer isec,ilow_run_geom,ihi_run_geom
      integer i,j
      integer ir,jr
      integer ind
      integer mbank
      real xlow_,xhi_
      real ylow_,yhi_
      real surf(whole:cover)
      logical ok(6)
c_end_var
c
c  executable code for routine EC_GETCAL_GEOM.F:
c----6----------------------------------------------------------------72
c
      IF (NWRITE .LT. 1) THEN
        NWRITE = NWRITE + 1
        CRMESS='This routine gets forward EC geomtery parameters'
        CALL RECMES(CRNAME,'I',CRMESS)
      ENDIF
*
      pi=2.*acos(0.)
      d2rad=pi/180.
*
      surf(whole) = whole_surf
      surf(inner) = inner_surf
      surf(outer) = outer_surf
      surf(cover) = cover_surf
      IF ( def_geom ) THEN
        WRITE(CRMESS,11) 
        CALL RECMES(CRNAME,'I',CRMESS)
*
        EcBsc_r=510.32
cStep        EcBsc_r=621.
        EcBsc_a=25.
cStep        EcBsc_a=20.3
        ylow=-182.974
        yhi =189.956
        dylow=0.43708
        dyhi =0.45419
        thickness=1.2381
        tgrho=1.95325
        do i = 1, MAXsectors
          xoff(i) = 0.0
          yoff(i) = 0.0
          zoff(i) = 0.0
          EcBsc_phi(i) = (i-1)*60.
          Do jr=1,3
            DO ir=1,3
              rotm(i,jr,ir)=0.0
              if(jr.EQ.ir)rotm(i,jr,ir)=1.
            ENDDO
          EndDo
        enddo
      ELSE
        CALL ec_read_eccg(irun,ok,ilow_run_geom,ihi_run_geom)
        WRITE(CRMESS,10) (isec,ok(isec),isec=1,6)
        CALL RECMES(CRNAME,'I',CRMESS)
*
      ENDIF
      do i = whole,cover
        ylow_= ylow - dylow*(surf(i)-1.)
        yhi_ = yhi + dyhi*(surf(i)-1.)
        xlow_=(ylow_-yhi_)/tgrho
        xhi_=-xlow_
        EcBsc_d(i) = (surf(i)-1.)*thickness
        EcBsc_H(i) = yhi_-ylow_
        EcBsc_H1(i) = abs(ylow_)
        EcBsc_H2(i) = yhi_
        Do j = 1,6
          Ec_edge_L(ec_u,i,j)=sqrt((xlow_)**2+(yhi_-ylow_)**2)
          Ec_edge_L(ec_v,i,j)=sqrt((xlow_-xhi_)**2)
          Ec_edge_L(ec_w,i,j)=sqrt((xhi_)**2+(yhi_-ylow_)**2)
        EndDo
      enddo
      do i = 1, MAXsectors
        EcBsc_x0a(i) = EcBsc_r*sin(EcBsc_a*d2rad)*
     +                 cos(EcBsc_phi(i)*d2rad) + xoff(i)
        EcBsc_y0a(i) = EcBsc_r*sin(EcBsc_a*d2rad)*
     +                 sin(EcBsc_phi(i)*d2rad) + yoff(i)
        EcBsc_z0a(i) = EcBsc_r*cos(EcBsc_a*d2rad) + zoff(i)
      enddo
      zEChi = EcBsc_r*cos(EcBsc_a*d2rad)-ylow*sin(EcBsc_a*d2rad)
      zEClow = EcBsc_r*cos(EcBsc_a*d2rad)-yhi*sin(EcBsc_a*d2rad)
*
* Get the plane for track matching
*
      Do i = 1,6
        ind=mbank(jw,'ECP ',i,7,1)
        n2sect(1,i)=sin(EcBsc_a*d2rad)*cos(EcBsc_phi(i)*d2rad)
        n2sect(2,i)=sin(EcBsc_a*d2rad)*sin(EcBsc_phi(i)*d2rad)
        n2sect(3,i)=cos(EcBsc_a*d2rad)
        rjw(ind+1)=n2sect(1,i)
        rjw(ind+2)=n2sect(2,i)
        rjw(ind+3)=n2sect(3,i)
        rjw(ind+4)=EcBsc_r
        rjw(ind+5)=EcBsc_d(whole)
        rjw(ind+6)=EcBsc_d(inner)
        rjw(ind+7)=EcBsc_d(outer)
      EndDo
*
10    FORMAT('Geometry data in sector',1x,6(i1,':',L1,',',1x))
11    FORMAT('Using the ideal Geometry settings')
      RETURN
      END
c
c------------------------------------------------------------------------------




