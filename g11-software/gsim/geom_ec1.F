      subroutine geom_ec1

c_begin_doc

c  Documentation for subroutine geom_ec1

c  $Id: geom_ec1.F,v 1.2 1997/04/24 15:38:30 holtrop Exp $

c  Purpose:
c  --------
c	defines ec1 geometry


c  Author:
c  -------
c  Elliott Wolin, College of William and Mary, 13-apr-95

c  Major revisions:
C    Created:  23-Dec-1993   S.Boiarinov
C    Modified: 22-Mar-1995   H.Avakian
C
c  ----------------

c_end_doc

      implicit none
      character*(*) cfile,crevis,cstate,cdate,cautho,crname,crauth
      character*132 crcsid
      parameter (cfile=  '$RCSfile: geom_ec1.F,v $')
      parameter (crevis= '$Revision: 1.2 $')
      parameter (cstate= '$State: Exp $')
      parameter (cdate=  '$Date: 1997/04/24 15:38:30 $')
      parameter (cautho= '$Author: holtrop $')
      parameter (crname= 'geom_ec1')
      parameter (crauth= 'Elliott Wolin')
      data crcsid/'
     1$Id: geom_ec1.F,v 1.2 1997/04/24 15:38:30 holtrop Exp $
     1 '/
      save

c  include files:
c  --------------
#include "gcbank.inc"
#include "gcflag.inc"
#include "gckine.inc"
#include "gconst.inc"
#include "gcnum.inc"
#include "ec1geom.inc"
#include "ec1tmed.inc"
c_end_inc


c  local variables:  *** most of this should be in BOS banks ***
c  ----------------
      real parcal(11)
      real thetas,dfscc,dflay
      real hwpss,hwpbs,dair,dfss,dsets,tanzy,tanzx
      integer ivol
      integer get_rotm,irots1,irots2 ! Rotation numbers  
c_end_var


c  executable code:
c  ----------------

      call geom_ec1_ini ! Dimension definition for LAEC

      hwpss=SCSTHI/2.
      hwpbs=PBSTHI/2.
      tanzy=(HS2-HS1)/(HWOSEC*2.)
      tanzx=(HB2-HB1)/(HWOSEC*2.)
      dflay=PBSTHI+SCSTHI
      dfscc=(PBSTHI-SCSTHI)/2.0
      thetas=atan(tanzy)*RADDEG

      dair=.5*(AIR2-AIR1)
      dfss= HWPSS-HWOSEC
      dsets=HWPSS

      parcal (1) = HWOSEC+0.5*(STS1+STS2+AIR1+AIR2)
      parcal (2) = thetas
      parcal (3) = 90.
      parcal (4) = HS1-0.5*(STS1+STS2+AIR1+AIR2)*tanzy
      parcal (5) = HB1-0.5*(STS1+STS2+AIR1+AIR2)*tanzx
      parcal (6) = HB1-0.5*(STS1+STS2+AIR1+AIR2)*tanzx
      parcal (7) = 0.
      parcal (8) = HS2+0.5*(STS1+STS2+AIR1+AIR2)*tanzy
      parcal (9) = HB2+0.5*(STS1+STS2+AIR1+AIR2)*tanzx
      parcal (10)= HB2+0.5*(STS1+STS2+AIR1+AIR2)*tanzx
      parcal (11)= 0.
      call gsvolu ('EC1 ','TRAP',ec1_med_air,parcal,11,ivol)
C


c  Rotation matrixes and positioning of LAEC 1/2 in CLAS CCCCCCCCCCCCCC

       irots2=get_rotm()
c

       call GSROTM(irots2,tex2,fix2,tey2,fiy2,tez2,fiz2)
       irots1=get_rotm() 
       call GSROTM(irots1,tex1,fix1,tey1,fiy1,tez1,fiz1)
       call GSPOS('EC1 ',2,'CLAS',xc1,yc1,zc1,irots1,'ONLY')
       call GSPOS('EC1 ',1,'CLAS',xc2,yc2,zc2,irots2,'ONLY')
c
c
      parcal (1) = HWOSEC+0.5*(STS1+STS2+AIR1+AIR2)
      parcal (2) = THETAS
      parcal (3) = 90.
      parcal (4) = HS1-0.5*(STS1+STS2+AIR1+AIR2)*tanzy
      parcal (5) = HB1-0.5*(STS1+STS2+AIR1+AIR2)*tanzx
      parcal (6) = HB1-0.5*(STS1+STS2+AIR1+AIR2)*tanzx
      parcal (7) = 0.
      parcal (8) = HS2+0.5*(STS1+STS2+AIR1+AIR2)*tanzy
      parcal (9) = HB2+0.5*(STS1+STS2+AIR1+AIR2)*tanzx
      parcal (10)= HB2+0.5*(STS1+STS2+AIR1+AIR2)*tanzx
      parcal (11)= 0.
      CALL GSVOLU('ECL-','TRAP',ec1_med_steel,parcal,11,IVOL)

C--  Position the mother volume in 'EC1'
      CALL GSPOS('ECL-',1,'EC1 ',0.,0.,0.,0,'ONLY')
C      CALL GSATT('ECL-','COLO',5)

C
C--  Define mother volume parameters for the air TRAP (air)
      parcal (1) = HWOSEC+0.5*(AIR1+AIR2)
      parcal (4) = HS1-AIR1*tanzy
      parcal (5) = HB1-AIR1*tanzx
      parcal (6) = HB1-AIR1*tanzx
      parcal (8) = HS2+AIR2*tanzy
      parcal (9) = HB2+AIR2*tanzx
      parcal (10)= HB2+AIR2*tanzx
C
      CALL GSVOLU('ECSE','TRAP',ec1_med_air,parcal,11,ivol)
C
      CALL GSPOS('ECSE', 1, 'ECL-', 0., dair*tanzy, dair, 0, 'ONLY')
C      CALL GSATT('ECSE','COLO',2)
C
C--  Define mother volume parameters for the first scintillator (PS)
      parcal (1) = HWPSS
      parcal (4) = HS1
      parcal (5) = HB1
      parcal (6) = HB1
      parcal (8) = HS1+SCSTHI*tanzy
      parcal (9) = HB1+SCSTHI*tanzx
      parcal (10)= HB1+SCSTHI*tanzx
C
      CALL GSVOLU('ECSS','TRAP',ec1_med_scint,parcal,11,IVOL)
C
C--  Position the first scintillator volume in 'ECSE'
      CALL GSPOS('ECSS', 1, 'ECSE', 0.,dfss*tanzy,dfss, 0, 'ONLY')
C      CALL GSATT('ECSS','COLO',3)
C
C--  Define active volume parameters (lead-13)
      parcal (1) = HWOSEC-HWPSS
      parcal (4) = HS1+dflay*tanzy
      parcal (5) = HB1+dflay*tanzx
      parcal (6) = HB1+dflay*tanzx
      parcal (8) = HS2
      parcal (9) = HB2
      parcal (10)= HB2
C
      CALL GSVOLU('ECSA','TRAP',ec1_med_lead,parcal,11,IVOL)
C
C--  Position the mother volume in 'ECSE'
      CALL GSPOS('ECSA', 1, 'ECSE', 0.,dsets*tanzy,dsets, 0, 'ONLY')
C      CALL GSATT('ECSA','COLO',5)
      CALL GSDVN('ECSL','ECSA',NPBL,3) ! Subdivide it in 32 parts
C      CALL GSATT('ECSL','COLO',5)
      CALL GSDVN2('ECSP','ECSL',1,3,DFSCC,ec1_med_scint) ! Plastic scintillator
C      CALL GSATT('ECSP','COLO',3)
C
      return
      end
