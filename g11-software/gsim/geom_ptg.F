      subroutine geom_ptg

c_begin_doc

c  Documentation for subroutine geom_ptg

c  $Id: geom_ptg.F,v 1.6 2004/10/15 14:34:03 lcsmith Exp $

c  Purpose:
c  --------
c     ******************************************************************
c     *                                                                *
c     *      Defines geometrie of polarized target                     *
c     *                                                                *
c     *       Author    M. Guckes  *******      (Oct 1987)             *
c     *      modified : V. Burkert *******      (May 1988)             *
c     *      rewritten: D. Conwell *******      (1996)                 *
c     *                                                                *
c     ******************************************************************

c  Major revisions:
c  ----------------
c     Elliott Wolin, College of William and Mary, 9-nov-94
c_end_doc

      implicit none
      character*(*) cfile,crevis,cstate,cdate,cautho,crname,crauth
      character*132 crcsid
      parameter (cfile=  '$RCSfile: geom_ptg.F,v $')
      parameter (crevis= '$Revision: 1.6 $')
      parameter (cstate= '$State: Exp $')
      parameter (cdate=  '$Date: 2004/10/15 14:34:03 $')
      parameter (cautho= '$Author: lcsmith $')
      parameter (crname= 'GEOM_PTG')
      parameter (crauth= 'Elliott Wolin')
      data crcsid/'
     1$Id: geom_ptg.F,v 1.6 2004/10/15 14:34:03 lcsmith Exp $
     1 '/
      save


c  include files:
c  --------------
#include "gsimpar.inc"
#include "gsimcontrol.inc"
#include "gcunit.inc"
#include "gclist.inc"
#include "gconst.inc"
#include "ffpar.inc"
#include "ffpar_mg.inc"
#include "ffpar_ptg.inc"
#include "ffpar_tg.inc"
#include "ptgtmed.inc"
#include "lcgeom.inc"
#include "cltmed.inc"
c_end_inc


c  local variables:
c  ----------------
      integer mpcon
      parameter (mpcon=4)
      real pcon(3+3*mpcon), tube(3)

c  Geometry parameters (it should be the same geometry as in the field
c  calculation routine PTF)
       data pcon  /    0.,  360., mpcon,
     1                5.6,  13.0,  13.0,
     2                7.8,  13.0, 18.55,
     3              11.15, 18.72,  27.0,
     4               16.0,  27.0,  27.0/

       integer ivol,i,mate,get_tmed,get_rotm,ptg_r1,ptg_r2,
     1         ptg_r3,ptg_r4,ptg_r5,foir_3,foir_4

       real ptg1(3), wndga(3), wndgb(3), casea(3), caseb(5), casec(5),
     1      cased(5), casee(3), targ1(3), targ2(3), prad1(3), 
     1      pcon1(3), pcon2(3), shela(3), shelb(3), shelc(3),
     1      sheld(3), shele(3), shelf(3), prad9(3), prad10(5), prad11(5),
     1      prad12(5),prad13(5),prad14(5),prad15(5),prad16(5), pspa1(3),
     1      pspa2(5), pspa3(5), pspa4(5), pspa5(5), pspa6(5), pspa7(5),
     1      pspa8(5), ptied(3), ptie1(3), ptie2(3), pspb2(5), prac10(5),
     1      pftar(3), targ3(3), pvac1(5)

   
c       data ptg1 /0.,100.,31./
       data ptg1 /0.,50.,35./
       data wndga /16.5,21.9,2.38/
       data wndgb /11.6,15.1,1.5/
       data casea /10.4,25.,2.69/
       data caseb /2.88,10.4,25.,17.5,25./
       data casec /2.88,17.5,25.,10.4,25./
       data cased /3.1,17.5,17.75,24.75,25./
       data casee /24.5,25.,3.1/
       data targ1 /.953,9.53,.635/
       data targ2 /7.94,9.53,3.81/
       data prad1 /25.,31.5,.25/
       data prad9 /29.5,30.,7.75/
       data prad10 /29.5,30.,5.,0.,80./
       data prac10 /29.5,30.,5.,100.,360./
       data prad11 /29.5,30.,10.,25.,35./
       data prad12 /29.5,30.,10.,75.,105./
       data prad13 /29.5,30.,10.,145.,155./
       data prad14 /29.5,30.,10.,205.,215./
       data prad15 /29.5,30.,10.,265.,275./
       data prad16 /29.5,30.,10.,325.,335./
       data pcon1 /34.,44.,1.25/
       data pcon2 /38.,44.,1.25/
       data pspa1 /32.25,32.5,11/
       data pspa2 /32.25,32.5,11,0.,80./
       data pspb2 /32.25,32.5,11,100.,360./
       data pspa3 /32.25,32.5,9.2,25.,35./
       data pspa4 /32.25,32.5,9.2,75.,105./
       data pspa5 /32.25,32.5,9.2,145.,155./
       data pspa6 /32.25,32.5,9.2,205.,215./
       data pspa7 /32.25,32.5,9.2,265.,275./
       data pspa8 /32.25,32.5,9.2,325.,335./
       data shela /10.,1.,29./
       data shelb /1.,2.75,11./
       data shelc /1.,21.5,9./
       data sheld /2.75,1.,11./
       data shele /1.5,1.,11./
       data shelf /15.75,1.,9./
       data ptied /.9,.9,19./
       data ptie1 /.9,.9,1.25/
       data ptie2 /0.,.6,.35/
       data pftar /.2,.2,.2/
       data pvac1 /15., 0., 1.5, 0., 30./

c define NH3
      real anh3(2), znh3(2), wnh3(2)
      data anh3 / 1., 14./
      data znh3 / 1.,  7./
      data wnh3 / 3.,  1./      

c_end_var

c define rotation for faces of outside shield
       ptg_r1=get_rotm()
       call gsrotm(ptg_r1,90.,30.,90.,120.,0.,0.)
       ptg_r2=get_rotm()
       call gsrotm(ptg_r2,90.,330.,90.,60.,0.,0.)
c define rotation for tie downs on coil casing
       ptg_r3=get_rotm()
       call gsrotm(ptg_r3,90.,0.,40.,90.,50.,270.)
       ptg_r4=get_rotm()
       call gsrotm(ptg_r4,90.,120.,40.,210.,50.,30.)
       ptg_r5=get_rotm()
       call gsrotm(ptg_r5,90.,240.,40.,330.,50.,150.)
c define rotations for shield fins
      foir_3=get_rotm()
      CALL GSROTM(foir_3,90.,60.,90.,150.,0.,0.)
      foir_4=get_rotm()
      CALL GSROTM(foir_4,90.,120.,90.,210.,0.,0.)

      call gsvolu('PTG ','TUBE',clas_med_helium,ptg1,3,IVOL)
      call gspos('PTG ',1,'UPST',0.,0.,tg_pos_ff(3),0,'ONLY')
      call gsvolu('WDG1','TUBE',clas_med_copper,wndga,3,IVOL)
      call gspos('WDG1',1,'PTG ',0.,0.,-9.58,0,'ONLY')
      call gspos('WDG1',2,'PTG ',0.,0.,9.58,0,'ONLY')
      call gsvolu('WDG2','TUBE',clas_med_copper,wndgb,3,IVOL)
      call gspos('WDG2',1,'PTG ',0.,0.,-6.68,0,'ONLY')
      call gspos('WDG2',2,'PTG ',0.,0.,6.68,0,'ONLY')
      call gsvolu('CAS1','TUBE',clas_med_iron,casea,3,IVOL)
      call gspos('CAS1',1,'PTG ',0.,0.,-5.88,0,'ONLY')
      call gspos('CAS1',2,'PTG ',0.,0.,5.88,0,'ONLY')
      call gsvolu('CAS2','CONE',clas_med_iron,caseb,5,IVOL)
      call gspos('CAS2',1,'PTG ',0.,0.,11.4,0,'ONLY')
      call gsvolu('CAS3','CONE',clas_med_iron,casec,5,IVOL)
      call gspos('CAS3',1,'PTG ',0.,0.,-11.4,0,'ONLY')
      call gsvolu('CAS4','CONE',clas_med_iron,cased,5,IVOL)
      call gspos('CAS4',1,'PTG ',0.,0.,17.4,0,'ONLY')
      call gsvolu('CAS5','TUBE',clas_med_iron,casee,3,IVOL)
      call gspos('CAS5',1,'PTG ',0.,0.,17.4,0,'ONLY')
      call gsvolu('TAR1','TUBE',clas_med_iron,targ1,3,IVOL)
      call gspos('TAR1',1,'PTG ',0.,0.,-4.445,0,'ONLY')
      call gsvolu('TAR2','TUBE',clas_med_iron,targ2,3,IVOL)
      call gspos('TAR2',1,'PTG ',0.,0.,0.,0,'ONLY')
      call gsvolu('RAD1','TUBE',clas_med_alu,prad1,3,IVOL)
      call gspos('RAD1',1,'PTG ',0.,0.,25.75,0,'ONLY')
      call gsvolu('RAD9','TUBE',clas_med_alu,prad9,3,IVOL)
      call gspos('RAD9',1,'PTG ',0.,0.,17.75,0,'ONLY')
      call gsvolu('RA10','TUBS',clas_med_alu,prad10,5,IVOL)
      call gspos('RA10',1,'PTG ',0.,0.,-17.7,0,'ONLY')
      call gsvolu('RB10','TUBS',clas_med_alu,prac10,5,IVOL)
      call gspos('RB10',1,'PTG ',0.,0.,-17.7,0,'ONLY')
      call gsvolu('RA11','TUBS',clas_med_alu,prad11,5,IVOL)
      call gspos('RA11',1,'PTG ',0.,0.,0.,0,'ONLY')
      call gsvolu('RA12','TUBS',clas_med_alu,prad12,5,IVOL)
      call gspos('RA12',1,'PTG ',0.,0.,0.,0,'ONLY')
      call gsvolu('RA13','TUBS',clas_med_alu,prad13,5,IVOL)
      call gspos('RA13',1,'PTG ',0.,0.,0.,0,'ONLY')
      call gsvolu('RA14','TUBS',clas_med_alu,prad14,5,IVOL)
      call gspos('RA14',1,'PTG ',0.,0.,0.,0,'ONLY')
      call gsvolu('RA15','TUBS',clas_med_alu,prad15,5,IVOL)
      call gspos('RA15',1,'PTG ',0.,0.,0.,0,'ONLY')
      call gsvolu('RA16','TUBS',clas_med_alu,prad16,5,IVOL)
      call gspos('RA16',1,'PTG ',0.,0.,0.,0,'ONLY')
      call gsvolu('SPA1','TUBE',clas_med_alu,pspa1,3,IVOL)
      call gspos('SPA1',1,'PTG ',0.,0.,20.2,0,'ONLY')
      call gsvolu('SPA2','TUBS',clas_med_alu,pspa2,5,IVOL)
      call gspos('SPA2',1,'PTG ',0.,0.,-20.2,0,'ONLY')
      call gsvolu('SPB2','TUBS',clas_med_alu,pspb2,5,IVOL)
      call gspos('SPB2',1,'PTG ',0.,0.,-20.2,0,'ONLY')
      call gsvolu('SPA3','TUBS',clas_med_alu,pspa3,5,IVOL)
      call gspos('SPA3',1,'PTG ',0.,0.,0.,0,'ONLY')
      call gsvolu('SPA4','TUBS',clas_med_alu,pspa4,5,IVOL)
      call gspos('SPA4',1,'PTG ',0.,0.,0.,0,'ONLY')
      call gsvolu('SPA5','TUBS',clas_med_alu,pspa5,5,IVOL)
      call gspos('SPA5',1,'PTG ',0.,0.,0.,0,'ONLY')
      call gsvolu('SPA6','TUBS',clas_med_alu,pspa6,5,IVOL)
      call gspos('SPA6',1,'PTG ',0.,0.,0.,0,'ONLY')
      call gsvolu('SPA7','TUBS',clas_med_alu,pspa7,5,IVOL)
      call gspos('SPA7',1,'PTG ',0.,0.,0.,0,'ONLY')
      call gsvolu('SPA8','TUBS',clas_med_alu,pspa8,5,IVOL)
      call gspos('SPA8',1,'PTG ',0.,0.,0.,0,'ONLY')
      call gsvolu('CON1','TUBE',clas_med_alu,pcon1,3,IVOL)
      call gspos('CON1',1,'PTG ',0.,0.,30.25,0,'ONLY')
      call gsvolu('CON2','TUBE',clas_med_alu,pcon2,3,IVOL)
      call gspos('CON2',1,'PTG ',0.,0.,32.75,0,'ONLY')
      call gsvolu('TIED','BOX ',clas_med_air,ptied,3,IVOL)
      call gspos('TIED',1,'PTG ',0.,.9,0.,ptg_r3,'MANY')
      call gsvolu('TIE1','BOX ',clas_med_alu,ptie1,3,IVOL)
      call gspos('TIE1',1,'TIED',0.,.9,17.5,0,'ONLY')
      call gsvolu('TIE2','TUBE',clas_med_alu,ptie2,3,IVOL)
      call gspos('TIE2',1,'TIED',0.,1.2,16.15,0,'ONLY')
      call gspos('TIED',2,'PTG ',0.,.9,0.,ptg_r4,'MANY')
      call gspos('TIED',3,'PTG ',0.,.9,0.,ptg_r5,'MANY')
      call gsvolu('SHE1','BOX ',clas_med_alu,shela,3,IVOL)
      call gspos('SHE1',1,'PTG ',0.,36.5,0.,0,'ONLY')
      call gspos('SHE1',2,'PTG ',0.,-36.5,0.,0,'ONLY')
      call gsvolu('SHE2','BOX ',clas_med_alu,shelb,3,IVOL)
      call gspos('SHE2',1,'PTG ',-36.5,18.75,0.,0,'ONLY')
      call gspos('SHE2',2,'PTG ',-36.5,-18.75,0.,0,'ONLY')
      call gspos('SHE2',3,'PTG ',36.5,18.75,0.,0,'ONLY')
      call gspos('SHE2',4,'PTG ',36.5,-18.75,0.,0,'ONLY')
      call gsvolu('SHE3','BOX ',clas_med_alu,shelc,3,IVOL)
      call gspos('SHE3',1,'PTG ',36.5,0.,20.,0,'ONLY')
      call gspos('SHE3',2,'PTG ',-36.5,0.,20.,0,'ONLY')
      call gspos('SHE3',3,'PTG ',36.5,0.,-20.,0,'ONLY')
      call gspos('SHE3',4,'PTG ',-36.5,0.,-20.,0,'ONLY')
      call gsvolu('SHE4','BOX ',clas_med_alu,sheld,3,IVOL)
      call gspos('SHE4',1,'PTG ',-34.62,22.,0,ptg_r1,'ONLY')
      call gspos('SHE4',2,'PTG ',34.62,22.,0,ptg_r2,'ONLY')
      call gspos('SHE4',3,'PTG ',34.62,-22.,0,ptg_r1,'ONLY')
      call gspos('SHE4',4,'PTG ',-34.62,-22.,0,ptg_r2,'ONLY')
      call gsvolu('SHE5','BOX ',clas_med_alu,shele,3,IVOL)
      call gspos('SHE5',1,'PTG ',-10.8,35.88,0,ptg_r1,'ONLY')
      call gspos('SHE5',2,'PTG ',10.8,35.88,0,ptg_r2,'ONLY')
      call gspos('SHE5',3,'PTG ',-10.8,-35.88,0,ptg_r2,'ONLY')
      call gspos('SHE5',4,'PTG ',10.8,-35.88,0,ptg_r1,'ONLY')
      call gsvolu('SHE6','BOX ',clas_med_alu,shelf,3,IVOL)
      call gspos('SHE6',1,'PTG ',-23.36,28.76,20.,ptg_r1,'ONLY')
      call gspos('SHE6',2,'PTG ',-23.36,28.76,-20.,ptg_r1,'ONLY')
      call gspos('SHE6',3,'PTG ',23.36,28.76,20.,ptg_r2,'ONLY')
      call gspos('SHE6',4,'PTG ',23.36,28.76,-20.,ptg_r2,'ONLY')
      call gspos('SHE6',5,'PTG ',-23.36,-28.76,20.,ptg_r2,'ONLY') 
      call gspos('SHE6',6,'PTG ',-23.36,-28.76,-20.,ptg_r2,'ONLY')
      call gspos('SHE6',7,'PTG ',23.36,-28.76,20.,ptg_r1,'ONLY')
      call gspos('SHE6',8,'PTG ',23.36,-28.76,-20.,ptg_r1,'ONLY')
      call gsvolu('VAC1','CONE',clas_med_vac,pvac1,5,IVOL)
      call gspos('VAC1',1,'PTG ',0.,0.,15.75,0,'ONLY')

      end
