      subroutine geom_foil

c_begin_doc

c  Documentation for subroutine geom_foil

c<<<<<< geom_foil.F
c  $Id: geom_foil.F,v 1.24 2011/04/04 17:14:00 hyonsuk Exp $
c>>>>>> 1.18

c  Purpose:
c  --------
c     ******************************************************************
c     *                                                                *
c     *     Defines geometry of the shielding foils (SHF+)             *
c     *                                                                *
c     *   ==>Called by : GELAS                                         *
c     *      Author:   V. Burkert  *******    (May 1988)               *
c     *                                                                *
c     *          MODIFIED by M. Guidal                                 *
c     *          c======c======c=======                                *
c     *          Removal, adding of some shieldings....                *  
c     *                                                                *
c     ******************************************************************

c  Major revisions:
c  ----------------
c     Elliott Wolin, College of William and Mary, 6-dec-94
c     C. Tur, Univ. of South Carolina, 25-Jul-04: added g7a upstream beam line
c             elements + scattering chamber (CHAMBER 5 in ffread) +
c             positionning of the g7a target (call to routine geom_tg_g7)
c     Rustam Niyazov,RPI, march-18-2008: added new smaller scattering chamber in DVCS volume
c     and exit window aluminum foil. Scattering chamber was moved with respect to DVCS volume
c     in order to satisfy relative distances from Drawing 66840-E-04248    
c     Silvia Niccolai: introduced target material (ND3) for eg1-dvcs-c; slight corrections to the NH3 as well.
c_end_doc


      implicit none
      character*(*) cfile,crevis,cstate,cdate,cautho,crname,crauth
      character*132 crcsid
      parameter (cfile=  '$RCSfile: geom_foil.F,v $')
c<<<<<< geom_foil.F
      parameter (crevis= '$Revision: 1.24 $')
c>>>>>> 1.18
      parameter (cstate= '$State: Exp $')
c<<<<<< geom_foil.F
      parameter (cdate=  '$Date: 2011/04/04 17:14:00 $')
      parameter (cautho= '$Author: hyonsuk $')
c>>>>>> 1.18
      parameter (crname= 'geom_foil')
      parameter (crauth= 'Elliott Wolin')
      data crcsid/'
c<<<<<< geom_foil.F
     1$Id: geom_foil.F,v 1.24 2011/04/04 17:14:00 hyonsuk Exp $
c>>>>>> 1.18
     1 '/
      save


c  include files:
c  --------------
#include "gsimpar.inc"
#include "gsimcontrol.inc"
#include "gcunit.inc"
#include "gclist.inc"
#include "gconst.inc"
#include "lcgeom.inc"
#include "clmate.inc"
#include "cltmed.inc"
#include "ffpar_foil.inc"
#include "tg_e2_mate.inc"
#include "tg_e2_tmed.inc"
#include "ptgtmed.inc"
#include "ffpar_tg.inc"
#include "ffpar_ptg.inc"
#include "ffpar_bckgr.inc"
#include "ffpar_tg_g7.inc"
c<<<<<< geom_foil.F
c======
#include "ffpar_dvcs.inc"
#include "ffpar_tg_e2.inc"
c>>>>>> 1.18
c_end_inc

c  local variables:
c  ----------------
      integer ivol,mate,foir_1,foir_2,foir_3,foir_4,get_rotm,np,get_tmed
c<<<<<< geom_foil.F
      integer get_mate, mol6_rot, mol7_rot, ptg_mat_nh3, ptg_mat_nd3, ptg_med 
c======
c-H.Hakobyan
      integer irt_sch,irt_sch1,irt_sch2,irt_sch3,irt_sch4,irt_sch5,irt_sch6
      integer irt_sch3F,irt_sch4F,irt_sch5F,irt_sch6F
      real sccyl(3),scsph(6),scexit(5),scexw(3),tbvolm(3)
      real c12(3),fe56(3),ala(3),alb(3),pb(3)
      real PAR1(3),PAR2(3),PAR3(3)

        DATA TBVOLM/0.,12.,20./
        DATA SCCYL/9.0, 10.016, 9.0/                 !the cylidrical part of scatter.chamber
        DATA SCSPH/9.0, 9.953, 0.0, 90., 0.0, 180.0/ !spherical part of sc.chamber
        DATA SCEXIT/7.25,0.673,0.757,1.27,1.354/     !exit part of sc.chamber (cone)
        DATA SCEXW/0.0, 1.27, 0.00355/               !exit window of sc.chamber 

            DATA PAR1/0.47625, 0.49403, 5.82422/
            DATA PAR2/0.47625, 0.54483, 1.78206/
            DATA PAR3/0.47625, 0.31750, 5.69329/
            DATA C12 /0., 0.15, 0.08615/
            DATA FE56/0., 0.15, 0.02   /
            DATA PB  /0., 0.15, 0.007  /
            DATA ALA /0., 0.15, 0.00075/
            DATA ALB /0., 0.15, 0.029  /
c-end_H.Hakobyan
c>>>>>> 1.18

      real tubem(3),tube1(3),tube2(3),tube3(3),flng1(3),flng2(3),
     1     flng3(3),flng4(3),shld1(5),shld2(3),shld3(3),shld4(3),
     1     shld5(5),shld6(5),shld7(5),shld8(3),shld9(3),prib(5),
     1     pgas1(3),pgas2(5),pgas3(3),ptar1(3),wind1(5),tuben(3),
     1     shl11(3),tubev(3),cham0(3),cham1(39),cham2(3),cham3(45),
     1     chamthickness,chamshift,tube4(3),flng5(9),parm(15),
     1     pmol9(5), pmolw(3),pmol1(5),pmol2(5),pmol3(3),pmol4(5),
     1     pmol5(3),pmol8(3),icvol4(3),pmol6(5),pmol7(5),flng6(21),
     1     flng7(21),flng9(21),flng10(21),tube5(3),tube6(3),cham4(30),
c<<<<<< geom_foil.F
     1     cham5(24),cham6(3),vac1(42), targptg(3), targrad(3), 
     1     beampip(3), vacpip(3),dvcs_c1(42),dvcs_c2(3),dvcs_c3(42)
c define NH3
      real anh3(2), znh3(2), wnh3(2)
      data anh3 / 1., 14./
      data znh3 / 1.,  7./
      data wnh3 / 3.,  1./  
c define ND3
      real and3(2), znd3(2), wnd3(2)
      data and3 / 1., 14./
      data znd3 / 1.,  7./
      data wnd3 / 6.,  1./
c
      real aptg(3),zptg(3),wptg(3),densptg
      data aptg /1.0,14.0,4.0/
      data zptg /1.0, 7.0,2.0/
      data wptg /0.1595,0.7445,0.0960/
      data densptg/0.6086/

c define NH3 and ND3 for eg1-dvcs
      real aptg_nh3_eg1dvcs(3),zptg_nh3_eg1dvcs(3),wptg_nh3_eg1dvcs(3)
      real densptg_nh3_eg1dvcs(3),rho_nh3_he,rho_nh3,rho_he,packfrac_nh3
      data aptg_nh3_eg1dvcs /1.0,14.0,4.0/
      data zptg_nh3_eg1dvcs /1.0, 7.0,2.0/
      data rho_nh3 /0.867/
      data rho_he /0.145/
      data packfrac_nh3 /0.58/

      real aptg_nd3_eg1dvcs(3),zptg_nd3_eg1dvcs(3),wptg_nd3_eg1dvcs(3)
      real densptg_nd3_eg1dvcs(3), rho_nd3_he, rho_nd3,packfrac_nd3
      data aptg_nd3_eg1dvcs /1.0,14.0,4.0/
      data zptg_nd3_eg1dvcs /1.0, 7.0,2.0/
      data rho_nd3 /1.007/
      data packfrac_nd3 /0.6/

c>>>>>> 1.18
      data tubem /0. , 30.,  610./
      data tubev /0. , 1.,  200./
      
      data tube1 /10.14,10.92,16.92/
      data tube2 /7.71,8.42,61.1/
      data tube3 /7.32,8.42,77.1/
      data tube4 /9.76,10.16,21.02/
      data tube5 /9.8,10.1,14.81/
      data tube6 /11.8,12.3,83.63/
      
      data flng1 /10.14,15.2,2.12/
      data flng2 /7.71,13.96,1.64/
      data flng3 /7.32,13.96,1.64/
      data flng4 /8.42,15.2,.68/
      data flng5 /0.0,360.0,2.0,0.0,8.65,10.16,2.8,8.65,10.16/
      data flng6 /0.0,360.0,6.0,0.0,12.3,15.2,0.4,12.3,15.2,
     &            0.4,9.8,15.2,0.8,9.8,15.2,0.8,10.1,15.2,
     &            1.6,10.1,15.2/
      data flng7 /0.0,360.0,6.0,0.0,10.1,15.2,0.3,10.1,15.2,
     &            0.3,9.8,15.2,1.5,9.8,15.2,1.5,9.8,12.3,1.9,
     &            9.8,12.3/
      data flng9 /0.0,360.0,6.0,0.0,12.3,15.2,0.4,12.3,15.2,
     &            0.4,11.8,15.2,0.95,11.8,15.2,0.95,11.8,12.3,
     &            1.35,11.8,12.3/   
      data flng10 /0.0,360.0,6.0,0.0,12.3,15.2,0.4,12.3,15.2,
     &             0.4,11.8,15.2,4.0,11.8,15.2,4.0,11.8,12.3,4.4,
     &             11.8,12.3/
 
      data shld1 /24.91, 1.05, 2.1, 3.65, 7.35/
      data shld2 /3.65, 6.35, 5.73/
      data shld3 /3.96, 6.35, 19.164/
      data shld4 /3.81, 5.24, 28.26/
      data shld5 /1.27, 3.81, 5.46, 3.81, 5.55/
      data shld6 /1.91, 3.81, 5.55, 5.4, 5.72/
      data shld7 /47.625, 5.4, 5.72, 5.4, 7.62/
      data shld8 /5.4, 7.62, 16.51/
      data shld9 /5.4, 10.16, 8.89/
      
      data prib /.47, .47, 17.47, 1.59, 22.91/
      
      data pgas1 /0., 9.53, 7.41/
      data pgas2 /13.01, 0., 9.53, 0., 1.59/
      data pgas3 /0., 1.59, 2.5/
      
      data ptar1 /9.53, 16.5, 1.27/
      data wind1 /.006, .006, 9., 1.59, 24.07/
      data tuben /0., 30., 50./
      data shl11 /12.8, 20.4, 102.6/
      data cham0 /0.0,12.0,64.0/
      data chamthickness / 0.96 /
      data chamshift / 3.72 /
c      data pmol1 /17.59, 3., 4.38, 3.65, 9.3/
c      data pmol2 /11.24, 3.97, 9.3, 3.97, 10.91/
c      data pmol3 /3.97, 10.91, 72.46/
      data pmol1 /18.0975, 2.700, 6.24, 3.334, 9.2964/
      data pmol2 /11.43, 3.334, 9.2964, 3.518, 10.922/
c      data pmol3 /3.17, 10.922, 72.46/
      data pmol3 /3.518, 10.922, 21.12/
c      data pmol4 /3.97, 8.0, 12.34/
c      data pmol5 /3.65, 3.97, 113.78/
c      data pmol6 /5.,6.,5./
c      data pmol7 /0.,6.,0.5/
c      data pmol8 /20.694, 2.0, 2.693, 2.5, 6.236/
ccc      data pmol4 /18.971, 2.2, 2.7479, 2.4714, 6.0674/
      data pmol4 /22.5, 2.38, 2.62, 2.38, 6.5/
      data pmol5 /3.518, 6.35, 5.715/
      data pmol6 /0.3175, 2.2, 2.6924, 2.2, 2.7479/
ccc      data pmol7 /1.905, 2.4, 6.0674, 2.5, 6.4007/
      data pmol7 /1.91, 2.20, 5.73, 2.20, 6.24/
c      data pmol8 /18.0975, 2.4714, 2.700, 3.1054,3.334 /
      data pmol9 /39.67, 0., 2.2, 0., 3.518/
      data pmolw /0., 2.35, 0.0036/
      data pmol8 /0., 3.518, 250./
      
c define NH3
      data targptg /0.,1.0,2.0/
      data targrad /0.,1.0,0.0715/
      data beampip /5.0,7.0,350.0/
      data vacpip /0.,10.0,400./
      
c Data for the shielding baffles
c ------------------------------
      integer nbaf,baff_1,i,i2,npcham1,npcham3
      real th,xpos,zpos,baff(11,3),dangle,dangleprime
      data baff/2.5,0.,0.,10.63,0.,6.14,0.,10.63,0.,6.14,0.,
     &          2.5,0.,0.,10.63,0.,6.14,0.,10.63,0.,6.14,0.,
     &          2.5,0.,0.,10.63,0.,6.14,0.,10.63,0.,6.14,0./
      
      foir_1=get_rotm()
      CALL GSROTM(foir_1,71.,0.,90.,90.,19.,180.)
      foir_2=get_rotm()
      CALL GSROTM(foir_2,109.,0.,90.,90.,19.,0.)
      foir_3=get_rotm()
      CALL GSROTM(foir_3,90.,60.,90.,150.,0.,0.)
      foir_4=get_rotm()
      CALL GSROTM(foir_4,90.,120.,90.,210.,0.,0.)

c_end_var


c  executable code:
c  ----------------
  
      piby2   =acos(0.)
c         if(nomate(foil))then
c            mate=16
c         else
            mate=9
c         endif
c	call gstmed(clas_med_alu,'aluminum$', mate,
c     1        0,init_ifield,init_fieldm,
c     1        0.5,   0.5,   0.05, 0.005, 0.003, 0, 0)
c        tmed=clas_med_alu
  
c  Mother volume of the shielding foil is a tube

      if(.not.nogeom(ptg).or..not.nogeom(sol)) tubem(2) = 50. 
      
      call gsvolu('FOIL','TUBE', clas_med_vac, TUBEM, 3, IVOL)
      call gspos('FOIL', 1, 'CLAS', 0., 0., 0., 0, 'MANY')
      call gsvolu('UPST','TUBE', clas_med_vac, TUBEM, 3, IVOL)
      call gspos('UPST', 1, 'CLAS', 
     &      upst_pos_ff(1), upst_pos_ff(2), upst_pos_ff(3), 0, 'MANY')

C WARNING these volumes are here to resolve conflicts in MANY volumes
C if removed mini torus geometry will take precedence.
C === Hyon-Suk Jo - April 4, 2011 === Not defining these volumes in
C case one uses PTG or SOL (and therefore not the mini torus) to avoid
C priority conflicts between FOI2 and IC volumes when IC is used. 

      if(.not.nogeom(ptg).or..not.nogeom(sol)) then
      else
         call gsvolu('FOI1','TUBE', clas_med_air, TUBEM, 3, IVOL)
         call gspos('FOI1', 1, 'FOIL', 0., 0., 0., 0, 'MANY')
         call gsvolu('FOI2','TUBE', clas_med_air, TUBEN, 3, IVOL)
         call gspos('FOI2', 1, 'FOI1', 0., 0., 0., 0, 'MANY')
      endif

      if (chamber_type_ff.eq.0) then
      
         call recmes(crname,'W','Scattering chamber not defined -> no geometry')
         call gsvolu('GAS2','CONE',clas_med_helium,pgas2,5,IVOL)
         call gspos('GAS2',1,'FOIL',0.,0.,5.06,0,'MANY')
         if(tg_e2)call ugeom_tg_e2
         
         
         elseif (chamber_type_ff.eq.2) then
      
c Foam scattering chamber geometry (S. Morrow)
c From drawing no. 66840-E-03016

         call gsvolu('BEA2','TUBE',clas_med_alu,TUBE2,3,IVOL)
         call gsvolu('BEA3','TUBE',clas_med_alu,TUBE3,3,IVOL)
         call gsvolu('BEA4','TUBE',clas_med_alu,TUBE4,3,IVOL)
         call gspos('BEA2',1,'UPST',0.,0.,-123.26,0,'ONLY')
         call gspos('BEA3',1,'UPST',0.,0.,-264.6,0,'ONLY')
         call gspos('BEA4',1,'UPST',0.,0.,-38.82,0,'ONLY')
 
         call gsvolu('FLA1','TUBE',clas_med_alu,flng1,3,IVOL)
         call gsvolu('FLA2','TUBE',clas_med_alu,flng2,3,IVOL)
         call gsvolu('FLA3','TUBE',clas_med_alu,flng3,3,IVOL)
         call gsvolu('FLA4','TUBE',clas_med_alu,flng3,3,IVOL)    
         call gsvolu('FLA5','PCON',clas_med_alu,flng5,9,IVOL)
         call gspos('FLA1',1,'UPST',0.,0.,-58.68,0,'ONLY')
         call gspos('FLA2',1,'UPST',0.,0.,-185.88,0,'ONLY')
         call gspos('FLA3',1,'UPST',0.,0.,-343.32,0,'ONLY')
         call gspos('FLA4',1,'UPST',0.,0.,-61.48,0,'ONLY')
         call gspos('FLA5',1,'UPST',0.,0.,-18.0,0,'ONLY')
         
ccc IMPORTANT - The definition of FLA5 and BEA4 MUST come
ccc             before CHAM in order for vacuum inside the
ccc             scattering chamber to be correctly defined.

         parm(1) = 0.
         parm(2) = 360.
         parm(3) = 4
         
         parm(4) = -64.
         parm(5) = 0.
         parm(6) = 12.
         
         parm(7) = 5.0
         parm(8) = 0.
         parm(9) = 12.
         
         parm(10) = 12.0
         parm(11) = 0.
         parm(12) = 2.5
         
         parm(13) = 25.
         parm(14) = 0.
         parm(15) = 2.5

         np=parm(3)*3+3
         call gsvolu('CHAM','PCON',clas_med_air,parm,np,ivol)
         call gspos('CHAM',1,'UPST',0.,0.,0.,0,'ONLY')
         
c         call gsvolu('CHAM','TUBE', clas_med_air, CHAM0, 3 , IVOL)
c         call gspos('CHAM', 1,'UPST', 0., 0., 0., 0, 'ONLY')

         cham1(1)=0.00     ! geometry for foam chamber
         cham1(2)=360.0
         cham1(3)=12.0
         cham1(4)=-15.80   ! upstream planes
         cham1(5)=9.20
         cham1(6)=cham1(5)+chamthickness
         cham1(7)=0.00     ! last plane before hemisphere begins
         cham1(8)=cham1(5)
         cham1(9)=cham1(5)+chamthickness
         i2=cham1(3)-5          !planes for downstream hemisphere
         dangle=piby2/(i2+1)    !angular steps wrt. z-axis 
         do i =1,i2             !no of planes in hemisphere
            cham1(i*3+7)=(cham1(5))*sin(i*dangle)
            cham1(i*3+8)=cham1(5)*cos(i*dangle)
            dangleprime =asin(cham1(i*3+7)/(cham1(5)+chamthickness))
            cham1(i*3+9)=(cham1(5)+chamthickness)*cos(dangleprime)
         enddo
         npcham1=cham1(3)*3+3
         cham1(npcham1-8)= 9.20 ! end of hemisphere
         cham1(npcham1-7)= 0.55
         dangleprime =asin(cham1((i2+1)*3+7)/(cham1(5)+chamthickness))
         cham1(npcham1-6)= (cham1(5)+chamthickness)*cos(dangleprime)
         cham1(npcham1-5)=cham1(5)+chamthickness
         cham1(npcham1-4)= 0.55
         cham1(npcham1-3)= 0.63
         cham1(npcham1-2)=23.80 ! end of tube section
         cham1(npcham1-1)= 1.54
         cham1(npcham1)  = 1.62

         cham2(1)=0.0
         cham2(2)=cham1(npcham1-1)
         cham2(3)=0.00071/2

         cham3(1)=0.00       ! geometry for the vacuum inside foam chamber
         cham3(2)=360.0
         cham3(3)=cham1(3)+2 ! no. of planes is 2 more than for chamber
         cham3(4)=-63.00     ! upstream planes
         cham3(5)=0.0
         cham3(6)=9.76
         cham3(7)=-17.00
         cham3(8)=0.0
         cham3(9)=9.76
         cham3(10)=-15.80
         cham3(11)=0.00
         cham3(12)=9.20
         cham3(13)=0.00     ! last plane before hemisphere begins
         cham3(14)=0.00
         cham3(15)=9.20
         npcham3=cham3(3)*3+3
         do i =1,(npcham3-15)/3
            cham3(i*3+13)=cham1(i*3+7)
            cham3(i*3+14)=0.0
            cham3(i*3+15)=cham1(i*3+8)
         enddo

         call gsvolu('CHA1','PCON',clas_med_lowdfoam, CHAM1, npcham1, IVOL)
         call gsvolu('CHA2','TUBE',clas_med_alu, CHAM2, 3 , IVOL)
         call gsvolu('CHA3','PCON',clas_med_vac, CHAM3, npcham3, IVOL)
         call gspos('CHA1', 1,'CHAM', 0., 0., 0.0, 0, 'ONLY')
         call gspos('CHA2', 1,'CHAM', 0., 0., 
     &        cham1(npcham1-2)-(cham2(3)), 0, 'ONLY')
         call gspos('CHA3', 1,'CHAM', 0., 0., 0.0, 0, 'MANY')
         
      elseif (chamber_type_ff.eq.5) then
      
c G7a scattering chamber and upstream beam pipe and flages geometry(C. Tur)
c + positionning of the g7a target. TGG7 and CHAMBER 5 cards in ffread
c From drawing no. 66850-E-03683 

         call gsvolu('BEA5','TUBE',clas_med_alu,tube5,3,IVOL)
         call gspos('BEA5',1,'UPST',0.,0.,-60.31,0,'ONLY')
         call gsvolu('BEA6','TUBE',clas_med_alu,tube6,3,IVOL)
         call gspos('BEA6',1,'UPST',0.,0.,-164.1,0,'ONLY')

         call gsvolu('FLA6','PCON',clas_med_alu,flng6,21,IVOL)
         call gsvolu('FLA7','PCON',clas_med_alu,flng7,21,IVOL)
         call gsvolu('FLA8','PCON',clas_med_alu,flng6,21,IVOL)
         call gsvolu('FLA9','PCON',clas_med_alu,flng9,21,IVOL)
         call gsvolu('FLA1','PCON',clas_med_alu,flng10,21,IVOL)
         call gspos('FLA6',1,'UPST',0.,0.,-44.3,0,'ONLY')
         call gspos('FLA7',1,'UPST',0.,0.,-45.8,0,'ONLY')
         call gspos('FLA8',1,'UPST',0.,0.,-75.92,0,'ONLY')
         call gspos('FLA9',1,'UPST',0.,0.,-76.87,0,'ONLY')
         call gspos('FLA1',1,'UPST',0.,0.,-80.87,0,'ONLY')
         
ccc IMPORTANT - The definition of FLA6 and BEA5 MUST come
ccc             before CHAM in order for vacuum inside the
ccc             scattering chamber to be correctly defined.
         
         parm(1) = 0.
         parm(2) = 360.
         parm(3) = 4
         
         parm(4) = -100.
         parm(5) = 0.
         parm(6) = 12.7
         
         parm(7) = -36.80
         parm(8) = 0.
         parm(9) = 12.7
         
         parm(10) = -23.8
         parm(11) = 0.
         parm(12) = 4.5
         
         parm(13) = 25.
         parm(14) = 0.
         parm(15) = 4.5

         np=parm(3)*3+3
         call gsvolu('CHAM','PCON',clas_med_air,parm,np,ivol)
         call gspos('CHAM',1,'UPST',0.,0.,0.,0,'ONLY')

      data cham4 /0.0,360.0,9.0,0.0,9.8,10.1,5.05,9.8,10.1,
     &            6.05,9.65,9.95,7.05,9.2,9.5,17.1,3.4,3.8,
     &            17.55,3.15,3.8,17.7,3.15,3.8,17.7,3.15,3.3,
     &            17.9,3.15,3.3/
         
      data cham5 /0.0,360.0,7.0,0.0,3.3,3.9,0.2,3.3,3.9,0.2,
     &            3.15,3.9,47.2,2.15,2.9,47.55,2.0,2.9,47.7,
     &            1.5,2.87,49.2,1.5,2.85/
 
      data cham6 /0.0,1.5,0.00355/
  
      data vac1 /0.0,360.0,12.0,0.0,0.0,11.8,10.0,0.0,11.8,
     &           10.0,0.0,9.8,47.07,0.0,9.8,48.07,0.0,9.65,
     &           49.07,0.0,9.2,59.12,0.0,3.4,59.57,0.0,3.15,
     &           59.92,0.0,3.15,106.92,0.0,2.15,107.27,0.0,
     &           2.0,107.42,0.0,1.5,108.916,0.0,1.5/

         call gsvolu('CHA4','PCON',clas_med_alu,cham4,30,IVOL)
         call gsvolu('CHA5','PCON',clas_med_lowdfoam,cham5,24,IVOL)
         call gsvolu('CHA6','TUBE',clas_med_alu,cham6,3,IVOL)
         call gsvolu('VAC1','PCON',clas_med_vac,vac1,42,IVOL)
         call gspos('CHA4',1,'CHAM',0.,0.,-43.5,0,'ONLY')
         call gspos('CHA5',1,'CHAM',0.,0.,-25.8,0,'ONLY')
         call gspos('CHA6',1,'CHAM',0.,0.,23.4,0,'ONLY')
         call gspos('VAC1',1,'CHAM',0.,0.,-85.52,0,'MANY')

       
*
* place the g7a target in CHAM here
*
 
        if(tg_g7)then
           nogeom(tg)=.TRUE.
           nogeom(ptg)=.TRUE.
          call geom_tg_g7
        endif        

c-H.Hakobyan
      elseif (chamber_type_ff.eq.6) then

         call gsvolu('BEA2','TUBE',clas_med_alu,TUBE2,3,IVOL)
         call gsvolu('BEA3','TUBE',clas_med_alu,TUBE3,3,IVOL)
         call gsvolu('BEA4','TUBE',clas_med_alu,TUBE4,3,IVOL)
         call gspos('BEA2',1,'UPST',0.,0.,-123.26,0,'ONLY')
         call gspos('BEA3',1,'UPST',0.,0.,-264.6,0,'ONLY')
         call gspos('BEA4',1,'UPST',0.,0.,-38.82,0,'ONLY')
 
         call gsvolu('FLA1','TUBE',clas_med_alu,flng1,3,IVOL)
         call gsvolu('FLA2','TUBE',clas_med_alu,flng2,3,IVOL)
         call gsvolu('FLA3','TUBE',clas_med_alu,flng3,3,IVOL)
         call gsvolu('FLA4','TUBE',clas_med_alu,flng3,3,IVOL)    
         call gsvolu('FLA5','PCON',clas_med_alu,flng5,9,IVOL)
         call gspos('FLA1',1,'UPST',0.,0.,-58.68,0,'ONLY')
         call gspos('FLA2',1,'UPST',0.,0.,-185.88,0,'ONLY')
         call gspos('FLA3',1,'UPST',0.,0.,-343.32,0,'ONLY')
         call gspos('FLA4',1,'UPST',0.,0.,-61.48,0,'ONLY')
         call gspos('FLA5',1,'UPST',0.,0.,-18.0,0,'ONLY') 
        
ccc IMPORTANT - The definition of FLA5 and BEA4 MUST come
ccc             before CHAM in order for vacuum inside the
ccc             scattering chamber to be correctly defined.

         parm(1) = 0.
         parm(2) = 360.
         parm(3) = 4
         
         parm(4) = -64.
         parm(5) = 0.
         parm(6) = 12.
         
         parm(7) = 5.0
         parm(8) = 0.
         parm(9) = 12.
         
         parm(10) = 12.0
         parm(11) = 0.
         parm(12) = 2.5
         
         parm(13) = 25.
         parm(14) = 0.
         parm(15) = 2.5

         np=parm(3)*3+3
         call gsvolu('CHAM','PCON',clas_med_air,parm,np,ivol)
         call gspos('CHAM',1,'UPST',0.,0.,0.,0,'ONLY')
c Foam scattering chamber geometry 
c From drawing no. 66850-E-03565


c-end_H.Hakobyan

c-chamber eg6
      elseif (chamber_type_ff.eq.7) then

         call ugeom_tg_eg6          
       
        

      elseif (chamber_type_ff.eq.1) then 
      
c Saclay scattering chamber geometry (with RIBS).

         call gsvolu('UPS1','TUBE', clas_med_air, TUBEM, 3, IVOL)
         call gspos('UPS1', 1, 'UPST', 0., 0., 0., 0, 'MANY')
         call gsvolu('UPS2','TUBE', clas_med_air, TUBEN, 3, IVOL)
         call gspos('UPS2', 1, 'UPS1', 0., 0., 0., 0, 'MANY')
 
         call gsvolu('BEA1','TUBE',clas_med_alu,TUBE1,3,IVOL)
         call gsvolu('BEA2','TUBE',clas_med_alu,TUBE2,3,IVOL)
         call gsvolu('BEA3','TUBE',clas_med_alu,TUBE3,3,IVOL)
         call gspos('BEA1',1,'UPST',0.,0.,-39.63,0,'ONLY')
         call gspos('BEA2',1,'UPST',0.,0.,-123.26,0,'ONLY')
         call gspos('BEA3',1,'UPST',0.,0.,-264.6,0,'ONLY')
 
         call gsvolu('FLA1','TUBE',clas_med_alu,flng1,3,IVOL)
         call gsvolu('FLA2','TUBE',clas_med_alu,flng2,3,IVOL)
         call gsvolu('FLA3','TUBE',clas_med_alu,flng3,3,IVOL)
         call gsvolu('FLA4','TUBE',clas_med_alu,flng3,3,IVOL)    
         call gspos('FLA1',1,'UPST',0.,0.,-58.68,0,'ONLY')
         call gspos('FLA2',1,'UPST',0.,0.,-185.88,0,'ONLY')
         call gspos('FLA3',1,'UPST',0.,0.,-343.32,0,'ONLY')
         call gspos('FLA4',1,'UPST',0.,0.,-61.48,0,'ONLY')

         call gsvolu('RIBS','TRD2',clas_med_alu,prib,5,IVOL)
         call gspos('RIBS',1,'UPS2',0.,0.,.147,0,'MANY')

         call gsvolu('GAS1','TUBE',clas_med_helium,pgas1,3,IVOL)
         call gspos('GAS1',1,'UPST',0.,0.,-15.35,0,'ONLY')
         call gsvolu('GAS2','CONE',clas_med_helium,pgas2,5,IVOL)
         call gspos('GAS2',1,'UPST',0.,0.,5.06,0,'ONLY')
         call gsvolu('GAS3','TUBE',clas_med_helium,pgas3,3,IVOL)
         call gspos('GAS3',1,'UPST',0.,0.,20.55,0,'ONLY')
         
         call gsvolu('VAC','TUBE',clas_med_vac,tubev,3,IVOL)
         call gspos('VAC',1,'UPST',0.,0.,105.,0,'ONLY')
         
         call gsvolu('TAR1','TUBE',clas_med_helium,ptar1,3,IVOL)
         call gspos('TAR1',1,'UPST',0.,0.,-21.49,0,'ONLY')
         call gsvolu('WIND','TRD2',clas_med_mylar,wind1,5,IVOL)
         call gspos('WIND',1,'UPS2',9.,0.,0.,foir_1,'ONLY')
         call gspos('WIND',2,'UPS2',-9.,0.,0.,foir_2,'ONLY')
         call gspos('UPS2', 2, 'UPS1', 0., 0., 0.,foir_3, 'MANY')
         call gspos('UPS2', 3, 'UPS1', 0., 0., 0.,foir_4, 'MANY')
                 elseif (chamber_type_ff.eq.3) then
           parm(1) = 0.
           parm(2) = 10.8
c           parm(3) = 30. !for correct HODO pisition
           parm(3) = 20.

           np=parm(3)*3+3
           call gsvolu('DVCS','TUBE',clas_med_air,parm,3,ivol)
           call gspos('DVCS',1,'CLAS',tg_pos_ff(1),tg_pos_ff(2),
     &          tg_pos_ff(3),0,'ONLY')
           print*,'dvcs_z==',tg_pos_ff(3)
           
C     Rustam Niyazov March. 2008
C     The CHA1, CHA3 volumes are based on drawing 66850-D-04245           
C     &            -9.8552, 3.175, 3.81,

      data cham1 /0.00,360.0,12.0,
     &            -11.5, 3.175, 3.81,
     &            0, 3.175, 3.81,
     &            0.635, 3.11085, 3.75671,
     &            1.27, 2.90994, 3.5921,
     &            1.905, 2.54, 3.29956,
     &            2.54, 1.905, 2.83981,
     &            3.048, 0.889, 2.286,
     &            3.175, 0.39624, 2.10606,
     &            3.302, 0.39624, 1.90076,
     &            3.556, 0.39624, 1.36783,
     &            3.81, 0.39624, 0.9525,
     &            4.445,0.39624,0.9525/
     
      data cham2 /0.0,0.395,0.00355/
      data cham3 /0.00,360.0,12.0,
     &            -11.5, 0.0, 3.175,
     &            0, 0.0, 3.175,
     &            0.635, 0.0, 3.11085,
     &            1.27, 0.0, 2.90994,
     &            1.905, 0.0, 2.54,
     &            2.54, 0.0, 1.905,
     &            3.048, 0.0, 0.889,
     &            3.175, 0.0, 0.39624,
     &            3.302, 0.0, 0.39624,
     &            3.556, 0.0, 0.39624,
     &            3.81, 0.0, 0.39624,
     &            4.445, 0.0, 0.39624,
     &            0.0,0.0,0.0,
     &            0.0,0.0,0.0/ 
   
      call gsvolu('CHA1','PCON',clas_med_lowdfoam, CHAM1,39, IVOL)
      call gsvolu('CHA2','TUBE',clas_med_alu, CHAM2, 3 , IVOL)
      call gsvolu('CHA3','PCON',clas_med_vac, CHAM3, 39, IVOL)
      call gspos('CHA1', 1,'DVCS', 0., 0., chamshift, 0, 'ONLY')
      call gspos('CHA2', 1,'DVCS', 0., 0.,chamshift+4.445, 0, 'ONLY')
      
      call gspos('CHA3', 1,'DVCS',0., 0., chamshift, 0, 'MANY')
      print *,'GEOM_FOIL',CHAM1
      return                    ! don't need all other stuff for DVCS
c                 

*
* Daria Sokhan, July 2010: e1-DVCS-2 scattering chamber
*      
      elseif (chamber_type_ff.eq.8) then
         
         parm(1) = 0.
         parm(2) = 10.8
         parm(3) = 45.
         
         call gsvolu('DVCS','TUBE',clas_med_air,parm,3,ivol)
         call gspos('DVCS',1,'CLAS',tg_pos_ff(1),tg_pos_ff(2),
     &        tg_pos_ff(3),0,'ONLY')     
         
         
         data dvcs_c1 /0.00, 360.0, 13.,            ! scattering chamber shell (polycone)
     &                 -8.7665, 3.8255, 4.1921,     ! edge at which the modelled bits of target start
     &                 -8.1048, 3.4434, 3.81,       ! end of slope on the outside surface of chamber
     &                 -7.365, 3.0163, 3.81,        ! start of tube part of scattering chamber (its total length is 19")
     &                 38.7589, 3.0163, 3.81,       ! start of quarter-circle bend on the inner surface
     &                 38.8589, 3.0084, 3.81,
     &                 38.9589, 2.9840, 3.81,
     &                 39.0589, 2.9410, 3.81,
     &                 39.1589, 2.8745, 3.81,
     &                 39.2589, 2.7727, 3.81,
     &                 39.3589, 2.5892, 3.81,
     &                 39.3939, 2.3813, 3.81,       ! end of the quarter-circle bend on the inner surface
     &                 39.3939, 1.27, 3.81,
     &                 40.895, 1.27, 3.81/
         
         data dvcs_c2  /0.0, 1.27, 0.0025/          ! scattering chamber exit window (tube)
         
         data dvcs_c3 /0.00, 360.0, 13.,            ! vacuum inside scattering chamber shell
     &                 -8.7665, 0.0, 3.8255,        ! edge at which the modelled bits of target start
     &                 -8.1048, 0.0, 3.4434,        ! end of slope on the outside surface of chamber
     &                 -7.365, 0.0, 3.0163,         ! start of tube part of scattering chamber (its total length is 19")
     &                 38.7589, 0.0, 3.0163,        ! start of quarter-circle bend on the inner surface
     &                 38.8589, 0.0, 3.0084,
     &                 38.9589, 0.0, 2.9840,
     &                 39.0589, 0.0, 2.9410,
     &                 39.1589, 0.0, 2.8745,
     &                 39.2589, 0.0, 2.7727,
     &                 39.3589, 0.0, 2.5892,
     &                 39.3939, 0.0, 2.3813,       ! end of the quarter-circle bend on the inner surface
     &                 39.3939, 0.0, 1.27,
     &                 40.895, 0.0, 1.27/

         
         call gsvolu('CH1 ','PCON',clas_med_lowdfoam, dvcs_c1, 42, IVOL)
         call gsvolu('CH2 ','TUBE',clas_med_alu, dvcs_c2, 3 , IVOL)
         call gsvolu('CH3 ','PCON',clas_med_vac, dvcs_c3, 42, IVOL)
         
         call gspos('CH1 ', 1,'DVCS', 0., 0., 0., 0, 'ONLY')
         call gspos('CH2 ', 1,'DVCS', 0., 0., 40.8975, 0, 'ONLY')   ! scattering chamber exit window 
         call gspos('CH3 ', 1,'DVCS',0., 0., 0., 0, 'MANY')
         
         print *,'GEOM_FOIL',dvcs_c1
         return                 ! don't need all other stuff for DVCS
           
c         
      elseif (chamber_type_ff.eq.4) then   
      
c  define NH3 for target 

        if(.not.nogeom(ptg)) then
c
c
c            ptg_mat_nh3 = get_mate()
c            call gsmixt(ptg_mat_nh3, 'PTG NH3$',
c     +           anh3, znh3, 1.0, -2, wnh3)
c======

            if(tg_type_ff.ne.28.and.tg_type_ff.ne.31.and.
     +       tg_type_ff.ne.32)then
               ptg_mat_nh3 = get_mate()
               call gsmixt(ptg_mat_nh3, 'PTG NH3$',
     +              aptg, zptg, densptg, 3, wptg)
            endif
            
            if(tg_type_ff.eq.28.or.tg_type_ff.eq.31) then
               ptg_mat_nh3 = get_mate()
               rho_nh3_he=packfrac_nh3*rho_nh3+(1-packfrac_nh3)*rho_he
               densptg_nh3_eg1dvcs(1)=packfrac_nh3*rho_nh3*(3./17.)
               densptg_nh3_eg1dvcs(2)=packfrac_nh3*rho_nh3*(14./17.)
               densptg_nh3_eg1dvcs(3)=(1-packfrac_nh3)*rho_he
               wptg_nh3_eg1dvcs(1)=densptg_nh3_eg1dvcs(1)/rho_nh3_he
               wptg_nh3_eg1dvcs(2)=densptg_nh3_eg1dvcs(2)/rho_nh3_he
               wptg_nh3_eg1dvcs(3)=densptg_nh3_eg1dvcs(3)/rho_nh3_he

               print *,"rho_nd3_he=",rho_nh3_he
               print *,"densptg_nd3_eg1dvcs(1)",densptg_nh3_eg1dvcs(1)
               print *,"densptg_nd3_eg1dvcs(2)",densptg_nh3_eg1dvcs(2)
               print *,"densptg_nd3_eg1dvcs(3)",densptg_nh3_eg1dvcs(3)
               print *,"wptg_nd3_eg1dvcs(1)",wptg_nh3_eg1dvcs(1)
               print *,"wptg_nd3_eg1dvcs(2)",wptg_nh3_eg1dvcs(2)
               print *,"wptg_nd3_eg1dvcs(3)",wptg_nh3_eg1dvcs(3)

               call gsmixt(ptg_mat_nh3, 'PTG NH3$',
     +              aptg_nh3_eg1dvcs, zptg_nh3_eg1dvcs, 
     +              densptg_nh3_eg1dvcs, 3, wptg_nh3_eg1dvcs)
               
            endif
            if(tg_type_ff.ne.32) then
               ptg_med_nh3 = get_tmed()
               
               if(nomate(ptg))then
                  mate=16
               else
                  mate=ptg_mat_nh3
               endif
            
               call gstmed(ptg_med_nh3,'PTG NH3$',mate,
     1              0,2,ptg_fieldm,
     1              0.5,   0.5,   0.05, 0.005, 0.003, 0, 0)
               
            endif
c     ptg_med_nh3 = get_tmed()

c            if(nomate(ptg))then
c               mate=16
c            else
c               mate=ptg_mat_nh3
c            endif
                        
            if(tg_type_ff.eq.32) then
            ptg_mat_nd3 = get_mate()
               rho_nd3_he=packfrac_nd3*rho_nd3+(1-packfrac_nd3)*rho_he
               densptg_nd3_eg1dvcs(1)=packfrac_nd3*rho_nd3*(6./20.)
               densptg_nd3_eg1dvcs(2)=packfrac_nd3*rho_nd3*(14./20.)
               densptg_nd3_eg1dvcs(3)=(1-packfrac_nd3)*rho_he
               wptg_nd3_eg1dvcs(1)=densptg_nd3_eg1dvcs(1)/rho_nd3_he
               wptg_nd3_eg1dvcs(2)=densptg_nd3_eg1dvcs(2)/rho_nd3_he
               wptg_nd3_eg1dvcs(3)=densptg_nd3_eg1dvcs(3)/rho_nd3_he

               print *,"rho_nd3_he=",rho_nd3_he
               print *,"densptg_nd3_eg1dvcs(1)",densptg_nd3_eg1dvcs(1)
               print *,"densptg_nd3_eg1dvcs(2)",densptg_nd3_eg1dvcs(2)
               print *,"densptg_nd3_eg1dvcs(3)",densptg_nd3_eg1dvcs(3)
               print *,"wptg_nd3_eg1dvcs(1)",wptg_nd3_eg1dvcs(1)
               print *,"wptg_nd3_eg1dvcs(2)",wptg_nd3_eg1dvcs(2)
               print *,"wptg_nd3_eg1dvcs(3)",wptg_nd3_eg1dvcs(3)
               

               call gsmixt(ptg_mat_nd3, 'PTG ND3$',
     +              aptg_nd3_eg1dvcs, zptg_nd3_eg1dvcs, 
     +         densptg_nd3_eg1dvcs, 3, wptg_nd3_eg1dvcs)
c>>>>>>1.18
               ptg_med_nd3 = get_tmed()
c            endif
      
c      call gstmed(ptg_med_nh3,'PTG NH3$',mate,
c     1       0,2,ptg_fieldm,
c     1       0.5,   0.5,   0.05, 0.005, 0.003, 0, 0)
        
               mate=ptg_mat_nd3
      call gstmed(ptg_med_nd3,'PTG ND3$',mate,
     1       0,2,ptg_fieldm,
     1       0.5,   0.5,   0.05, 0.005, 0.003, 0, 0)
      endif

      if (ntarget.eq.2) ptg_med = clas_med_h2liq
      if (ntarget.ge.3.and.tg_type_ff.ne.32) ptg_med = ptg_med_nh3
      if (ntarget.ge.3.and.tg_type_ff.eq.32) ptg_med = ptg_med_nd3
      targptg(3) = tleng/2.

      call gsvolu('TARG','TUBE', ptg_med, targptg, 3, IVOL)
      call gspos('TARG',1,'UPST',0.,0.,tg_pos_ff(3), 0, 'ONLY')
c     call gspos('TARG',1,'HVAC',0.,0.,-245., 0, 'ONLY')

      if (ntarget.eq.4) then
        call gsvolu('HVAC','TUBE', clas_med_vac,vacpip, 3, IVOL)
        call gspos('HVAC',1,'UPST',0.,0.,tg_pos_ff(3)+245., 0, 'ONLY')
        call gsvolu('PIPE','TUBE', clas_med_alu,beampip, 3, IVOL)
        call gspos('PIPE',1,'HVAC',0.,0.,tg_pos_ff(3)+200.4, 0, 'ONLY')
        call gsvolu('RAD ','TUBE', clas_med_copper, targrad, 3, IVOL)
        call gspos('RAD ',1,'HVAC',0.,0.,-25.4-245., 0, 'ONLY')
      endif
          
          if (ntarget.ne.4.and.ntarget.ne.3) then
         
          call gsvolu('MOL1','CONE',clas_med_lead,pmol1,5,IVOL)
          call gsvolu('MOL2','CONE',clas_med_lead,pmol2,5,IVOL)
          call gsvolu('MOL3','TUBE',clas_med_lead,pmol3,3,IVOL)
          call gsvolu('MOL4','CONE',clas_med_lead,pmol4,5,IVOL)
          call gsvolu('MOL5','TUBE',clas_med_lead,pmol5,3,IVOL)
          call gsvolu('MOL6','CONE',clas_med_iron,pmol6,5,IVOL)
c          call gsvolu('MOL7','CONE',clas_med_iron,pmol7,5,IVOL)
c          call gsvolu('MOL8','CONE',clas_med_stainless_steel,pmol8,5,IVOL)
          call gsvolu('MOL9','CONE',clas_med_vac,pmol9,5,IVOL)
c          call gsvolu('MOLW','TUBE',clas_med_alu,pmolw,3,IVOL)
          call gsvolu('MOL8','TUBE',clas_med_vac,pmol8,3,IVOL)

c	  call gsvolu('MOL9','TUBE',clas_med_lead,pmol9,3,IVOL)
c	  call gsvolu('MOL10','CONE',clas_med_stainless_steel,pmol10,5,IVOL)
c	  call gsvolu('MOL11','CONE',clas_med_stainless_steel,pmol11,5,IVOL)
c	  call gsvolu('MOL12','CONE',clas_med_stainless_steel,pmol12,5,IVOL)
	  
c          call gspos('MOL1',1,'FOIL',0.,0.,-6.22,0,'ONLY')
c          call gspos('MOL2',1,'FOIL',0.,0.,22.61,0,'ONLY')
c          call gspos('MOL3',1,'FOIL',0.,0.,106.31,0,'ONLY')
c          call gspos('MOL4',1,'FOIL',0.,0.,191.11,0,'ONLY')
c          call gspos('MOL5',1,'FOIL',0.,0.,125.15,0,'ONLY')
c          call gspos('MOL8',1,'FOIL',0.,0.,-44.5,0,'ONLY')

          call gspos('MOL1',1,'FOIL',0.,0.,-7.3115,0,'ONLY')
          call gspos('MOL2',1,'FOIL',0.,0.,22.216,0,'ONLY')
          call gspos('MOL3',1,'FOIL',0.,0.,54.766,0,'ONLY')
          call gspos('MOL4',1,'FOIL',0.,0.,-47.5,0,'ONLY')
          call gspos('MOL5',1,'FOIL',0.,0.,81.60,0,'ONLY')
	  call gspos('MOL6',1,'FOIL',0.,0.,-67.469,0,'ONLY')
c          call gspos('MOL7',1,'FOIL',0.,0.,-27.30,0,'ONLY')
c          call gspos('MOL8',1,'FOIL',0.,0.,-7.3115,0,'ONLY')
          call gspos('MOL9',1,'FOIL',0.,0.,39.88,0,'ONLY')
c          call gspos('MOLW',1,'FOIL',0.,0.,-67.70,0,'ONLY')
          call gspos('MOL8',1,'FOIL',0.,0.,329.325,0,'ONLY')

c          call gspos('MOL9',1,'FOIL',0.,0.,81.60,0,'ONLY')
c          call gspos('MOL10',1,'FOIL',0.,0.,74.9085,0,'ONLY')
c	  call gspos('MOL11',1,'FOIL',0.,0.,0.0,0,'ONLY')
c	  call gspos('MOL12',1,'FOIL',0.,0.,0.0,0,'ONLY')


          endif
          
        endif
         
c<<<<<<< geom_foil.F
cc======
        go to 77                    ! jump to shielding baffles code
c>>>>>>> 1.18
      endif

c    Downstream shielding which is the same for all scattering chambers.

      if (ntarget.ne.4) then
      
      if (chamber_type_ff.lt.3) then
        call gsvolu('SHD1','CONE',clas_med_lead,shld1,5,IVOL)
        call gspos('SHD1',1,'FOIL',0.,0.,52.216,0,'ONLY')
      endif
      
      call gsvolu('SHD2','TUBE',clas_med_lead,shld2,3,IVOL)
      call gspos('SHD2',1,'FOIL',0.,0.,82.85,0,'ONLY')
      call gsvolu('SHD3','TUBE',clas_med_lead,shld3,3,IVOL)
      call gspos('SHD3',1,'FOIL',0.,0.,108.06,0,'ONLY')
      call gsvolu('SHD4','TUBE',clas_med_iron,shld4,3,IVOL)
      call gspos('SHD4',1,'FOIL',0.,0.,155.49,0,'ONLY')
      call gsvolu('SHD5','CONE',clas_med_iron,shld5,5,IVOL)
      call gspos('SHD5',1,'FOIL',0.,0.,185.01,0,'ONLY')
      call gsvolu('SHD6','CONE',clas_med_iron,shld6,5,IVOL)
      call gspos('SHD6',1,'FOIL',0.,0.,188.19,0,'ONLY')
      call gsvolu('SHD7','CONE',clas_med_iron,shld7,5,IVOL)
      call gspos('SHD7',1,'FOIL',0.,0.,237.72,0,'ONLY')
      call gsvolu('SHD8','TUBE',clas_med_iron,shld8,3,IVOL)
      call gspos('SHD8',1,'FOIL',0.,0.,301.85,0,'ONLY')
      call gsvolu('SHD9','TUBE',clas_med_lead,shld9,3,IVOL)
      call gspos('SHD9',1,'FOIL',0.,0.,327.25,0,'ONLY')
      call gsvolu('SH11','TUBE',clas_med_iron,shl11,3,IVOL)
      call gspos('SH11',1,'FOIL',0.,0.,442.6,0,'ONLY')
    
c     Position wedge shaped 5 cm thick Pb shielding baffles between main torus coils.  
c     Use ffread card: BAFF nbaf th1 z1 th2 z2 th3 z3 
c     Parameters are nbaf: number of baffles (up to 3)
c                      th: scattering angle of top of wedge
c                       z: distance from target to center of wedge
      
 77   nbaf = foil_baffles(1)
      
      if (nbaf.ge.1.and.nbaf.le.3) then
      
        baff_1=get_rotm()
        CALL GSROTM(baff_1,90.,270.,90.,0.,0.,0.)
      
        do i = 1,nbaf
          th		= foil_baffles(i*2)*3.14149/180.
          zpos		= foil_baffles(i*2+1)
          if (i.eq.1) call gsvolu('BAFF','TRAP',clas_med_lead,baff(1,i),0,IVOL)
          baff(4,i)	= 0.5*(zpos*tan(th)-14.)
          baff(6,i)	= baff(4,i)*0.5774*2
          baff(8,i)	= baff(4,i)
          baff(10,i)	= baff(6,i)
          xpos		= 14. + baff(4,i)
          call gsposp('BAFF',i,'LSS-',xpos,0.,zpos,baff_1,'ONLY',baff(1,i),11)
        enddo
      
      endif
      
      endif
              
      return
      end

c---------------------------------------------------------------------------------
