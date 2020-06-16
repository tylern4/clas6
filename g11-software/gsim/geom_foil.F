      subroutine geom_foil

c_begin_doc

c  Documentation for subroutine geom_foil

c  $Id: geom_foil.F,v 1.13 2004/10/15 14:33:33 lcsmith Exp $

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
c     *          ======================                                *
c     *          Removal, adding of some shieldings....                *  
c     *                                                                *
c     ******************************************************************

c  Major revisions:
c  ----------------
c     Elliott Wolin, College of William and Mary, 6-dec-94
c     C. Tur, Univ. of South Carolina, 25-Jul-04: added g7a upstream beam line
c             elements + scattering chamber (CHAMBER 5 in ffread) +
c             positionning of the g7a target (call to routine geom_tg_g7)
c_end_doc


      implicit none
      character*(*) cfile,crevis,cstate,cdate,cautho,crname,crauth
      character*132 crcsid
      parameter (cfile=  '$RCSfile: geom_foil.F,v $')
      parameter (crevis= '$Revision: 1.13 $')
      parameter (cstate= '$State: Exp $')
      parameter (cdate=  '$Date: 2004/10/15 14:33:33 $')
      parameter (cautho= '$Author: lcsmith $')
      parameter (crname= 'geom_foil')
      parameter (crauth= 'Elliott Wolin')
      data crcsid/'
     1$Id: geom_foil.F,v 1.13 2004/10/15 14:33:33 lcsmith Exp $
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
#include "cltmed.inc"
#include "ffpar_foil.inc"
#include "tg_e2_tmed.inc"
#include "ptgtmed.inc"
#include "ffpar_tg.inc"
#include "ffpar_ptg.inc"
#include "ffpar_bckgr.inc"
#include "ffpar_tg_g7.inc"
c_end_inc

c  local variables:
c  ----------------
      integer ivol,mate,foir_1,foir_2,foir_3,foir_4,get_rotm,np,get_tmed
      integer get_mate, ptg_mat_nh3, ptg_med, mol6_rot, mol7_rot

      real tubem(3),tube1(3),tube2(3),tube3(3),flng1(3),flng2(3),
     1     flng3(3),flng4(3),shld1(5),shld2(3),shld3(3),shld4(3),
     1     shld5(5),shld6(5),shld7(5),shld8(3),shld9(3),prib(5),
     1     pgas1(3),pgas2(5),pgas3(3),ptar1(3),wind1(5),tuben(3),
     1     shl11(3),tubev(3),cham0(3),cham1(39),cham2(3),cham3(45),
     1     chamthickness,tube4(3),flng5(9),parm(15),
     1     pmol1(5),pmol2(5),pmol3(3),pmol4(3),pmol5(3),targptg(3),
     1     icvol4(3),pmol6(3),pmol7(3),flng6(21),flng7(21),flng9(21),
     1     flng10(21),tube5(3),tube6(3),cham4(30),cham5(24),cham6(3),
     1     vac1(42)

c define NH3
      real anh3(2), znh3(2), wnh3(2)
      data anh3 / 1., 14./
      data znh3 / 1.,  7./
      data wnh3 / 3.,  1./  
      
      data tubem /0. , 30.,  550./
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

      data pmol1 /17.59, 3., 4.38, 3.65, 9.3/
      data pmol2 /11.24, 3.97, 9.3, 3.97, 10.91/
      data pmol3 /3.97, 10.91, 72.46/
      data pmol4 /3.97, 8.0, 12.34/
      data pmol5 /3.65, 3.97, 113.78/
      data pmol6 /5.,6.,5./
      data pmol7 /0.,6.,0.5/
      data targptg /0.,1.0,0.75/
      
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

      if(.not.nogeom(ptg)) tubem(2) = 50. 
      
      call gsvolu('FOIL','TUBE', clas_med_vac, TUBEM, 3, IVOL)
      call gspos('FOIL', 1, 'CLAS', 0., 0., 0., 0, 'MANY')
      call gsvolu('UPST','TUBE', clas_med_vac, TUBEM, 3, IVOL)
      call gspos('UPST', 1, 'CLAS', 
     &      upst_pos_ff(1), upst_pos_ff(2), upst_pos_ff(3), 0, 'MANY')

C WARNING these volumes are here to resolve conflicts in MANY volumes
C if removed mini torus geometry will take precedence.

      call gsvolu('FOI1','TUBE', clas_med_air, TUBEM, 3, IVOL)
      call gspos('FOI1', 1, 'FOIL', 0., 0., 0., 0, 'MANY')
      call gsvolu('FOI2','TUBE', clas_med_air, TUBEN, 3, IVOL)
      call gspos('FOI2', 1, 'FOI1', 0., 0., 0., 0, 'MANY')

      if (chamber_type_ff.eq.0) then
      
         call recmes(crname,'W','Scattering chamber not defined -> no geometry')
         
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
         
      elseif (chamber_type_ff.eq.4) then   
      
c  define NH3 for target 

        if(.not.nogeom(ptg)) then

          ptg_mat_nh3 = get_mate()
          call gsmixt(ptg_mat_nh3, 'PTG NH3$',
     +             anh3, znh3, 1.0, -2, wnh3)

          ptg_med_nh3 = get_tmed()

          if(nomate(ptg))then
             mate=16
          else
             mate=ptg_mat_nh3
          endif
      
          call gstmed(ptg_med_nh3,'PTG NH3$',mate,
     1       0,2,ptg_fieldm,
     1       0.5,   0.5,   0.05, 0.005, 0.003, 0, 0)
        
          if (ntarget.eq.2) ptg_med = clas_med_h2liq
          if (ntarget.eq.3) ptg_med = ptg_med_nh3
          targptg(3) = tleng/2.

          call gsvolu('TARG','TUBE', ptg_med, targptg, 3, IVOL)
          call gsvolu('MOL1','CONE',clas_med_lead,pmol1,5,IVOL)
          call gsvolu('MOL2','CONE',clas_med_lead,pmol2,5,IVOL)
          call gsvolu('MOL3','TUBE',clas_med_lead,pmol3,3,IVOL)
          call gsvolu('MOL4','TUBE',clas_med_lead,pmol4,3,IVOL)
          call gsvolu('MOL5','TUBE',clas_med_lead,pmol5,3,IVOL)
         
          call gspos('TARG',1,'FOIL',0.,0.,tg_pos_ff(3), 0, 'ONLY')
          call gspos('MOL1',1,'FOIL',0.,0.,-6.22,0,'ONLY')
          call gspos('MOL2',1,'FOIL',0.,0.,22.61,0,'ONLY')
          call gspos('MOL3',1,'FOIL',0.,0.,106.31,0,'ONLY')
          call gspos('MOL4',1,'FOIL',0.,0.,191.11,0,'ONLY')
          call gspos('MOL5',1,'FOIL',0.,0.,125.15,0,'ONLY')
          
        endif
         
      endif

c    Downstream shielding which is the same for all scattering chambers.

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
      
      nbaf = foil_baffles(1)
      
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
              
      return
      end

c---------------------------------------------------------------------------------
