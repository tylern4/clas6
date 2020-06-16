      subroutine geom_foil

c_begin_doc

c  Documentation for subroutine geom_foil

c  $Id: geom_foil_new.F,v 1.3 2001/05/11 14:38:09 sam Exp $

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

c_end_doc


      implicit none
      character*(*) cfile,crevis,cstate,cdate,cautho,crname,crauth
      character*132 crcsid
      parameter (cfile=  '$RCSfile: geom_foil_new.F,v $')
      parameter (crevis= '$Revision: 1.3 $')
      parameter (cstate= '$State: Exp $')
      parameter (cdate=  '$Date: 2001/05/11 14:38:09 $')
      parameter (cautho= '$Author: sam $')
      parameter (crname= 'geom_foil')
      parameter (crauth= 'Elliott Wolin')
      data crcsid/'
     1$Id: geom_foil_new.F,v 1.3 2001/05/11 14:38:09 sam Exp $
     1 '/
      save


c  include files:
c  --------------
#include "gcunit.inc"
#include "gclist.inc"
#include "gconst.inc"
#include "lcgeom.inc"
#include "cltmed.inc"
#include "ffpar_foil.inc"
#include "tg_e2_tmed.inc"
c_end_inc

c  local variables:
c  ----------------
      integer ivol,mate,foir_1,foir_2,foir_3,foir_4,get_rotm

      real tubem(3),tube1(3),tube2(3),tube3(3),flng1(3),flng2(3),
     1     flng3(3),flng4(3),shld1(5),shld2(3),shld3(3),shld4(3),
     1     shld5(5),shld6(5),shld7(5),shld8(3),shld9(3),prib(5),
     1     pgas1(3),pgas2(5),pgas3(3),ptar1(3),wind1(5),tuben(3),
     1     shl11(3),tubev(3),cham0(3),cham1(27),cham2(3),
     1     tube4(3),flng5(9)

      data tubem /0. , 30.,  550./
      data tubev /0. , 1.,  200./
      data tube1 /10.14,10.92,16.92/
      data tube2 /7.71,8.42,61.1/
      data tube3 /7.32,8.42,77.1/
      data tube4 /9.76,10.16,19.70/
      data flng1 /10.14,15.2,2.12/
      data flng2 /7.71,13.96,1.64/
      data flng3 /7.32,13.96,1.64/
      data flng4 /8.42,15.2,.68/
      data flng5 /0.0,360.0,2.0,0.0,8.65,10.16,2.8,8.65,10.16/
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
      data cham0 /0.0,12.0,30.0/
      data cham1 /0.00,360.0,8.0,
     &          -15.80,9.20,10.16,
     &            0.00,9.20,10.16,
     &            2.54,8.98,9.96,
     &            5.08,8.28,9.34,
     &            7.62,6.20,8.20,
     &            9.20,0.55,4.31,
     &           10.16,0.55,0.63,
     &           23.80,1.54,1.62/
      data cham2 /0.0,1.54,0.00071/

c Data for the shielding baffles
c ------------------------------
      integer nbaf,baff_1,i
      real th,xpos,zpos,baff(11,3)
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
      
      if (chamber_type_ff.le.0 .or. chamber_type_ff.ge.3) then
         call recmes(crname,'W','Scattering chamber not defined -> no geometry')
         return
c
ccc New scattering chamber (made of low density foam) geometry (s.morrow).
ccc From drawing no. 66840-E-03016.
      elseif ( chamber_type_ff.eq.2 ) then
         call gsvolu('CHAM','TUBE', clas_med_air, CHAM0, 3 , IVOL)
         call gspos('CHAM', 1,'UPST', 0., 0., 0., 0, 'MANY')
c
         call gsvolu('CHA1','PCON', clas_med_polistyrene, CHAM1, 27, IVOL)
         call gsvolu('CHA2','TUBE', clas_med_alu, CHAM2, 3 , IVOL)
         call gspos('CHA1', 1,'CHAM', 0., 0., 0.0, 0, 'ONLY')
         call gspos('CHA2', 1,'CHAM', 0., 0., 23.80, 0, 'ONLY')
c 
         call gsvolu('BEA2','TUBE',clas_med_alu,TUBE2,3,IVOL)
         call gsvolu('BEA3','TUBE',clas_med_alu,TUBE3,3,IVOL)
         call gsvolu('BEA4','TUBE',clas_med_alu,TUBE4,3,IVOL)
         call gspos('BEA2',1,'UPST',0.,0.,-123.26,0,'ONLY')
         call gspos('BEA3',1,'UPST',0.,0.,-264.6,0,'ONLY')
         call gspos('BEA4',1,'UPST',0.,0.,-37.50,0,'ONLY')
c 
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
c
ccc Old scattering chamber geometry (with RIBS).
ccc Defaults to this if the new chamber is not specifically chosen.    
      else
         call gsvolu('UPS1','TUBE', clas_med_air, TUBEM, 3, IVOL)
         call gspos('UPS1', 1, 'UPST', 0., 0., 0., 0, 'MANY')
         call gsvolu('UPS2','TUBE', clas_med_air, TUBEN, 3, IVOL)
         call gspos('UPS2', 1, 'UPS1', 0., 0., 0., 0, 'MANY')
c 
         call gsvolu('BEA1','TUBE',clas_med_alu,TUBE1,3,IVOL)
         call gsvolu('BEA2','TUBE',clas_med_alu,TUBE2,3,IVOL)
         call gsvolu('BEA3','TUBE',clas_med_alu,TUBE3,3,IVOL)
         call gspos('BEA1',1,'UPST',0.,0.,-39.63,0,'ONLY')
         call gspos('BEA2',1,'UPST',0.,0.,-123.26,0,'ONLY')
         call gspos('BEA3',1,'UPST',0.,0.,-264.6,0,'ONLY')

c 
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
      endif
c
ccc Downstream shielding which is the same for both scattering chambers.
c      call gsvolu('SHD1','CONE',clas_med_lead,shld1,5,IVOL)
c      call gspos('SHD1',1,'FOI2',0.,0.,52.216,0,'MANY')
      call gsvolu('SHD1','CONE',clas_med_lead,shld1,5,IVOL)
      call gspos('SHD1',1,'FOIL',0.,0.,52.216,0,'ONLY')
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
