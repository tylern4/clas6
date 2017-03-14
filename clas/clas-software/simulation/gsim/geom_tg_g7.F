      subroutine geom_tg_g7
  
c_begin_doc
c
c  Documentation for subroutine geom_tg_g7
c  
c  Purpose:
c  --------
c     ******************************************************************
c     *                                                                *
c     *       g7 target implementation                                 *
c     *       Author: Clarisse Tur                                     *
c     *       Date: 07/25/04                                           *
c     *                                                                *
c     ******************************************************************
c
c  This subroutine is called in geom_foil.F after the definitions of
c  the g7 upstream beam pipe and flanges and of the g7 scattering chamber. 
c  To use those things have the following lines in your ffread:
c  TGG7
c  CHAMBER 5
c
c  Taken from drawing 66840-E-03662
c
c_end_doc

      implicit none
      character*(*) cfile,crevis,cstate,cdate,cautho,crname,crauth
      character*132 crcsid,crmess
      parameter (cfile=  '$RCSfile: geom_tg_g7.F,v $')
      parameter (crevis= '$Revision: 1.4 $')
      parameter (cstate= '$State: Exp $')
      parameter (cdate=  '$Date: 2004/08/23 19:30:18 $')
      parameter (cautho= '$Author: clarisse $')
      parameter (crname= 'GEOM_TG_G7')
      parameter (crauth= 'clarisse')
      data crcsid/'
     1$Id: geom_tg_g7.F,v 1.4 2004/08/23 19:30:18 clarisse Exp $
     1 '/
      save

c  include files:
c  --------------
#include "gcbank.inc"
#include "gcnum.inc"
#include "gsimpar.inc"
#include "gsimcontrol.inc"
#include "ffpar_foil.inc"
#include "ffpar_tg.inc"
#include "ffpar_mg.inc"
#include "clmate.inc"
#include "cltmed.inc"
#include "ffpar_tg_g7.inc"
c_end_inc


*
*  external functions
*  ------------------

      integer  get_rotm

*  local variables:
*  ----------------
      real  piby3

      integer irot1,irot2,irot3,irot4,irot5     !rotation matrix #

      integer ivol                              !volume#

      real  tmaxfd, stmax, deemax, epsil, stmin   !track.media cuts

      real tgp1(21),tgp2(21),tgp3(12)           !arrays for volume definitions
      real tgp4(27),tgp5(18),tge1(12)           !
      real tgw1(3),tge2(18),tgw2(3)             !
      real tgs1(12),tgs2(15),tgs3(27)           !
      real tgs4(9),tgs5(27),tgs6(3)             !
      real tgin(36),tgh1(27),tgy1(9)            !
      real tgf1(9),tgh2(21),tgf2(9)             !
      real tgf3(9),tgf4(9),tgh3(15)             !
      real tgs7(9),tgff(9)                      !

      real xypos,xpos,ypos                      !distribution pipe positions


c_end_var

c executable code:
c ----------------


      piby3=acos(-1.)*1./3.


*
***************************
*  define tracking media
***************************
*
*  these parameters are only(!) valid if  Data Card 'AUTO' = 0  !
      TMAXFD =  0.1       !max. angular deviation in one step [degree]
      STMAX  =  1.0       !max. step  [cm]
      DEEMAX =  0.01      !max. energy loss per step  [0;1]
      EPSIL  =  0.001     !boundary crossing precision
      STMIN  =  0.001     !min. step  [cm]


*
*****************************
* Geometry definitions
*****************************
*
*  (a) mother volume (vacuum)
*   ------------------------
*  the target and support structure for g7 are positioned in'CHAM'(itself 
*  in 'UPST')
*
 
      data tgp1 /0.0,360.0,6.0,0.0,0.0,5.8,9.55,0.0,5.8,12.0,
     &           0.0,3.35,12.8,0.0,3.35,13.85,0.0,2.2,45.65,0.0,2.2/

      call gsvolu('TG  ','PCON',clas_med_vac,tgp1,21,ivol)
    
      call gspos('TG  ',1,'CHAM',0.,0.,-40.3,0,'ONLY')

*
*  (b) target cell (defines liq. d2 volume):
*   ---------------------------------------
*
      data tgp2 /0.0,360.0,6.0,0.0,0.9,1.5,7.15,0.9,1.45,7.25,0.8,
     &           1.45,7.25,0.0,1.45,12.5,0.0,1.4,12.9,0.0,0.65/

      call  gsvolu('TGC1','PCON',clas_med_d2liq,tgp2,21,ivol)

      call gspos('TGC1',1,'TG  ',0.,0.,12.95,0,'MANY')


      
      data tgp3 /0.0,360.0,3.0,0.0,0.0,0.25,0.15,0.0,0.787,0.5,0.0,0.787/
 
      call  gsvolu('TGC2','PCON',clas_med_d2liq,tgp3,12,ivol)

      call gspos('TGC2',1,'TG  ',0.,0.,19.7,0,'MANY')
 
*
*  (c) target cell walls:
*   --------------------
*

* inner base tube end cap and window (left)

      data tge1 /0.0,360.0,3.0,0.0,0.25,0.263,0.15,0.787,0.8,
     &           0.5,0.787,0.8/

      call  gsvolu('TGE1','PCON',clas_med_kapton,tge1,12,ivol)

      call gspos('TGE1',1,'TG  ',0.,0.,19.7,0,'MANY')


      data tgw1 /0.0,0.25,0.00075/

      call  gsvolu('TGW1','TUBE',clas_med_alu,tgw1,3,ivol)

      call gspos('TGW1',1,'TG  ',0.,0.,19.7,0,'MANY')
    

* outer base cell end cap and window (right)

      data tge2 /0.0,360.0,5.0,0.0,1.4,1.413,0.6,1.4,1.413,1.0,0.637,
     &           0.65,1.0,0.25,0.65,1.013,0.25,0.65/
    
      call  gsvolu('TGE2','PCON',clas_med_kapton,tge2,18,ivol)

      call gspos('TGE2',1,'TG  ',0.,0.,24.85,0,'MANY')

     
      data tgw2 /0.0,0.25,0.00075/

      call  gsvolu('TGW2','TUBE',clas_med_alu,tgw2,3,ivol)

      call gspos('TGW2',1,'TG  ',0.,0.,25.851,0,'MANY')


*
*  (d) support structure (= outer base ring + inner base tube + gas 
*   -------------------     distribution tubes):
*

* outer base ring (kapton):

      data tgp4 /0.0,360.0,8.0,0.0,1.0,1.5,0.3,1.0,1.8,0.3,0.9,
     &           1.8,1.0,0.9,2.5,1.0,1.5,2.5,1.15,1.5,2.65,1.45,
     &           1.5,2.65,2.5,1.5,1.5/

      call  gsvolu('TGFA','PCON',clas_med_kapton,tgp4,27,ivol)
      
      call gspos('TGFA',1,'TG  ',0.,0.,11.0,0,'MANY')


* inner base tube(kapton)

      data tgp5 /0.0,360.0,5.0,0.0,0.8,1.0,0.3,0.8,1.0,0.3,0.8,
     &           0.9,9.1,0.8,0.9,9.2,0.8,0.8/

      call  gsvolu('TGTU','PCON',clas_med_kapton,tgp5,18,ivol)

      call gspos('TGTU',1,'TG  ',0.,0.,11.0,0,'MANY')

* gas distribution tubes + their superinsulation:

*   1-  virtual volumes to contain others:
  
      xypos=4.6

      data tgs1 /0.0,360.0,3.0,0.0,0.0,0.85,6.65,0.0,0.85,7.3,0.0,0.85/
     
      call  gsvolu('TGS1','PCON',clas_med_vac,tgs1,12,ivol)

      call gspos('TGS1',1,'TG  ',0.,xypos,2.15,0,'MANY') 

      

      irot1=get_rotm()
      call gsrotm(irot1,90.,0.,45.,90.,-45.,-90.)

      data tgs2 /0.0,360.0,4.0,0.0,0.0,0.85,3.45,0.0,0.8,3.45,
     &           0.0,0.5,4.9,0.0,0.5/
 
      call  gsvolu('TGS2','PCON',clas_med_vac,tgs2,15,ivol)

     

*   2-  distribution pipes:

      data tgs3 /0.0,360.0,8.0,0.0,0.4,0.6,1.35,0.4,0.6,1.35,
     &           0.4,0.7,2.55,0.4,0.7,2.55,0.4,0.6,4.1,0.4,0.6,
     &           4.1,0.4,0.5,7.3,0.4,0.5/
      
      call  gsvolu('TGS3','PCON',clas_med_alu,tgs3,27,ivol) 

      call gspos('TGS3',1,'TGS1',0.,0.,0.,0,'MANY')

       
      data tgs4 /0.0,360.0,2.0,0.0,0.4,0.5,4.8,0.4,0.5/

      call  gsvolu('TGS4','PCON',clas_med_alu,tgs4,9,ivol) 

      call gspos('TGS4',1,'TGS2',0.,0.,0.1,0,'MANY')


*   3-  superinsulation of the pipes (cerex + aluminized mylar):

      data tgs5 /0.0,360.0,8.0,0.0,0.6,0.8,1.35,0.6,0.8,1.35,
     &           0.7,0.8,2.55,0.7,0.8,2.55,0.6,0.8,4.1,0.6,0.8,
     &           4.1,0.5,0.8,7.3,0.5,0.8/

      call  gsvolu('TGS5','PCON',clas_med_cerexmyl1,tgs5,27,ivol) 

      call gspos('TGS5',1,'TGS1',0.,0.,0.,0,'MANY')


      data tgs6 /0.5,0.8,1.725/

      call  gsvolu('TGS6','TUBE',clas_med_cerexmyl1,tgs6,3,ivol) 

      call gspos('TGS6',1,'TGS2',0.,0.,1.725,0,'MANY')

* now place TGS2:
      
      call gspos('TGS2',1,'TG  ',0.,xypos,8.9,irot1,'MANY')


*   4- two more copies of TGS1 and TGS2 rotated by 120 degrees:

      irot2=get_rotm()
      call gsrotm(irot2,-90.,-120.,-90.,-30.,0.,0.)
      irot3=get_rotm()
      call gsrotm(irot3,90.,120.,90.,210.,0.,0.)
      xpos=xypos*sin(piby3)
      ypos=xypos*cos(piby3)

      call gspos('TGS1',2,'TG  ',xpos,-ypos,2.15,0,'MANY')
      call gspos('TGS1',3,'TG  ',-xpos,-ypos,2.15,0,'MANY')

      irot4=get_rotm()
      call gsrotm(irot4,-90.,-120.,-135.,-30.,-45.,-30.)
      call gspos('TGS2',2,'TG  ',xpos,-ypos,8.9,irot4,'MANY')
      irot5=get_rotm()
      call gsrotm(irot5,90.,120.,45.,210.,45.,45.)
      call gspos('TGS2',3,'TG  ',-xpos,-ypos,8.9,irot5,'MANY')

*
* support ring for the distribution tubes
*

      data tgs7 /0.0,360.0,2.0,0.0,3.9,5.3,0.3,3.9,5.3/

      call  gsvolu('TGS7','PCON',clas_med_alu,tgs7,9,ivol) 

      call gspos('TGS7',1,'TG  ',0.,0.,3.95,0,'MANY')

*
*  (e) superinsulation of the Liq D2 cell (cerex + aluminized mylar):
*   ---------------------------------------------------------------
*

      data tgin /0.0,360.0,11.0,0.0,1.0,1.9,0.3,1.0,2.08,
     &           0.3,1.5,2.08,1.15,2.5,2.95,1.25,2.4,2.95,
     &           1.45,2.65,2.95,1.75,2.65,2.95,1.9,2.5,2.95,
     &           2.8,1.5,2.0,3.0,1.5,1.8,14.2,1.4,1.7/

      call  gsvolu('TGIN','PCON',clas_med_cerexmyl1,tgin,36,ivol)

      call gspos('TGIN',1,'TG  ',0.,0.,10.7,0,'MANY')

*
* (f) forward solid foils and their support structure (foam):
*  --------------------------------------------------------
*

* --------- 1st carbon + support

      data tgh1 /0.0,360.0,8.0,0.0,1.4,1.7,0.6,1.4,1.7,0.85,
     &           1.0,1.7,1.0,0.8,1.5,1.1,0.8,1.1,3.4,0.8,1.1,
     &           3.4,0.45,1.1,3.5,0.45,1.1/

      call  gsvolu('TGH1','PCON',clas_med_lowdfoam,tgh1,27,ivol)

      call gspos('TGH1',1,'TG  ',0.,0.,24.9,0,'MANY')

    
      data tgy1 /100.0,340.0,2.0,0.0,0.6,1.1,0.15,0.6,1.1/

      call  gsvolu('TGY1','PCON',clas_med_lowdfoam,tgy1,9,ivol)

      call gspos('TGY1',1,'TG  ',0.,0.,28.4,0,'MANY')

      
      data tgf1 /0.0,360.0,2.0,0.0,0.0,0.6,0.1,0.0,0.6/

      call  gsvolu('TGF1','PCON',clas_med_rigidgraphite,tgf1,9,ivol)

      call gspos('TGF1',1,'TG  ',0.,0.,28.4,0,'MANY')

      data tgff /0.0,360.0,2.0,0.0,0.0,0.6,0.05,0.0,0.6/

      call  gsvolu('TGFF','PCON',clas_med_flexgraphite,tgff,9,ivol)

      call gspos('TGFF',1,'TG  ',0.,0.,28.5,0,'MANY')


* --------- iron + support
      
      data tgh2 /0.0,360.0,6.0,0.0,0.45,1.1,0.1,0.45,1.1,0.1,
     &           0.8,1.1,2.2,0.8,1.1,2.2,0.45,1.1,2.3,0.45,1.1/

      call  gsvolu('TGH2','PCON',clas_med_lowdfoam,tgh2,21,ivol)

      call gspos('TGH2',1,'TG  ',0.,0.,28.55,0,'MANY')

      
      call gspos('TGY1',2,'TG  ',0.,0.,30.85,0,'MANY')

   
      data tgf2 /0.0,360.0,2.0,0.0,0.0,0.6,0.06,0.0,0.6/

      call  gsvolu('TGF2','PCON',clas_med_iron,tgf2,9,ivol)

      call gspos('TGF2',1,'TG  ',0.,0.,30.85,0,'MANY')

* --------- 2nd carbon + support  

      call gspos('TGH2',2,'TG  ',0.,0.,31.0,0,'MANY') 


      call gspos('TGY1',3,'TG  ',0.,0.,33.3,0,'MANY')


      call gspos('TGF1',2,'TG  ',0.,0.,33.3,0,'MANY')

      call gspos('TGFF',2,'TG  ',0.,0.,33.4,0,'MANY')


* --------- lead + support  

      call gspos('TGH2',3,'TG  ',0.,0.,33.45,0,'MANY') 


      call gspos('TGY1',4,'TG  ',0.,0.,35.75,0,'MANY')

     
      data tgf3 /0.0,360.0,2.0,0.0,0.0,0.6,0.1,0.0,0.6/

      call  gsvolu('TGF3','PCON',clas_med_lead,tgf3,9,ivol)

      call gspos('TGF3',1,'TG  ',0.,0.,35.75,0,'MANY')


* --------- 3rd carbon + support  

      call gspos('TGH2',4,'TG  ',0.,0.,35.90,0,'MANY') 


      call gspos('TGY1',5,'TG  ',0.,0.,38.20,0,'MANY')

     
      call gspos('TGF1',3,'TG  ',0.,0.,38.20,0,'MANY')

      call gspos('TGFF',3,'TG  ',0.,0.,38.30,0,'MANY')

* --------- titanium + support  

      call gspos('TGH2',5,'TG  ',0.,0.,38.35,0,'MANY') 


      call gspos('TGY1',6,'TG  ',0.,0.,40.65,0,'MANY')


      data tgf4 /0.0,360.0,2.0,0.0,0.0,0.6,0.1,0.0,0.6/

      call  gsvolu('TGF4','PCON',clas_med_titanium,tgf4,9,ivol)

      call gspos('TGF4',1,'TG  ',0.,0.,40.65,0,'MANY')

* --------- 4th carbon + support  

      call gspos('TGH2',6,'TG  ',0.,0.,40.80,0,'MANY') 


      call gspos('TGY1',7,'TG  ',0.,0.,43.10,0,'MANY')

     
      call gspos('TGF1',4,'TG  ',0.,0.,43.10,0,'MANY')
 
      call gspos('TGFF',4,'TG  ',0.,0.,43.20,0,'MANY')

* --------- last part of support structure

      data tgh3 /0.0,360.0,4.0,0.0,0.45,1.1,0.1,0.45,1.1,0.1,
     &           0.8,1.1,1.15,0.8,1.1/
      
      call  gsvolu('TGH3','PCON',clas_med_lowdfoam,tgh3,15,ivol)

      call gspos('TGH3',1,'TG  ',0.,0.,43.25,0,'MANY')
       
      
      end
 

c----------------------------------------------------------------

      subroutine ffky_tg_g7

*
* creates the TGG7 card for ffread to use the g7 target 
*

* include file:
* ------------

#include "ffpar_tg_g7.inc"

* set up the g7 target for use (not set by default):
* --------------------------------------------------
      
      tg_g7=.false.
      call ffkey('TGG7',tg_g7,1,'LOGI')
      

      return
      end

c-----------------------------------------------------------------
