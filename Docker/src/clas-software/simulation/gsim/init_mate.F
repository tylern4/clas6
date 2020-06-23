      subroutine init_mate

c_begin_doc

c  Documentation for subroutine init_mate

c  $Id: init_mate.F,v 1.15 2009/12/22 15:02:41 natasha Exp $

c  Purpose:
c  --------
c     ******************************************************************
c     *                                                                *
c     *      Material definition                                       *
c     *                                                                *
c     ******************************************************************

c  Major revisions:
c  ----------------
c     Elliott Wolin, College of William and Mary, 16-nov-94
c     Clarisse Tur, Univ. of SC, Aug 19, 2004

c_end_doc


      implicit none
      character*(*) cfile,crevis,cstate,cdate,cautho,crname,crauth
      character*132 crcsid
      parameter (cfile=  '$RCSfile: init_mate.F,v $')
      parameter (crevis= '$Revision: 1.15 $')
      parameter (cstate= '$State: Exp $')
      parameter (cdate=  '$Date: 2009/12/22 15:02:41 $')
      parameter (cautho= '$Author: natasha $')
      parameter (crname= 'init_mate')
      parameter (crauth= 'Elliott Wolin')
      data crcsid/'
     1$Id: init_mate.F,v 1.15 2009/12/22 15:02:41 natasha Exp $
     1 '/
      save


c  include files:
c  --------------
#include "gcbank.inc"
#include "gcnum.inc"
#include "clmate.inc"
c_end_inc

c  external functions
c  ------------------

      integer get_mate

c  local variables:
c  ----------------
      integer mxmyla,mxlgls,mxch2
      parameter (mxmyla = 3)
      parameter (mxlgls = 5)
      parameter (mxch2=2)

c  define mylar << junk: Mylar=C5H4O2
c
c      real amyla(mxmyla),zmyla(mxmyla),wmyla(mxmyla),densmy
c      data amyla,            zmyla,            wmyla,            densmy
c     1    /12.01, 1.01, 16.0,  6.0,  1.0,  8.0,  5.0,  4.0,  2.0, 1.39/

c  define lead glas
      real algls(mxlgls),zlgls(mxlgls),wlgls(mxlgls)
      data      algls /  16.0, 207.0,  28.0,  42.0,  14.0/
      data      zlgls /   8.0,  82.0,  14.0,  19.0,   7.0/
      data      wlgls /  1.36,  0.55,  0.38,  0.10,  0.02/
c
c
c  define CH2
      real ach2(mxch2),zch2(mxch2),wch2(mxch2),dench2
      data ach2,       zch2,    wch2,   dench2
     1     /12.01,1.01, 6.0,1.0, 1.0,2.0,1.35/ 
c
c  define polystyrene
      real apol(2),zpol(2),wpol(2),depol
      data apol,       zpol,    wpol,   depol
     1    /12.01,1.01, 6.0,1.0, 1.,1.,  1.032/
	
*
*  define stainless steel
      REAL A_steel(3), Z_steel(3), W_steel(3),dens_steel
      DATA A_steel / 56. , 52. , 59. /
      DATA Z_steel / 26. , 24. , 28. /
      DATA W_steel / 7.4 , 1.8 , 0.8 /
      DATA dens_steel / 7.75 /
*
*  material definition CEREX (fk: plastics but which one: take C5H8O2 ??)
      REAL A_cerex(3), Z_cerex(3), W_cerex(3), dens_cerex
      DATA A_cerex / 12.01, 1.01, 16.0 / 
      DATA Z_cerex /  6.0,  1.0,   8.0 /
      DATA W_cerex /  5.0,  8.0,   2.0 /
      DATA dens_cerex / 1.18 /
*
*  Polyethylene compound parameters
      REAL  A_pol(2),Z_pol(2), W_pol(2), dens_pol
      DATA  A_pol / 12.01, 1.01/
      DATA  Z_pol / 6.0,  1.0/
      DATA  W_pol / 1.,   2./
      DATA  dens_pol / 0.95 /
*
*  Mylar compound parameters
      REAL  A_mylar(3), Z_mylar(3), W_mylar(3), dens_mylar
      DATA  A_mylar /12.01, 1.01, 16.00/
      DATA  Z_mylar / 6.0,  1.0,   8.0/
      DATA  W_mylar / 5.,   4.,    2./
      DATA  dens_mylar / 1.387 /
*
*  Kapton compound parameters
      REAL  A_kapton(3), Z_kapton(3), W_kapton(3), dens_kapton
      DATA  A_kapton /12.01, 1.01, 16.00/
      DATA  Z_kapton / 6.0,  1.0,   8.0/
      DATA  W_kapton / 5.,   4.,    2./
      DATA  dens_kapton / 1.401 /
*
*  components of aluminized mylar (mylar,aluminum)
      REAL A_alumyl(2), Z_alumyl(2), W_alumyl(2), D_alumyl 
      DATA A_alumyl / 8.735, 26.98 /
      DATA Z_alumyl / 4.545, 13.0  /
      DATA W_alumyl / 0.88,  0.12 /
      DATA D_alumyl / 1.52 /
*
*  components in alumaseal (mylar,aluminum,glue)
      REAL A_alumaseal(3), Z_alumaseal(3), W_alumaseal(3), D_alumaseal 
      DATA A_alumaseal / 8.735, 26.98, 6.675 /
      DATA Z_alumaseal / 4.545, 13.0 , 3.60  /
      DATA W_alumaseal / 0.290, 0.565, 0.145 /
      DATA D_alumaseal / 1.968 /
*
*  materials in composite alumaseal/kapton/epoxy
*   (fk: components of epoxy ?? take plastics - cerex instead)
      REAL A_comp_alumaseal(3), Z_comp_alumaseal(3)
      REAL W_comp_alumaseal(3), D_comp_alumaseal 
      DATA A_comp_alumaseal /18.745, 8.735, 6.675 /
      DATA Z_comp_alumaseal / 9.185, 4.545, 3.60  /
      DATA W_comp_alumaseal / 0.332, 0.477, 0.191 /
      DATA D_comp_alumaseal / 1.548 /
*
*  relative weight of materials in heat shields (cerex,alu)
      REAL W_cerexalu1(2)
      DATA W_cerexalu1 / 0.435, 0.565 /         !27:35
      REAL W_cerexalu2(2)
      DATA W_cerexalu2 / 0.517, 0.483 /         !15:14
*
*  relative weight of materials in heat shields (cerex,aluminized mylar)
      REAL W_cerexmyl(3)
      DATA W_cerexmyl / 0.532, 0.412, 0.056 /         !25:22 (alum.mylar 9:1)
      REAL dens_alu
      DATA dens_alu / 2.70 /

      REAL         AA(3), ZZ(3), dens
      INTEGER      jma, i
      CHARACTER*20 chmate(5)
      DATA chmate /'LIQ HYDROGEN        ','LIQ DEUTERIUM       ',
     &             'LIQ HELIUM4         ','LIQ NITROGEN        ',
     &             'LIQ NEON            '/
*
*  material definition low density foam
      real alowdfoam(4), zlowdfoam(4), wlowdfoam(4), dens_lowdfoam
c            
c                  H     N     C     O
c                  -     -     -     -
      data alowdfoam / 1. , 14. , 12. , 16. /
      data zlowdfoam / 1. ,  7. ,  6. ,  8. /
      data wlowdfoam / 13. ,  1.,   7. ,  2. /
      data dens_lowdfoam / 0.105 /

*  materials in composite carbon fiber/epoxy
*   (fk: components of epoxy ?? take plastics - cerex instead)
      REAL A_carb_fiber(2), Z_carb_fiber(2)
      REAL W_carb_fiber(2), D_carb_fiber 
      DATA A_carb_fiber /12.010, 6.675 /
      DATA Z_carb_fiber / 6.00, 3.60  /
      DATA W_carb_fiber / 0.8, 0.2 /
      DATA D_carb_fiber / 1.5 /

*
*  packing fraction for superinsulation material (typically 3.5%)
*  note: as of 6/04 volumes of superinsulation have reduced thickness
      REAL pack_frac_insulation
      DATA pack_frac_insulation / 0.035 /

*
*  butanol (frozen spin target)
      REAL abutanol(3),zbutanol(3),wbutanol(3),dens_butanol, pack_frac_buta
*            C_4 H_9 O H =   C_4   H_10   O_1
      data      abutanol / 12.01,  1.01, 16.01 /
      data      zbutanol /  6.0,   1.0,   8.0  /
      data      wbutanol /  4.0,  10.0,   1.0  /
      data  dens_butanol / 0.985 /
      data  pack_frac_buta / 0.62  /      !packing fraction 0.63=SMC, 0.61=BONN
*
*  NH3 (frozen spin target)
      REAL A_nh3(2),Z_nh3(2),W_nh3(2),dens_nh3, pack_frac_nh3
*                        N     H_3
      data  A_nh3    / 14.0,  1.01 /
      data  Z_nh3    /  7.0,  1.0  /
      data  W_nh3    /  1.0,  3.0  /
      data  dens_nh3 / 0.867 /           !SMC
      data  pack_frac_nh3 / 0.68  /      !SMC

*  ND3 (frozen spin target)
      REAL A_nd3(2),Z_nd3(2),W_nd3(2),dens_nd3, pack_frac_nd3
*                        N     D_3
      data  A_nd3    / 14.0,  1.01 /
      data  Z_nd3    /  7.0,  1.0  /
      data  W_nd3    /  1.0,  6.0  /
      data  dens_nd3 / 1.007 /           !SMC
      data  pack_frac_nd3 / 0.68  /      !SMC

*
* titanium dioxide TiO2
*
      real atio2(2),ztio2(2),wtio2(2),dens_tio2
      data atio2 /47.867, 16.0/
      data ztio2 /22.0,   8.0/
      data wtio2 /1.0,    2.0/
      data dens_tio2 /4.23/
*

c_end_var


c  executable code:
c  ----------------

*  change material name for liquid material
      jma=LQ(jmate-1)
      if(jma.EQ.0) then
        call recmes(CRNAME,'W','MATERIAL bank not initialized!')
        call GMATE
      endif
      do i=1,3
         jma=LQ(jmate-i)
         call uctoh(chmate(i),IQ(jma+1),4,20)
      enddo
      do i=7,8
         jma=LQ(jmate-i)
         call uctoh(chmate(i-3),IQ(jma+1),4,20)
      enddo
*  set indices for predefined materials (GMATE)
      imate_liq_h2   = 1
      imate_liq_d2   = 2
      imate_liq_he4  = 3
      imate_liq_n2   = 7
      imate_liq_neon = 8
      imate_air      =15
      imate_vacuum   =16
      imate_lithium  = 4
      imate_beryllium= 5
      imate_carbon   = 6
      imate_aluminum = 9
      imate_iron     =10
      imate_copper   =11
      imate_tungsten =12
      imate_lead     =13
      imate_uranium  =14

c  clas titanium material
      imate_titanium = get_mate()
      call gsmate(imate_titanium, 'TITANIUM FOIL$',
     +             47.867, 22.0, 4.54, 3.56, 3.5, 0,0)

c  clas rigid grapite material
      imate_rigidgraphite = get_mate()
      call gsmate(imate_rigidgraphite, 'RIGID GRAPHITE$',
     +             12.011, 6.0, 1.8, 23.72, 23.7, 0,0)

c  clas flexible graphite material
      imate_flexgraphite = get_mate()
      call gsmate(imate_flexgraphite, 'FLEX GRAPHITE$',
     +             12.011, 6.0, 1.1, 38.81, 38.8, 0,0)

c  clas helium material
      imate_helium = get_mate()
      call gsmate(imate_helium, 'HELIUM GAS$',
     +             4.00, 2.0, 17.8E-5, 5.3E5, 3.66E5, 0,0)
c
      imate_helium6 = get_mate()
      call gsmate(imate_helium6, 'HELIUM GAS 6atm$',
     +             4.00, 2.0, 106.8E-5, 1.87E5, 3.66E5, 0,0)
c
c  clas liq.helium material
      imate_liq_he3 = get_mate()
      call gsmate(imate_liq_he3, 'LIQ HELIUM3$',
     +               3.00, 2.0, 0.07, 756.0, 613.0, 0,0)
c
c  clas liq.helium4 material
      call gsmate(imate_liq_he4, 'LIQ HELIUM4$',
     +               4.00, 2.0, 0.146, 755.0, 613.0, 0,0)
c
c
c clas_tio2
       imate_tio2=get_mate()
       call gsmixt(imate_tio2,'TITANIUM DIOXID$',
     +            atio2, ztio2, dens_tio2, -2, wtio2 ) 
c
c clas_ch2
       imate_ch2=get_mate()
       call gsmixt(imate_ch2,'SOLID CH2$',
     +            ach2, zch2, dench2, -mxch2, wch2 ) 
c
c  clas mylar
c      clas_mat_mylar = get_mate() 
c      call gsmixt(clas_mat_mylar,'CLAS Mylar$',
c     +             amyla, zmyla, densmy, -mxmyla, wmyla)
c
c  clas carbon
c      clas_mat_carbon = get_mate()  
c PDG pure graphite material parameters, in the middle of the industry range
c      call gsmate(clas_mat_carbon, 'CLAS Carbon$',
c     +             12.01, 6.0, 2.265, 18.8, 600.0, 0,0)
c
      imate_stainless_steel=get_mate()
      call gsmixt(imate_stainless_steel,'STAINLESS STEEL$',
     1            A_steel, Z_steel, dens_steel, -3, W_steel)

      imate_polystyrene=get_mate()
      call gsmixt(imate_polystyrene,'POLYSTYRENE$',
     1            apol, zpol, depol, -2, wpol)

      imate_polyethylene = get_mate()
      call gsmixt(imate_polyethylene,'POLYETHYLENE$',
     &            A_pol,Z_pol,dens_pol,-2,W_pol)

      imate_cerex = get_mate()
      call gsmixt(imate_cerex,'CEREX$',
     &            A_cerex,Z_cerex,dens_cerex,-3,W_cerex)

      imate_mylar = get_mate()
      call gsmixt(imate_mylar,'MYLAR$',
     &            A_mylar,Z_mylar,dens_mylar,-3,W_mylar)

      imate_kapton = get_mate()
      call gsmixt(imate_kapton,'KAPTON$',
     &            A_kapton,Z_kapton,dens_kapton,-3,W_kapton)

      imate_alumylar = get_mate()                
      call gsmixt(imate_alumylar,'ALUMIN. MYLAR$',
     &            A_alumyl,Z_alumyl,D_alumyl,2,W_alumyl)

      imate_alumaseal = get_mate()
      call gsmixt(imate_alumaseal,'ALUMASEAL$',
     &            A_alumaseal,Z_alumaseal,D_alumaseal,3,W_alumaseal)

      imate_comp_alumaseal = get_mate()
      call gsmixt(imate_comp_alumaseal,'ALUMASEAL/KAPTON$',
     &   A_comp_alumaseal,Z_comp_alumaseal,D_comp_alumaseal,3,W_comp_alumaseal)

      AA(1)=A_comp_alumaseal(3)
      ZZ(1)=Z_comp_alumaseal(3)
      AA(2)=A_alumaseal(2)
      ZZ(2)=Z_alumaseal(2)
      dens=dens_cerex*W_cerexalu1(1)+dens_alu*W_cerexalu1(2)
      imate_cerexalu1 = get_mate()
      call gsmixt(imate_cerexalu1,'CEREX/ALU 27/35$',
     &            AA,ZZ,dens,2,W_cerexalu1)

      dens=dens_cerex*W_cerexalu2(1)+dens_alu*W_cerexalu2(2)
      imate_cerexalu2 = get_mate()
      call gsmixt(imate_cerexalu2,'CEREX/ALU 15/14$',
     &            AA,ZZ,dens,2,W_cerexalu2)

      AA(2)=A_alumyl(1)
      ZZ(2)=Z_alumyl(1)
      AA(3)=A_alumyl(2)
      ZZ(3)=Z_alumyl(2)
      dens=dens_cerex*W_cerexmyl(1)+dens_mylar*W_cerexmyl(2)+
     &              dens_alu*W_cerexmyl(3)
      imate_cerexmyl1 = get_mate()
      call gsmixt(imate_cerexmyl1,'CEREX/ALUM.MYL 25/22',
     &            AA,ZZ,dens,3,W_cerexmyl)

      imate_lowdfoam = get_mate()
      call gsmixt(imate_lowdfoam,'Low Density Foam PMI',
     +             alowdfoam, zlowdfoam, dens_lowdfoam, -4, wlowdfoam)

      imate_carb_fiber = get_mate()
      call gsmixt(imate_carb_fiber,'CARBON FIBER&EPOXY$',
     &   A_carb_fiber,Z_carb_fiber,D_carb_fiber,2,W_carb_fiber)

      imate_butanol=get_mate()
      CALL GSMIXT(imate_butanol,'BUTANOL$',
     &      Abutanol,Zbutanol, dens_butanol*pack_frac_buta,-3,Wbutanol)

      imate_nh3=get_mate()
      CALL GSMIXT(imate_nh3,'NH3$', A_nh3,Z_nh3, dens_nh3*pack_frac_nh3,-2,W_nh3)

      imate_nd3=get_mate()
      CALL GSMIXT(imate_nd3,'ND3$', A_nd3,Z_nd3, dens_nd3*pack_frac_nd3,-2,W_nd3)

      end

c----------------------------------------------------------------------------
