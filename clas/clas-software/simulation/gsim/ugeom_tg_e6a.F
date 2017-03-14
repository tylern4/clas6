      subroutine ugeom_tg_e6a


#include "gsimpar.inc"
#include "gsimcontrol.inc"
#include "gcbank.inc"
#include "clmate.inc"
#include "cltmed.inc"
#include "tg_e2_tmed.inc" 
#include "ffpar_tg_e2.inc"
#include "gcnum.inc"
#include "gcunit.inc"  
#include "ffpar_foil.inc"
       
        integer get_rotm

        DIMENSION TBCLAS(3),TBWIND(3),TBALU(3),TBVAC(3),TINSUL(3),TCELL(3)
        DIMENSION CINSUL(5),CCELL(5),CLIQ(5)
        DIMENSION SCCYL(3),SCSPH(6),SCEXC(5),SCEXT(3), SCEXW(3)

        DATA TBCLAS/0.,12.,20./                     !mother volum (vacuum)
        DATA TBWIND/0.,0.2,0.00075/                 !inlet&exit wind.(aluminum)
        DATA TBALU/0.,0.25,0.00075/                 !front part of insulation (aluminum)
        DATA TBVAC/0.,0.2,1.0/                      !upstream of inlet wind.(vacuum)
        DATA CINSUL/4.5,0.7,0.704,0.45,0.454/       !insulation cover (cerex)
        DATA TINSUL/0.25,0.45,0.002/
        DATA CCELL/3.0915,0.6,0.605,0.35,0.355/     !target cell (kapton)
        DATA TCELL/0.2,0.35,0.0025/                 !around of exit window
        DATA CLIQ/3.0915,0.,0.6,0.,0.35/            !target liquid (d2 or h2)



      CALL GSVOLU('VOLM', 'TUBE', clas_med_vac    , TBCLAS  , 3, IV1)
      CALL GSVOLU('VVAC', 'TUBE', clas_med_vac    , TBVAC   , 3, IV2)
      CALL GSVOLU('VWIN', 'TUBE', clas_med_alu    , TBWIND  , 3, IV3)
      CALL GSVOLU('VALU', 'TUBE', clas_med_alu    , TBALU   , 3, IV4)
      CALL GSVOLU('VINS', 'CONE', clas_med_cerex  , CINSUL  , 5, IV5)
      CALL GSVOLU('VIN2', 'TUBE', clas_med_cerex  , TINSUL  , 3, IV6)
      CALL GSVOLU('VCEL', 'CONE', clas_med_kapton , CCELL   , 5, IV7)
      CALL GSVOLU('VCE2', 'TUBE', clas_med_kapton , TCELL   , 3, IV8)
      CALL GSVOLU('VLD2', 'CONE', clas_med_d2liq  , CLIQ    , 5, IV9)
      CALL GSVOLU('VLH2 ', 'CONE', clas_med_h2liq , CLIQ    , 5, IV10)
      CALL GSVOLU('VLNO ', 'CONE', clas_med_vac   , CLIQ    , 5, IV11)
      
      

      CALL GSPOS('VOLM',1,'GAS2',0.,0.,-5.06    , 0, 'MANY' )
      if(itg_type(2).eq.2)goto 111  !set only scat.chamber
      if(itg_type(1).eq.2)then                !liquid is D
      CALL GSPOS('VLD2',1,'VOLM',0.,0.,-0.5915  , 0, 'ONLY' )
      elseif(itg_type(1).eq.1)then            !liquid is H
      CALL GSPOS('VLH2',1,'VOLM',0.,0.,-0.5915  , 0, 'ONLY' )
      elseif(itg_type(1).eq.0)then            !empty cell
      CALL GSPOS('VLNO',1,'VOLM',0.,0.,-0.5915  , 0, 'ONLY' )
      endif
      CALL GSPOS('VCEL',1,'VOLM',0.,0.,-0.5915   ,0, 'ONLY' )
      CALL GSPOS('VINS',1,'VOLM',0.,0., 0.0     , 0, 'ONLY' )
      CALL GSPOS('VIN2',1,'VOLM',0.,0., 4.498   , 0, 'ONLY' )
      CALL GSPOS('VALU',1,'VOLM',0.,0., 4.49925 , 0, 'ONLY' )
      CALL GSPOS('VWIN',1,'VOLM',0.,0.,-2.49925 , 0, 'ONLY' )
      CALL GSPOS('VWIN',2,'VOLM',0.,0., 2.49925 , 0, 'ONLY' )
      CALL GSPOS('VVAC',1,'VOLM',0.,0.,-3.5  , 0, 'ONLY' )


c Foam scattering chamber geometry 
c From drawing no. 66850-E-03543
c


      if (itg_type(2).eq.1)then

 111     continue
      irt_sch=get_rotm()

      CALL GSROTM(irt_sch,90.,90.,0.,0.,90.,0.)

       DATA SCCYL/4.365, 5.0, 6.0/                  !the cylidrical part of scatter.chamber
       DATA SCSPH/4.365, 5.0, 0.0, 90., 0.0, 180.0/ !spherical part of sc.chamber
       DATA SCEXC/3.0,0.352,0.436,1.27,1.354/       !exit part of sc.chamber (cone)
       DATA SCEXT/1.27,1.354,4.8375/                !exit part of sc.chamber (tube)
       DATA SCEXW/0.0, 1.27, 0.00355/               !exit window of sc.chamber 


      CALL GSVOLU('VCYL', 'TUBE', clas_med_lowdfoam , SCCYL   , 3, IV12)
      CALL GSVOLU('VSPH', 'SPHE', clas_med_lowdfoam , SCSPH   , 6, IV13)
      CALL GSVOLU('VEXT', 'TUBE', clas_med_cfiber   , SCEXT  ,  3, IV14)
      CALL GSVOLU('VEXC', 'CONE', clas_med_cfiber   , SCEXC  ,  5, IV15)
      CALL GSVOLU('VEXW', 'TUBE', clas_med_alu      , SCEXW   , 3, IV16)

      CALL GSPOS('VCYL',1,'VOLM',0.,0.,-2.5    , 0, 'ONLY' )
      CALL GSPOS('VSPH',1,'VOLM',0.,0., 3.5    , irt_sch, 'MANY' )
      CALL GSPOS('VEXT',1,'VOLM',0.,0., 18.7025   , 0, 'ONLY' )
      CALL GSPOS('VEXC',1,'VOLM',0.,0., 10.865   , 0, 'ONLY' )
      CALL GSPOS('VEXW',1,'VOLM',0.,0., 23.54   , 0, 'ONLY' )

      endif

      CALL GSORD('VOLM',3)
      CALL GPVOLU(0)               ! PRINTOUT VOLUE PARAMETERS
C
C----------------------------------------------------------------
C
      CALL GGCLOS
C
      RETURN
      END




*************************************************************************
  
      subroutine vert_e6a(vx_poz)
c  include files:
c  --------------

#include "gsimpar.inc"
#include "gsimcontrol.inc"
#include "mcin.inc"
#include "gcflag.inc"
#include "ffpar.inc"
#include "tg_e2_mate.inc"
#include "tg_e2_tmed.inc"
#include "ffpar_tg_e2.inc"
#include "ffpar_tg.inc"
c_end_inc

      real vx_poz(3)
      real uran(3)
       call grndm(uran,3)

 
          if(itg_type(1).eq.1.or.itg_type(1).eq.2)then

          vx_poz(1)=0.0
          vx_poz(2)=0.0
          vx_poz(3)=-2.5+5.*uran(3)
             
          elseif(itg_type(1).eq.0.0)then

          vx_poz(1)=0.0
          vx_poz(2)=0.0
          vx_poz(3)=0.0

          endif
             return
             end



