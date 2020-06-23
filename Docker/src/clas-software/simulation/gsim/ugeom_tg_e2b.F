      subroutine ugeom_tg_e2b

#include "gsimpar.inc"
#include "gsimcontrol.inc"
#include "ffpar_tg_e2.inc"

      if (itg_type(1).eq.1)then
         call e2b_crio
     
      elseif(itg_type(1).eq.2)then
         call e2b_stg
     
         endif
      return
      end



       subroutine e2b_crio

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
       
        integer get_rotm,irt_sch

        DIMENSION TBCLAS(3),TBWIND(3),TBALU(3),TBVAC(3),TINSUL(3),TCELL(3)
        DIMENSION CINSUL(5),CCELL(5),CLIQ(5)
        DIMENSION SCCYL(3),SCSPH(6),SCEXIT(5), SCEXW(3)
        DIMENSION C12(3),FE56(3),CH2(3),PB(3)        

        DATA TBCLAS/0.,12.,20./                     !mother volum (vacuum)
        DATA TBWIND/0.,0.2,0.00075/                 !inlet&exit wind.(aluminum)
        DATA TBALU/0.,0.25,0.00075/                 !front part of insulation (aluminum)
        DATA TBVAC/0.,0.2,0.5915/                   !in front of inlet wind.(vacuum)
        DATA CINSUL/3.683,0.7,0.704,0.45,0.454/     !insulation cover (cerex)
        DATA TINSUL/0.25,0.45,0.002/
        DATA CCELL/3.0915,0.6,0.605,0.35,0.355/     !target cell (kapton)
        DATA TCELL/0.2,0.35,0.0025/                 !around of exit window
        DATA CLIQ/3.0915,0.,0.6,0.,0.35/            !target liquid (he3 or h2)
        DATA C12/0.65, 0.3, 0.05/
        DATA FE56/0.65, 0.3, 0.0075/
        DATA CH2/0.65, 0.3 ,0.035/
        DATA PB/0.65, 0.3, 0.044/



      CALL GSVOLU('VOLM', 'TUBE', clas_med_vac    , TBCLAS  , 3, IV1)
      CALL GSPOS('VOLM',1,'GAS2',0.,0.,-5.06    , 0, 'ONLY' )
      CALL GSVOLU('VVAC', 'TUBE', clas_med_vac    , TBVAC   , 3, IV2)
      CALL GSVOLU('VWIN', 'TUBE', clas_med_alu    , TBWIND  , 3, IV3)
      CALL GSVOLU('VALU', 'TUBE', clas_med_alu    , TBALU   , 3, IV4)
      CALL GSVOLU('VINS', 'CONE', clas_med_cerex  , CINSUL  , 5, IV5)
      CALL GSVOLU('VIN2', 'TUBE', clas_med_cerex  , TINSUL  , 3, IV6)
      CALL GSVOLU('VCEL', 'CONE', clas_med_kapton , CCELL   , 5, IV7)
      CALL GSVOLU('VCE2', 'TUBE', clas_med_kapton , TCELL   , 3, IV8)

      if(itg_type(2).eq.1.or.itg_type(2).eq.3)then
      CALL GSVOLU('VHE3', 'CONE', clas_med_liq_he3, CLIQ    , 5, IV9)
      CALL GSVOLU('VH2 ', 'CONE', clas_med_h2liq  , CLIQ    , 5, IV10)

      elseif(itg_type(2).eq.2)then
      CALL GSVOLU('VHE3', 'CONE', clas_med_vac, CLIQ    , 5, IV9)        !empty crio. target
      CALL GSVOLU('VH2 ', 'CONE', clas_med_vac  , CLIQ    , 5, IV10)     !
      CALL GSVOLU('VCH2','BOX ',clas_med_ch2    , CH2   ,3, IVCH2)       !solid tg. 2CH 
      CALL GSPOS('VCH2', 1, 'VOLM', 0., 0.,  1.035  ,  0, 'ONLY')

      elseif(itg_type(2).eq.12)then
      CALL GSVOLU('VHE3', 'CONE', clas_med_vac, CLIQ    , 5, IV9)        !empty crio. target
      CALL GSVOLU('VH2 ', 'CONE', clas_med_vac  , CLIQ    , 5, IV10) 
      CALL GSVOLU('VCAR','BOX ',clas_med_carbon , C12   ,3, IVCAR)       !solid tg.  12C 
      CALL GSPOS('VCAR', 1, 'VOLM', 0., 0., 1.05   ,  0, 'ONLY')

      elseif(itg_type(2).eq.56)then
      CALL GSVOLU('VHE3', 'CONE', clas_med_vac, CLIQ    , 5, IV9)        !empty crio. target
      CALL GSVOLU('VH2 ', 'CONE', clas_med_vac  , CLIQ    , 5, IV10) 
      CALL GSVOLU('VF56','BOX ',clas_med_iron   , FE56  ,3, IVF56)       !solid target 56Fe
      CALL GSPOS('VF56', 1, 'VOLM', 0., 0., 1.0075  ,  0, 'ONLY')

      elseif(itg_type(2).eq.207)then
      CALL GSVOLU('VHE3', 'CONE', clas_med_vac, CLIQ    , 5, IV9)        !empty crio. target
      CALL GSVOLU('VH2 ', 'CONE', clas_med_vac  , CLIQ    , 5, IV10) 
      CALL GSVOLU('VLED','BOX ',clas_med_lead    ,PB    ,3, IVLED )      !solid tg. 207Pb
      CALL GSPOS('VLED', 1, 'VOLM', 0., 0.,  1.044  ,  0, 'ONLY')
    
      endif


      if(itg_type(2).eq.3)then
      CALL GSPOS('VHE3',1,'VOLM',0.,0.,-4.5915  , 0, 'ONLY' )

      elseif(itg_type(2).eq.1)then
      CALL GSPOS('VH2 ',1,'VOLM',0.,0.,-4.5915  , 0, 'ONLY' )

      endif

      CALL GSPOS('VCEL',1,'VOLM',0.,0.,-4.5915   ,0, 'ONLY' )
      CALL GSPOS('VINS',1,'VOLM',0.,0.,-4.0     , 0, 'ONLY' )
      CALL GSPOS('VIN2',1,'VOLM',0.,0.,-0.319   , 0, 'ONLY' )
      CALL GSPOS('VALU',1,'VOLM',0.,0.,-0.31775 , 0, 'ONLY' )
      CALL GSPOS('VWIN',1,'VOLM',0.,0.,-1.49925 , 0, 'ONLY' )
      CALL GSPOS('VWIN',2,'VOLM',0.,0.,-6.50075 , 0, 'ONLY' )
      CALL GSPOS('VVAC',1,'VOLM',0.,0.,-7.0915  , 0, 'ONLY' )


c Foam scattering chamber geometry 
c From drawing no. 66850-E-03565
c


      if (itg_type(3).eq.1)then

      irt_sch=get_rotm()

      CALL GSROTM(irt_sch,90.,90.,0.,0.,90.,0.)

       DATA SCCYL/9.0, 10.016, 9.0/                 !the cylidrical part of scatter.chamber
       DATA SCSPH/9.0, 9.953, 0.0, 90., 0.0, 180.0/ !spherical part of sc.chamber
       DATA SCEXIT/7.25,0.673,0.757,1.27,1.354/     !exit part of sc.chamber (cone)
       DATA SCEXW/0.0, 1.27, 0.00355/               !exit window of sc.chamber 


      CALL GSVOLU('VCYL', 'TUBE', clas_med_lowdfoam , SCCYL   , 3, IV11)
      CALL GSVOLU('VSPH', 'SPHE', clas_med_lowdfoam , SCSPH   , 6, IV12)
      CALL GSVOLU('VEXI', 'CONE', clas_med_cfiber   , SCEXIT  , 5, IV13)
      CALL GSVOLU('VEXW', 'TUBE', clas_med_alu      , SCEXW   , 3, IV14)

      CALL GSPOS('VCYL',1,'VOLM',0.,0.,-9.0    , 0, 'ONLY' )
      CALL GSPOS('VSPH',1,'VOLM',0.,0., 0.00    , irt_sch, 'MANY' )
      CALL GSPOS('VEXI',1,'VOLM',0.,0., 16.25   , 0, 'ONLY' )
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


*==================================================================      

      SUBROUTINE e2b_stg
c
      DIMENSION TBCLAS(3)
      DIMENSION C12(3),FE56(3),CH2(3),PB(3)

#include "gcbank.inc"
#include "gsimpar.inc"
#include "gsimcontrol.inc"
#include "clmate.inc"
#include "cltmed.inc"
#include "tg_e2_mate.inc"
#include "tg_e2_tmed.inc"
#include "ffpar_tg_e2.inc"

      DATA TBCLAS/0.0, 12.0, 20.0/     
      DATA C12/0.65, 0.3, 0.05/
      DATA FE56/0.65, 0.3, 0.0075/
      DATA CH2/0.65, 0.3 ,0.035/
      DATA PB/0.65, 0.3, 0.044/
c
      CALL GSVOLU('VOLM','TUBE',clas_med_vac    , TBCLAS,3, IVOLM)       !Mother volume
      CALL GSVOLU('VF56','BOX ',clas_med_iron   , FE56  ,3, IVF56)       !solid target 56Fe
      CALL GSVOLU('VCAR','BOX ',clas_med_carbon , C12   ,3, IVCAR)       !solid tg.  12C
      CALL GSVOLU('VCH2','BOX ',clas_med_ch2    , CH2   ,3, IVCH2)       !solid tg. 2CH
      CALL GSVOLU('VLED','BOX ',clas_med_lead    ,PB    ,3, IVLED )      !solid tg. 207Pb


      CALL GSPOS('VOLM', 1, 'GAS2', 0., 0., -5.06 ,  0, 'ONLY')
      if(itg_type(2).eq.56)then
      CALL GSPOS('VF56', 1, 'VOLM', 0., 0.,  1.0075  ,  0, 'ONLY')

      elseif(itg_type(2).eq.12)then
      CALL GSPOS('VCAR', 1, 'VOLM', 0., 0.,  1.05  ,  0, 'ONLY')

      elseif(itg_type(2).eq.2)then
      CALL GSPOS('VCH2', 1, 'VOLM', 0., 0.,  1.035  ,  0, 'ONLY')

      elseif(itg_type(2).eq.207)then
      CALL GSPOS('VLED', 1, 'VOLM', 0., 0.,  1.044  ,  0, 'ONLY')

      endif
C----------------------------------------------------------------
      CALL GSORD('VOLM',3)
      CALL GPVOLU(0)               ! PRINTOUT VOLUE PARAMETERS
C----------------------------------------------------------------
      CALL GGCLOS
C
      RETURN
      END
C      
*************************************************************************


*************************************************************************
  
      subroutine vert_e2b(vx_poz)
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

          vx_poz(1)=0.0
          vx_poz(2)=0.0
          vx_poz(3)=0.0
         
      if(itg_type(1).eq.1.and.
     &  (itg_type(2).eq.1.or.itg_type(2).eq.3))then
             vx_poz(1)=0.0
             vx_poz(2)=0.0
             vx_poz(3)=-6.5+5.*uran(3)

      elseif((itg_type(1).eq.2.or.itg_type(1).eq.1)
     &        .and.itg_type(2).eq.12)then
             vx_poz(1)=0.0
             vx_poz(2)=0.0
             vx_poz(3)=1.0+0.1*uran(3)
      elseif((itg_type(1).eq.2.or.itg_type(1).eq.1)
     &        .and.itg_type(2).eq.56)then
             vx_poz(1)=0.0
             vx_poz(2)=0.0
             vx_poz(3)=1.0+0.015*uran(3)
      elseif((itg_type(1).eq.2.or.itg_type(1).eq.1)
     &       .and.itg_type(2).eq.2)then
             vx_poz(1)=0.0
             vx_poz(2)=0.0
             vx_poz(3)=1.0+0.07*uran(3)
      elseif((itg_type(1).eq.2.or.itg_type(1).eq.1)
     &       .and.itg_type(2).eq.207)then
             vx_poz(1)=0.0
             vx_poz(2)=0.0
             vx_poz(3)=1.0+0.088*uran(3)

             endif
             return
             end
