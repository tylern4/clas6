      subroutine ugeom_tg_e5

#include "gsimpar.inc"
#include "gsimcontrol.inc"
#include "gcbank.inc"
#include "clmate.inc"
#include "cltmed.inc"
#include "ffpar_tg_e2.inc"
   
        integer get_rotm,irt_sch


        DIMENSION TBCLAS(3)
        DIMENSION TBCELL(3),TBINS(3),TBH2(3),TBD2(3)
        DIMENSION TGCON1(5),TGC1INS(5),TGC1H2(5),TGC1D2(5)
        DIMENSION TGCON2(5),TGC2INS(5),TGC2H2(5),TGC2D2(5)
        DIMENSION TBWIND(3)
        DIMENSION TB1(3),TB2(3),TB3(3),H2TB(3)
        DIMENSION TB1CON(5),TB2CON(5),TB3CON(5),H2CON(5)
        DIMENSION TBVAC1(3),TBVAC2(3),TBVAC3(3),H2VAC(3)
        DIMENSION BCON1(5), BCON2(5), BD2(5)
        DIMENSION BCON3(5), BCON4(5), BH2(5)
        DIMENSION BTUB(3)

        DIMENSION SCCYL(3),SCSPH(6),SCEXIT(5), SCEXW(3)
        DIMENSION CHEAT(5),THEAT(3),THEAL(3)
        

        DATA TBCLAS/0.,12.,20./                      !mother volum
        DATA TBCELL/0.486,0.491,2.356/               !target cell
        DATA TBH2/0.,0.486,2.356/                    !liquid H2
        DATA TBD2/0.,0.486,2.356/                    !liquid D2
        DATA TBINS/0.491,0.496,2.356/                !insulation
        DATA TGCON1/0.072,0.486,0.491,0.38,0.385/    !tg.cell top part
        DATA TGC1H2/0.072,0.,0.486,0.,0.38/          !H2
        DATA TGC1D2/0.072,0.,0.486,0.,0.38/          !D2
        DATA TGC1INS/0.072,0.491,0.496,0.385,0.39/   !insulation
        DATA TGCON2/0.072,0.38,0.385,0.168,0.175/    !tg.cell top part
        DATA TGC2H2/0.072,0.,0.38,0.,0.168/          !H2
        DATA TGC2D2/0.072,0.,0.38,0.,0.168/          !D2
        DATA TBWIND/0.,0.168,0.000635/               !tg. window(Al)
        DATA TGC2INS/0.072,0.385,0.39,0.175,0.18/    !insulation
        DATA TB1/0.598,0.6234,2.5/                   !inside tube 1
        DATA TB1CON/0.14,0.598,0.6234,0.3429,0.3683/
        DATA TB2/0.8265,0.8315,2.533/                !outside tube 2
        DATA H2TB/0.6234,0.8265,2.5/
        DATA H2CON/0.14,0.6234,0.8265,0.3683,0.6129/
        DATA TB3/0.8315,0.8365,2.533/                !insulation-Al_mylar
        DATA TB2CON/0.1068,0.8265,0.8315,0.6129,0.6179/
        DATA TB3CON/0.1068,0.8315,0.8365,0.6179,0.6229/ !insulation
        DATA TBVAC1/0.3429,0.3683,2.155/             !inside vacuum tube
        DATA TBVAC2/0.6129,0.6179,2.155/             !outside vacuum tube
        DATA H2VAC/0.3683,0.6129,2.155/
        DATA TBVAC3/0.6179,0.6229,2.155/

        DATA BCON1/1.4518,3.77,3.775,0.8265,0.8315/
        DATA BCON2/1.345,2.775,2.8,0.598,0.6234/
        DATA BCON3/1.345,2.5225,2.5275,0.486,0.491/
        DATA BCON4/1.0275,1.655,1.66,0.168,0.173/
        DATA BTUB/0.168,0.173,0.3175/
        DATA BD2/1.0275,1.66,2.5225,0.173,0.486/
        DATA BH2/1.345,2.8,3.77,0.6234,0.8265/
      
      CALL GSVOLU('VOLM', 'TUBE', clas_med_vac, TBCLAS  , 3, IV1)
      CALL GSVOLU('WNE5', 'TUBE', clas_med_alu, TBWIND  , 3, IV2)
  
      CALL GSVOLU('VCEL', 'TUBE', clas_med_alu, TBCELL  , 3, IV3)  !
      CALL GSVOLU('VCN1', 'CONE', clas_med_alu, TGCON1  , 5, IV4)  !tg. cell
      CALL GSVOLU('VCN2', 'CONE', clas_med_alu, TGCON2  , 5, IV5)  !

      CALL GSVOLU('VH2L', 'TUBE',clas_med_h2liq, TBH2   , 3, IV6)  !
      CALL GSVOLU('VC1H', 'CONE',clas_med_h2liq,TGC1H2  , 5, IV7)  ! H2
      CALL GSVOLU('VC2H', 'CONE',clas_med_h2liq,TGC2H2  , 5, IV8)  !

      CALL GSVOLU('VD2L', 'TUBE',clas_med_d2liq, TBD2   , 3, IV9)  !
      CALL GSVOLU('VC1D', 'CONE',clas_med_d2liq,TGC1D2  , 5, IV10) ! D2
      CALL GSVOLU('VC2D', 'CONE',clas_med_d2liq,TGC2D2  , 5, IV11) !

      CALL GSVOLU('VINS', 'TUBE',clas_med_alumylar , TBINS , 3, IV12)  
      CALL GSVOLU('VCI1', 'CONE',clas_med_alumylar , TGC1INS , 5, IV13)
      CALL GSVOLU('VCI2', 'CONE',clas_med_alumylar , TGC2INS , 5, IV14)

      CALL GSVOLU('VTB1', 'TUBE',clas_med_alu, TB1        , 3, IV15)  !inside tube
      CALL GSVOLU('VT1C', 'CONE',clas_med_alu, TB1CON     , 5, IV16)  !around D2 cell

      CALL GSVOLU('VHTB', 'TUBE',clas_med_h2liq, H2TB     , 3, IV17)  !H2 between tb1
      CALL GSVOLU('VH2C', 'CONE',clas_med_h2liq, H2CON    , 5, IV18)  !and tb2


      CALL GSVOLU('VTB2', 'TUBE',clas_med_alu  , TB2      , 3, IV19)  !outside tube 
      CALL GSVOLU('VT2C', 'CONE',clas_med_alu  , TB2CON   , 5, IV20)  !around D2 cell

      CALL GSVOLU('VTB3', 'TUBE',clas_med_alumylar , TB3     , 3, IV21) !isulation
      CALL GSVOLU('VT3C', 'CONE',clas_med_alumylar , TB3CON  , 5, IV22) !around D2 cell

      CALL GSVOLU('VTV1', 'TUBE',clas_med_alu, TBVAC1     , 3, IV23)    !
      CALL GSVOLU('VTV2', 'TUBE',clas_med_alu, TBVAC2     , 3, IV24)    !vacuum between cells 
      CALL GSVOLU('VTV3', 'TUBE',clas_med_alumylar , TBVAC3  , 3, IV25) !

      CALL GSVOLU('VHVC', 'TUBE', clas_med_h2liq,H2VAC     , 3, IV26)

      CALL GSVOLU('VBC1', 'CONE', clas_med_alu    , BCON1 , 5, IV27) !
      CALL GSVOLU('VBC2', 'CONE', clas_med_alu    , BCON2 , 5, IV28) !
      CALL GSVOLU('VBC3', 'CONE', clas_med_alu    , BCON3 , 5, IV29) !
      CALL GSVOLU('VBC4', 'CONE', clas_med_alu    , BCON4 , 5, IV30) !
      CALL GSVOLU('VBD2', 'CONE', clas_med_d2liq  , BD2   , 5, IV31) !
      CALL GSVOLU('VBH2', 'CONE', clas_med_h2liq  , BH2   , 5, IV32) !
      CALL GSVOLU('VBTB', 'TUBE', clas_med_alu    , BTUB  , 3, IV33) !

      CALL GSPOS('VOLM',1,'GAS2',0.,0.,-5.06    , 0,'ONLY')
      CALL GSPOS('VCEL',1,'VOLM',0.,0.,-9.815045, 0,'ONLY')
      CALL GSPOS('VCN1',1,'VOLM',0.,0.,-7.38705 , 0,'ONLY')
      CALL GSPOS('VCN2',1,'VOLM',0.,0.,-7.24305 , 0,'ONLY')
      CALL GSPOS('VD2L',1,'VOLM',0.,0.,-9.815045, 0,'ONLY')
      CALL GSPOS('VC1D',1,'VOLM',0.,0.,-7.38705 , 0,'ONLY')
      CALL GSPOS('VC2D',1,'VOLM',0.,0.,-7.24305 , 0,'ONLY')

      CALL GSPOS('VTB1',1,'VOLM',0.,0.,-9.67105 , 0,'ONLY')
      CALL GSPOS('VT1C',1,'VOLM',0.,0.,-7.03105 , 0,'ONLY')

      CALL GSPOS('VTB2',1,'VOLM',0.,0.,-9.67105 , 0,'ONLY')
      CALL GSPOS('VT2C',1,'VOLM',0.,0.,-7.03125 , 0,'ONLY')

      CALL GSPOS('VHTB',1,'VOLM',0.,0.,-9.67105 , 0,'ONLY')
      CALL GSPOS('VH2C',1,'VOLM',0.,0.,-7.03125 , 0,'ONLY')


      CALL GSPOS('VTB3',1,'VOLM',0.,0.,-9.67105 , 0,'ONLY')
      CALL GSPOS('VT3C',1,'VOLM',0.,0.,-7.03125 , 0,'ONLY')

      CALL GSPOS('VTV1',1,'VOLM',0.,0.,-4.87605 , 0,'ONLY')
      CALL GSPOS('VTV2',1,'VOLM',0.,0.,-4.87625 , 0,'ONLY')
      CALL GSPOS('VTV3',1,'VOLM',0.,0.,-4.87625 , 0,'ONLY')

      CALL GSPOS('VHVC',1,'VOLM',0.,0.,-4.87625 , 0,'ONLY')

      CALL GSPOS('VCEL',2,'VOLM',0.,0.,-0.509245, 0,'ONLY')
      CALL GSPOS('VCN1',2,'VOLM',0.,0., 1.918755, 0,'ONLY')
      CALL GSPOS('VCN2',2,'VOLM',0.,0., 2.062755, 0,'ONLY')

      CALL GSPOS('VH2L',2,'VOLM',0.,0.,-0.509245, 0,'ONLY')
      CALL GSPOS('VC1H',2,'VOLM',0.,0., 1.918755, 0,'ONLY')
      CALL GSPOS('VC2H',2,'VOLM',0.,0., 2.06755 , 0,'ONLY')

      CALL GSPOS('VINS',1,'VOLM',0.,0.,-0.509245, 0,'ONLY')
      CALL GSPOS('VCI1',1,'VOLM',0.,0., 1.918755, 0,'ONLY')
      CALL GSPOS('VCI2',1,'VOLM',0.,0., 2.066755, 0,'ONLY')

      CALL GSPOS('WNE5',1,'VOLM',0.,0.,-7.170415, 0,'ONLY')
      CALL GSPOS('WNE5',2,'VOLM',0.,0., 2.13539 , 0,'ONLY')
      CALL GSPOS('WNE5',3,'VOLM',0.,0.,-12.17041, 0,'ONLY')
      
      CALL GSPOS('VBTB',1,'VOLM',0.,0.,-12.238545,0,'ONLY')
      CALL GSPOS('VBC1',1,'VOLM',0.,0.,-13.198545,0,'ONLY')
      CALL GSPOS('VBC2',1,'VOLM',0.,0.,-13.198545,0,'ONLY')
      CALL GSPOS('VBC3',1,'VOLM',0.,0.,-13.198545,0,'ONLY')
      CALL GSPOS('VBC4',1,'VOLM',0.,0.,-13.198545,0,'ONLY')
      CALL GSPOS('VBD2',1,'VOLM',0.,0.,-13.198545,0,'ONLY')
      CALL GSPOS('VBH2',1,'VOLM',0.,0.,-13.198545,0,'ONLY')  


       if (itg_type(1).eq.1)goto 111 ! take off sc.chamber and heat sheld
       if (itg_type(1).eq.2)goto 112 ! take off sc.chamber 

* geometry of scattering chamber DR.N 66840-E-03017 
      irt_sch=get_rotm()

      CALL GSROTM(irt_sch,90.,90.,0.,0.,90.,0.)

       DATA SCCYL/11., 12.219, 12.14/                 !the cylidrical part of scatter.chamber
       DATA SCSPH/11., 11.953, 0.0, 90., 0.0, 180.0/  !spherical part of sc.chamber
       DATA SCEXIT/6.385, 0.635, 0.719, 1.535, 1.619/ !exit part of sc.chamber (cone)
       DATA SCEXW/0.0, 1.619, 0.00355/                !exit window of sc.chamber 


      CALL GSVOLU('VCYL', 'TUBE', clas_med_lowdfoam , SCCYL   , 3, IV11)
      CALL GSVOLU('VSPH', 'SPHE', clas_med_lowdfoam , SCSPH   , 6, IV12)
      CALL GSVOLU('VEXI', 'CONE', clas_med_cfiber   , SCEXIT  , 5, IV13)
      CALL GSVOLU('VEXW', 'TUBE', clas_med_alu      , SCEXW   , 3, IV14)

      CALL GSPOS('VCYL',1,'VOLM',0.,0.,-13.41   , 0, 'ONLY' )
      CALL GSPOS('VSPH',1,'VOLM',0.,0.,-1.27    , irt_sch, 'MANY' )
      CALL GSPOS('VEXI',1,'VOLM',0.,0., 16.115   , 0, 'ONLY' )
      CALL GSPOS('VEXW',1,'VOLM',0.,0., 22.5   , 0, 'ONLY' )

 112  continue
       if (itg_type(1).eq.3)goto 111 !take off heat sheld
* heat sheld alum.mylar + cerex

      DATA CHEAT/15.,7.0, 7.038,3.932,3.97/ ! cone
      DATA THEAT/0.0,3.97,0.0034/           !front part al.mylar + cerex
      DATA THEAL/0.0,3.97,0.00127/          !front part aluminum

      CALL GSVOLU('VHEC', 'CONE',clas_med_cerexmyl1 ,CHEAT , 5, IV15)
      CALL GSPOS('VHEC',1,'VOLM',0.,0.,-10.0   , 0, 'ONLY' )
      CALL GSVOLU('VHET', 'TUBE',clas_med_cerexmyl1 ,THEAT , 3, IV16)
      CALL GSPOS('VHET',1,'VOLM',0.,0.,5.17  , 0, 'ONLY' )
      CALL GSVOLU('VHEA', 'TUBE',clas_med_alu ,THEAL , 3, IV17)
      CALL GSPOS('VHEA',1,'VOLM',0.,0.,5.10807 , 0, 'ONLY' )

 111  continue
      CALL GSORD('VOLM',3)
      CALL GPVOLU(0)               ! PRINTOUT VOLUE PARAMETERS
C
C----------------------------------------------------------------
C
      CALL GGCLOS
C
      RETURN
      END

C----------------------------------------------------------------

        subroutine vert_e5(vx_poz)
c  include files:
c  --------------

#include "ffpar_tg.inc"
c_end_inc
      real vx_poz(3)
      real uran(3)
       call grndm(uran,3)

          if(ivx_e5.eq.1)then

             vx_poz(1)=0.0
             vx_poz(2)=0.0
             vx_poz(3)=-12.17+5.*uran(3)

          elseif(ivx_e5.eq.2)then

             vx_poz(1)=0.0
             vx_poz(2)=0.0
             vx_poz(3)=-2.5+5.*uran(3)

          elseif(ivx_e5.eq.3)then
             vx_poz(1) = 0.0
             vx_poz(2) = 0.0
             vx_poz(3) = 0.0

          endif
        
             return
             end
*========================================================
