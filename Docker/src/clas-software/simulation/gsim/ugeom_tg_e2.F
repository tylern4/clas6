      subroutine ugeom_tg_e2

#include "gsimpar.inc"
#include "gsimcontrol.inc"
#include "ffpar_tg_e2.inc"
#include "ffpar_foil.inc"
      if (itg_type(1).eq.1)then
         call ugeom_Apr15_may7
     
      elseif(itg_type(1).eq.2)then
         call ugeom_may8_may12
     
      elseif (itg_type(1).eq.3)then
         call ugeom_may12_may15
     
      else if (itg_type(1).eq.4)then
         call ugeom_may15_may27
     
      else if(itg_type(1).eq.0)then
         call ugeom_stg

         endif
      return
      end

C---------------------------------------------------------------- 

C---------------------------------------------------------------- 
      SUBROUTINE UGEOM_Apr15_may7
C
C----------------------------------------------------------------
	DIMENSION TBCLAS(3)
      DIMENSION CNOUT1(5),TBOUT1(3),TBOUT2(3),CNOUT2(5)
      DIMENSION TBIN1(3),CNIN1(5),TBIN2(3)
      DIMENSION TBIZ1(3),TBIZ2(3),TBIZ3(3)
      DIMENSION WND_I(3),EXWND(3),CONPL(5)
      DIMENSION CHL1(5),CHL2(5),THL4(3),CHL5(5)
      DIMENSION THL3(3),TBIZ4(3),TBIZ5(3)
      DIMENSION CIZ6(5),CIZ7(5),CIZ8(5)
      DIMENSION SC12TG(3),SFETG(3),SCH2TG(3)

#include "gcbank.inc"
#include "gsimpar.inc"
#include "gsimcontrol.inc"
#include "clmate.inc"
#include "cltmed.inc"
#include "tg_e2_mate.inc"
#include "tg_e2_tmed.inc"
#include "ffpar_tg_e2.inc"
c
C---------------------------------------------------------------- 
C     VOLUME SIZE PARAMETERS
C----------------------------------------------------------------  
C
        real vx_poz(3)
	DATA TBCLAS/0.0,4.0,6.0/                          !par. of mather volume
        DATA CHL1/0.412,1.632,2.523,1.632,1.897/
        DATA THL4/0.0,0.454,2.4/
        DATA CHL2/0.823,1.632,1.897,0.343,0.608/
        DATA CHL5/0.1,0.0,0.454,0.0,0.171/
        DATA THL3/0.343,0.454,0.396/
        DATA CNOUT1/1.214,2.523,2.555,0.454,0.486/      !Al-target cell(outside wall)
	DATA CNOUT2/0.1,0.454,0.459,0.171,0.176/        !---------------------------
	DATA TBOUT1/0.454,0.486,0.296/                  !---------------------------
	DATA TBOUT2/0.454,0.459,2.395/                  !---------------------------
	DATA TBIN1/1.6,1.632,0.412/                     !Al target cell(inside wall)
        DATA TBIN2/0.311,0.343,0.423/                   !--------------------------
        DATA CNIN1/0.825,1.6,1.632,0.311,0.343/         !--------------------------
        DATA TBIZ1/0.6,1.1,3.425/                       !Polistyrene foam 5mm THK
        DATA TBIZ2/1.2,1.2063,3.15/                     !9 layers of CEREX
        DATA TBIZ3/1.7,1.7028,3.2/                      !4 layers of CEREX
        DATA TBIZ4/0.0,0.585,0.001/                     !Al 20 microns
        DATA TBIZ5/0.0,1.8,0.0007/                      !2 layers CEREX
        DATA CIZ6/1.6,3.7,3.9,1.2,1.4/
        DATA CIZ7/1.4,4.0,4.1,1.7,1.9/
        DATA CIZ8/1.4,4.3,4.4,2.1,2.2/
	DATA WND_I/0.0,0.16,0.0008/                     !Al 16microns(inlet wnd.)
	DATA EXWND/0.0,0.17,0.0005/                     !Al 10microns(exit wnd.)
	DATA CONPL/1.25,2.7,3.3,0.6,1.2/                !Polistyrene foam about con.surf.
        DATA SC12TG/0.5,0.5,0.05/
	DATA SFETG/0.5,0.5,0.0075/
	DATA SCH2TG/0.5,0.5,0.035/
	
        call vert_e2(vx_poz)   
C----------------------------------------------------------------
C     VOLUMES SHAPES
C----------------------------------------------------------------
C
      CALL GSVOLU('VOLM','TUBE',clas_med_vac    , TBCLAS,3, IVOLM)                !Mother volume
          if(itg_type(2).eq.0)then
      CALL GSVOLU('VHU1','CONE', clas_med_vac , CHL1  ,5, IVHL1)
      CALL GSVOLU('VHU2','CONE', clas_med_vac , CHL2  ,5, IVHL2)
      CALL GSVOLU('VHU3','TUBE', clas_med_vac , THL4  ,3, IVHL3)
      CALL GSVOLU('VHU4','CONE', clas_med_vac , CHL5  ,5, IVHL4)
      CALL GSVOLU('VHU5','TUBE', clas_med_vac , THL3  ,3, IVHL5)
         
          elseif(itg_type(2).eq.1)then
      CALL GSVOLU('VHU1','CONE', clas_med_h2liq , CHL1  ,5, IVHL1)
      CALL GSVOLU('VHU2','CONE', clas_med_h2liq , CHL2  ,5, IVHL2)
      CALL GSVOLU('VHU3','TUBE', clas_med_h2liq , THL4  ,3, IVHL3)
      CALL GSVOLU('VHU4','CONE', clas_med_h2liq , CHL5  ,5, IVHL4)
      CALL GSVOLU('VHU5','TUBE', clas_med_h2liq , THL3  ,3, IVHL5)
       
        elseif(itg_type(2).eq.2)then
      CALL GSVOLU('VHU1','CONE', clas_med_d2liq , CHL1  ,5, IVHL1)
      CALL GSVOLU('VHU2','CONE', clas_med_d2liq , CHL2  ,5, IVHL2)
      CALL GSVOLU('VHU3','TUBE', clas_med_d2liq , THL4  ,3, IVHL3)
      CALL GSVOLU('VHU4','CONE', clas_med_d2liq , CHL5  ,5, IVHL4)
      CALL GSVOLU('VHU5','TUBE', clas_med_d2liq , THL3  ,3, IVHL5)
       
          elseif(itg_type(2).eq.3)then
      CALL GSVOLU('VHU1','CONE', clas_med_liq_he3 , CHL1  ,5, IVHL1)
      CALL GSVOLU('VHU2','CONE', clas_med_liq_he3 , CHL2  ,5, IVHL2)
      CALL GSVOLU('VHU3','TUBE', clas_med_liq_he3 , THL4  ,3, IVHL3)
      CALL GSVOLU('VHU4','CONE', clas_med_liq_he3 , CHL5  ,5, IVHL4)
      CALL GSVOLU('VHU5','TUBE', clas_med_liq_he3 , THL3  ,3, IVHL5)
        
        elseif(itg_type(2).eq.4)then
      CALL GSVOLU('VHU1','CONE', clas_med_helium4 , CHL1  ,5, IVHL1)
      CALL GSVOLU('VHU2','CONE', clas_med_helium4 , CHL2  ,5, IVHL2)
      CALL GSVOLU('VHU3','TUBE', clas_med_helium4 , THL4  ,3, IVHL3)
      CALL GSVOLU('VHU4','CONE', clas_med_helium4 , CHL5  ,5, IVHL4)
      CALL GSVOLU('VHU5','TUBE', clas_med_helium4 , THL3  ,3, IVHL5)
        endif
      CALL GSVOLU('VAU1','CONE',clas_med_alu    , CNOUT1,5, IVCO1)
      CALL GSVOLU('VAU2','CONE',clas_med_alu    , CNOUT2,5, IVCO2)
      CALL GSVOLU('VAU3','TUBE',clas_med_alu    , TBOUT1,3, IVTO1)
      CALL GSVOLU('VAU4','TUBE',clas_med_alu    , TBOUT2,3, IVTO2)
      CALL GSVOLU('VAU5','TUBE',clas_med_alu    , TBIN1 ,3, IVTI1)
      CALL GSVOLU('VAU6','TUBE',clas_med_alu    , TBIN2 ,3, IVTI2)
      CALL GSVOLU('VAU7','CONE',clas_med_alu    , CNIN1 ,5, IVCI1)
      CALL GSVOLU('VPU1','TUBE',clas_med_polistyrene , TBIZ1 ,3, IVIZ2)
      CALL GSVOLU('VMU1','TUBE',clas_med_mylar  , TBIZ2 ,3, IVIZ2)
      CALL GSVOLU('VMU2','TUBE',clas_med_mylar  , TBIZ3 ,3, IVIZ3)
      CALL GSVOLU('VAU8','TUBE',clas_med_alu    , TBIZ4 ,3, IVIZ4)
      CALL GSVOLU('VMU3','TUBE',clas_med_mylar  , TBIZ5 ,3, IVIZ5)
      CALL GSVOLU('VMU4','CONE',clas_med_mylar  , CIZ6  ,5, IVIZ6)
      CALL GSVOLU('VMU5','CONE',clas_med_mylar  , CIZ7  ,5, IVIZ7)
      CALL GSVOLU('VMU6','CONE',clas_med_mylar  , CIZ8  ,5, IVIZ8)
      CALL GSVOLU('VAU9','TUBE',clas_med_alu    , WND_I ,3, IVIWD)
      CALL GSVOLU('VAU0','TUBE',clas_med_alu    , EXWND ,3, IVEWD)
      CALL GSVOLU('VPU2','CONE',clas_med_polistyrene , CONPL ,5, IVCPL)
      CALL GSVOLU('VFTG','BOX ',clas_med_iron   , SFETG ,3, IVFTG)                 !solid target 56Fe
      CALL GSVOLU('VC12','BOX ',clas_med_carbon ,SC12TG ,3, IVC12)                 !solid tg. C12
      CALL GSVOLU('VCH2','BOX ',clas_med_ch2    ,SCH2TG ,3, IVFTG)                 !solid tg. CH2
C----------------------------------------------------------------
C     VOLUMES POSITIONS IN THE MOTHER VOLUME
C----------------------------------------------------------------
C
      CALL GSPOS('VOLM' ,1, 'GAS2' ,0. ,0. , -5.71    ,0, 'ONLY')   
      CALL GSPOS('VAU1' ,1, 'VOLM' ,0. ,0. , -4.306   ,0, 'ONLY')
      CALL GSPOS('VHU1' ,1, 'VOLM' ,0. ,0. , -5.404   ,0, 'ONLY')
      CALL GSPOS('VHU2' ,1, 'VOLM' ,0. ,0. , -4.169   ,0, 'ONLY')
      CALL GSPOS('VHU3' ,1, 'VOLM' ,0. ,0. , -0.10    ,0, 'ONLY')
      CALL GSPOS('VHU4' ,1, 'VOLM' ,0. ,0. ,  2.40    ,0, 'ONLY')
      CALL GSPOS('VHU5' ,1, 'VOLM' ,0. ,0. , -2.896   ,0, 'ONLY')
      CALL GSPOS('VAU2' ,1, 'VOLM' ,0. ,0. ,  2.30    ,0, 'ONLY')
      CALL GSPOS('VAU3' ,1, 'VOLM' ,0. ,0. , -2.796   ,0, 'ONLY')
      CALL GSPOS('VAU4' ,1, 'VOLM' ,0. ,0. , -0.30    ,0, 'ONLY')
      CALL GSPOS('VAU5' ,1, 'VOLM' ,0. ,0. , -5.404   ,0, 'ONLY')
      CALL GSPOS('VAU6' ,1, 'VOLM' ,0. ,0. , -2.923   ,0, 'ONLY')
      CALL GSPOS('VAU7' ,1, 'VOLM' ,0. ,0. , -4.169   ,0, 'ONLY')
      CALL GSPOS('VPU1' ,1, 'VOLM' ,0. ,0. ,  0.265   ,0, 'ONLY')
      CALL GSPOS('VMU1' ,1, 'VOLM' ,0. ,0. ,  0.64    ,0, 'ONLY')
      CALL GSPOS('VMU2' ,1, 'VOLM' ,0. ,0. ,  0.64    ,0, 'ONLY')
      CALL GSPOS('VAU8' ,1, 'VOLM' ,0. ,0. ,  3.65    ,0, 'ONLY')
      CALL GSPOS('VMU3' ,1, 'VOLM' ,0. ,0. ,  3.8     ,0, 'ONLY')
      CALL GSPOS('VMU4' ,1, 'VOLM' ,0. ,0. , -4.20    ,0, 'ONLY')
      CALL GSPOS('VMU5' ,1, 'VOLM' ,0. ,0. , -4.15    ,0, 'ONLY')
      CALL GSPOS('VMU6' ,1, 'VOLM' ,0. ,0. , -4.15    ,0, 'ONLY')
      CALL GSPOS('VAU9' ,1, 'VOLM' ,0. ,0. , -2.508   ,0, 'ONLY')
      CALL GSPOS('VAU0' ,1, 'VOLM' ,0. ,0. ,  2.505   ,0, 'ONLY')
      CALL GSPOS('VPU2' ,1, 'VOLM' ,0. ,0. , -4.25    ,0, 'ONLY')

C----------------------------------------------------------------
      if(itg_type(3).eq.56)then
         CALL GSPOS('VFTG',1,'VOLM',0.,0.,5.5,0,'ONLY')
        
      elseif(itg_type(3).eq.12)then
         CALL GSPOS('VC12',1,'VOLM',0.,0.,5.5,0,'ONLY')
        
      elseif(itg_type(3).eq.2)then
         CALL GSPOS('VCH2',1,'VOLM',0.,0.,5.5,0,'ONLY')
      endif
c----------------------------------------------------------------
      CALL GSORD('VOLM',3)
      CALL GPVOLU(0)               ! PRINTOUT VOLUE PARAMETERS
C----------------------------------------------------------------
      CALL GGCLOS
C
      RETURN
      END
****************************************************************      

       SUBROUTINE UGEOM_may8_may12
C----------------------------------------------------------------
C
C----------------------------------------------------------------
      DIMENSION TBCLAS(3),TBHL1(3),TBWND(3),TBIZ1(3)
      DIMENSION TBIZ2(3),TBCP1(3),TBCP2(3)
      DIMENSION TBVUC(3),TBCP3(3),CNCP4(5)
      DIMENSION TBHL2(3),TBCP5(3)
      DIMENSION TBHL3(3)
      DIMENSION CNPAR(5),CONHL(5)
      DIMENSION SC12TG(3),SFETG(3),SCH2TG(3),TWALL(3)
      DIMENSION TIZ2_OUT(3),TBIZ3(3),TBIZ4(3),TIZ4_OUT(3),TBIZ5(3)

#include "gcbank.inc"
#include "gsimpar.inc"
#include "gsimcontrol.inc"
#include "clmate.inc"
#include "cltmed.inc"
#include "tg_e2_mate.inc"
#include "tg_e2_tmed.inc"
#include "ffpar_tg_e2.inc"
C---------------------------------------------------------------- 
C     VOLUME SIZE PARAMETERS
C---------------------------------------------------------------- 
        real vx_poz(3)
        DATA TBCLAS/0.,4.,6./                         !par.of mather volume 
	DATA TBHL1/0.,1.3845,2.025/                   !target liq.
	DATA TBHL2/0.85,1.2345,0.5/                   !tg.lq.
	DATA TBHL3/0.85,1.3845,0.3/                   !tg.lq.
	DATA TWALL/1.3845,1.3972,2.025/               !cell walls 0.005 inch kapton
        DATA TBWND/0.,0.25,0.00075/                   !inlet and exit wind. Al
        DATA TBIZ1/1.55,1.5528,2.4/                   !4 layere CEREX
        DATA TBIZ2/2.25,2.252,3.85/                   !20 micron Al foil
        DATA TIZ2_OUT/0.25,2.27,0.001/                !----------------
        DATA TBIZ3/2.35,2.3507,3.9/                   !1 layere CEREX
        DATA TBIZ4/2.4,2.402,3.95/                    !20 micron Al foil
        DATA TIZ4_OUT/0.0,2.42,0.001/                 !----------------
        DATA TBIZ5/2.6,2.6035,3.95/                   !5 layere CEREX 
        DATA TBCP1/0.7,0.85,1.6/                      !cell base "Aliminum"
	DATA TBCP2/0.25,0.85,0.075/                   !----------------
	DATA TBCP3/1.3845,2.15,0.5/                   !----------------
	DATA CNCP4/0.375,0.85,1.3845,0.85,2.15/       !---------------
        DATA TBCP5/1.2345,1.3845,0.5/
	DATA TBVUC/0.0,0.7,1.525/                     !vacuum inside tbcp1
        DATA CNPAR/0.25,1.3845,1.3972,0.25,0.2627/    !sherical shall at the exit of liq. tg.
	DATA CONHL/0.25,0.,1.3845,0.,0.25/            !tg.liq. in this shell
	DATA SC12TG/0.5,0.5,0.05/
	DATA SFETG/0.5,0.5,0.0075/
	DATA SCH2TG/0.5,0.5,0.035/
        call vert_e2(vx_poz)   
C----------------------------------------------------------------
C     VOLUMES SHAPES
C----------------------------------------------------------------

      CALL GSVOLU('VOLM','TUBE',clas_med_vac,    TBCLAS,  3,IVOLM)        !Mother volume
           if(itg_type(2).eq.0)then
      CALL GSVOLU('VH_1','TUBE', clas_med_vac, TBHL1,   3,IVOLH)          !empty target 
      CALL GSVOLU('VH_2','TUBE', clas_med_vac, TBHL2,   3,IVHL2)          !empty target 
      CALL GSVOLU('VH_3','TUBE', clas_med_vac, TBHL3,   3,IVHL3)          !empty target 
      CALL GSVOLU('VH_4','CONE',clas_med_vac, CONHL,    5,IVCHL)          !empty target
          
          else if(itg_type(2).eq.1)then
      CALL GSVOLU('VH_1','TUBE', clas_med_h2liq, TBHL1,   3,IVOLH)        !target liquid H2
      CALL GSVOLU('VH_2','TUBE', clas_med_h2liq, TBHL2,   3,IVHL2)        !target liquid H2
      CALL GSVOLU('VH_3','TUBE', clas_med_h2liq, TBHL3,   3,IVHL3)        !target liquid H2
      CALL GSVOLU('VH_4','CONE',clas_med_h2liq, CONHL,    5,IVCHL)        !target liquid H2
          
           elseif(itg_type(2).eq.2)then
      CALL GSVOLU('VH_1','TUBE', clas_med_d2liq, TBHL1,   3,IVOLH)        !target liquid D2
      CALL GSVOLU('VH_2','TUBE', clas_med_d2liq, TBHL2,   3,IVHL2)        !target liquid D2
      CALL GSVOLU('VH_3','TUBE', clas_med_d2liq, TBHL3,   3,IVHL3)        !target liquid D2
      CALL GSVOLU('VH_4','CONE',clas_med_d2liq, CONHL,    5,IVCHL)        !target liquid D2
          
           elseif(itg_type(2).eq.3)then
      CALL GSVOLU('VH_1','TUBE', clas_med_liq_he3, TBHL1,   3,IVOLH)      !target liquid He3
      CALL GSVOLU('VH_2','TUBE', clas_med_liq_he3, TBHL2,   3,IVHL2)      !target liquid He3
      CALL GSVOLU('VH_3','TUBE', clas_med_liq_he3, TBHL3,   3,IVHL3)      !target liquid He3
      CALL GSVOLU('VH_4','CONE',clas_med_liq_he3, CONHL,    5,IVCHL)      !target liquid He4
          
           elseif(itg_type(2).eq.4)then
      CALL GSVOLU('VH_1','TUBE', clas_med_helium4, TBHL1,   3,IVOLH)      !target He4
      CALL GSVOLU('VH_2','TUBE', clas_med_helium4, TBHL2,   3,IVHL2)      !target He4
      CALL GSVOLU('VH_3','TUBE', clas_med_helium4, TBHL3,   3,IVHL3)      !target He4
      CALL GSVOLU('VH_4','CONE', clas_med_helium4, CONHL,   5,IVCHL)      !target He4
           endif
        
      CALL GSVOLU('VM_1','TUBE',clas_med_mylar,  TBIZ1,   3,IVIZ3)        !4-layers cerex
      CALL GSVOLU('VA_1','TUBE',clas_med_Al_foil,    TBIZ2,   3,IVML2)    !20 micron Al foil
      CALL GSVOLU('VA_2','TUBE',clas_med_Al_foil,    TIZ2_OUT,3,IV2OUT)   !---------------
      CALL GSVOLU('VM_2','TUBE',clas_med_mylar,  TBIZ3,   3,IVIZ3)        !1-layer CEREX
      CALL GSVOLU('VA_3','TUBE',clas_med_Al_foil,    TBIZ4,   3,IVIZ4)    !20 micron Al foil
      CALL GSVOLU('IA_4','TUBE',clas_med_Al_foil,    TIZ4_OUT,3,IV4OUT)   !---------------
      CALL GSVOLU('VM_3','TUBE',clas_med_mylar,  TBIZ5,   3,IVIZ5)        !5-layers CEREX 
      CALL GSVOLU('VA_6','TUBE',clas_med_alu,    TBCP1,   3,IVCP1)        !cell base Aluminum
      CALL GSVOLU('VA_7','TUBE',clas_med_alu,    TBCP2,   3,IVCP2)        !---------------
      CALL GSVOLU('VA_8','TUBE',clas_med_alu,    TBCP3,   3,IVCP3)        !---------------        
      CALL GSVOLU('VA_9','CONE',clas_med_alu,    CNCP4,   5,IVCP4)        !---------------
      CALL GSVOLU('VA_0','TUBE',clas_med_alu,    TBCP5,   3,IVCP5)        !---------------
      CALL GSVOLU('VVAC','TUBE',clas_med_vac,    TBVUC,   3,IVVAC)        !inside aluminum(vacuum)
      CALL GSVOLU('VA_5','TUBE',clas_med_alu,    TBWND,   3,IVOLW)        !inlet and exit wind.(Al)
      CALL GSVOLU('VM_4','CONE',clas_med_mylar,  CNPAR,   5,IVCON)        !0.005 inch kapton
      CALL GSVOLU('VM_5','TUBE',clas_med_mylar,  TWALL,   3,IVWLL)        !cell walls 0.005 inch kapton
      CALL GSVOLU('VFTG','BOX ',clas_med_iron,   SFETG,   3,IVFTG)        !solid target
      CALL GSVOLU('VC12','BOX ',clas_med_carbon, SC12TG,  3,IVC12)        !solid tg. C12
      CALL GSVOLU('VCH2','BOX ',clas_med_ch2   , SCH2TG,  3,IVFTG)        !solid tg. CH2
      
C
C----------------------------------------------------------------
C     VOLUMES POSITIONS IN THE MOTHER VOLUME
C----------------------------------------------------------------
C
      CALL GSPOS('VOLM',1,'GAS2',0.,0., -5.74   ,   0, 'ONLY')
      CALL GSPOS('VH_1',1,'VOLM',0.,0., -0.625  ,   0, 'MANY')
      CALL GSPOS('VH_2',1,'VOLM',0.,0., -3.15   ,   0, 'ONLY')
      CALL GSPOS('VH_3',1,'VOLM',0.,0., -3.95   ,   0, 'ONLY')
      CALL GSPOS('VA_5',1,'VOLM',0.,0., 1.90075 ,   0, 'ONLY')
      CALL GSPOS('VA_5',2,'VOLM',0.,0., -2.049  ,   0, 'ONLY')
      CALL GSPOS('VM_1',1,'VOLM',0.,0., -0.8    ,   0, 'ONLY')
      CALL GSPOS('VA_1',1,'VOLM',0.,0., -0.45   ,   0, 'ONLY')
      CALL GSPOS('VA_2',1,'VOLM',0.,0.,  3.41   ,   0, 'ONLY')
      CALL GSPOS('VM_2',1,'VOLM',0.,0., -0.4    ,   0, 'ONLY')
      CALL GSPOS('VA_3',1,'VOLM',0.,0., -0.35   ,   0, 'ONLY')
      CALL GSPOS('VA_4',1,'VOLM',0.,0.,  3.61   ,   0, 'ONLY')
      CALL GSPOS('VM_3',1,'VOLM',0.,0., -0.35   ,   0, 'ONLY')
      CALL GSPOS('VA_6',1,'VOLM',0.,0., -3.5    ,   0, 'ONLY')
      CALL GSPOS('VA_7',1,'VOLM',0.,0., -1.975  ,   0, 'ONLY')
      CALL GSPOS('VA_8',1,'VOLM',0.,0., -3.8    ,   0, 'MANY')
      CALL GSPOS('VA_9',1,'VOLM',0.,0., -4.725  ,   0, 'MANY')
      CALL GSPOS('VA_0',1,'VOLM',0.,0., -3.15   ,   0, 'ONLY')
      CALL GSPOS('VVAC',1,'VOLM',0.,0., -3.575  ,   0, 'ONLY')
      CALL GSPOS('VM_4',1,'VOLM',0.,0.,1.65     ,   0, 'ONLY')
      CALL GSPOS('VH_4',1,'VOLM',0.,0.,1.65     ,   0, 'ONLY')
      CALL GSPOS('VM_5',1,'VOLM',0.,0.,-0.625   ,   0, 'ONLY')
	
C----------------------------------------------------------------
      if(itg_type(3).eq.56)then
         CALL GSPOS('VFTG',1,'VOLM',0.,0.,5.5,0,'ONLY')
        
      elseif(itg_type(3).eq.12)then
         CALL GSPOS('VC12',1,'VOLM',0.,0.,5.5,0,'ONLY')
        
      elseif(itg_type(3).eq.2)then
         CALL GSPOS('VCH2',1,'VOLM',0.,0.,5.5,0,'ONLY')
      endif
c----------------------------------------------------------------

      CALL GSORD('VOLM',3)
      CALL GPVOLU(0)               ! PRINTOUT VOLUE PARAMETERS
C
C----------------------------------------------------------------
C
      CALL GGCLOS
C
      RETURN
      END
C      
************************************************************************
C---------------------------------------------------------------- 
      SUBROUTINE UGEOM_may12_may15
C----------------------------------------------------------------

      DIMENSION TBCLAS(3),TBHL1(3),TBWND(3)
      DIMENSION TBIZ2(3),TBCP1(3),TBCP2(3)
      DIMENSION TBVUC(3),TBCP3(3),CNCP4(5)
      DIMENSION TBHL2(3),TBCP5(3)
      DIMENSION TBHL3(3)
      DIMENSION CNPAR(5),CONHL(5)
      DIMENSION SC12TG(3),SFETG(3),SCH2TG(3),TWALL(3)
****************************************************************
      DIMENSION TBIZ3(3),TIZ2_OUT(3),TBIZ4(3),TIZ4_OUT(3)
      DIMENSION TBIZ5(3)

#include "gcbank.inc"
#include "gsimpar.inc"
#include "gsimcontrol.inc"
#include "clmate.inc"
#include "cltmed.inc"
#include "tg_e2_mate.inc"
#include "tg_e2_tmed.inc"
#include "ffpar_tg_e2.inc"
#include "gcnum.inc"
#include "gcunit.inc"  
#include "ffpar_foil.inc"
c
C---------------------------------------------------------------- 
C     VOLUME SIZE PARAMETERS
C----------------------------------------------------------------  
C
        real vx_poz(3)
        DATA TBCLAS/0.,4.,6./                          !mother volume
	DATA TBHL1/0.,1.3845,2.115/                    !tg.liq. He3
	DATA TBHL2/0.85,1.2345,0.5/                    !------------
	DATA TBHL3/0.85,1.3845,0.3/                    !-----------
	DATA TWALL/1.3845,1.3972,2.115/                !cell walls 0.005inch kapton
        DATA TBWND/0.,0.25,0.00075/                    !in. and ex. wind.Al 15 microns
        DATA TBIZ2/2.25,2.252,3.85/                    !20 micron Al foil
        DATA TIZ2_OUT/0.25,2.27,0.001/                 !----------------
        DATA TBIZ3/2.35,2.3507,3.9/                    !1 layere CEREX
        DATA TBIZ4/2.4,2.402,3.95/                     !20 micron Al foil
        DATA TIZ4_OUT/0.0,2.42,0.001/                  !----------------
        DATA TBIZ5/2.6,2.6035,3.95/                    !5 layere CEREX 
        DATA TBCP1/0.7,0.85,1.5/                       !cell base TORLON
	DATA TBCP2/0.25,0.85,0.075/                    !---------------
	DATA TBCP3/1.3845,1.95,0.35/                   !--------------- 
	DATA CNCP4/0.425,0.85,1.1,0.85,1.95/           !--------------
        DATA TBCP5/1.2345,1.3845,0.5/                        !--------------
	DATA TBVUC/0.0,0.7,1.6/                        !vuc. inside hl1
        DATA CNPAR/0.25,1.3845,1.3972,0.25,0.2627/     !cell walls 0.005 kap.
	DATA CONHL/0.25,0.,1.3845,0.,0.25/             !tg.liq. He3
	DATA SC12TG/0.5,0.5,0.05/                      !solid tg. 
	DATA SFETG/0.5,0.5,0.0075/
	DATA SCH2TG/0.5,0.5,0.035/
        call vert_e2(vx_poz)        
C-----------------------------------------------------------------------------------------
      CALL GSVOLU('VOLM','TUBE',clas_med_vac,     TBCLAS,  3,IVOLM)       !Mother volume
           if(itg_type(2).eq.0)then
      CALL GSVOLU('VH_1','TUBE',clas_med_vac,  TBHL1,   3,IVOLH)          !empty target 
      CALL GSVOLU('VH_2','TUBE',clas_med_vac,  TBHL2,   3,IVHL2)          !empty target 
      CALL GSVOLU('VH_3','TUBE',clas_med_vac,  TBHL3,   3,IVHL3)          !empty target
      CALL GSVOLU('VH_4','CONE',clas_med_vac,  CONHL,   5,IVCHL)          !empty target 
           
           elseif(itg_type(2).eq.1)then
      CALL GSVOLU('VH_1','TUBE',clas_med_h2liq,  TBHL1,   3,IVOLH)        !target liquid H2
      CALL GSVOLU('VH_2','TUBE',clas_med_h2liq,  TBHL2,   3,IVHL2)        !target liquid H2
      CALL GSVOLU('VH_3','TUBE',clas_med_h2liq,  TBHL3,   3,IVHL3)        !target liquid H2
      CALL GSVOLU('VH_4','CONE',clas_med_h2liq,  CONHL,   5,IVCHL)        !target liquid H2
           
           elseif(itg_type(2).eq.2)then
      CALL GSVOLU('VH_1','TUBE',clas_med_d2liq,  TBHL1,   3,IVOLH)        !target liquid D2
      CALL GSVOLU('VH_2','TUBE',clas_med_d2liq,  TBHL2,   3,IVHL2)        !target liquid D2
      CALL GSVOLU('VH_3','TUBE',clas_med_d2liq,  TBHL3,   3,IVHL3)        !target liquid D2
      CALL GSVOLU('VH_4','CONE',clas_med_d2liq,  CONHL,   5,IVCHL)        !target liquid D2
           
           elseif(itg_type(2).eq.3)then
      CALL GSVOLU('VH_1','TUBE',clas_med_liq_he3,  TBHL1,   3,IVOLH)      !target liquid He3
      CALL GSVOLU('VH_2','TUBE',clas_med_liq_he3,  TBHL2,   3,IVHL2)      !target liquid He3
      CALL GSVOLU('VH_3','TUBE',clas_med_liq_he3,  TBHL3,   3,IVHL3)      !target liquid He3
      CALL GSVOLU('VH_4','CONE',clas_med_liq_he3,  CONHL,   5,IVCHL)      !target liquid He3
          
           elseif(itg_type(2).eq.4)then
      CALL GSVOLU('VH_1','TUBE',clas_med_helium4,  TBHL1,   3,IVOLH)      !target He4
      CALL GSVOLU('VH_2','TUBE',clas_med_helium4,  TBHL2,   3,IVHL2)      !target He4
      CALL GSVOLU('VH_3','TUBE',clas_med_helium4,  TBHL3,   3,IVHL3)      !target He4
      CALL GSVOLU('VH_4','CONE',clas_med_helium4,  CONHL,   5,IVCHL)      !target He4
           endif
        
      CALL GSVOLU('VA_1','TUBE',clas_med_Al_foil,     TBIZ2,   3,IVML2)   !20 micron Al foil
      CALL GSVOLU('VA_2','TUBE',clas_med_Al_foil,     TIZ2_OUT,3,IV2OUT)  !---------------
      CALL GSVOLU('VM_2','TUBE',clas_med_mylar,   TBIZ3,   3,IVIZ3)       !1-layer CEREX
      CALL GSVOLU('VA_3','TUBE',clas_med_Al_foil,     TBIZ4,   3,IVIZ4)   !20 micron Al foil
      CALL GSVOLU('VA_4','TUBE',clas_med_Al_foil,     TIZ4_OUT,3,IV4OUT)  !---------------
      CALL GSVOLU('VM_3','TUBE',clas_med_mylar,   TBIZ5,   3,IVIZ5)       !5-layers CEREX 
      CALL GSVOLU('VM_6','TUBE',clas_med_mylar,   TBCP1,   3,IVCP1)       !cell base TORLON
      CALL GSVOLU('VM_7','TUBE',clas_med_mylar,   TBCP2,   3,IVCP2)       !cell base TORLON
      CALL GSVOLU('VM_8','TUBE',clas_med_mylar,   TBCP3,   3,IVCP3)       !cell base TORLON
      CALL GSVOLU('VM_9','CONE',clas_med_mylar,   CNCP4,   5,IVCP4)       !cell base TORLON
      CALL GSVOLU('VM_0','TUBE',clas_med_mylar,   TBCP5,   3,IVCP5)       !cell base TORLON
      CALL GSVOLU('VVAC','TUBE',clas_med_vac,     TBVUC,   3,IVVAC)       !inside liq.(vacuum)
      CALL GSVOLU('VA_5','TUBE',clas_med_alu,     TBWND,   3,IVOLW)       !inlet and exit wind.
      CALL GSVOLU('VM_4','CONE',clas_med_mylar,   CNPAR,   5,IVCON)       !cell wall 0.005 inch kapton
      CALL GSVOLU('VM_5','TUBE',clas_med_mylar,   TWALL,   3,IVWLL)       !cell walls 0.005 inch kapton
      CALL GSVOLU('VFTG','BOX ',clas_med_iron,    SFETG,   3,IVFTG)       !solid target
      CALL GSVOLU('VC12','BOX ',clas_med_carbon,  SC12TG,  3,IVC12)       !solid tg. C12
      CALL GSVOLU('VCH2','BOX ',clas_med_ch2   ,  SCH2TG,  3,IVFTG)       !solid tg. CH2
      
C
C---------------------------------------------------------------------------------------
c
C     VOLUMES POSITIONS IN THE MOTHER VOLUME
C--------------------------------------------------------------------------------------
C
      CALL GSPOS('VOLM',1,'GAS2',0.,0., -5.68 ,  0, 'ONLY')
      CALL GSPOS('VH_1',1,'VOLM',0.,0., -0.635,  0, 'MANY')
      CALL GSPOS('VH_2',1,'VOLM',0.,0., -3.25 ,  0, 'ONLY')
      CALL GSPOS('VH_3',1,'VOLM',0.,0., -4.05 ,  0, 'ONLY')
      CALL GSPOS('VA_5',1,'VOLM',0.,0.,  1.98 ,  0, 'ONLY')
      CALL GSPOS('VA_5',2,'VOLM',0.,0., -2.15 ,  0, 'ONLY')
      CALL GSPOS('VA_1',1,'VOLM',0.,0., -0.45 ,  0, 'ONLY')
      CALL GSPOS('VA_2',1,'VOLM',0.,0.,  3.41 ,  0, 'ONLY')
      CALL GSPOS('VM_2',1,'VOLM',0.,0., -0.4  ,  0, 'ONLY')
      CALL GSPOS('VA_3',1,'VOLM',0.,0., -0.35 ,  0, 'ONLY')
      CALL GSPOS('VA_4',1,'VOLM',0.,0.,  3.61 ,  0, 'ONLY')
      CALL GSPOS('VM_3',1,'VOLM',0.,0., -0.35 ,  0, 'ONLY')
      CALL GSPOS('VM_6',1,'VOLM',0.,0., -3.5  ,  0, 'ONLY')
      CALL GSPOS('VM_7',1,'VOLM',0.,0., -2.0075, 0, 'ONLY')
      CALL GSPOS('VM_8',1,'VOLM',0.,0., -3.8  ,  0, 'MANY')
      CALL GSPOS('VM_0',1,'VOLM',0.,0., -3.25,   0, 'ONLY')
      CALL GSPOS('VM_9',1,'VOLM',0.,0., -4.575,  0, 'MANY')
      CALL GSPOS('VVAC',1,'VOLM',0.,0., -3.5  ,  0, 'ONLY')
      CALL GSPOS('VM_4',1,'VOLM',0.,0.,  1.73 ,  0, 'ONLY')
      CALL GSPOS('VH_4',1,'VOLM',0.,0.,  1.73 ,  0, 'ONLY')
      CALL GSPOS('VM_5',1,'VOLM',0.,0., -0.635,  0, 'ONLY')      
C----------------------------------------------------------------
      if(itg_type(3).eq.56)then
         CALL GSPOS('VFTG',1,'VOLM',0.,0.,5.5,0,'ONLY')
        
      elseif(itg_type(3).eq.12)then
         CALL GSPOS('VC12',1,'VOLM',0.,0.,5.5,0,'ONLY')
        
      elseif(itg_type(3).eq.2)then
         CALL GSPOS('VCH2',1,'VOLM',0.,0.,5.5,0,'ONLY')
      endif
c----------------------------------------------------------------
      CALL GSORD('VOLM',3)
      CALL GPVOLU(0)               ! PRINTOUT VOLUE PARAMETERS
C----------------------------------------------------------------
      CALL GGCLOS
C
      RETURN
      END

C---------------------------------------------------------------- 
      SUBROUTINE UGEOM_may15_may27
C----------------------------------------------------------------

      DIMENSION TBCLAS(3),TBHL1(3),TBWND(3),TBIZ1(3)
      DIMENSION TBALF(3),TBIZ2(3),TBCP1(3),TBCP2(3)
      DIMENSION TBVUC(3),TBCP3(3),TBCP5(3),CNCP4(5)
      DIMENSION TBHL2(3)
      DIMENSION TBOUT(3),TBHL3(3)
      DIMENSION CNPAR(5),CONHL(5)
      DIMENSION SC12TG(3),SFETG(3),SCH2TG(3),TWALL(3)

#include "gcbank.inc"
#include "gsimpar.inc"
#include "gsimcontrol.inc"
#include "clmate.inc"
#include "cltmed.inc"
#include "tg_e2_mate.inc"
#include "tg_e2_tmed.inc"
#include "ffpar_tg_e2.inc"
c
C---------------------------------------------------------------- 
C     VOLUME SIZE PARAMETERS
C----------------------------------------------------------------  

        real vx_poz(3)
        DATA TBCLAS/0.,4.,6./
	DATA TBHL1/0.,1.3845,2.115/
	DATA TBHL2/0.85,1.2345,0.5/
	DATA TBHL3/0.85,1.3845,0.3/
	DATA TWALL/1.3845,1.3972,2.115/
        DATA TBWND/0.,0.25,0.00075/
        DATA TBIZ1/1.55,1.5528,2.5/
        DATA TBALF/2.000,2.002,3.25/
        DATA TBIZ2/2.002,2.0055,3.25/
        DATA TBCP1/0.7,0.85,1.5/
	DATA TBCP2/0.25,0.85,0.075/
	DATA TBCP3/1.3845,1.95,0.35/
	DATA CNCP4/0.425,0.85,1.1,0.85,1.95/
        DATA TBCP5/1.2345,1.3845,0.5/
	DATA TBVUC/0.0,0.7,1.425/
        DATA TBOUT/0.0,1.95,0.01/
        DATA CNPAR/0.25,1.3845,1.3972,0.25,0.2627/
	DATA CONHL/0.25,0.,1.3845,0.,0.25/
	DATA SC12TG/0.5,0.5,0.05/
	DATA SFETG/0.5,0.5,0.0075/
	DATA SCH2TG/0.5,0.5,0.035/
        call vert_e2(vx_poz)    

C     VOLUMES SHAPES
C----------------------------------------------------------------
C
      CALL GSVOLU('VOLM','TUBE', clas_med_vac   ,  TBCLAS,3,IVOLM)      !Mother volume
         if(itg_type(2).eq.0)then
      CALL GSVOLU('VH_1','TUBE', clas_med_vac,  TBHL1,3,IVHL1)          !empty target 
      CALL GSVOLU('VH_2','TUBE', clas_med_vac,  TBHL2,3,IVHL2)          !empty target 
      CALL GSVOLU('VH_3','TUBE', clas_med_vac,  TBHL3,3,IVHL3)          !empty target 
      CALL GSVOLU('VH_4','CONE', clas_med_vac,  CONHL,5,IVHL4)          !empty target 
         
          elseif(itg_type(2).eq.1)then
      CALL GSVOLU('VH_1','TUBE', clas_med_h2liq,  TBHL1,3,IVHL1)        !target liquid H2
      CALL GSVOLU('VH_2','TUBE', clas_med_h2liq,  TBHL2,3,IVHL2)        !target liquid H2
      CALL GSVOLU('VH_3','TUBE', clas_med_h2liq,  TBHL3,3,IVHL3)        !target liquid H2
      CALL GSVOLU('VH_4','CONE', clas_med_h2liq,  CONHL,5,IVHL4)        !target liquid H2
         
          elseif(itg_type(2).eq.2)then
      CALL GSVOLU('VH_1','TUBE', clas_med_d2liq,  TBHL1,3,IVHL1)        !target liquid D2
      CALL GSVOLU('VH_2','TUBE', clas_med_d2liq,  TBHL2,3,IVHL2)        !target liquid D2
      CALL GSVOLU('VH_3','TUBE', clas_med_d2liq,  TBHL3,3,IVHL3)        !target liquid D2
      CALL GSVOLU('VH_4','CONE', clas_med_d2liq,  CONHL,5,IVHL4)        !target liquid D2
         
         elseif(itg_type(2).eq.3)then
      CALL GSVOLU('VH_1','TUBE', clas_med_liq_he3,  TBHL1,3,IVHL1)       !target liquid He3
      CALL GSVOLU('VH_2','TUBE', clas_med_liq_he3,  TBHL2,3,IVHL2)       !target liquid He3
      CALL GSVOLU('VH_3','TUBE', clas_med_liq_he3,  TBHL3,3,IVHL3)       !target liquid He3
      CALL GSVOLU('VH_4','CONE', clas_med_liq_he3,  CONHL,5,IVHL4)       !target liquid He3
        
         elseif(itg_type(2).eq.4)then
      CALL GSVOLU('VH_1','TUBE', clas_med_helium4,  TBHL1,3,IVHL1)       !target liquid He4
      CALL GSVOLU('VH_2','TUBE', clas_med_helium4,  TBHL2,3,IVHL2)       !target liquid He4
      CALL GSVOLU('VH_3','TUBE', clas_med_helium4,  TBHL3,3,IVHL3)       !target liquid He4
      CALL GSVOLU('VH_4','CONE', clas_med_helium4,  CONHL,5,IVHL4)       !target liquid He4
         endif

      CALL GSVOLU('VA_1','TUBE', clas_med_Al_foil   ,  TBALF,3,IVALF)    !20 micron Al foil
      CALL GSVOLU('VM_1','TUBE', clas_med_mylar ,  TBIZ1,3,IVLML)        !4-layers cerex
      CALL GSVOLU('VM_2','TUBE', clas_med_mylar ,  TBIZ2,3,IVML2)        !5-layers cerex
      CALL GSVOLU('VC_1','TUBE', clas_med_copper,  TBCP1,3,IVCP1)        !cell base copper
      CALL GSVOLU('VC_2','TUBE', clas_med_copper,  TBCP2,3,IVCP2)        !cell base copper
      CALL GSVOLU('VC_3','TUBE', clas_med_copper,  TBCP3,3,IVCP3)        !cell base copper
      CALL GSVOLU('VC_4','CONE', clas_med_copper,  CNCP4,5,IVCP4)        !cell base copper
      CALL GSVOLU('VC_5','TUBE', clas_med_copper,  TBCP5,3,IVCP5)        !cell base copper
      CALL GSVOLU('VVAC','TUBE', clas_med_vac   ,  TBVUC,3,IVVAC)        !inside liq.(vacuum)
      CALL GSVOLU('VA_5','TUBE', clas_med_alu   ,  TBWND,3,IVOLW)        !inlet and exit wind.
      CALL GSVOLU('VM_3','TUBE', clas_med_mylar ,  TBOUT,3,IVOUT)        !insulating reference foil
      CALL GSVOLU('VM_4','CONE', clas_med_mylar ,  CNPAR,5,IVCON)        !0.005 inch kapton
      CALL GSVOLU('VM_5','TUBE', clas_med_mylar ,  TWALL,3,IVWLL)        !cell walls 0.005 inch kapton
      CALL GSVOLU('VFTG','BOX ', clas_med_iron  ,  SFETG,3,IVFTG)        !solid target 56Fe
      CALL GSVOLU('VC12','BOX ', clas_med_carbon,  SC12TG,3,IVC12)       !solid tg. C12
      CALL GSVOLU('VCH2','BOX ', clas_med_ch2   ,  SCH2TG,3,IVFTG)       !solid tg. CH2
      
C
C----------------------------------------------------------------
C     VOLUMES POSITIONS IN THE MOTHER VOLUME
C----------------------------------------------------------------
C
c      CALL GSPOS('VOLM',1,'GAS2',0.,0., -6.56   ,0, 'ONLY')
      CALL GSPOS('VOLM',1,'GAS2',0.,0., -5.68   ,0, 'ONLY')
      CALL GSPOS('VH_1',1,'VOLM',0.,0., -0.635  ,0, 'MANY')
      CALL GSPOS('VH_2',1,'VOLM',0.,0., -3.25   ,0, 'ONLY')
      CALL GSPOS('VH_3',1,'VOLM',0.,0., -4.05   ,0, 'ONLY')
      CALL GSPOS('VA_5',1,'VOLM',0.,0.,  1.98   ,0, 'ONLY')
      CALL GSPOS('VA_5',2,'VOLM',0.,0., -2.15   ,0, 'ONLY')
      CALL GSPOS('VM_1',1,'VOLM',0.,0., -0.7    ,0, 'ONLY')
      CALL GSPOS('VA_1',1,'VOLM',0.,0., -1.05   ,0, 'ONLY')
      CALL GSPOS('VM_2',1,'VOLM',0.,0., -1.05   ,0, 'ONLY')
      CALL GSPOS('VC_1',1,'VOLM',0.,0., -3.5    ,0, 'ONLY')
      CALL GSPOS('VC_2',1,'VOLM',0.,0., -2.0075 ,0, 'ONLY')
      CALL GSPOS('VC_3',1,'VOLM',0.,0., -3.8    ,0, 'MANY')
      CALL GSPOS('VC_4',1,'VOLM',0.,0., -4.575  ,0, 'MANY')
      CALL GSPOS('VC_5',1,'VOLM',0.,0., -3.25   ,0, 'ONLY')
      CALL GSPOS('VVAC',1,'VOLM',0.,0., -3.575  ,0, 'ONLY')
      CALL GSPOS('VM_3',1,'VOLM',0.,0.,  2.2    ,0, 'ONLY')
      CALL GSPOS('VM_4',1,'VOLM',0.,0.,  1.73   ,0, 'ONLY')
      CALL GSPOS('VH_4',1,'VOLM',0.,0.,  1.73   ,0, 'ONLY')
      CALL GSPOS('VM_5',1,'VOLM',0.,0.,-0.635   ,0, 'ONLY')
      
C----------------------------------------------------------------
      if(itg_type(3).eq.56)then
         CALL GSPOS('VFTG',1,'VOLM',0.,0.,5.5,0,'ONLY')
        
      elseif(itg_type(3).eq.12)then
         CALL GSPOS('VC12',1,'VOLM',0.,0.,5.5,0,'ONLY')
        
      elseif(itg_type(3).eq.2)then
         CALL GSPOS('VCH2',1,'VOLM',0.,0.,5.5,0,'ONLY')
      endif
c----------------------------------------------------------------

      CALL GSORD('VOLM',3)
      CALL GPVOLU(0)               ! PRINTOUT VOLUE PARAMETERS

C----------------------------------------------------------------

      CALL GGCLOS
C

      RETURN
      END
*==================================================================      

      SUBROUTINE ugeom_stg
c
      DIMENSION TBCLAS(3)
      DIMENSION SC12TG(3),SFETG(3),SCH2TG(3)

#include "gcbank.inc"
#include "gsimpar.inc"
#include "gsimcontrol.inc"
#include "clmate.inc"
#include "cltmed.inc"
#include "tg_e2_mate.inc"
#include "tg_e2_tmed.inc"
#include "ffpar_tg_e2.inc"
#include "ffpar_foil.inc"
      DATA TBCLAS/0.0,4.0,6.0/     
      DATA SC12TG/0.5,0.5,0.05/
      DATA SFETG/0.5,0.5,0.0075/
      DATA SCH2TG/0.5,0.5,0.035/
c
      CALL GSVOLU('VOLM','TUBE',1    , TBCLAS,3, IVOLM)                  !Mother volume
      CALL GSVOLU('VFTG','BOX ',7   , SFETG ,3, IVFTG)                   !solid target 56Fe
      CALL GSVOLU('VC12','BOX ',clas_med_carbon ,SC12TG ,3, IVC12)       !solid tg. C12
      CALL GSVOLU('VCH2','BOX ',clas_med_ch2    ,SCH2TG ,3, IVch2)       !solid tg. CH2
C----------------------------------------------------------------
      CALL GSPOS('VOLM',1,'GAS2',0.,0., -5.06 ,  0, 'ONLY')

      if(itg_type(3).eq.56)then
        CALL GSPOS('VFTG',1,'VOLM',0.,0.,5.5,0,'ONLY')
        
      elseif(itg_type(3).eq.12)then
        CALL GSPOS('VC12',1,'VOLM',0.,0.,5.5,0,'ONLY')

      elseif(itg_type(3).eq.2)then
        CALL GSPOS('VCH2',1,'VOLM',0.,0.,5.5,0,'ONLY')

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
  
      subroutine vert_e2(vx_poz)
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

*vertex are smearing in the cryo.target
       if(itg_type(1).eq.1.and.itg_type(2).ne.0
     &               .and.ivx_e2.ne.0)then
             vx_poz(1)=0.0
             vx_poz(2)=0.0
             vx_poz(3)=-2.6+5.*uran(3)
      elseif(itg_type(1).eq.2.and.itg_type(2).ne.0
     &                  .and.ivx_e2.ne.0)then
             vx_poz(1)=0.0
             vx_poz(2)=0.0
             vx_poz(3)=-2.5+3.72*uran(3)
      elseif(itg_type(1).eq.3.and.itg_type(2).ne.0
     &                  .and.ivx_e2.ne.0)then
             vx_poz(1)=0.0
             vx_poz(2)=0.0
             vx_poz(3)=-2.656+4.016*uran(3)
      elseif(itg_type(1).eq.4.and.itg_type(2).ne.0
     &  .and.ivx_e2.ne.0)then
             vx_poz(1)=0.0
             vx_poz(2)=0.0
             vx_poz(3)=-2.64+4.0*uran(3)
*
*vertex are smearing in the solid target
      elseif(itg_type(3).eq.12.and.itg_type(2).eq.0.and.ivx_e2.ne.0)then
             vx_poz(1)=0.0
             vx_poz(2)=0.0
             vx_poz(3)=5.45+0.1*uran(3)
*
      elseif(itg_type(3).eq.2.and.itg_type(2).eq.0.and.ivx_e2.ne.0)then
             vx_poz(1)=0.0
             vx_poz(2)=0.0
             vx_poz(3)=5.465+0.07*uran(3)
*
      elseif(itg_type(3).eq.56.and.itg_type(2).eq.0.and.ivx_e2.ne.0)then
             vx_poz(1)=0.0
             vx_poz(2)=0.0
             vx_poz(3)=5.4925+0.015*uran(3)

             endif
             return
             end
*========================================================
*==========================================================
      subroutine ffky_tg_e2


c  include files:
c  --------------
#include "ffpar.inc"
#include "gsimpar.inc"
#include "gsimcontrol.inc"
#include "gckine.inc"
#include "ffpar_tg_e2.inc"


      call ffkey('TGE2', tg_e2,1,'LOGI')
      tg_e2=.false.
c
c     define the tg_e2.type dependece to  run period
      call ffkey('TGTP',itg_type,3,'INTE')
c
      call ffkey('VERT_E2',ivx_e2,1,'INTE')
      ivx_e2=1

      return
      end

*============================================================


     






















