      subroutine ugeom_tg_eg6


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
#include "ffpar_mg.inc"
       
        integer get_rotm
        DIMENSION TBVAC(3),TBWIND(3),TBCELL(3),TBTGHe(3)
        DIMENSION TBCHHe(3),TBCHW(3)

        DIMENSION BPCON (12),WBPC1(15),WBPC2(21)
        DIMENSION TBWBAS (3),TBCAP(3), TBCHe(3),TBHeW(3) 
        DIMENSION HWAPC(15)
        DIMENSION TBEPD1(3),TBEPD2(3),TBEPD3(3),TBEPD4(3)
c          
        DATA TBVAC/0.,12.,31.775/           !mother volum (vacuum)
c         DATA TBVAC/0.,12.,50.0/ 
        DATA TBWIND/0.,0.3,0.001/           !inlet&exit wind.(aluminum)
        DATA TBWBAS/0.303, 0.46, 0.15/      !exit wind.base torlon
        DATA TBCELL/0.3, 0.303, 14.615/     !target cell (kapton)
        DATA TBTGHe/0., 0.3, 14.615/        !target gas (He4 6 atm)
        DATA TBCHHe/0.303,2.15, 14.615/     !Sc.chamber gas (He4 1atm)
        DATA TBCHW /2.15,2.1506,14.615/     !Sc.chamber wall (Al)
        DATA TBCAP/1.905,1.9075,18.06/      !Sc.chamber wall(kapton)
        DATA TBCHe/0., 1.905, 18.06/        !Sc.ch.cap.tube gas (He4 1atm)
        DATA TBHeW/0., 1.31, 0.0009/        !He. wind. (Al)
        DATA TBEPD1/1.995, 7.8994, 0.15875/ !end plate downstream (foam)
        DATA TBEPD2/3.2, 5.7912, 0.508/

        DATA BPCON /0., 360.,3,  -3.1, 0.303, 1.2, 
     &                            2.6, 0.303, 1.2,
     &                            3.1, 0.303, 0.925/      !Base
 
        DATA  WBPC1/0., 360.,4., -0.85, 0.303, 0.8,
     &                           -0.15, 0.303, 0.8, 
     &                           -0.149,0.303, 0.5,
     &                             0.85,0.303, 0.5/      !exit wind. accembly
c
        DATA  WBPC2/0., 360.,6., -0.85, 1.2, 1.99,
     &                           -0.63, 1.2, 1.99, 
     &                           -0.629,1.2, 2.15,
     &                           -0.15, 1.2, 2.15,
     &                           -0.149,1.7, 1.905,
     &                            0.85, 1.7, 1.905/

        DATA HWAPC/0., 360., 4., -0.35 ,  1.53, 1.905,
     &                            0.15 ,  1.53, 1.905,
     &                            0.1499, 1.31, 1.905,
     &                            0.35,   1.31, 1.905/    

        CALL GSVOLU('EG6M', 'TUBE', clas_med_vac    , TBVAC  ,  3, IV)
        CALL GSVOLU('VWIN', 'TUBE', clas_med_alu    , TBWIND ,  3, IV)
        call gsatt ('VWIN', 'COLO',5)
        CALL GSVOLU('VWBS', 'TUBE', clas_med_kapton , TBWBAS ,  3, IV)
        call gsatt ('VWBS', 'COLO',2)
        CALL GSVOLU('VCEL', 'TUBE', clas_med_kapton , TBCELL ,  3, IV)
        CALL GSVOLU('THE4', 'TUBE', clas_med_helium6, TBTGHe ,  3, IV)
        call gsatt ('THE4', 'COLO',4)
        CALL GSVOLU('CHE4', 'TUBE', clas_med_helium , TBCHHe ,  3, IV)
        call gsatt ('CHE4', 'COLO',3)
        CALL GSVOLU('VCHW', 'TUBE', clas_med_alu    , TBCELL ,  3, IV)
        call gsatt ('VCHW', 'COLO',6)
        CALL GSVOLU('VBAS', 'PCON', clas_med_alu    , BPCON  , 12, IV)
        call gsatt ('VBAS', 'COLO',6)
        CALL GSVOLU('WBP1', 'PCON', clas_med_kapton , WBPC1  , 15, IV)
        call gsatt ('WBP1', 'COLO',2)
        CALL GSVOLU('WBP2', 'PCON', clas_med_kapton , WBPC2  , 21,IV)
        call gsatt ('WBP2', 'COLO',2)
        CALL GSVOLU('WASC', 'TUBE', clas_med_kapton , TBCAP  ,  3,IV)
        CALL GSVOLU('CPHE', 'TUBE', clas_med_helium , TBCHe  ,  3,IV)
        CALL GSVOLU('WAPC', 'PCON', clas_med_kapton , HWAPC  , 15,IV)
        CALL GSVOLU('WINH', 'TUBE', clas_med_alu    , TBHeW  ,  3,IV)
        call gsatt ('WINH', 'COLO',3)
        CALL GSVOLU('ENP1', 'TUBE', clas_med_lowdfoam , TBEPD1,  3,IV15)      
        CALL GSVOLU('ENP2', 'TUBE', clas_med_lowdfoam , TBEPD2,  3,IV16)        
c
        CALL GSPOS('EG6M',1,'FOIL',0., 0.,soln_pos  , 0, 'ONLY')
        CALL GSPOS('VCHW',1,'EG6M',0., 0., 0.       , 0, 'ONLY' )
        CALL GSPOS('CHE4',1,'EG6M',0., 0., 0.       , 0, 'ONLY' )
        CALL GSPOS('VCEL',1,'EG6M',0., 0., 0.       , 0, 'ONLY' )
        CALL GSPOS('THE4',1,'EG6M',0., 0., 0.       , 0, 'ONLY' )
        CALL GSPOS('VWIN',1,'EG6M',0., 0., -14.6148 , 0, 'ONLY' )
        CALL GSPOS('VWIN',2,'EG6M',0., 0.,  14.6148 , 0, 'ONLY' )
        CALL GSPOS('VWBS',1,'EG6M',0., 0.,  14.465  , 0, 'ONLY' )
        CALL GSPOS('WBP1',1,'EG6M',0., 0.,  13.1135 , 0, 'ONLY' )
        CALL GSPOS('WBP2',1,'EG6M',0., 0.,  13.1135 , 0, 'ONLY' )
        CALL GSPOS('VBAS',1,'EG6M',0., 0., -11.515  , 0, 'ONLY' )
        CALL GSPOS('WASC',1,'EG6M',0., 0.,  31.175  , 0, 'ONLY' )
        CALL GSPOS('CPHE',1,'EG6M',0., 0.,  31.175  , 0, 'MANY' )
        CALL GSPOS('WAPC',1,'EG6M',0., 0.,  48.885  , 0, 'ONLY' )
        CALL GSPOS('WINH',1,'EG6M',0., 0.,  49.2341 , 0, 'ONLY' )
        CALL GSPOS('ENP1',1,'EG6M',0., 0.,  12.065  , 0, 'ONLY' )
        CALL GSPOS('ENP2',1,'EG6M',0., 0.,  11.39825, 0, 'ONLY' )
        
c
       CALL GSORD('EG6M',3)
       CALL GPVOLU(0)               ! PRINTOUT VOLUE PARAMETERS

       RETURN
       END


        subroutine vert_eg6(vx_poz)
c
c  include files:
c  --------------
#include "ffpar_tg.inc"
#include "ffpar_mg.inc"
c_end_inc
c
      real vx_poz(3)
      real uran(3)
       call grndm(uran,3)
c
       if(ivx_eg6)then
       
                         vx_poz(1)=0.0
                         vx_poz(2)=0.0
                         vx_poz(3)=(soln_pos-15.)+30.*uran(3)
c
          endif
      
             return
             end





