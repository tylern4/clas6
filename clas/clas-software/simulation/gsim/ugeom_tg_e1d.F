      SUBROUTINE ugeom_tg_e1d

c     == E1D target implementation ==
c     == Implemented simple target for GSIM tests ==
c     == Author: G. Gavalian ==
C     == Date  : 02/19/2002  ==
C
c ===============================================================================
C - This simple implementation turned to get more complicated and 
C   in more details than I expected at the begginning so I might as well add
C   it to CVS.
C
C   Gagik Gavalian (02/22/2002)
C================================================================================
C
C

#include "gsimpar.inc"
#include "gsimcontrol.inc"
#include "gcbank.inc"
#include "clmate.inc"
#include "cltmed.inc"

      REAL g_dens_mylar, g_dens_cerex
      REAL ins_len,hsend_pos

      REAL TINS_LEN
      REAL TINS_RAD
      REAL TINSUL_RAD_CEREX, TINSUL_RAD_MYLAR
      DIMENSION TBCLAS(3),TLIQTAR(3),THEATSIDE(3),THEATEND(3)
      DIMENSION TINSUL_CEREX(3),TINSUL_MYLAR(3),TALLWALL(3)
      DIMENSION TTIP_GEOM(5),TEND_WALL(3),TREF_FOIL(3),TBACK_WALL(3)

      DATA g_dens_mylar/1.387/
      DATA g_dens_cerex/1.18/

        DATA TBCLAS/0.,4.,6./
        DATA TLIQTAR/0.,0.982,2.5/
        DATA THEATSIDE/0.983,1.0,2.5/
        DATA TALLWALL/0.,0.,0./
        DATA THEATEND/0.,1.0,0.25/

      

c      ins_len = (0.001/g_dens_mylar)*5
c      THEATSIDE(2) = THEATSIDE(1)+ins_len
c      THEATEND(3)  = ins_len/2.
c      hsend_pos    = 2.5+THEATEND(3)

          TINS_RAD = 0.982
          TINS_LEN = 5.0
          TTIP_LEN = 0.312

          TLIQTAR(1) = 0.
          TLIQTAR(2) = TINS_RAD
          TLIQTAR(3) = TINS_LEN/2. - TTIP_LEN/2.
          
          TALLWALL(1) = TINS_RAD
          TALLWALL(2) = TINS_RAD+(0.002*2.54)
          TALLWALL(3) = TLIQTAR(3)
          
          TINSUL_RAD_CEREX = (0.001/g_dens_cerex)*5
          TINSUL_RAD_MYLAR = (0.00088/g_dens_mylar)*5
          
          TINSUL_CEREX(1) = TALLWALL(2)
          TINSUL_CEREX(2) = TINSUL_CEREX(1)+ TINSUL_RAD_CEREX
          TINSUL_CEREX(3) = 6.3/2.
          
          TINSUL_MYLAR(1) = TINSUL_CEREX(2)
          TINSUL_MYLAR(2) = TINSUL_MYLAR(1) + TINSUL_RAD_MYLAR
          TINSUL_MYLAR(3) = 6.3/2.

      WRITE(*,*) 
      WRITE(*,*) 
      WRITE(*,*) 
      WRITE(*,*) "INITIALIZING e1d TARGET "
      WRITE(*,*) "====================================="
      WRITE(*,*) "Insulation Rad = ", ins_Len, "  END: ",hsend_pos
      WRITE(*,*)
      WRITE(*,*)



      CALL GSVOLU('VOLM','TUBE',clas_med_vac,    TBCLAS,  3,IVOLM) 
      CALL GSVOLU('LIQT','TUBE',clas_med_h2liq,  TLIQTAR,   3,IVOLH)
      CALL GSVOLU('ALLW','TUBE',clas_med_alu,TALLWALL, 3,IVOLH)
      CALL GSVOLU('INSC','TUBE',clas_med_mylar,TINSUL_CEREX, 3,IVOLH)
      CALL GSVOLU('INSM','TUBE',clas_med_mylar,TINSUL_MYLAR, 3,IVOLH)

c======== Creating the TIP of the target =======================

c  FIRST Segment of tip ========================================

      TTIP_RADIUS  = 0.104*3
      TTIP_GEOM(1) = 0.104/2.
      TTIP_GEOM(2) = 0.
      TTIP_GEOM(3) = TINS_RAD
      TTIP_GEOM(4) = 0.
      TTIP_GEOM(5) = 0.67+sqrt(TTIP_RADIUS**2-0.104**2)
      
      CALL GSVOLU('LT01','CONE',clas_med_h2liq,TTIP_GEOM,5,IVOLH)
      TTIP_GEOM(2) = TTIP_GEOM(3)
      TTIP_GEOM(4) = TTIP_GEOM(5)
      TTIP_GEOM(3) = TTIP_GEOM(3) + (0.002*2.54)
      TTIP_GEOM(5) = TTIP_GEOM(5) + (0.003*2.54)
      CALL GSVOLU('TIP1','CONE',clas_med_alu,TTIP_GEOM,5,IVOLH)

c  Second Segment of TIP =======================================

      TTIP_GEOM(1) = 0.104/2.
      TTIP_GEOM(2) = 0.
      TTIP_GEOM(3) = 0.67 + sqrt(TTIP_RADIUS**2-0.104**2)
      TTIP_GEOM(4) = 0.
      TTIP_GEOM(5) = 0.67 + sqrt(TTIP_RADIUS**2-0.208**2)
      CALL GSVOLU('LT02','CONE',clas_med_h2liq,TTIP_GEOM,5,IVOLH)
      TTIP_GEOM(2) = TTIP_GEOM(3)
      TTIP_GEOM(4) = TTIP_GEOM(5)
      TTIP_GEOM(3) = TTIP_GEOM(3) + (0.003*2.54)
      TTIP_GEOM(5) = TTIP_GEOM(5) + (0.003*2.54)
      CALL GSVOLU('TIP2','CONE',clas_med_alu,TTIP_GEOM,5,IVOLH)

c  Third Segment of TIP =======================================
      TTIP_GEOM(1) = 0.104/2.
      TTIP_GEOM(2) = 0.
      TTIP_GEOM(3) = 0.67 + sqrt(TTIP_RADIUS**2-0.208**2)
      TTIP_GEOM(4) = 0.
      TTIP_GEOM(5) = 0.67
      CALL GSVOLU('LT03','CONE',clas_med_h2liq,TTIP_GEOM,5,IVOLH)
      TTIP_GEOM(2) = TTIP_GEOM(3)
      TTIP_GEOM(4) = TTIP_GEOM(5)
      TTIP_GEOM(3) = TTIP_GEOM(3) + (0.003*2.54)
      TTIP_GEOM(5) = TTIP_GEOM(5) + (0.002*2.54)      
      CALL GSVOLU('TIP3','CONE',clas_med_alu,TTIP_GEOM,5,IVOLH)

c This is the end wall of the target cell

      TBACK_WALL(1) = 0.
      TBACK_WALL(2) = 0.982
      TBACK_WALL(3) = 0.000015/2.
      CALL GSVOLU('BCKW','TUBE',clas_med_alu,TBACK_WALL,3,IVOLH)
      TEND_WALL(1) = 0.
      TEND_WALL(2) = 0.67
      TEND_WALL(3) = 0.000014/2.
      CALL GSVOLU('ENDW','TUBE',clas_med_alu,TEND_WALL,3,IVOLH)
      TREF_FOIL(1) = 0.
      TREF_FOIL(2) = 0.982
      TREF_FOIL(3) = 0.0000125/2.
      CALL GSVOLU('REFF','TUBE',clas_med_alu,TREF_FOIL,3,IVOLH)

c      CALL GSVOLU('HITS','TUBE',clas_med_mylar ,  THEATSIDE,   3,IVOLH)
c      CALL GSVOLU('HITE','TUBE',clas_med_mylar ,  THEATEND,   3,IVOLH)



      CALL GSPOS('VOLM' ,1, 'GAS2' ,0. ,0. , -5.07   ,0, 'ONLY')
      TTAR_CENTER_POS = TLIQTAR(3) - 2.5

      CALL GSPOS('LIQT',1,'VOLM',0.,0., TTAR_CENTER_POS ,0,'ONLY')
      CALL GSPOS('ALLW' ,1, 'VOLM' ,0. ,0. , TTAR_CENTER_POS,0, 'ONLY')
      CALL GSPOS('INSC' ,1, 'VOLM' ,0. ,0. , 0.65   ,0, 'ONLY')
      CALL GSPOS('INSM' ,1, 'VOLM' ,0. ,0. , 0.65   ,0, 'ONLY')
      TSEG_POS = TTAR_CENTER_POS +  TLIQTAR(3) + 0.104/2.
      CALL GSPOS('LT01',1,'VOLM',0.,0., TSEG_POS ,0,'ONLY')
      CALL GSPOS('TIP1',1,'VOLM',0.,0., TSEG_POS ,0,'ONLY')
      TSEG_POS = TSEG_POS + 0.104
      CALL GSPOS('LT02',1,'VOLM',0.,0., TSEG_POS ,0,'ONLY')
      CALL GSPOS('TIP2',1,'VOLM',0.,0., TSEG_POS ,0,'ONLY')
      TSEG_POS = TSEG_POS + 0.104
      CALL GSPOS('LT03',1,'VOLM',0.,0., TSEG_POS ,0,'ONLY')
      CALL GSPOS('TIP3',1,'VOLM',0.,0., TSEG_POS ,0,'ONLY')
      TSEG_POS = 2.5+TEND_WALL(3)
      CALL GSPOS('ENDW',1,'VOLM',0.,0., TSEG_POS ,0,'ONLY')
      TSEG_POS = TREF_FOIL(3)+3.8
      CALL GSPOS('REFF',1,'VOLM',0.,0., TSEG_POS ,0,'ONLY')
      TSEG_POS = -2.5-TBACK_WALL(3)
      CALL GSPOS('BCKW',1,'VOLM',0.,0., TSEG_POS ,0,'ONLY')
      


      CALL GSORD('VOLM',3)
      CALL GPVOLU(0)               ! PRINTOUT VOLUE PARAMETERS
C----------------------------------------------------------------
      CALL GGCLOS

      RETURN
      END
