      subroutine ffky_tg

c_begin_doc
c  Documentation for subroutine ffky_tg
c
c  $Id: ffky_tg.F,v 1.15 2009/12/22 14:58:24 natasha Exp $
c
c  Purpose:
c  --------
c	ffkey defs for cryo target
c
*------------------------------------------------------------------------
*   target length (total length incl. possible spherical parts) and radius,
*   cell wall material, thickness, etc ....
*   to make it easier:  
* (1) target type: number OR name OR a run number during this running period) 
*                  -> predefined values (extracted from drawings)
* (2) target material:  (default: predefined by target type;
*                        but can be changed by card: tg_mate_ff )
* (3) flag to switch on/off target cell walls and superinsulation (def.: ON)
*     or material definition for cell wall
* (4) position of target center           (def.: x=0.0 ; y=0.0 ; z=0.0)
* (5) superinsulation type and thickness  (def.: predefined by target type)
* (6) material and position of secondary target (for e2,e5, foils,...)
* (7) user geometry: array to set a user defined geometry, etc
* (8) kin. energy cuts for tracking media in target area
*
*------------------------------------------------------------------------
* (1) tg_type_ff:     target type number (#) 
* ---------------
*  tg_type_ff may be specified by number or name ("run_period")
*
******************************
*  (I) target cells for photon beam:   (length=total length incl. end caps)
*      -  Saclay target cells  (#1-5)
*      -  Jlab target cells    (#6-9)
*              (if targets only differ in length/material etc, 
*               they are subsummized under the same group)  
*
*   #  group  cell wall      end cap     length    diameter   names
*  -1   0    user defined  (cylindrical shape)
*   1   1    Mylar,170mu    Mylar,170mu    180mm   59.7mm  'g1a','g1b',g6a',g6b'
*   2   1    Mylar,170mu    Mylar,170mu    100mm   59.7mm  
*   3   2    Kapton,120mu   Kapton,120mu   168mm   43.0mm  
*   4   2    Kapton,120mu   Kapton,120mu   157mm   43.0mm  
*   5   2    Kapton,120mu   Kapton,120mu   107mm   43.0mm  
*   6   3    Mylar,127mu    Mylar,127mu    100.0mm 40.0mm    'g2a'    
*   7   3    Kapton,127mu   Kapton,127mu   180.0mm 40.0mm    'g1c'
*   8   3    Kapt/Alu,241mu Kapt/Alu,241mu 180.0mm 40.0mm    'g3a'
*   9   3    Kapton,127mu   Kapton,127mu   180.0mm 27.8mm    'g8a','g6c'
*         (end caps have Alu,15mu foils in the center d=5mm)
*
******************************
*  (II) targets for electron beam:  
*                           NOTE: these numbers are not confirmed!! (fk 4/99)
*   #  group  cell wall      end cap       length  diameter  names
*  -1   0    user defined  (cylindrical shape)
*   1   1    Kapton,120mu   Kapton,120mu    50mm    8.6mm    'e1a'
*   2   1    Alu,120mu      Alu,120mu       38mm    8.6mm    'e1b1'
*   3   1    Alu,120mu      Alu,120mu       38mm    8.6mm    'e1b2'
*   5   3    e5 target
* 10-13 2    e2 targets
*
*------------------------------------------------------------------------
* (2) tg_mate_ff:     target material  (either 'name' or 'mate'-number:)
* ---------------
*  name: 'HYDR','DEUT','PROT','NEUT','HEL3','HEL4','C12 ','CH2 ','FE56',
*  mate:   111 ,  112 ,  111 ,  112 ,  113 ,  114 ,  122 ,  123 ,  131 ,
*
*  name:  'AL  ','CU  ','LEAD','TUNG','STST','KAPT','MYLA','ALMY','CERX',
*  mate:    132 ,  133 ,  134 ,  135 ,  136 ,  161 ,  162 ,  163 ,  164 ,
*                         tungsten^^ , ^^STainless STeel  , ^^aluminized mylar
*  name: 'ALSE','FOAM','HEL ','AIR ','VAC '
*  mate:   171 ,  173 ,  103 ,  102 ,  101 
*   ALumaSEal^^, ^^polystyrene foam
*
*  NOTE: cryotargets only allow liquids and gases/vac.  (i.e. 100<mate<120);
*        solids are only allowed in special cases (like e2, user def.target)
*        and as second target (see below)
*
*------------------------------------------------------------------------
* (3) tg_cell_ff: 
* ---------------
*  (a)  flag for switching on/off target cell walls and superinsulation
*           flag:     'ON  ' , 'OFF ' , 'NONE' , 'CELL' , 'INS '
*           number:    -1         0        0        -2     -3
*                   no cell wall^^        cell only^^     ^^superins. only
*  or (b) material definition for cell wall:
*           'name' or 'mate' as above  (mate > 130)
*
*------------------------------------------------------------------------
* (4) tg_pos_ff:   position of target center (x,y,z)
* --------------
*
*------------------------------------------------------------------------
* (5) tg_ins_ff:   array to define type and thickness of superinsulation
* --------------
*          tg_ins_ff(1)=type: xxx.yyy 
*                   xxx='mate#' for standard tube,cone ca. 1" from target cell
*                   yyy='mate#' for layers tightly surrounding the target
*          tg_ins_ff(2)=thickness of material 'xxx' (radial superins.)
*          tg_ins_ff(3)=thickness of material 'xxx' (axial superins.)
*          tg_ins_ff(4)=thickness of material 'yyy'
*          tg_ins_ff(5)=distance betw. target cell and radial superinsulation
*          tg_ins_ff(6)=distance betw. target cell and axial superinsulation
*   defined mate#: 161=kapton
*                  162= mylar
*                  163= aluminized mylar
*                  164= cerex
*                  165= cerex/aluminum (rel.density 27:35)
*                  166= cerex/aluminum (rel.density 15:14)
*                  167= cerex/aluminized mylar (rel.dens. 25:22)
*                  171= alumaseal: mylar/aluminum/glue (rel.dens. 18:35:9)
*                  172= composite alumaseal/kapton/epoxy (rel.dens.89:62:35)
*                  173= polystyrene foam
*
*------------------------------------------------------------------------
* (6)  cards  tg2_xxx  allow to define a second target:
*     tg2_mate_ff:   material of target2
*     tg2_pos_ff:    position of target2 center (x,y,z)
* ---------------
*       tg2_mate_ff:  material name or number:
*        (a) generally solid targets: 
*             'FE56','CH2 ','C12 ','AL  ','CU  ','KAPT','MYLA','ALMY',etc
*        (b) except for e5-target (2 cryo targets):  material='HYDR','DEUT'
*
*       tg2_pos_ff:  position (wrt. center of primary target)
*                    size specified via tg_user_ff(1) and tg_user_ff(2)
*
*------------------------------------------------------------------------
* (7) tg_user_ff:  user defined target geometry: 
* ---------------
*          tg_user_ff(1)=length 
*          tg_user_ff(2)=diameter  
*          tg_user_ff(3)=thickness of cell walls  
*          tg_user_ff(4)=target group (given by cell shape and support struct.)
*          tg_user_ff(5) not used
*          tg_user_ff(6) not used
*
*------------------------------------------------------------------------
* (8) tg_tmedcuts_ff: cuts for tracking media in target area  (see /GCCUTS/)
*          tg_tmedcuts_ff(1)=kin.energy cut for gammas        (cuts in GeV)
*          tg_tmedcuts_ff(2)=kin.energy cut for electrons
*          tg_tmedcuts_ff(3)=kin.energy cut for neutral hadrons
*          tg_tmedcuts_ff(4)=kin.energy cut for charged hadrons
*          tg_tmedcuts_ff(5)=kin.energy cut for muons
*
*------------------------------------------------------------------------
c
c  Author:
c  -------
c     Franz Klein, March 99

c  Major revisions:
c  ----------------
c     F.Klein, Feb.01:  add ffkey for superinsulation 
c_end_doc

      implicit none
      character*(*) cfile,crevis,cstate,cdate,cautho,crname,crauth
      character*132 crcsid
      parameter (cfile=  '$RCSfile: ffky_tg.F,v $')
      parameter (crevis= '$Revision: 1.15 $')
      parameter (cstate= '$State: Exp $')
      parameter (cdate=  '$Date: 2009/12/22 14:58:24 $')
      parameter (cautho= '$Author: natasha $')
      parameter (crname= 'ffky_tg')
      parameter (crauth= 'Franz Klein')
      data crcsid/'
     1$Id: ffky_tg.F,v 1.15 2009/12/22 14:58:24 natasha Exp $
     1 '/
      save

c  include files:
c  --------------
#include "ffpar_tg.inc"
c_end_inc

c  local variables:
c  ----------------
c_end_var

c  executable code:
c  ----------------
*
* (1) target type:
*       NOTE:  the card TARGET can cover: tg_type_ff,tg_cell_ff
* (2) flag to switch on/off cell walls and superinsulation
*
      tg_type_ff=1              !default values for e1/g1 in 1998
      tg_cell_ff=-1
      call ffkey('TARGET',tg_type_ff,2,'MIXE')

*----------------------------
* (3) target material
*       NOTE:  the card TGMATE covers both values: tg_mate_ff,tg2_mate_ff
*
      tg_mate_ff=0              !take default value from tg_type_ff
      tg2_mate_ff=0             !no second target
      call ffkey('TGMATE',tg_mate_ff,2,'MIXE')
      call ffkey('TG2MATE',tg2_mate_ff,1,'MIXE')

*----------------------------
* (4) target position (x,y,z)
*
      tg_pos_ff(1)=0.0
      tg_pos_ff(2)=0.0
      tg_pos_ff(3)=0.0
      call ffkey('TGPOS',tg_pos_ff,3,'REAL')

*----------------------------
* (5) superinsulation  [usually predefined]
*
      tg_ins_ff(1)=-1.0         !take default value from tg_type_ff
      tg_ins_ff(2)=-1.0
      tg_ins_ff(3)=-1.0
      tg_ins_ff(4)=-1.0
      tg_ins_ff(5)=-1.0
      tg_ins_ff(6)=-1.0
      call ffkey('TGINS',tg_ins_ff,6,'REAL')

*----------------------------
* (6) second target - option for e5-target and additional solid target
*     position (x,y,z)  (wrt. center of primary target)
      tg2_pos_ff(1)=0.0
      tg2_pos_ff(2)=0.0
      tg2_pos_ff(3)=0.0
      call ffkey('TG2POS',tg2_pos_ff,3,'REAL')
      call ffkey('VERT_E5',ivx_e5,1,'INTE')
      ivx_e5=1
c
      call ffkey('VEG6',ivx_eg6,1,'LOGI')
      ivx_eg6=.false.
          
c
      teg2_pos_ff(1)=0.0
      teg2_pos_ff(2)=0.0
      teg2_pos_ff(3)=-25.0
      call ffkey('TEG2POS',teg2_pos_ff,3,'REAL')
      call ffkey('VEG2',ivx_eg2(1),1,'INTE')
*      ivx_eg2=1
      print *, 'VOW =',ivx_eg2(1)

*----------------------------
* (7) user defined target volume
*     (length, diameter, wall thickness, dist. to superins., support structure)
*       NOTE:  these values are taken instead of 'tg_type_ff' 
*                   if tg_user_ff(1)>0.0 && tg_user_ff(2)>0.0
*
      tg_user_ff(1)=0.0              !target length
      tg_user_ff(2)=0.0              !target diameter
      tg_user_ff(3)=0.0              !target wall thickness
      tg_user_ff(4)=0                !target group (defines shape of
                                     !       target cell and support struct.) 
      tg_user_ff(5)=0                !not used
      tg_user_ff(6)=0                !not used
      call ffkey('TGUSER',tg_user_ff,6,'REAL')

*
*----------------------------
* (8)  kin. energy cuts for tracking media in target area
*
      tg_tmedcuts_ff(1)=0.001          ! for gammas
      tg_tmedcuts_ff(2)=0.001          ! for electrons
      tg_tmedcuts_ff(3)=0.005          ! for neutral hadrons
      tg_tmedcuts_ff(4)=0.005          ! for charged hadrons
      tg_tmedcuts_ff(5)=0.001          ! for muons
      call ffkey('TGCUTS',tg_tmedcuts_ff,5,'REAL')
*
*----------------------------
* (9) addition to scattering chamber length (added at downstream end) for g11 target
*     does not affect target position, default value of 0.0 is for g11 specs
      tg_scatz_ff = 0.0
      call ffkey("SCHDLEN",tg_scatz_ff,1,'REAL')
*
*----------------------------
*   target parameters saved during geometry definition
      tgpar_save(1)=0                !target length (w/o cell wall)
      tgpar_save(2)=0                !target radius (max. value w/o cell wall)
      tgpar_save(3)=0                !target wall thickness
      tgpar_save(4)=0                !target end cap thickness (inlet window)
      tgpar_save(5)=0                !material id for cell wall
      tgpar_save(6)=0                !position of upstream end of target cell
                                     !                     in volume 'TG' 
      tgpar_save(7)=0                !position of upstream end of targ.cell
      tgpar_save(8)=0                !upstream end of complete target structure
      tgpar_save(9)=0                !downstream end of complete target struct.
      tgpar_save(10)=0               !max. radius of complete target struct.
                         ! tgpar_save(7)-(10) wrt.center of scattering chamber
      tgpar_save(11)=0               !not used
      tgpar_save(12)=0               !z-offset of scattering chamber wrt.CLAS center
*----------------------------
*   field maximum and tracking routine (GRKUTA or GHELIX)
*   in target volumes defined in 'ffky_mg.F'
 
      return
      end

c------------------------------------------------------------------------------
