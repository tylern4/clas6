c  ffpar_tg.inc

c  $Id: ffpar_tg.inc,v 1.14 2009/12/22 15:05:54 natasha Exp $

      character*132 ciffpar_tg
      data ciffpar_tg/
     1'$Id: ffpar_tg.inc,v 1.14 2009/12/22 15:05:54 natasha Exp $'
     1/

*------------------------------------------------------------------------
*                 FFREAD cards for cryotargets:
*  TARGET <type> [<cell_flag>]
*  TGMATE <mate> [<tg2_mate>]
*  TGPOS   <x> <y> <z>
*  TGINS   <tg_ins_ff(1)> ... <tg_ins_ff(6)> 
*  TG2MATE <tg2_mate>
*  TG2POS  <x> <y> <z>
*  TGUSER  <tg_user_ff(1)> ... <tg_user_ff(6)>
*  TGCUTS  <gamma>  <electron>  <neutral hadr>  <charged had>  <muon>
*
*------------------------------------------------------------------------
*   target length (total length incl. possible spherical parts) and radius,
*   cell wall material, thickness, etc ....
*   to make it easier:  
* (1) target type: number OR name OR a run number during this running period) 
*                  -> predefined values (extracted from drawings)
* (2) target material:  (default: predefined by target type;
*                        but can be changed by card: tg_mate_ff )
* (3) flag to switch on/off target cell walls and superinsulation (def.: ON)
*     or define target wall material
* (4) position of target center           (def.: x=0.0 ; y=0.0 ; z=0.0)
* (5) superinsulation type and thickness  (def.: predefined by target type)
* (6) material and position of secondary target (for e2,e5, foils,...)
* (7) user geometry: array to set a user defined geometry, etc
* (8) kinetic energy cuts for tracking media in target area (in GeV)
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
*   9   4    Kapton,127mu   Kapton,127mu   180.0mm 27.8mm    'g8a','g6c'
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
* 10-13 2    e2 targets
*   5   3    e5 target
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
* (a) flag for switching on/off target cell walls and superinsulation
*       flag:    'ON  ' , 'OFF ' , 'NONE' , 'CELL' , 'INS '
*       number:   -1        0         0        -2     -3
*               no cell wall^^        cell only^^     ^^superins. only
* (b) or the user can specify the target wall material:
*       'name'  and  'mate'  as above ( mate > 130 )
*
*------------------------------------------------------------------------
* (4) tg_pos_ff:   position of target center (x,y,z)
* --------------
*
*------------------------------------------------------------------------
* (5) tg_ins_ff:   array to define type and thickness of superinsulation
* --------------
*          tg_ins_ff(1)=type: xxx.yyy 
*                   xxx='mate#' for standard tube/cone ca. 1" from target cell
*                   yyy='mate#' for layers tightly surrounding the target
*          tg_ins_ff(2)=thickness of material 'xxx' (radial superins.)
*                                  packed as solid block!
*          tg_ins_ff(3)=thickness of material 'xxx' (axial superins.)
*                                  packed as solid block!
*          tg_ins_ff(4)=thickness of material 'yyy'
*                                  packed as solid block!
*          tg_ins_ff(5)=distance betw. target cell and radial superinsulation
*          tg_ins_ff(6)=distance betw. target cell and axial superinsulation
*   defined mate#: 161= kapton
*                  162= mylar
*                  163= aluminized mylar
*                  164= cerex
*                  165= cerex/aluminum (rel.density 27:35)
*                  166= cerex/aluminum (rel.density 15:14)
*                  167= cerex/aluminized mylar (rel.density 25:22)
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
*             'FE56','CH2 ','C12 ','KAPT','MYLA','ALMY',etc
*        (b) except for e5-target (2 cryo targets):  material='HYDR','DEUT'
*
*       tg2_pos_ff:  position (wrt. center of primary target)
*                    size specified via tg_user_ff(1) and tg_user_ff(2)
*
*------------------------------------------------------------------------
* (7) tg_user_ff:  user defined target geometry: 
* ---------------
*        tg_user_ff(1)=length
*        tg_user_ff(2)=diameter 
*        tg_user_ff(3)=thickness of cell wall
*        tg_user_ff(4)=target group (depends on cell shape and support struct.)
*        tg_user_ff(5) not used 
*        tg_user_ff(6) not used 
*
*------------------------------------------------------------------------
* (8) tg_tmedcuts_ff: overwrite GEANT cuts for tracking media in target area
*        tg_tmedcuts_ff(1) =min. kin. energy for gammas    (in GeV)
*        tg_tmedcuts_ff(2) =min. kin. energy for electrons
*        tg_tmedcuts_ff(3) =min. kin. energy for neutral hardons
*        tg_tmedcuts_ff(4) =min. kin. energy for charged hardons
*        tg_tmedcuts_ff(5) =min. kin. energy for muons
*
*------------------------------------------------------------------------
* (9) tg_scatz_ff:  Addition to length of new scattering chamber which is used
*                   with new start counter and beam pipe and g11 target.
*                   (the default value of 0.0 is to g11 specs)
*                   A value of 1.0 means 1cm longer than g11's scattering chamber,
*                   where the added length is at the downstream end of the chamber
*
*------------------------------------------------------------------------

      integer  tg_type_ff, tg_mate_ff, tg_cell_ff, tg2_mate_ff,ivx_e5,check_finger
      integer  ivx_eg2(1)
      logical ivx_eg6
      real     tg_pos_ff(3), tg_ins_ff(6), tg2_pos_ff(3), tg_user_ff(6)
      real     tg_tmedcuts_ff(5), tgpar_save(12), tg_scatz_ff
      real     teg2_pos_ff(3)

      COMMON/ffpar_tg/tg_type_ff,tg_mate_ff,tg_cell_ff,tg2_mate_ff,ivx_e5,
     &                ivx_eg2,tg_pos_ff,tg_ins_ff,tg2_pos_ff,tg_user_ff,
     &                tg_tmedcuts_ff,tgpar_save,tg_scatz_ff,check_finger,
     &                teg2_pos_ff,ivx_eg6
