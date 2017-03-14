
       subroutine geom_tg
  
c_begin_doc
c  Documentation for subroutine geom_tg
c  
c  Purpose:
c  --------
c     ******************************************************************
c     *                                                                *
c     *             Defines the geometry for CLAS targets              *
c     *                                                                *
c     *                                                                *
c     ******************************************************************
c
c  Author:          F.Klein, March 99
c  Major revisions: F.Klein, Feb. 01
c                   N.Baltzell, Nov. 04 (New BeamPipe for New StartCounter)
c_end_doc



      implicit none
      character*(*) cfile,crevis,cstate,cdate,cautho,crname,crauth
      character*132 crcsid,crmess
      parameter (cfile=  '$RCSfile: geom_tg.F,v $')
      parameter (crevis= '$Revision: 1.31 $')
      parameter (cstate= '$State: Exp $')
      parameter (cdate=  '$Date: 2009/12/22 15:08:25 $')
      parameter (cautho= '$Author: natasha $')
      parameter (crname= 'GEOM_TG')
      parameter (crauth= 'fklein')
      data crcsid/'
     1$Id: geom_tg.F,v 1.31 2009/12/22 15:08:25 natasha Exp $
     1 '/
      save

c  include files:
c  --------------
#include "gcbank.inc"
#include "gcnum.inc"
#include "gsimpar.inc"
#include "gsimcontrol.inc"
#include "ffpar_st.inc"
#include "ffpar_tg.inc"
#include "ffpar_tg_e2.inc"
#include "ffpar_mg.inc"
#include "clmate.inc"
#include "cltmed.inc"
c_end_inc

*
*  external functions
*  ------------------

      integer  iucomp, get_tmed

*  local variables:
*  ----------------

      integer i, j
      character*4 cname
      integer ind, ind2, ind3   !for index of target material
      integer imat1, imat2      !for material#
      integer qbeam             !=-1: electron ; =0: photon
      data  qbeam / 99 /        !unknown
      integer icellflag         !flag to switch on/off wall+support volumes
      data icellflag / -1 /     !default=ON
      integer experiment        !specifies which dvcs target to use
      integer tgroup            !group of target types (given by cell shape
*                                                      and support structure)
* for photon beam: define beam pipe volume
*
      integer ivol              !volume#
      real parm(30)             !parameters for PCON volume
      integer tmed_pipe         !track.medium for carbon fiber pipe
      real tmaxfd, stmax, deemax, epsil, stmin    !track.media cuts 
*
*                                              (according to C.Marchand, Mar.99)
*  radius of carbon fiber tube at 5 planes perpend. to z:
*  1.plane upstream outside CLAS,
*  2.plane upstream of cone    (cone from rad=9.4 to rad=3.35 with opening 30 deg)
*  3.plane downstream of cone  (plane3=plane2+10.479)
*  4.plane downstream outside of CLAS

      real bpip_pln(4), bpip_rad(4), bpip_thick(4)
      data bpip_pln   / -300., 17.2, 27.679, 310. /
      data bpip_rad   /  9.6,  9.6,  3.55,  3.55 /
      data bpip_thick /  0.2,  0.2,   0.2,   0.2 /

*  new beam pipe for new start counter and g11 target (N.Baltzell, Nov.04)
*  *needs to be generalized for different length targets
*  analagous definitions to old beam pipe -- planes perpindicular to z
*  1.plane upstream outside of CLAS
*  2.plane at upstream end of cone
*  3.plane at downstream end of cone = upstream end of scattering chamber

************* fklein nov.05
* kludge to use the new start counter: extend beam pipe through target area
*  configuration NOT checked!

*      real bpip_newst_pln(3), bpip_newst_rad(3)
*      data bpip_newst_pln  / -300., -39.2, -27.88 /
*      data bpip_newst_rad  /  9.6,   9.6,   4.73 /
      real bpip_newst_pln(6), bpip_newst_rad(6)
      data bpip_newst_pln  / -300., -39.2, -31., 22., 29., 310./
      data bpip_newst_rad  /  9.6,   9.6,   8.0, 8.0, 1.1, 1.1 /
      real bpip_newst_thick
      data bpip_newst_thick /0.2/
*
******************************************************************
*
*  TARGET TYPES:
*  -------------     defined by index or run period (name)
*  
      integer  MAX_RUN_PERIODS
      parameter (MAX_RUN_PERIODS=44)
      integer  itg_runs(MAX_RUN_PERIODS)    !tg_type for run periods
                                            !different for electron/photon beam
      integer  irun_min(MAX_RUN_PERIODS)    !first run (clasprod_xxxxx), 
      integer  irun_max(MAX_RUN_PERIODS)    !last run (clasprod_xxxxx) when
                                            !       this target was in use
      character*4 ctg_runs(MAX_RUN_PERIODS) !target material for run period
      integer  h_runs(MAX_RUN_PERIODS)      !hollerith version of below
      character*4 ch_runs(MAX_RUN_PERIODS)  !name of run period
      data ch_runs  /'e1a ','e1b ','e1b1','e1b2','g1a ','g6a ','g1b ','e1c ',
     &           'e1c1','e1c2','e1c3','e2a ','e2a1','e2a2','e2a3','e2a4',
     &           'g6b ','g2a ','g1c ','g3a ','g3a1','g3a2','e1d ','e1d1',
     &           'e1d2','e5a ','g8a ','g6c ','e1-6','e16a','e1e ','e2b',
     &            'e6a','g2b','g10a','g11a','dvcs','eg2','eg6','egd1',
     &            'dv1a', 'dv1b','egd2','egd3'/
      data irun_min /  7690,  8625,  8625,  8625, 10911, 12013, 12841, 16047,
     &               16047, 16047, 16047, 17864, 17864, 18220, 18253, 18308,
     &              18801, 19846, 20850, 22033, 22033, 22416, 22828, 22828,
     &             23760, 24023, 30001, 31001, 30500, 30500, 35676,32828,
     &             31464, 37905, 42310, 43490,46500,40399,43490,58708,
     &             46500,57318,59221, 60221/ 
      data irun_max /  8100,  9558,  9558,  9558, 11915, 12683, 13438, 17840,
     &               17840, 17840, 17840, 18522, 18206, 18252, 18307, 18522,
     &              19745, 20760, 22002, 22547, 22402, 22402, 23968, 23745,
     &             23968, 24579, 31000, 30499, 31500, 31500, 36620,33993,
     &             32175, 37985, 43260, 44133,47455,42150,44133,59220,
     &             47455,58631,60184,60645/
      data itg_runs /   1  ,  1   ,  1   ,  1   ,  1   ,  1   ,  1   ,  2   ,
     &                 2  ,  3   ,  3   ,  10  ,  10  ,  11  ,  12  ,  13  ,
     &                1  ,  6   ,  7   ,  8   ,  8   ,  8   ,  15  ,  15  ,
     &              15  ,  5   ,  9   ,  9   ,  16 ,  16  ,  17  ,  18   ,
     &             19  ,  8   ,  10 ,   11 ,   25 ,  26 ,   27,    28   ,
     &             29 ,  30 , 31, 32/
      data ctg_runs /'HYDR','HYDR','HYDR','HYDR','HYDR','HYDR','HYDR','HYDR',
     &              'HYDR','HYDR','HYDR','HEL4','HEL4','HEL4','C12 ','HEL3',
     &             'HYDR','DEUT','HYDR','HEL3','HEL3','HEL4','HYDR','HYDR',
     &            'DEUT','DEUT','HYDR','HYDR','HYDR','HYDR', 'HYDR','HE3',
     &             'DEUT','DEUT','DEUT','HYDR','HYDR','DEUT','HEL4','NH3',
     &             'HYDR','HYDR','NH3','ND3'/
*
*  NOTES: short changes in run condition neglected: 
*           e1d: run#23863-23876='HYDR'(?)
*   dvcs and dv1a are the same experiment, but 'dvcs' calls original geometry
*   while 'dv1a' calls the new geometry file with minor bugs fixed 
*   (Daria's modifications). 'dv1b' is e1-dvcs-part2.
******************************************************************
*  (I) target cells for photon beam:   (length=total length incl. end caps)
*      -  Saclay target cells  (#1-5)
*      -  Jlab target cells    (#6-9)
*              (if targets only differ in length/material etc, 
*                                they belong to the same group)  
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
*   8   3    Kapt/Alu,241mu Kapt/Alu,241mu 180.0mm 40.0mm    'g3a','g2b'
*   9   4    Kapton,127mu   Kapton,127mu   180.0mm 27.8mm    'g8a','g6c'
*         (end caps have Alu,15mu foils in the center d=5mm)
*  10   5    Kapton,127mu   Alu,15mu       240mm    40mm     'g10a'
*  11   8    Kapton,127mu   Alu,15mu       400mm    40mm     'g11a'
*  12   6    Alu,241mu      Alu,241mu      50mm     15mm     'polt' 'froz' 
*          (frozen spin target - design)
*  13   6    KEL-F,620mu    Mylar,Alu      50mm    25.2mm   'HD  ' 'HICE' 'DICE' 
*          (HD target from BNL)
*  14   7   test for LPRD 
*
******************************
*  support structures (gas distribution pipes and base rings)
*   and superinsulation for photon targets
*   group#  insulation  for tg_type#
*      1     163.0 (?)      1-2     (Saclay target shape "cigare") 
*      2     163.0 (?)      3-5     (Saclay target shape "cuivre")
*      3     167.0 (?)      6-7     (for Jlab photon target)
*      3     165.164          8     (for Jlab 'g3' target)
*      4     165.0 (?)        9     (for Jlab 'g8' target)
*      5     167.0           10     (for Jlab 'g10a' target)
*      6     0.0             12     (for Jlab frozen spin target (design))
*      7     test for LPRD
*      8     167.0           11     (for JLab 'g11a' target)
*
      INTEGER MAX_PHTG_GROUPS
      DATA MAX_PHTG_GROUPS / 8 /

****************************************************************
*  (II) targets for electron beam:  
*                           NOTE: these numbers are not confirmed!! (fk 4/99)
*   #  group  cell wall      end cap       length  diameter  names
*  -1   0    user defined  (cylindrical shape)
*   1   1    Kapton,120mu   Kapton,120mu    50mm    8.6mm    'e1a','e1b'
*   2   2    Alu,120mu      Alu,120mu       38mm    8.6mm    'e1c'
* 10-13 3    e2 targets(N.Dashyan)
*   5   4    e5 target (N.Dashyan)
*  15   5    e1d target (G.Gavalian)
*  16   6    e1-6 target (S.Morrow)
*  17   7    e1e target (C. Smith)
*  18   8    e2b target (N.Dashyan)
*  19   9    e6a target (N.Dashyan)
*  25  10    dvcs target
*  26  11    eg2  target (H.Hakobyan)
*  27  12    eg6  target (N.Dashyan)
*  28        eg1-dvcs  target (S. Niccolai)
*  29  13    dvcs-e1a target (D. Sokhan modifications)
*  30  13    dvcs-e1b target (D. Sokhan)
*  31        eg1-dvcs-b  target (S. Niccolai)
*  31        eg1-dvcs-c  target (S. Niccolai)

      INTEGER MAX_ELTG_GROUPS
      DATA MAX_ELTG_GROUPS / 13 /

******************************
*  support structures (gas distribution pipes and base rings)
*   and superinsulation for electron targets
*   group#  insulation  for tg_type#
*      1     163.0 (?)      1-2     (for Jlab electron target d=8.6mm)
*      2     special  e2 targets
*      3     e5 target
*
******************************************************************
*
*  target material: specified in ffread card or via run period
*             NOTE: LAST 'material' MUST BE VACUUM
* 
      integer  MAX_TGMAT
      parameter  (MAX_TGMAT=37)
      character*4  ch_tg(MAX_TGMAT)          !material name (or no.) as defined
      integer      h_tg(MAX_TGMAT)           !hollerith of same as above
      integer      mate_tg(MAX_TGMAT)        ! in ffread card
      integer      idmate_tg(MAX_TGMAT)      ! geant mate id

      data ch_tg/'PROT','NEUT','HYDR','DEUT','H2  ','D2  ','LH2 ','LD2 ',
     &          'HEL3','HEL4','BUTA','NH3 ','HD  ','HICE','DICE',
     &         'C12 ','CH2 ','FE56','AL  ','CU  ','LEAD','TUNG','STST',
     &        'KAPT','MYLA','ALMY','CERX','CXAL','CXA2','CXMY',
     &       'ALSE','ALSC','FOAM','HEL ','AIR ','VAC ', 'ND3'/
      data mate_tg/ 111 , 112 ,  111 ,  112 ,  111 ,  112 ,  111 , 112 ,
     &            113 ,  114 ,  116, 118, 119, 120, 121,
     &           122 ,  123 ,  131 ,  132 ,  133 ,  134 ,  135 ,  136 , 
     &          161 ,  162 ,  163 ,  164 ,  165 ,  166 ,  167 ,
     &         171 ,  172, 173 ,  103 ,  102 ,  101, 180 /
*  ALSE=alumaseal, FOAM=polystyrene, STST=stainless steel
*
*  for cryotargets only liquid material, gases (helium,air) and 'VAC '(empty) 
*    is allowed (except e2 targets)  i.e.  100<mate_tg<120
*
******************************************************************
* in geom_HD_cryostat.F HD, H2ice, D2ice have medium # 29,30,31 (relative to 
*                                                              common offset)
      integer imate_hd,imate_h2ice,imate_d2ice
      data imate_hd,imate_h2ice,imate_d2ice /29, 30, 31/
******************************************************************
*
*  all material that may be used (esp. insulation material)
*  liquid (cryo) target material last
*
      integer MAX_MAT, MAX_INSMAT
      parameter (MAX_MAT=31, MAX_INSMAT=21)
      integer mymate(MAX_MAT), idmate(MAX_MAT)
      data mymate/ 122, 123, 131, 132, 133, 134, 135, 136, 
     &         161, 162, 163, 164, 165, 166, 167, 171, 172, 173,
     &        101, 102, 103, 111, 112, 113, 114, 116, 118, 119, 120, 121, 180/

******************************************************************
*
*  flag to switch on/off target cell walls and superinsulation
*
      integer  MAX_CELLFLAGS
      parameter (MAX_CELLFLAGS=5)
      integer  cell_flags(MAX_CELLFLAGS)     !flags for wall+support vol.
      integer  hcell_flag(MAX_CELLFLAGS)     ! as def. in ffread card
      data hcell_flag  /4HOFF ,4HNONE,4HON  ,4HCELL,4HINS /
      data cell_flags  /   0  ,   0  ,  -1  ,  -2  ,  -3  /
c         no wall/superins.^^    cell+wall only^^     ^^cell+insulation only
c_end_var


c  executable code:
c  ----------------
*
*  dummy conversion: character string -> hollerith
      do i=1,MAX_RUN_PERIODS
         call UCTOH(ch_runs(i),h_runs(i),4,4)
      enddo
      do i=1,MAX_TGMAT
         call UCTOH(ch_tg(i),h_tg(i),4,4)
      enddo
*
*  get target type (if user specified it by "name" or "run number")
*  ---------------
      i=0
      if(tg_type_ff.GE.-1 .AND. tg_type_ff.LE.MAX_RUN_PERIODS) goto 10
      if(tg_type_ff.GT.0 .AND. tg_type_ff.LE.irun_max(MAX_RUN_PERIODS)) then
        do i=MAX_RUN_PERIODS,1,-1
          if(tg_type_ff.GE.irun_min(i).AND.tg_type_ff.LE.irun_max(i)) goto 10
        enddo
        i=0
      else
        call UHTOC(tg_type_ff,4,cname,4)
        call CUTOL(cname)
        call UCTOH(cname,tg_type_ff,4,4)
        i=iucomp(tg_type_ff,h_runs,MAX_RUN_PERIODS)
      endif
 10   continue

*  get target type and beam charge from run period
      if(i.GT.0) then
        tg_type_ff=itg_runs(i)
        write(crmess,'(A,I2,3A,2(I6,A))') 'Target #',tg_type_ff,' chosen:  ',
     &    ch_runs(i),' (for run',irun_min(i),' to',irun_max(i),')'
        call recmes(crname,'I',crmess)
        if(tg_mate_ff.eq.0 .OR. tg_mate_ff.eq.-1)
     &                  call UCTOH(ctg_runs(i),tg_mate_ff,4,4)
        if(ch_runs(i)(1:1).eq.'e' .OR. ch_runs(i)(1:1).eq.'d') then
          qbeam=-1
          nogeom(st)=.TRUE.             !no start counter for electron beam
        elseif(ch_runs(i)(1:1).eq.'g') then
          qbeam=0
          nogeom(foil)=.TRUE.           !"foil" is beampipe material for e-beam
        endif
      endif
*  else assume that electron beam is chosen if no ST geometry defined
      if(qbeam.gt.0) then
        if(nogeom(st)) then
          qbeam=-1
        else
          qbeam=0
          nogeom(foil)=.TRUE.          !"foil" is beampipe material for e-beam
        endif
      endif
*
*  special case: user defined target geometry (volume: tube)
      if(tg_user_ff(1).GT.0.0 .AND. tg_user_ff(2).GT.0.0) tg_type_ff=-1

********************************************
*  stop here if 'tg_type_ff' not set       *
*                                          *
      if(tg_type_ff.eq.0) then
        call recmes(crname,'W','Target type not defined -> no target geometry')
        return
      endif
*                                          *
********************************************
*
*  define target group (given by cell shape and type of support structure)

      tgroup=0
* electron targets: 
      if(qbeam.EQ.-1) then
        if(tg_type_ff.EQ.-1) then
          if(tg_user_ff(4).GT.0) then
            tgroup=tg_user_ff(4)
            if(tgroup.LT.1 .OR. tgroup.GT.MAX_ELTG_GROUPS) tgroup=0
          endif
        elseif(tg_type_ff.GE.1 .AND. tg_type_ff.LE.1) then
          tgroup=1
        elseif(tg_type_ff.GE.2 .AND. tg_type_ff.LE.3) then
          tgroup=2
        elseif(tg_type_ff.GE.10 .AND. tg_type_ff.LE.13) then
          tgroup=3
        elseif(tg_type_ff.EQ.5) then
          tgroup=4
        elseif(tg_type_ff.EQ.15) then
          tgroup=5
        elseif(tg_type_ff.EQ.16) then
          tgroup=6
        elseif(tg_type_ff.EQ.17) then
          tgroup=7
        elseif(tg_type_ff.EQ.18) then
          tgroup=8
        elseif(tg_type_ff.EQ.19) then
          tgroup=9
        elseif(tg_type_ff.EQ.25) then
          tgroup=10
        elseif(tg_type_ff.EQ.26) then
          tgroup=11
        elseif(tg_type_ff.EQ.27) then
          tgroup=12
        elseif(tg_type_ff.EQ.29 .OR. tg_type_ff.EQ.30) then
          tgroup=13
          experiment=tg_type_ff
        else
          write(crmess,'(A,I4,A)')'unknown electron TARGET geometry (type=',
     &                  tg_type_ff,') -> no target defined'
          call recmes(crname,'W',crmess)
          return
        endif

      else
* photon targets: 
        if(tg_type_ff.EQ.-1) then
          if(tg_user_ff(4).GT.0) then
            tgroup=tg_user_ff(4)
            if(tgroup.LT.1 .OR. tgroup.GT.MAX_PHTG_GROUPS) tgroup=0
          endif
        elseif(tg_type_ff.GE.1 .AND. tg_type_ff.LE.2) then
          tgroup=1
        elseif(tg_type_ff.GE.3 .AND. tg_type_ff.LE.5) then
          tgroup=2
        elseif(tg_type_ff.GE.6 .AND. tg_type_ff.LE.8) then
          tgroup=3
        elseif(tg_type_ff.EQ.9)  then
          tgroup=4
        elseif(tg_type_ff.EQ.10) then
          tgroup=5
        elseif(tg_type_ff.EQ.11) then
          tgroup=8
        elseif(tg_type_ff.EQ.12 .OR. tg_type_ff.EQ.13) then
          tgroup=6
        elseif(tg_type_ff.EQ.14) then
          tgroup=7
        else
          write(crmess,'(A,I4,A)')'unknown photon TARGET geometry (type=',
     &                  tg_type_ff,') -> no target defined'
          call recmes(crname,'W',crmess)
          return
        endif
*
******************************************************
*  beam pipe and carbon fiber tube for photon targets
*
*  NOTE: beam pipe position moves with start counter position 
*
*  define mother volume  = 'BEAM'
*  and carbon fiber tube = 'BPIP' (for beam pipe)
*
        call GLOOK('BEAM',IQ(jvolum+1), NVOLUM, ivol)
        if(ivol.LE.0) then
*  the following parameters are only valid if  Data Card 'AUTO' = 0  !
           TMAXFD =  0.1        !max. angular deviation in one step [degree]
           STMAX  =  5.0        !max. step  [cm]
           DEEMAX =  0.01       !max. energy loss per step  [0;1]
           EPSIL  =  0.001      !boundary crossing precision
           STMIN  =  0.001      !min. step  [cm]
           tmed_pipe=get_tmed()
           CALL GSTMED( tmed_pipe,'CARBON PIPE$', imate_carbon, 0,
     &          init_ifield,init_fieldm,tmaxfd,
     &          stmax, deemax, epsil, stmin, 0 , 0 )
           
           parm(1)=0.           !start angle (azimuthal)
           parm(2)=360.         !diff. to end angle (=parm(1)+parm(2))
*
           if(st_type.EQ.1) then
*New Beam Pipe for new start counter
*              parm(3)=3. !number of planes (perp.to z) with different radii
*              do i=1,3
*                 parm(1+i*3)=bpip_newst_pln(i)     !plane position in z 
*                 parm(2+i*3)=0.                    !inner radius
*                 parm(3+i*3)=bpip_newst_rad(i)     !outer radius 
*              enddo
*              call gsvolu('BEAM','PCON',clas_med_vac,parm,15,ivol)
*              call gspos('BEAM',1,'CLAS',0.0,0.0,st_zoff_ff+tg_pos_ff(3),0,'ONLY')

*  define carbon fiber tube, ends at upstream end of scattering chamber
*              parm(3)=3.
*              do i=1,3
*                 parm(2+i*3)=parm(3+i*3)-bpip_thick(i) !inner radius
*              enddo
*              call gsvolu('BPIP','PCON',tmed_pipe,parm,12,ivol)
*              call gspos('BPIP',1,'BEAM',0.,0.,0.,0,'ONLY')

              parm(3)=6. !number of planes (perp.to z) with different radii
              do i=1,6
                 parm(1+i*3)=bpip_newst_pln(i)     !plane position in z 
                 parm(2+i*3)=0.                    !inner radius
                 parm(3+i*3)=bpip_newst_rad(i)     !outer radius 
              enddo
              call gsvolu('BEAM','PCON',clas_med_vac,parm,21,ivol)
              call gspos('BEAM',1,'CLAS',0.0,0.0,st_zoff_ff,0,'ONLY')
              do i=1,6
                 parm(2+i*3)=parm(3+i*3)-bpip_newst_thick !inner radius
              enddo
              call gsvolu('BPIP','PCON',tmed_pipe,parm,21,ivol)
              call gspos('BPIP',1,'BEAM',0.,0.,0.,0,'ONLY')

              tgpar_save(12)=st_zoff_ff              
*
           else
*Old Beam Pipe for old start counter
*  define 'BPIP' mother volume  (vaccum)
             parm(3)=4. !number of planes (perp.to z) with different radii
             do i=1,4
                parm(1+i*3)=bpip_pln(i)     !plane position in z 
                parm(2+i*3)=0.              !inner radius
                parm(3+i*3)=bpip_rad(i)     !outer radius 
             enddo
             call gsvolu('BEAM','PCON',clas_med_vac,parm,15,ivol)
             call gspos('BEAM',1,'CLAS',0.0,0.0,st_zoff_ff,0,'ONLY')
             tgpar_save(12)=st_zoff_ff

*  define carbon fiber tube
             do i=1,4
                parm(2+i*3)=parm(3+i*3)-bpip_thick(i) !inner radius
             enddo
             call gsvolu('BPIP','PCON',tmed_pipe,parm,15,ivol)
             call gspos('BPIP',1,'BEAM',0.,0.,0.,0,'ONLY')
           endif

        endif

      endif                          ! (qbeam==-1 || qbeam==0)
*
***********************
* material definitions 
*
      do i=1,MAX_MAT
        if(mymate(i).EQ.101)   idmate(i)=imate_vacuum
        if(mymate(i).EQ.102)   idmate(i)=imate_air
        if(mymate(i).EQ.103)   idmate(i)=imate_helium
        if(mymate(i).EQ.111)   idmate(i)=imate_liq_h2
        if(mymate(i).EQ.112)   idmate(i)=imate_liq_d2
        if(mymate(i).EQ.113)   idmate(i)=imate_liq_he3
        if(mymate(i).EQ.114)   idmate(i)=imate_liq_he4
        if(mymate(i).EQ.116)   idmate(i)=imate_butanol
        if(mymate(i).EQ.118)   idmate(i)=imate_nh3
        if(mymate(i).EQ.180)   idmate(i)=imate_nd3
        if(mymate(i).EQ.119)   idmate(i)=imate_hd
        if(mymate(i).EQ.120)   idmate(i)=imate_h2ice
        if(mymate(i).EQ.121)   idmate(i)=imate_d2ice
        if(mymate(i).EQ.122)   idmate(i)=imate_carbon
        if(mymate(i).EQ.123)   idmate(i)=imate_ch2
        if(mymate(i).EQ.131)   idmate(i)=imate_iron
        if(mymate(i).EQ.132)   idmate(i)=imate_aluminum
        if(mymate(i).EQ.133)   idmate(i)=imate_copper
        if(mymate(i).EQ.134)   idmate(i)=imate_lead
        if(mymate(i).EQ.135)   idmate(i)=imate_tungsten
        if(mymate(i).EQ.136)   idmate(i)=imate_stainless_steel
        if(mymate(i).EQ.161)   idmate(i)=imate_kapton
        if(mymate(i).EQ.162)   idmate(i)=imate_mylar
        if(mymate(i).EQ.163)   idmate(i)=imate_alumylar
        if(mymate(i).EQ.164)   idmate(i)=imate_cerex
        if(mymate(i).EQ.165)   idmate(i)=imate_cerexalu1
        if(mymate(i).EQ.166)   idmate(i)=imate_cerexalu2
        if(mymate(i).EQ.167)   idmate(i)=imate_cerexmyl1
        if(mymate(i).EQ.171)   idmate(i)=imate_alumaseal
        if(mymate(i).EQ.172)   idmate(i)=imate_comp_alumaseal
        if(mymate(i).EQ.173)   idmate(i)=imate_polystyrene
      enddo

**********************************
* get material id# for target materials
      do i=1,MAX_TGMAT
        idmate_tg(i)=0
        do j=1,MAX_MAT
          if(mate_tg(i).EQ.mymate(j)) idmate_tg(i)=idmate(j)
        enddo
      enddo

********************************************************
*  flag to switch on/off cell walls and superinsulation
*  OR  material definition for cell walls 
*
      ind3=-1
      if(tg_cell_ff.GE.-5 .AND. tg_cell_ff.LE.0) then
        icellflag=tg_cell_ff
      elseif(tg_cell_ff.GT.0 .AND. tg_cell_ff.LT.30) then
* geant material id?
        do i=1,MAX_TGMAT
          if(tg_cell_ff.EQ.idmate_tg(i).AND.mate_tg(i).GT.130) ind3=i
        enddo
      elseif (tg_cell_ff.GT.130 .AND. tg_cell_ff.LT.200) then
        do i=1,MAX_TGMAT
          if(tg_cell_ff.EQ.mate_tg(i)) ind3=i
        enddo
      else
        i=IUCOMP(tg_cell_ff,hcell_flag,MAX_CELLFLAGS)
        if(i.GT.0) then
          icellflag=cell_flags(i)
        else
          i=IUCOMP(tg_cell_ff,h_tg,MAX_TGMAT)
          if(i.GT.0 .AND. mate_tg(i).GT.130) ind3=i
        endif
      endif
        
******************
* target material 
*
      if(tg_mate_ff.eq.0) tg_mate_ff=101       !unknown? assume empty target
      ind=0
* geant material id?
      if(tg_mate_ff.GT.0 .AND. tg_mate_ff.LT.30) then
        do i=1,MAX_TGMAT
          if(tg_mate_ff.EQ.idmate_tg(i)) ind=i
        enddo
      elseif (tg_mate_ff.GT.100 .AND. tg_mate_ff.LT.200) then
         do i=1,MAX_TGMAT
            if(tg_mate_ff.EQ.mate_tg(i)) ind = i
         enddo
      else
         ind=IUCOMP(tg_mate_ff,h_tg,MAX_TGMAT)
      endif
      if(ind.EQ.0) ind=MAX_TGMAT

*  only liquid material or gases or empty ('VAC ') allowed for cyrotargets
      if(tgroup.GT.0 .AND. (qbeam.EQ.0 .OR. tg_type_ff.LT.10)) then
         if(mate_tg(ind).GT.120) then 
           write(CRMESS,'(3A)')'Target material ',ch_tg(ind),
     &            ' not allowed for cryotargets -> set to VACUUM'
           call recmes(CRNAME,'W',CRMESS)
           ind=MAX_TGMAT
         endif
      endif

*  write material name into 'tg_mate_ff' variable 
      tg_mate_ff=h_tg(ind)

******************************
*  material for second target (if chosen)
*
      ind2=0
      if(tg2_mate_ff.NE.0) then
* geant standard material?
         if(tg2_mate_ff.GT.0 .AND. tg2_mate_ff.LT.30) then
            do i=1,MAX_TGMAT
               if(tg2_mate_ff.EQ.idmate_tg(i)) ind2=i
            enddo
         elseif (tg2_mate_ff.GT.100 .AND. tg2_mate_ff.LT.200) then
            do i=1,MAX_TGMAT
               if(tg2_mate_ff.EQ.mate_tg(i)) then
                  ind2 = i
               endif
            enddo
         else
            ind2=IUCOMP(tg2_mate_ff,h_tg,MAX_TGMAT)
         endif

         if(ind2.GT.0) then
*  no second target for user defined geometry
           if(tgroup.EQ.0) then
             ind2=0
*  no liquid material allowed for second target (except e5 target)
           elseif(qbeam.EQ.0 .OR. tg_type_ff.NE.5) then
             if(mate_tg(ind2).GT.110 .AND. mate_tg(ind2).LT.120) then
               write(CRMESS,'(3A)')'Target material ',ch_tg(ind2),
     &            ' not allowed for second target -> skip second target'
               call recmes(CRNAME,'W',CRMESS)
               ind2=0
             endif
           endif
         endif
*  write material name into 'tg2_mate_ff' variable
         if(ind2.GT.0) then
            tg2_mate_ff=h_tg(ind2)
         else
            tg2_mate_ff=0
         endif
      endif

********************************
*
*  superinsulation
*  (a) set array to zero if flag 'CELL' was chosen
*
      if(icellflag.EQ.0 .OR. icellflag.EQ.-2) tg_ins_ff(1)=0.0

*  (b) fix for unknown id# of superinsulation material
*  check for imat<30       -> probably geant material id#
*        for 100<mat<200  -> convert to geant material id#
*        for imat2=n00 or imat2=nn0 
*               -> tg_ins_ff(1) was probably given as 'xxx.y' or 'xxx.yy'

      if(tg_ins_ff(1).GT.0.0) then
         imat1=IFIX(tg_ins_ff(1))
         imat2=IFIX(ABS(tg_ins_ff(1)-imat1)*1000)
         if(imat1.GT.0) then
            j=0
            if(imat1.LT.30) then
               do i=1,MAX_INSMAT
                  if(imat1.EQ.idmate(i)) j=imat1
               enddo
            elseif(imat1.GT.100 .AND. imat1.LT.200) then
               do i=1,MAX_INSMAT
                  if(imat1.EQ.mymate(i)) j=idmate(i)
               enddo
            endif
            if(j.EQ.0) then
               write(CRMESS,'(A,I4,A,F7.3,A)') 'Material number not valid:',
     &           imat1,' (TGINS ',tg_ins_ff(1),')  -> NO insulation'
               call recmes(CRNAME,'W',CRMESS)
               imat2=0
            endif
            imat1=j
         endif
         if(imat2.NE.0) then
            if((imat2/10)*10.EQ.imat2) imat2=imat2/10
            if((imat2/10)*10.EQ.imat2) imat2=imat2/10
            j=0
            if(imat2.LT.30) then
               do i=1,MAX_INSMAT
                  if(imat2.EQ.idmate(i)) j=imat2
               enddo
            elseif(imat2.GT.100 .AND. imat2.LT.200) then
               do i=1,MAX_INSMAT
                  if(imat2.EQ.mymate(i)) j=idmate(i)
               enddo
            endif
            if(j.EQ.0) then
               write(CRMESS,'(A,I4,A,F7.3,A)') 'Material number not valid:',
     &           imat2,' (TGINS ',tg_ins_ff(1),')  -> NO insulation'
               call recmes(CRNAME,'W',CRMESS)
               imat1=0
            endif
            imat2=j
         endif
         tg_ins_ff(1)=imat1+imat2*0.001
      endif
      if(tg_ins_ff(1).EQ.0.0 .AND. icellflag.NE.0) icellflag=-2  !no superinsulation
*
*  if index for second target: ind2=0, let it point to 'VACUUM'
      if(ind2.EQ.0) ind2=MAX_TGMAT
*
*  write flag or mate into 'tg_cell_ff' variable
      if(ind3.GT.0) then
        tgpar_save(5)= idmate_tg(ind3)
        tg_cell_ff   = h_tg(ind3)
      else
        tgpar_save(5)=ind3
        do i=MAX_CELLFLAGS,1,-1
          if(cell_flags(i).EQ.icellflag)  tg_cell_ff=hcell_flag(i)
        enddo
      endif
*
***************************************************
*  call subroutines which define target geometries 
*
      if(qbeam.eq.-1) then
*  electron targets:

        if(tgroup.EQ.1) then
           call geom_eltg1(idmate_tg(ind))

        elseif(tgroup.EQ.2) then
           call geom_eltg2(idmate_tg(ind))

        elseif(tgroup.EQ.3) then
* temporary fix for e2 targets
          if(mate_tg(ind).EQ.imate_vacuum)  itg_type(2)=0
          if(mate_tg(ind).EQ.imate_liq_h2)  itg_type(2)=1
          if(mate_tg(ind).EQ.imate_liq_d2)  itg_type(2)=2
          if(mate_tg(ind).EQ.imate_liq_he3) itg_type(2)=3
          if(mate_tg(ind).EQ.imate_liq_he4) itg_type(2)=4
          if(mate_tg(ind2).EQ.imate_iron)   itg_type(3)=56
          if(mate_tg(ind2).EQ.imate_carbon) itg_type(3)=12
          if(mate_tg(ind2).EQ.imate_ch2)    itg_type(3)=2
          if(tg_type_ff.EQ.10) call ugeom_Apr15_May7
          if(tg_type_ff.EQ.11) call ugeom_May8_May12
          if(tg_type_ff.EQ.12) call ugeom_May12_May15
          if(tg_type_ff.EQ.13) call ugeom_May15_May27
          ind2=MAX_TGMAT               !second target defined in subroutine

        elseif(tgroup.EQ.4) then
* e5 target
c          call geom_eltg5(idmate_tg(ind),idmate_tg(ind2))

           call ugeom_tg_e5

        elseif(tgroup.EQ.5) then
* e1d target
          call ugeom_tg_e1d
        elseif(tgroup.EQ.6) then
* e1-6 target
          call ugeom_tg_e16(idmate_tg(ind))
        elseif(tgroup.EQ.7) then
          call ugeom_tg_e1e(idmate_tg(ind))

        elseif(tgroup.EQ.8) then
* e2b target
           call ugeom_tg_e2b    

        elseif(tgroup.EQ.9) then
* e6a target
           call ugeom_tg_e6a          

        elseif(tgroup.EQ.10) then
* dvcs target
           call ugeom_tg_dvcs(idmate_tg(ind)) 

        elseif(tgroup.EQ.11) then
* eg2 target
           call ugeom_tg_eg2(idmate_tg(ind))
           
        elseif(tgroup.EQ.12) then
* eg6 target
cc           call ugeom_tg_eg6
           
        elseif(tgroup.EQ.13) then
* New e1-dvcs1 and e1-dvcs2 target geometries
           
           call ugeom_tg_dvcs2(idmate_tg(ind),experiment)
                     
        endif

      else
*  photon targets:

         if(tgroup.EQ.1) then
            call geom_phtg1(idmate_tg(ind))

         elseif(tgroup.EQ.2) then
            call geom_phtg2(idmate_tg(ind))

         elseif(tgroup.EQ.3) then
            call geom_phtg3(idmate_tg(ind))

         elseif(tgroup.EQ.4) then
            call geom_phtg4(idmate_tg(ind))

         elseif(tgroup.EQ.5) then
            call geom_phtg5(idmate_tg(ind))

         elseif(tgroup.EQ.6) then
           if( tg_type_ff.eq.12) then
             call geom_tg_frost(idmate_tg(ind))
           elseif( tg_type_ff.eq.13) then
             call geom_hd_cryostat(idmate_tg(ind))
           endif
         elseif(tgroup.EQ.7) then
*            call ugeom_lprd

         elseif(tgroup.EQ.8) then
             call geom_phtg8(idmate_tg(ind))

         endif

      endif
*
*  user defined target (cylindrical shape) or second target (solid)
      if(tgroup.EQ.0) then
         call geom_usertg(qbeam,idmate_tg(ind))
      elseif(tg2_mate_ff.NE.0 .AND. mate_tg(ind2).GT.120) then
         call geom_usertg(qbeam,idmate_tg(ind2))
      endif
*  
*  ordering of defined target volumes:
**      if(tgpar_save(1).GT.0) call gsord('TG  ',4)   !4=cylindrical ordering

      end

c-----------------------------------------------------------------------------



