      subroutine ugeom_tg_eg2(itg_mate)
      
      implicit none
      character*(*) cfile,crevis,cstate,cdate,cautho,crname,crauth
      character*132 crcsid,crmess
      parameter (cfile=  '$RCSfile: ugeom_tg_eg2.F,v $')
      parameter (crevis= '$Revision: 1.4 $')
      parameter (cstate= '$State: Exp $')
      parameter (cdate=  '$Date: 2007/10/22 20:55:45 $')
      parameter (cautho= '$Author: holtrop $')
C crname will only use 8 characters.
      parameter (crname= 'UGTG_EG2')
      parameter (crauth= 'hayk')
      data crcsid/'
     1     $Id: ugeom_tg_eg2.F,v 1.4 2007/10/22 20:55:45 holtrop Exp $
     1     '/
      save
      
c     include files:
c     --------------
#include "gcbank.inc"
#include "gcnum.inc"
#include "gsimpar.inc"
#include "gsimcontrol.inc"
#include "ffpar_foil.inc"
#include "ffpar_tg_e2.inc"
#include "ffpar_tg.inc"
#include "ffpar_mg.inc"
#include "clmate.inc"
#include "cltmed.inc"
c     _end_inc


*
*     input parameters
*     ----------------
      
      integer itg_mate          !target material number
*     
*     external functions
*     ------------------
      
      integer  iucomp, lnblnk, get_tmed, get_rotm
      
*     local variables:
*     ----------------
      real  twopiby3, piby2,wzpos
      integer i, np
      integer istrlen
      integer ivol              !volume#
      integer irot1, irot2, irot3 !rotation_matrix#
      integer i2                !#plane in PolyCONe for target cell
      real  tg_zpos2            !pos. of tube-like part of target cell
      real  dangle              !step size for cone of target cell
      real  parm(50), pars(20)  !arrays for volume definitions
      real  xpos, ypos, zpos, xypos !position of volumes
      real  xytg, ztg           !position of target center
      real  tglenby2, tgrad, thickcell !target info 
      real  tgoffset            !Distance from upstream cell to CLAS center
      real  thickwindow,radwindow !Al window info
      integer tmed_tg           !track.medium for target material
      integer imatcell, tmed_cell !mate# and track.medium for target cell
      integer imatins, tmed_ins !for superinsulation layers 
      real  distrad, distax     !insulation (distance from target)
      real  thickrad, thickax   !insulation (thickness)
      real  tmaxfd, stmax, deemax, epsil, stmin !track.media cuts 
      real  aa,zz,dens,radl,absl !material constants
      character*20 cmat_tg, cmatcell, cmatins !material names
      character*4  cmoth        !mother volume
      real  z0pos               !z-position in mother volume
*     
*     default values for target geometry  
*     ----------------------------------
*     
      real tg_len(1:1)          !target length (depends on target type)
      real tg_diam(2)           !target diameter
      real tg_thickcell         !cell wall thickness  (material: Kapton)
      real tg_thickwindow       !Al window thickness
      real tg_radwindow         !Al window radius
      
      real tg_distrad           !radius of superinsulation
      real ins_dlen             !length of layers in downstream dir.
      real tg_thickins          !total thickness of superinsulation 
                                !layers (material: aluminized Mylar)
                                !packed as solid block (ca. 3.5% of total)
      
      data tg_len                 / 3.621 /
      data tg_diam,tg_thickcell   / 1.609, 0.696, 0.0051 /
      data tg_thickwindow         / 0.0015 /
      data tg_radwindow           / 0.6 /
      data tg_distrad,tg_thickins / 5.2, 0.00715 / 
      data ins_dlen               / 2.00 /
*     
*     gas/liquid distribution pipes & support structures 
*     --------------------------------------------------
*     
*     pcon:           base ring (torlon) at entry window position 
*     tube1 & tube2:  horizontal, connected to base ring and condenser
*     
      real tg_flpos(10)         ! relative to right edge of flange
      real tg_flthick(10)       ! thickness of torlon ring
      real tg_fldrad(10)        ! relative inner radius wrt. cell radius
      data tg_flpos /-4.78,-4.15,-3.53,-3.53,-2.84,-2.84,-1.47,-1.23,-0.33,0.0/
      data tg_flthick /0.330,1.75,1.75,0.26,0.26,0.41,0.41,0.244,0.244,0.0/
      data tg_fldrad  /-0.455,-0.455,-0.455,0.635,0.635,0.635,0.635,0.635,0.013,0.013/
      
      real tg_tupos(10)         !relative to left edge of flange
      real tg_tuthick(10)       !thickness of inner base tube (support)
      real tg_turad(10)         !thickness of inner base tube (support)
      real tgtu_x, tgtu_b
      data tg_tuthick / 0.05,0.05,0.145,0.145,0.05,0.05,0.025,0.025,0.055,.125/
      data tg_tupos   /0.0,1.25,1.25,2.54,2.54,4.59,4.59,6.19,6.19,6.26/
      data tg_turad / 0.3,0.3,0.3,0.3,0.3,0.3,0.3,0.3,0.27,0.2/
      
      real tg_epx_r1(10),tg_epx_r2(10),tg_epx_pos(10)
      real tg_ecp_r1(2),tg_ecp_r2(2),tg_ecp_pos(2)
      
      real tg_lh2_r1(10),tg_lh2_r2(10),tg_lh2_pos(10)
      real tg_tube1(3), tg_tube2(3), tg_tube3(3)
      data tg_tube1  / 0.45, 0.55, 5.5 /
      data tg_tube2  / 0.35, 0.45, 2.8 /
      data tg_tube3  / 4.08, 4.90, 0.15 /
      
*     offset vs. volume GAS2 to position the target in CLAS center:
      real  eltg_zoff
      data  eltg_zoff /-5.06/


c     -hayk
      real COS17, SIN17, COS30, SIN30,COS19_5,SIN19_5,COS80_5,SIN80_5
      real COS27_12,SIN27_12,COS25_96,SIN25_96,COS10,SIN10
      DATA COS17 /0.9563/
      DATA SIN17 /0.2924/
      DATA COS30 /0.8660/
      DATA SIN30 /0.5/
      DATA COS19_5 /0.9426/
      DATA SIN19_5 /0.3338/
      DATA COS80_5 /0.1650/
      DATA SIN80_5 /0.9863/
      DATA COS27_12 /0.8901/
      DATA SIN27_12 /0.4559/
      DATA COS25_96 /0.8991/
      DATA SIN25_96 /0.4377/
      DATA COS10 /0.9848/
      DATA SIN10 /0.1736/
      integer irt_sch,irt_sch1,irt_sch2,irt_sch3,irt_sch4,irt_sch5,irt_sch6
      integer irt_trd1,irt_trd2,irt_trd3,irt_trd4,irt_trd5,irt_trd6
      integer irt_sch3F,irt_sch4F,irt_sch5F,irt_sch6F
      integer irt_wire1,irt_wire2,irt_wire3,irt_wire4,irt_wire5,irt_wire6
      integer irt_wire1_ON,irt_wire2_ON,irt_wire3_ON,irt_wire4_ON,irt_wire5_ON,irt_wire6_ON
      integer irt_level1_ON,irt_level2_ON,irt_level3_ON,irt_level4_ON,irt_level5_ON,irt_level6_ON
      integer irt_level1,irt_level2,irt_level3,irt_level4,irt_level5,irt_level6
      integer circ_level1,circ_level2,circ_level3,circ_level4,circ_level5,circ_level6
      integer irt_targ1,irt_targ2,irt_targ3,irt_targ4,irt_targ5,irt_targ6
      integer circ_targ1,circ_targ2,circ_targ3,circ_targ4,circ_targ5,circ_targ6
      integer irt_targ1_ON,irt_targ2_ON,irt_targ3_ON,irt_targ4_ON,irt_targ5_ON,irt_targ6_ON
      integer circ_leg1_ON,circ_leg2_ON,circ_leg3_ON,circ_leg4_ON,circ_leg5_ON,circ_leg6_ON
      integer irt_tbox1_ON,irt_tbox2_ON,irt_tbox3_ON,irt_tbox4_ON,irt_tbox5_ON,irt_tbox6_ON
      integer circ_leg10,circ_leg11
      real sccyl(3),scsph(6),scexit(5),scexw(3),tbvolm(3)
      real c12(3),fe56(3),ala(3),alb(3),pb(3),sn(3)
      real PAR1(3),PAR2(3),PAR3(3),PAR4(3),PAR5(3),PAR6(4)
      real PARWIRE_ON(3),PARWIRE(3),PAR7(3),PAR8(3),PAR9(3),PAR10(3)
      real t_PAR1(3),t_PAR2(3),t_PAR3(5),t_PAR4(3)
      real t_PAR_TARG(3),t_PAR_CIRC(5),t_PAR_ARM(4)
      real X1(6),Y1(6),Z1,X2(6),Y2(6),Z2,X3(6),Y3(6),Z3
      real X4(6),Y4(6),Z4,X5(6),Y5(6),Z5,X6(6),Y6(6),Z6
      real X7(6),Y7(6),Z7,X8(6),Y8(6),Z8,X9(6),Y9(6),Z9
      real X7_ON(6),Y7_ON(6),Z7_ON
      real X8_ON(6),Y8_ON(6),Z8_ON,X9_ON(6),Y9_ON(6),Z9_ON
      real X10(6),Y10(6),Z10,X10_ON(6),Y10_ON(6),Z10_ON
      real X11(6),Y11(6),Z11,X11_ON(6),Y11_ON(6),Z11_ON
      real t_X1(6),t_Y1(6),t_Z1,t_X2(6),t_Y2(6),t_Z2
      real t_X3(6),t_Y3(6),t_Z3,t_X4(6),t_Y4(6),t_Z4
      real t_X1_ON(6),t_Y1_ON(6),t_Z1_ON,t_X2_ON(6),t_Y2_ON(6),t_Z2_ON
      real t_X3_ON(6),t_Y3_ON(6),t_Z3_ON,t_X4_ON(6),t_Y4_ON(6),t_Z4_ON
      real t_X5(6),t_Y5(6),t_Z5,t_X6_r(6),t_Y6_r(6),t_Z6_r,t_X6_l(6),t_Y6_l(6),t_Z6_l
      real delta_tar
      
      DATA TBVOLM/0.,12.,20./
      DATA SCCYL/9., 10.016, 9./ !the cylidrical part of scatter.chamber
      DATA SCSPH/9., 9.953, 0., 90., 0., 180./ !spherical part of sc.chamber
      DATA SCEXIT/7.25,0.673,0.757,1.27,1.354/ !exit part of sc.chamber (cone)
      DATA SCEXW/0., 1.27, 0.00355/ !exit window of sc.chamber 

      DATA C12 /0., 0.15, 0.08615/
      DATA FE56/0., 0.15, 0.02   /
      DATA PB  /0., 0.15, 0.007  /
      DATA ALA /0., 0.15, 0.00075/
      DATA ALB /0., 0.15, 0.029  /
      DATA SN /0., 0.15,  0.0156 /

c     -hayk
      
      
c     _end_var
      
c     executable code:
c     ----------------
      piby2   =acos(0.)
      twopiby3=acos(-1.)*2./3.
      
      imatcell= imate_kapton 
      imatins = imate_cerexmyl1
      
      tmed_cell= clas_med_kapton ! cell walls kapton
      tmed_ins = clas_med_cerexmyl1 ! superins. CEREX/Alum.mylar
      
      if(itg_mate.EQ.imate_vacuum) then ! Choose empty or full (H2) cell
         tmed_tg  = clas_med_vac
      elseif(itg_mate.EQ.imate_liq_h2) then
         tmed_tg  = clas_med_h2liq
      elseif(itg_mate.EQ.imate_liq_d2) then
         tmed_tg  = clas_med_d2liq
      else
         call recmes(crname,'W',
     &        'Only HYDR, DEUT or VAC targets supported for eg2')
         return
      endif
      
      tglenby2 = tg_len(1)/2
      tgrad    = tg_diam(1)/2
      
      thickwindow = tg_thickwindow
      radwindow   = tg_radwindow
      thickcell   = tg_thickcell
      thickrad    = tg_thickins
      
c     Thickness of axial part of super ins. 
      
      thickax   = tg_thickins
      distrad   = tg_distrad
      distax    = ins_dlen
      
      do i =1,10
         tg_tupos(i) =tg_tupos(i)+tg_flpos(1) 
      enddo
      
      tg_lh2_r1(1)  = tg_turad(3)+tg_tuthick(3)
      tg_lh2_r2(1)  = tg_fldrad(4)+tgrad
      tg_lh2_pos(1) = tg_flpos(4)
      tg_lh2_r1(2)  = tg_lh2_r1(1)
      tg_lh2_r2(2)  = tg_lh2_r2(1)
      tg_lh2_pos(2) = tg_tupos(5)
      tg_lh2_r1(3)  = tg_turad(5)+tg_tuthick(5)
      tg_lh2_r2(3)  = tg_lh2_r2(2)
      tg_lh2_pos(3) = tg_tupos(5)
      tg_lh2_r1(4)  = tg_lh2_r1(3)
      tg_lh2_r2(4)  = tg_lh2_r2(3)
      tg_lh2_pos(4) = tg_flpos(8)
      tg_lh2_r1(5)  = tg_lh2_r1(4)
      tg_lh2_r2(5)  = tg_fldrad(9)+tgrad
      tg_lh2_pos(5) = tg_flpos(9)
      tg_lh2_r1(6)  = tg_lh2_r1(5)
      tg_lh2_r2(6)  = tg_lh2_r2(5)
      tg_lh2_pos(6) = tg_tupos(7)
      tg_lh2_r1(7)  = tg_turad(7)+tg_tuthick(7)
      tg_lh2_r2(7)  = tg_lh2_r2(6)
      tg_lh2_pos(7) = tg_tupos(7)
      tg_lh2_r1(8)  = tg_turad(7)+tg_tuthick(7)
      tg_lh2_r2(8)  = 0.595
      tg_lh2_pos(8) = tg_tupos(10)
      tg_lh2_r1(9)  = 0.0
      tg_lh2_r2(9)  = tg_lh2_r2(8)
      tg_lh2_pos(9) = tg_tupos(10)
      tg_lh2_r1(10)  = tg_lh2_r1(9)
      tg_lh2_r2(10)  = tg_diam(2)/2.
      tg_lh2_pos(10) = tg_tupos(10)+2.0
      
      tg_epx_r1(1)  = tg_lh2_r2(10) + thickcell/0.992 + 0.188*0.126
      tg_epx_r2(1)  = tg_epx_r1(1)
      tg_epx_pos(1) = tg_lh2_pos(10)-0.188
      
      tg_epx_r1(2)  = tg_lh2_r2(10) + thickcell/0.992 + 0.159*0.126
      tg_epx_r2(2)  = tg_epx_r1(2) + 0.044
      tg_epx_pos(2) = tg_lh2_pos(10)-0.159
      
      tg_epx_r1(3)  = tg_lh2_r2(10) + thickcell/0.992
      tg_epx_r2(3)  = tg_epx_r1(3) + 0.005
      tg_epx_pos(3) = tg_lh2_pos(10)
      
      tg_epx_r1(4)  = tg_lh2_r2(10) 
      tg_epx_r2(4)  = tg_epx_r1(3) + 0.005
      tg_epx_pos(4) = tg_lh2_pos(10)
      
      tg_epx_r1(5)  = tg_lh2_r2(10)
      tg_epx_r2(5)  = tg_epx_r1(3) 
      tg_epx_pos(5) = tg_lh2_pos(10) + 0.01
      
      tg_ecp_r1(1)  = tg_epx_r2(2)
      tg_ecp_r2(1)  = tg_ecp_r1(1)+0.0062
      tg_ecp_pos(1) = tg_epx_pos(2)
      
      tg_ecp_r1(2)  = tg_epx_r2(5)
      tg_ecp_r2(2)  = tg_ecp_r1(2)+0.0062
      tg_ecp_pos(2) = tg_epx_pos(5)
      
      tgoffset = tg_lh2_pos(10)-tg_flpos(10)-0.931
      
*     
*     stop if beam pipe volume not defined
*     
*     - until 'FOIL' volumes are cleaned up, put target into 'GAS2' volume
*     which is positioned at z=5.06 (so: add an artificial target offset)
*     - If using the CHAM scattering volume, position TG at the centre of CHAM, 
*     otherwise put it in GAS2 (s.morrow).
      
      if (chamber_type_ff.le.0 .or. chamber_type_ff.ge.7) then
         call recmes(crname,'W',
     &        'Scattering chamber not defined -> no geometry')
         return
      elseif ( chamber_type_ff.eq.6 ) then
         cmoth='CHAM'
         z0pos=tg_pos_ff(3)
         tgpar_save(12)=upst_pos_ff(3)
      else
         cmoth='GAS2'
         z0pos=tg_pos_ff(3)+eltg_zoff
         tgpar_save(12)=0.0
      endif
      
      call GLOOK(cmoth, IQ(jvolum+1), NVOLUM, ivol)
      if(ivol.LE.0) then
         write(crmess,'(3A)') 'Scattering chamber volume "',cmoth,
     &        '" not defined -> no target geometry'
         call recmes(crname,'W',crmess)
         return
      endif
      call recmes(crname,'I',
     &     'Define geometry for eg2 target')
*     
***************************
*     define tracking media
***************************
*     
*     these parameters are only(!) valid if  Data Card 'AUTO' = 0  !
      TMAXFD =  0.1             !max. angular deviation in one step [degree]
      STMAX  =  1.0             !max. step  [cm]
      DEEMAX =  0.01            !max. energy loss per step  [0;1]
      EPSIL  =  0.001           !boundary crossing precision
      STMIN  =  0.001           !min. step  [cm]
*     
*     target material
*     
      call GFMATE(itg_mate,cmat_tg,aa,zz,dens,radl,absl,parm,np)
      call GFMATE(imatcell,cmatcell,aa,zz,dens,radl,absl,parm,np)
      call GFMATE(imatins,cmatins,aa,zz,dens,radl,absl,parm,np)
      
*     print info
      
      istrlen=LNBLNK(cmat_tg)
      write(crmess,'(3A,F7.2,A)') 'set Target (material=',
     &     cmat_tg(1:istrlen),')  at z-pos.:',teg2_pos_ff(3),'cm wrt CHAM.'
      call recmes(crname,'I',crmess)
      write(crmess,'(A,F7.2,A)') 'Chamber z-pos position is at ',tgpar_save(12),'cm.'
      call recmes(crname,'I',crmess)
      istrlen=LNBLNK(cmatcell)
      write(crmess,'(2(A,F7.3),A,F6.4,3A)') '   Target length:',
     &     tglenby2*2,'cm;  radius:',tgrad,'cm;  cell thickness:',
     &     thickcell,'cm (',cmatcell(1:istrlen),')'
      call recmes(crname,'I',crmess)

      WRITE(crmess,*) 'Second target type: ',itg_type(1)
      call recmes(crname,'I',crmess)

*     
************************
*     geometry definitions
************************
*     
*     mother volume (vacuum)
*     target and support structure for electron beam is positioned in 'UPST',
*     or 'GAS2' depending on which scattering chamber is being used.
*     
c      pars(1)=0.0
c      pars(2)=MAX((tgrad+distrad+thickrad)*1.05,
c     &     tg_tube1(2)+tg_tube2(3)/2.+tgrad)
c      pars(3)=tglenby2+distax/2+ABS(tg_flpos(1)/2)+tg_tube1(3)+tg_tube2(3)
c      call gsvolu('TG  ','TUBE',clas_med_vac,pars,3,ivol)
c      xytg=sqrt(tg_pos_ff(1)**2+tg_pos_ff(2)**2)
c      ztg =pars(3)-tglenby2-distax
c     call gspos('TG  ',1,cmoth, tg_pos_ff(1),tg_pos_ff(2),
c     &     z0pos-ztg,0,'ONLY')
c     call gspos('TG  ',1,cmoth, 0.,0., -36.14099921,0,'ONLY')    ! -(29+7.14099979)
c      call gspos('TG  ',1,cmoth, 0.,0., -38.17123361,0,'MANY') ! -(27.54098361+10.63025)
c      print*,'///position TG===',z0pos-ztg
c Foam scattering chamber geometry 
c From drawing no. 66850-E-03565

 
      CALL GSVOLU('VOLM', 'TUBE', clas_med_vac    , TBVOLM  , 3, IVOL)
      CALL GSPOS('VOLM',1,'CHAM',0.,0.,-1.316+teg2_pos_ff(3), 0, 'ONLY' )
      irt_sch=get_rotm()
      CALL GSROTM(irt_sch,90.,90.,0.,0.,90.,0.)
      
      irt_sch1=get_rotm()
      CALL GSROTM(irt_sch1,90.,0.,73.,90.,17.,-90.)
      
      irt_sch2=get_rotm()
      CALL GSROTM(irt_sch2,90.,0.,107.,90.,17.,90.)
      
      irt_sch3F=get_rotm()
      CALL GSROTM(irt_sch3F,90.,-60.,90.,30.,0.,0.)
      
      irt_sch3=get_rotm()
      CALL GSROTM(irt_sch3,90.,-60.,73.,30.,17.,-90.)
      
      irt_sch4F=get_rotm()
      CALL GSROTM(irt_sch4F,90.,60.,90.,150.,0.,0.)
      
      irt_sch4=get_rotm()
      CALL GSROTM(irt_sch4,90.,60.,107.,150.,17.,90.)
      
      irt_sch5F=get_rotm()
      CALL GSROTM(irt_sch5F,90.,-60.,90.,30.,0.,0.)
      
      irt_sch5=get_rotm()
      CALL GSROTM(irt_sch5,90.,-60.,107.,30.,17.,90.)
      
      irt_sch6F=get_rotm()
      CALL GSROTM(irt_sch6F,90.,60.,90.,150.,0.,0.)
      
      irt_sch6=get_rotm()
      CALL GSROTM(irt_sch6,90.,60.,73.,150.,17.,-90.)
      
      irt_trd1=get_rotm()
      CALL GSROTM(irt_trd1,163.,90.,90.,180.,73.,90.)
      
      irt_trd2=get_rotm()
      CALL GSROTM(irt_trd2,17.,90.,90.,180.,73.,-90.)
      
      irt_trd3=get_rotm()
      CALL GSROTM(irt_trd3,163.,30.,90.,120.,73.,30.)
      
      irt_trd4=get_rotm()
      CALL GSROTM(irt_trd4,163.,-30.,90.,60.,73.,-30.)
      
      irt_trd5=get_rotm()
      CALL GSROTM(irt_trd5,163.,-150.,90.,-60.,73.,-150.)
      
      irt_trd6=get_rotm()
      CALL GSROTM(irt_trd6,163.,150.,90.,-120.,73.,150.)

      irt_wire1_ON=get_rotm()
      CALL GSROTM(irt_wire1_ON,90.,0.,64.04,90.,25.96,-90.)

      irt_wire2_ON=get_rotm()
      CALL GSROTM(irt_wire2_ON,90.,180.,64.04,-90.,25.96,90.)

      irt_wire3_ON=get_rotm()
      CALL GSROTM(irt_wire3_ON,90.,-60.,64.04,30.,25.96,-150.)

      irt_wire4_ON=get_rotm()
      CALL GSROTM(irt_wire4_ON,90.,-120.,64.04,-30.,25.96,150.)

      irt_wire5_ON=get_rotm()
      CALL GSROTM(irt_wire5_ON,90.,120.,64.04,-150.,25.96,30.)
      
      irt_wire6_ON=get_rotm()
      CALL GSROTM(irt_wire6_ON,90.,60.,64.04,150.,25.96,-30.)
      
      irt_wire1=get_rotm()
      CALL GSROTM(irt_wire1,90.,0.,62.88,90.,27.12,-90.)
      
      irt_wire2=get_rotm()
      CALL GSROTM(irt_wire2,90.,0.,117.12,90.,27.12,90.)
      
      irt_wire3=get_rotm()
      CALL GSROTM(irt_wire3,90.,-60.,62.88,30.,27.12,-90.)
      
      irt_wire4=get_rotm()
      CALL GSROTM(irt_wire4,90.,60.,117.12,150.,27.12,90.)
      
      irt_wire5=get_rotm()
      CALL GSROTM(irt_wire5,90.,-60.,117.12,30.,27.12,90.)
      
      irt_wire6=get_rotm()
      CALL GSROTM(irt_wire6,90.,60.,62.88,150.,27.12,-90.)

      irt_level1_ON=get_rotm()
      CALL GSROTM(irt_level1_ON,90.,-180.,160.5,-90.,70.5,-90.)

      irt_level2_ON=get_rotm()
      CALL GSROTM(irt_level2_ON,90.,0.,160.5,90.,70.5,90.)

      irt_level3_ON=get_rotm()
      CALL GSROTM(irt_level3_ON,90.,120.,160.5,-150.,70.5,-150.)

      irt_level4_ON=get_rotm()
      CALL GSROTM(irt_level4_ON,90.,60.,160.5,150.,70.5,150.)

      irt_level5_ON=get_rotm()
      CALL GSROTM(irt_level5_ON,90.,-60.,160.5,30.,70.5,30.)

      irt_level6_ON=get_rotm()
      CALL GSROTM(irt_level6_ON,90.,-120.,160.5,-30.,70.5,-30.)
      
      irt_level1=get_rotm()
      CALL GSROTM(irt_level1,90.,-180.,80.5,-90.,9.5,90.)
      
      irt_level2=get_rotm()
      CALL GSROTM(irt_level2,90.,0.,80.5,90.,9.5,-90.)
      
      irt_level3=get_rotm()
      CALL GSROTM(irt_level3,90.,120.,80.5,-150.,9.5,30.)
      
      irt_level4=get_rotm()
      CALL GSROTM(irt_level4,90.,60.,80.5,150.,9.5,-30.)
      
      irt_level5=get_rotm()
      CALL GSROTM(irt_level5,90.,-60.,80.5,30.,9.5,-150.)
      
      irt_level6=get_rotm()
      CALL GSROTM(irt_level6,90.,-120.,80.5,-30.,9.5,150.)
      
      circ_level1=get_rotm()
      CALL GSROTM(circ_level1,0.,90.,90.,90.,90.,-180.)
      
      circ_level2=get_rotm()
      CALL GSROTM(circ_level2,0.,-90.,90.,-90.,90.,0.)
      
      circ_level3=get_rotm()
      CALL GSROTM(circ_level3,0.,30.,90.,30.,90.,120.)
      
      circ_level4=get_rotm()
      CALL GSROTM(circ_level4,0.,-30.,90.,-30.,90.,30.)
      
      circ_level5=get_rotm()
      CALL GSROTM(circ_level5,0.,-150.,90.,-150.,90.,-30.)
      
      circ_level6=get_rotm()
      CALL GSROTM(circ_level6,0.,150.,90.,150.,90.,-120.)

      irt_targ1_ON=get_rotm()
      CALL GSROTM(irt_targ1_ON,0.,90.,90.,180.,90.,-90.)

      irt_targ2_ON=get_rotm()
      CALL GSROTM(irt_targ2_ON,0.,-90.,90.,0.,90.,90.)

      irt_targ3_ON=get_rotm()
      CALL GSROTM(irt_targ3_ON,0.,30.,90.,120.,90.,-150.)

      irt_targ4_ON=get_rotm()
      CALL GSROTM(irt_targ4_ON,0.,-30.,90.,60.,90.,150.)

      irt_targ5_ON=get_rotm()
      CALL GSROTM(irt_targ5_ON,0.,-150.,90.,-60.,90.,30.)

      irt_targ6_ON=get_rotm()
      CALL GSROTM(irt_targ6_ON,0.,150.,90.,-120.,90.,-30.)
      
      irt_targ1=get_rotm()
      CALL GSROTM(irt_targ1,80.,90.,90.,180.,10.,-90.)
      
      irt_targ2=get_rotm()
      CALL GSROTM(irt_targ2,80.,-90.,90.,0.,10.,90.)
      
      irt_targ3=get_rotm()
      CALL GSROTM(irt_targ3,80.,30.,90.,120.,10.,-150.)
      
      irt_targ4=get_rotm()
      CALL GSROTM(irt_targ4,80.,-30.,90.,60.,10.,150.)
      
      irt_targ5=get_rotm()
      CALL GSROTM(irt_targ5,80.,-150.,90.,-60.,10.,30.)
      
      irt_targ6=get_rotm()
      CALL GSROTM(irt_targ6,80.,150.,90.,-120.,10.,-30.)
      
      circ_targ1=get_rotm()
      CALL GSROTM(circ_targ1,90.,0.,10.,-90.,100.,-90.)
      
      circ_targ2=get_rotm()
      CALL GSROTM(circ_targ2,90.,-180.,10.,90.,100.,90.)
      
      circ_targ3=get_rotm()
      CALL GSROTM(circ_targ3,90.,-60.,10.,-150.,100.,-150.)
      
      circ_targ4=get_rotm()
      CALL GSROTM(circ_targ4,90.,-120.,10.,150.,100.,150.)
      
      circ_targ5=get_rotm()
      CALL GSROTM(circ_targ5,90.,120.,10.,30.,100.,30.)
      
      circ_targ6=get_rotm()
      CALL GSROTM(circ_targ6,90.,60.,10.,-30.,100.,-30.)

      irt_tbox1_ON=get_rotm()
      CALL GSROTM(irt_tbox1_ON,90.,-180.,90.,-90.,0.,0.)

      irt_tbox2_ON=get_rotm()
      CALL GSROTM(irt_tbox2_ON,90.,0.,90.,90.,0.,0.)

      irt_tbox3_ON=get_rotm()
      CALL GSROTM(irt_tbox3_ON,90.,120.,90.,-150.,0.,0.)

      irt_tbox4_ON=get_rotm()
      CALL GSROTM(irt_tbox4_ON,90.,60.,90.,150.,0.,0.)

      irt_tbox5_ON=get_rotm()
      CALL GSROTM(irt_tbox5_ON,90.,-60.,90.,30.,0.,0.)

      irt_tbox6_ON=get_rotm()
      CALL GSROTM(irt_tbox6_ON,90.,-120.,90.,-30.,0.,0.)

      circ_leg10=get_rotm()
      CALL GSROTM(circ_leg10,90.,-120.,0.,0.,150.,90.)

      circ_leg11=get_rotm()
      CALL GSROTM(circ_leg11,90.,-60.,180.,0.,30.,90.)
      
      
      CALL GSVOLU('VCYL', 'TUBE', clas_med_lowdfoam , SCCYL   , 3, IVOL)
      CALL GSVOLU('VSPH', 'SPHE', clas_med_lowdfoam , SCSPH   , 6, IVOL)
      CALL GSVOLU('VEXI', 'CONE', clas_med_cfiber   , SCEXIT  , 5, IVOL)
      CALL GSVOLU('VEXW', 'TUBE', clas_med_alu      , SCEXW   , 3, IVOL)
      
      CALL GSPOS('VCYL',1,'VOLM',0.,0.,-9.0    , 0, 'ONLY' )
      CALL GSPOS('VSPH',1,'VOLM',0.,0., 0.00    , irt_sch, 'MANY' )
      CALL GSPOS('VEXI',1,'VOLM',0.,0., 16.25   , 0, 'ONLY' )
      CALL GSPOS('VEXW',1,'VOLM',0.,0., 23.54   , 0, 'ONLY' )
      

c     solid target(from y to x axis 1,3,4,2,5,6)
      DATA PAR1/0.3175, 0.49403, 5.82422/
      DATA PAR2/0.3175, 0.54483, 1.78206/
      DATA PAR3/0.3175, 0.31750, 5.69329/
      DATA PAR4/0.3175, 0.11938, 1.74625/
      DATA PAR5/0.3175, 0.35687, 0.95250/
      DATA PAR6/0.3175, 0.95250, 0.3175, 0.3175/
      DATA PARWIRE_ON/0., 0.0795, 1.566/
      DATA PARWIRE/0., 0.0795, 2.79/
      DATA PAR7/0.1143, 0.87376, 0.47543/
      DATA PAR8/0., 0.4754, 0.1143/
      DATA PAR9/0.15875, 0.3175, 0.79375/
      DATA PAR10/0.15875, 0.16002, 1.27/

      DATA t_PAR1/0.0635, 0.0635, 0.7493/
      DATA t_PAR2/0., 0.13068, 0.254/
      DATA t_PAR3/0.1905, 0., 0.13068, 0., 0.0635/
      DATA t_PAR4/0., 0.0635, 0.9017/
      DATA t_PAR_TARG/0.185, 0.2377, 0.08615/
      DATA t_PAR_CIRC/0.15,0.1754,0.08615,-30.,210./
      DATA t_PAR_ARM/0.1087, 0.1381, 0.08615, 0.0254/

      DATA delta_tar/0.0746/

      X1(1)=0
      Y1(1)=8.396
      Z1=-13.263
      Y1(2)=-Y1(1)
      X1(3)=Y1(1)*COS30
      Y1(3)=Y1(1)*SIN30
      Y1(4)=-Y1(3)
      X1(5)=-X1(3)

      X2(1)=0
      Y2(1)=7.873
      Z2=-8.148
      Y2(2)=-Y2(1)
      X2(3)=Y2(1)*COS30
      Y2(3)=Y2(1)*SIN30
      Y2(4)=-Y2(3)
      X2(5)=-X2(3)

      X3(1)=0
      Y3(1)=6.923
      Z3=-2.086
      Y3(2)=-Y3(1)
      X3(3)=Y3(1)*COS30
      Y3(3)=Y3(1)*SIN30
      Y3(4)=-Y3(3)
      X3(5)=-X3(3)

      X4(1)=0
      Y4(1)=Y3(1)-(PAR3(3)-PAR4(3))*SIN17-(PAR3(2)+PAR4(2))*COS17
      Z4=Z3+(PAR3(3)-PAR4(3))*COS17-(PAR3(2)+PAR4(2))*SIN17
      Y4(2)=-Y4(1)
      X4(3)=Y4(1)*COS30
      Y4(3)=Y4(1)*SIN30
      Y4(4)=-Y4(3)
      X4(5)=-X4(3)

      X5(1)=0
      Y5(1)=Y4(1)-(PAR5(2)+PAR4(2))*COS17
      Z5=Z4-(PAR5(2)+PAR4(2))*SIN17
      Y5(2)=-Y5(1)
      X5(3)=Y5(1)*COS30
      Y5(3)=Y5(1)*SIN30
      Y5(4)=-Y5(3)
      X5(5)=-X5(3)

      X6(1)=0
      Y6(1)=Y5(1)-(PAR6(3)+PAR5(2))*COS17
      Z6=Z5-(PAR6(3)+PAR5(2))*SIN17
      Y6(2)=-Y6(1)
      X6(3)=Y6(1)*COS30
      Y6(3)=Y6(1)*SIN30
      Y6(4)=-Y6(3)
      X6(5)=-X6(3)

      Y8_ON(1)=Y5(1)-PAR5(2)-PAR7(2)*SIN19_5
      Z8_ON=Z5-PAR7(2)*COS19_5
      Y8_ON(2)=-Y8_ON(1)
      X8_ON(3)=Y8_ON(1)*COS30
      Y8_ON(3)=Y8_ON(1)*SIN30
      Y8_ON(4)=-Y8_ON(3)
      X8_ON(5)=-X8_ON(3)

      X8(1)=0
      Y8(1)=Y5(1)-PAR5(2)-PAR7(2)*SIN80_5
      Z8=Z5+PAR7(2)*COS80_5
      Y8(2)=-Y8(1)
      X8(3)=Y8(1)*COS30
      Y8(3)=Y8(1)*SIN30
      Y8(4)=-Y8(3)
      X8(5)=-X8(3)

      Y9_ON(1)=Y8_ON(1)-PAR7(2)*SIN19_5
      Z9_ON=Z8_ON-PAR7(2)*COS19_5
      Y9_ON(2)=-Y9_ON(1)
      X9_ON(3)=Y9_ON(1)*COS30
      Y9_ON(3)=Y9_ON(1)*SIN30
      Y9_ON(4)=-Y9_ON(3)
      X9_ON(5)=-X9_ON(3)

      X9(1)=0
      Y9(1)=Y8(1)-PAR7(2)*SIN80_5
      Z9=Z8+PAR7(2)*COS80_5
      Y9(2)=-Y9(1)
      X9(3)=Y9(1)*COS30
      Y9(3)=Y9(1)*SIN30
      Y9(4)=-Y9(3)
      X9(5)=-X9(3)

      Y10_ON(1)=4.182
      Z10_ON=-0.757
      Y10_ON(2)=-Y10_ON(1)
      X10_ON(3)=Y10_ON(1)*COS30
      Y10_ON(3)=Y10_ON(1)*SIN30
      Y10_ON(4)=-Y10_ON(3)
      X10_ON(5)=-X10_ON(3)

      X10(1)=0
      Y10(1)=Y9(1)+(PAR9(3)-PAR8(2))*SIN27_12
      Z10=Z9-(PAR9(3)-PAR8(2))*COS27_12
      Y10(2)=-Y10(1)
      X10(3)=Y10(1)*COS30
      Y10(3)=Y10(1)*SIN30
      Y10(4)=-Y10(3)
      X10(5)=-X10(3)

      Y11_ON(1)=Y10_ON(1)+(PAR9(3)+PAR10(3))*SIN25_96
      Z11_ON=Z10_ON-(PAR9(3)+PAR10(3))*COS25_96
      Y11_ON(2)=-Y11_ON(1)
      X11_ON(3)=Y11_ON(1)*COS30
      Y11_ON(3)=Y11_ON(1)*SIN30
      Y11_ON(4)=-Y11_ON(3)
      X11_ON(5)=-X11_ON(3)

      X11(1)=0
      Y11(1)=Y10(1)+(PAR9(3)+PAR10(3))*SIN27_12
      Z11=Z10-(PAR9(3)+PAR10(3))*COS27_12
      Y11(2)=-Y11(1)
      X11(3)=Y11(1)*COS30
      Y11(3)=Y11(1)*SIN30
      Y11(4)=-Y11(3)
      X11(5)=-X11(3)

      Y7_ON(1)=Y11_ON(1)+(PARWIRE_ON(3)+PAR10(3))*SIN25_96
      Z7_ON=Z11_ON-(PARWIRE_ON(3)+PAR10(3))*COS25_96
      Y7_ON(2)=-Y7_ON(1)
      X7_ON(3)=Y7_ON(1)*COS30
      Y7_ON(3)=Y7_ON(1)*SIN30
      Y7_ON(4)=-Y7_ON(3)
      X7_ON(5)=-X7_ON(3)

      X7(1)=0
      Y7(1)=Y11(1)+(PARWIRE(3)+PAR10(3))*SIN27_12
      Z7=Z11-(PARWIRE(3)+PAR10(3))*COS27_12
      Y7(2)=-Y7(1)
      X7(3)=Y7(1)*COS30
      Y7(3)=Y7(1)*SIN30
      Y7(4)=-Y7(3)
      X7(5)=-X7(3)

      t_Y1_ON(1)=Y5(1)-PAR5(2)*COS17-t_PAR1(3)-delta_tar
      t_Z1_ON=Z5-PAR5(2)*SIN17
      t_Y1_ON(2)=-t_Y1_ON(1)
      t_X1_ON(3)=t_Y1_ON(1)*COS30
      t_Y1_ON(3)=t_Y1_ON(1)*SIN30
      t_Y1_ON(4)=-t_Y1_ON(3)
      t_X1_ON(5)=-t_X1_ON(3)

      t_X1(1)=0
      t_Y1(1)=Y5(1)-PAR5(2)*COS17-(t_PAR1(3)+delta_tar)*SIN10
      t_Z1=Z5-PAR5(2)*SIN17+(t_PAR1(3)+delta_tar)*COS10
      t_Y1(2)=-t_Y1(1)
      t_X1(3)=t_Y1(1)*COS30
      t_Y1(3)=t_Y1(1)*SIN30
      t_Y1(4)=-t_Y1(3)
      t_X1(5)=-t_X1(3)

      t_Y2_ON(1)=t_Y1_ON(1)-(t_PAR1(3)+t_PAR2(3))
      t_Z2_ON=t_Z1_ON
      t_Y2_ON(2)=-t_Y2_ON(1)
      t_X2_ON(3)=t_Y2_ON(1)*COS30
      t_Y2_ON(3)=t_Y2_ON(1)*SIN30
      t_Y2_ON(4)=-t_Y2_ON(3)
      t_X2_ON(5)=-t_X2_ON(3)

      t_X2(1)=0
      t_Y2(1)=t_Y1(1)-(t_PAR1(3)+t_PAR2(3))*SIN10
      t_Z2=t_Z1+(t_PAR1(3)+t_PAR2(3))*COS10
      t_Y2(2)=-t_Y2(1)
      t_X2(3)=t_Y2(1)*COS30
      t_Y2(3)=t_Y2(1)*SIN30
      t_Y2(4)=-t_Y2(3)
      t_X2(5)=-t_X2(3)

      t_Y3_ON(1)=t_Y2_ON(1)-(t_PAR2(3)+t_PAR3(1))
      t_Z3_ON=t_Z1_ON
      t_Y3_ON(2)=-t_Y3_ON(1)
      t_X3_ON(3)=t_Y3_ON(1)*COS30
      t_Y3_ON(3)=t_Y3_ON(1)*SIN30
      t_Y3_ON(4)=-t_Y3_ON(3)
      t_X3_ON(5)=-t_X3_ON(3)

      t_X3(1)=0
      t_Y3(1)=t_Y2(1)-(t_PAR2(3)+t_PAR3(1))*SIN10
      t_Z3=t_Z2+(t_PAR2(3)+t_PAR3(1))*COS10
      t_Y3(2)=-t_Y3(1)
      t_X3(3)=t_Y3(1)*COS30
      t_Y3(3)=t_Y3(1)*SIN30
      t_Y3(4)=-t_Y3(3)
      t_X3(5)=-t_X3(3)

      t_Y4_ON(1)=t_Y3_ON(1)-(t_PAR3(1)+t_PAR4(3))
      t_Z4_ON=t_Z1_ON
      t_Y4_ON(2)=-t_Y4_ON(1)
      t_X4_ON(3)=t_Y4_ON(1)*COS30
      t_Y4_ON(3)=t_Y4_ON(1)*SIN30
      t_Y4_ON(4)=-t_Y4_ON(3)
      t_X4_ON(5)=-t_X4_ON(3)

      t_X4(1)=0
      t_Y4(1)=t_Y3(1)-(t_PAR3(1)+t_PAR4(3))*SIN10
      t_Z4=t_Z3+(t_PAR3(1)+t_PAR4(3))*COS10
      t_Y4(2)=-t_Y4(1)
      t_X4(3)=t_Y4(1)*COS30
      t_Y4(3)=t_Y4(1)*SIN30
      t_Y4(4)=-t_Y4(3)
      t_X4(5)=-t_X4(3)

      t_X5(1)=0
      t_Y5(1)=(Y5(1)-PAR5(2)*COS17)*(1-SIN10)
      t_Z5=1.316+(Y5(1)-PAR5(2)*COS17)*COS10
      t_Y5(2)=-t_Y5(1)
      t_X5(3)=t_Y5(1)*COS30
      t_Y5(3)=t_Y5(1)*SIN30
      t_Y5(4)=-t_Y5(3)
      t_X5(5)=-t_X5(3)

      t_Y6_l(1)=-(0.2377-0.1234*COS30)
      t_X6_l(1)=-(0.042+0.1234*SIN30-0.0127)
      t_X6_r(1)=-t_X6_l(1)

C     the position of the first  BOX in VOLM is (y=8.396; z=-13.263) (cms)
C     the position of the second BOX in VOLM is (y=7.873; z=-8.148) (cms)
C     the position of the third  BOX in VOLM is (y=6.923; z=-2.086) (cms)

      CALL GSVOLU('VAR1', 'BOX ', clas_med_alu   , PAR1   , 3, IVOL)
      CALL GSPOS('VAR1', 1, 'VOLM', X1(1), Y1(1), Z1, 0, 'ONLY')
      CALL GSPOS('VAR1', 2, 'VOLM', X1(1), Y1(2), Z1, 0, 'ONLY')
      CALL GSPOS('VAR1', 3, 'VOLM', X1(3), Y1(3), Z1, irt_sch3F, 'ONLY')
      CALL GSPOS('VAR1', 4, 'VOLM', X1(3), Y1(4), Z1, irt_sch4F, 'ONLY')
      CALL GSPOS('VAR1', 5, 'VOLM', X1(5), Y1(4), Z1, irt_sch5F, 'ONLY')
      CALL GSPOS('VAR1', 6, 'VOLM', X1(5), Y1(3), Z1, irt_sch6F, 'ONLY')

      CALL GSVOLU('VAR2', 'BOX ', clas_med_alu   , PAR2   , 3, IVOL)
      CALL GSPOS('VAR2', 1, 'VOLM', X2(1), Y2(1), Z2, irt_sch1, 'ONLY')
      CALL GSPOS('VAR2', 2, 'VOLM', X2(1), Y2(2), Z2, irt_sch2, 'ONLY')
      CALL GSPOS('VAR2', 3, 'VOLM', X2(3), Y2(3), Z2, irt_sch3, 'ONLY')
      CALL GSPOS('VAR2', 4, 'VOLM', X2(3), Y2(4), Z2, irt_sch4, 'ONLY')
      CALL GSPOS('VAR2', 5, 'VOLM', X2(5), Y2(4), Z2, irt_sch5, 'ONLY')
      CALL GSPOS('VAR2', 6, 'VOLM', X2(5), Y2(3), Z2, irt_sch6, 'ONLY')

      CALL GSVOLU('VAR3', 'BOX ', clas_med_alu   , PAR3   , 3, IVOL)
      CALL GSPOS('VAR3', 1, 'VOLM', X3(1), Y3(1), Z3, irt_sch1, 'ONLY')
      CALL GSPOS('VAR3', 2, 'VOLM', X3(1), Y3(2), Z3, irt_sch2, 'ONLY')
      CALL GSPOS('VAR3', 3, 'VOLM', X3(3), Y3(3), Z3, irt_sch3, 'ONLY')
      CALL GSPOS('VAR3', 4, 'VOLM', X3(3), Y3(4), Z3, irt_sch4, 'ONLY')
      CALL GSPOS('VAR3', 5, 'VOLM', X3(5), Y3(4), Z3, irt_sch5, 'ONLY')
      CALL GSPOS('VAR3', 6, 'VOLM', X3(5), Y3(3), Z3, irt_sch6, 'ONLY')

      CALL GSVOLU('VAR4', 'BOX ', clas_med_alu   , PAR4   , 3, IVOL)
      CALL GSPOS('VAR4', 1, 'VOLM', X4(1), Y4(1), Z4, irt_sch1, 'ONLY')
      CALL GSPOS('VAR4', 2, 'VOLM', X4(1), Y4(2), Z4, irt_sch2, 'ONLY')
      CALL GSPOS('VAR4', 3, 'VOLM', X4(3), Y4(3), Z4, irt_sch3, 'ONLY')
      CALL GSPOS('VAR4', 4, 'VOLM', X4(3), Y4(4), Z4, irt_sch4, 'ONLY')
      CALL GSPOS('VAR4', 5, 'VOLM', X4(5), Y4(4), Z4, irt_sch5, 'ONLY')
      CALL GSPOS('VAR4', 6, 'VOLM', X4(5), Y4(3), Z4, irt_sch6, 'ONLY')

      CALL GSVOLU('VAR5', 'BOX ', clas_med_alu   , PAR5   , 3, IVOL)
      CALL GSPOS('VAR5', 1, 'VOLM', X5(1), Y5(1), Z5, irt_sch1, 'ONLY')
      CALL GSPOS('VAR5', 2, 'VOLM', X5(1), Y5(2), Z5, irt_sch2, 'ONLY')
      CALL GSPOS('VAR5', 3, 'VOLM', X5(3), Y5(3), Z5, irt_sch3, 'ONLY')
      CALL GSPOS('VAR5', 4, 'VOLM', X5(3), Y5(4), Z5, irt_sch4, 'ONLY')
      CALL GSPOS('VAR5', 5, 'VOLM', X5(5), Y5(4), Z5, irt_sch5, 'ONLY')
      CALL GSPOS('VAR5', 6, 'VOLM', X5(5), Y5(3), Z5, irt_sch6, 'ONLY')

      CALL GSVOLU('VAR6', 'TRD1', clas_med_alu   , PAR6   , 4, IVOL)
      CALL GSPOS('VAR6', 1, 'VOLM', X6(1), Y6(1), Z6, irt_trd1, 'ONLY')
      CALL GSPOS('VAR6', 2, 'VOLM', X6(1), Y6(2), Z6, irt_trd2, 'ONLY')
      CALL GSPOS('VAR6', 3, 'VOLM', X6(3), Y6(3), Z6, irt_trd3, 'ONLY')
      CALL GSPOS('VAR6', 4, 'VOLM', X6(3), Y6(4), Z6, irt_trd4, 'ONLY')
      CALL GSPOS('VAR6', 5, 'VOLM', X6(5), Y6(4), Z6, irt_trd5, 'ONLY')
      CALL GSPOS('VAR6', 6, 'VOLM', X6(5), Y6(3), Z6, irt_trd6, 'ONLY')

C     Wire from stainless steel with thickness 0.159(cms) and length 6.485(cms)
C     In case of target on beam it has 25.96 digree with z axis and in case 
C     wire out of beam it has 27.12 digree with z axis
C     the position of the WIRE in VOLM is (y=5.390; z=-3.246) (cms)
      CALL GSVOLU('WIRE', 'TUBE', clas_med_iron   , PARWIRE   , 3, IVOL)
      CALL GSVOLU('WION', 'TUBE', clas_med_iron   , PARWIRE_ON   , 3, IVOL)
C     Thin BOX connecting WIRE vith VAR6
      CALL GSVOLU('VAR7', 'BOX ', clas_med_alu   , PAR7   , 3, IVOL)
C     The Circle on VAR7
      CALL GSVOLU('VAR8', 'TUBE', clas_med_alu   , PAR8   , 3, IVOL)
C     The big BOX connecting circle with wire
      CALL GSVOLU('VAR9', 'BOX ', clas_med_alu   , PAR9   , 3, IVOL)
C     The small BOX connecting circle with wire
      CALL GSVOLU('VA10', 'BOX ', clas_med_alu   , PAR10   , 3, IVOL)
C     The stainless steel part of target stalk
      CALL GSVOLU('TVR1', 'BOX ', clas_med_iron   , t_PAR1   , 3, IVOL)
C     The aluminum tube part of target stalk
      CALL GSVOLU('TVR2', 'TUBE', clas_med_alu   , t_PAR2   , 3, IVOL)
C     The aluminum cone part of target stalk
      CALL GSVOLU('TVR3', 'CONE', clas_med_alu   , t_PAR3   , 5, IVOL)
C     The carbon tube part of target stalk
      CALL GSVOLU('TVR4', 'TUBE', clas_med_carbon   , t_PAR4   , 3, IVOL)
C     Solid targets
      CALL GSVOLU('VALA', 'TUBE', clas_med_alu   , ALA   , 3, IVOL)
      CALL GSVOLU('VALB', 'TUBE', clas_med_alu   , ALB   , 3, IVOL)
      CALL GSVOLU('VF56', 'TUBE',clas_med_iron   , FE56  , 3, IVOL)       !solid target 56Fe
      CALL GSVOLU('VCAR', 'TUBE',clas_med_carbon , C12   , 3, IVOL)       !solid tg.  12C
      CALL GSVOLU('VLED', 'TUBE',clas_med_lead   , PB    , 3, IVOL)      !solid tg. 207Pb
      CALL GSVOLU('VTIN', 'TUBE',clas_med_lead   , SN    , 3, IVOL)      !solid tg. 119Sn
C     The vacum BOX wich keeps inside the solid target system
      CALL GSVOLU('TCAR', 'BOX ', clas_med_vac   , t_PAR_TARG   , 5, IVOL)
      CALL GSPOS('VCAR', 1, 'TCAR', 0., 0.0623,  0.  ,  0, 'ONLY')
      CALL GSVOLU('TF56', 'BOX ', clas_med_vac   , t_PAR_TARG   , 5, IVOL)
      CALL GSPOS('VF56', 1, 'TF56', 0., 0.0623,  0.  ,  0, 'ONLY')
      CALL GSVOLU('TLED', 'BOX ', clas_med_vac   , t_PAR_TARG   , 5, IVOL)
      CALL GSPOS('VLED', 1, 'TLED', 0., 0.0623,  0.  ,  0, 'ONLY')
      CALL GSVOLU('TALA', 'BOX ', clas_med_vac   , t_PAR_TARG   , 5, IVOL)
      CALL GSPOS('VALA', 1, 'TALA', 0., 0.0623,  0.  ,  0, 'ONLY')
      CALL GSVOLU('TALB', 'BOX ', clas_med_vac   , t_PAR_TARG   , 5, IVOL)
      CALL GSPOS('VALB', 1, 'TALB', 0., 0.0623,  0.  ,  0, 'ONLY')
      CALL GSVOLU('TTIN', 'BOX ', clas_med_vac   , t_PAR_TARG   , 5, IVOL)
      CALL GSPOS('VTIN', 1, 'TTIN', 0., 0.0623,  0.  ,  0, 'ONLY')
C     The carbon circle surrounding the solid target
      CALL GSVOLU('TVR5', 'TUBS', clas_med_carbon   , t_PAR_CIRC   , 5, IVOL)
      CALL GSPOS('TVR5', 1, 'TCAR', 0., 0.0623, 0., 0, 'ONLY')
      CALL GSPOS('TVR5', 2, 'TF56', 0., 0.0623, 0., 0, 'ONLY')
      CALL GSPOS('TVR5', 3, 'TLED', 0., 0.0623, 0., 0, 'ONLY')
      CALL GSPOS('TVR5', 4, 'TALA', 0., 0.0623, 0., 0, 'ONLY')
      CALL GSPOS('TVR5', 5, 'TALB', 0., 0.0623, 0., 0, 'ONLY')
      CALL GSPOS('TVR5', 6, 'TTIN', 0., 0.0623, 0., 0, 'ONLY')
      CALL GSVOLU('TVR6', 'TRD1', clas_med_carbon   , t_PAR_ARM   , 5, IVOL)
C     The carbon left arm of carbon circle surrounding the solid target
      CALL GSPOS('TVR6', 1, 'TCAR', t_X6_l(1), t_Y6_l(1), 0., circ_leg11, 'ONLY')
      CALL GSPOS('TVR6', 2, 'TF56', t_X6_l(1), t_Y6_l(1), 0., circ_leg11, 'ONLY')
      CALL GSPOS('TVR6', 3, 'TLED', t_X6_l(1), t_Y6_l(1), 0., circ_leg11, 'ONLY')
      CALL GSPOS('TVR6', 4, 'TALA', t_X6_l(1), t_Y6_l(1), 0., circ_leg11, 'ONLY')
      CALL GSPOS('TVR6', 5, 'TALB', t_X6_l(1), t_Y6_l(1), 0., circ_leg11, 'ONLY')
      CALL GSPOS('TVR6', 6, 'TTIN', t_X6_l(1), t_Y6_l(1), 0., circ_leg11, 'ONLY')
C     The carbon right arm of carbon circle surrounding the solid target
      CALL GSPOS('TVR6', 7, 'TCAR', t_X6_r(1), t_Y6_l(1), 0., circ_leg10, 'ONLY')
      CALL GSPOS('TVR6', 8, 'TF56', t_X6_r(1), t_Y6_l(1), 0., circ_leg10, 'ONLY')
      CALL GSPOS('TVR6', 9, 'TLED', t_X6_r(1), t_Y6_l(1), 0., circ_leg10, 'ONLY')
      CALL GSPOS('TVR6', 10, 'TALA', t_X6_r(1), t_Y6_l(1), 0., circ_leg10, 'ONLY')
      CALL GSPOS('TVR6', 11, 'TALB', t_X6_r(1), t_Y6_l(1), 0., circ_leg10, 'ONLY')
      CALL GSPOS('TVR6', 12, 'TTIN', t_X6_r(1), t_Y6_l(1), 0., circ_leg10, 'ONLY')

      if(itg_type(1).eq.56)then
      CALL GSPOS('WIRE', 1, 'VOLM', X7(1), Y7(1), Z7, irt_wire1, 'ONLY')
      CALL GSPOS('WION', 1, 'VOLM', X7(1), Y7_ON(2), Z7_ON, irt_wire2_ON, 'ONLY')
      CALL GSPOS('WIRE', 2, 'VOLM', X7(3), Y7(3), Z7, irt_wire3, 'ONLY')
      CALL GSPOS('WIRE', 3, 'VOLM', X7(3), Y7(4), Z7, irt_wire4, 'ONLY')
      CALL GSPOS('WIRE', 4, 'VOLM', X7(5), Y7(4), Z7, irt_wire5, 'ONLY')
      CALL GSPOS('WIRE', 5, 'VOLM', X7(5), Y7(3), Z7, irt_wire6, 'ONLY')

      CALL GSPOS('VAR7', 1, 'VOLM', X8(1), Y8(1), Z8, irt_level1, 'ONLY')
      CALL GSPOS('VAR7', 2, 'VOLM', X8(1), Y8_ON(2), Z8_ON, irt_level2_ON, 'ONLY')
      CALL GSPOS('VAR7', 3, 'VOLM', X8(3), Y8(3), Z8, irt_level3, 'ONLY')
      CALL GSPOS('VAR7', 4, 'VOLM', X8(3), Y8(4), Z8, irt_level4, 'ONLY')
      CALL GSPOS('VAR7', 5, 'VOLM', X8(5), Y8(4), Z8, irt_level5, 'ONLY')
      CALL GSPOS('VAR7', 6, 'VOLM', X8(5), Y8(3), Z8, irt_level6, 'ONLY')

      CALL GSPOS('VAR8', 1, 'VOLM', X9(1), Y9(1), Z9, circ_level1, 'ONLY')
      CALL GSPOS('VAR8', 2, 'VOLM', X9(1), Y9_ON(2), Z9_ON, circ_level2, 'ONLY')
      CALL GSPOS('VAR8', 3, 'VOLM', X9(3), Y9(3), Z9, circ_level3, 'ONLY')
      CALL GSPOS('VAR8', 4, 'VOLM', X9(3), Y9(4), Z9, circ_level4, 'ONLY')
      CALL GSPOS('VAR8', 5, 'VOLM', X9(5), Y9(4), Z9, circ_level5, 'ONLY')
      CALL GSPOS('VAR8', 6, 'VOLM', X9(5), Y9(3), Z9, circ_level6, 'ONLY')

      CALL GSPOS('VAR9', 1, 'VOLM', X10(1), Y10(1), Z10, irt_wire1, 'ONLY')
      CALL GSPOS('VAR9', 2, 'VOLM', X10(1), Y10_ON(2), Z10_ON, irt_wire2_ON, 'ONLY')
      CALL GSPOS('VAR9', 3, 'VOLM', X10(3), Y10(3), Z10, irt_wire3, 'ONLY')
      CALL GSPOS('VAR9', 4, 'VOLM', X10(3), Y10(4), Z10, irt_wire4, 'ONLY')
      CALL GSPOS('VAR9', 5, 'VOLM', X10(5), Y10(4), Z10, irt_wire5, 'ONLY')
      CALL GSPOS('VAR9', 6, 'VOLM', X10(5), Y10(3), Z10, irt_wire6, 'ONLY')

      CALL GSPOS('VA10', 1, 'VOLM', X11(1), Y11(1), Z11, irt_wire1, 'ONLY')
      CALL GSPOS('VA10', 2, 'VOLM', X11(1), Y11_ON(2), Z11_ON, irt_wire2_ON, 'ONLY')
      CALL GSPOS('VA10', 3, 'VOLM', X11(3), Y11(3), Z11, irt_wire3, 'ONLY')
      CALL GSPOS('VA10', 4, 'VOLM', X11(3), Y11(4), Z11, irt_wire4, 'ONLY')
      CALL GSPOS('VA10', 5, 'VOLM', X11(5), Y11(4), Z11, irt_wire5, 'ONLY')
      CALL GSPOS('VA10', 6, 'VOLM', X11(5), Y11(3), Z11, irt_wire6, 'ONLY')

      CALL GSPOS('TVR1', 1, 'VOLM', t_X1(1), t_Y1(1), t_Z1, irt_targ1, 'ONLY')
      CALL GSPOS('TVR1', 2, 'VOLM', t_X1(1), t_Y1_ON(2), t_Z1_ON, irt_targ2_ON, 'ONLY')
      CALL GSPOS('TVR1', 3, 'VOLM', t_X1(3), t_Y1(3), t_Z1, irt_targ3, 'ONLY')
      CALL GSPOS('TVR1', 4, 'VOLM', t_X1(3), t_Y1(4), t_Z1, irt_targ4, 'ONLY')
      CALL GSPOS('TVR1', 5, 'VOLM', t_X1(5), t_Y1(4), t_Z1, irt_targ5, 'ONLY')
      CALL GSPOS('TVR1', 6, 'VOLM', t_X1(5), t_Y1(3), t_Z1, irt_targ6, 'ONLY')

      CALL GSPOS('TVR2', 1, 'VOLM', t_X2(1), t_Y2(1), t_Z2, irt_targ1, 'ONLY')
      CALL GSPOS('TVR2', 2, 'VOLM', t_X2(1), t_Y2_ON(2), t_Z2_ON, irt_targ2_ON, 'ONLY')
      CALL GSPOS('TVR2', 3, 'VOLM', t_X2(3), t_Y2(3), t_Z2, irt_targ3, 'ONLY')
      CALL GSPOS('TVR2', 4, 'VOLM', t_X2(3), t_Y2(4), t_Z2, irt_targ4, 'ONLY')
      CALL GSPOS('TVR2', 5, 'VOLM', t_X2(5), t_Y2(4), t_Z2, irt_targ5, 'ONLY')
      CALL GSPOS('TVR2', 6, 'VOLM', t_X2(5), t_Y2(3), t_Z2, irt_targ6, 'ONLY')

      CALL GSPOS('TVR3', 1, 'VOLM', t_X3(1), t_Y3(1), t_Z3, irt_targ1, 'ONLY')
      CALL GSPOS('TVR3', 2, 'VOLM', t_X3(1), t_Y3_ON(2), t_Z3_ON, irt_targ2_ON, 'ONLY')
      CALL GSPOS('TVR3', 3, 'VOLM', t_X3(3), t_Y3(3), t_Z3, irt_targ3, 'ONLY')
      CALL GSPOS('TVR3', 4, 'VOLM', t_X3(3), t_Y3(4), t_Z3, irt_targ4, 'ONLY')
      CALL GSPOS('TVR3', 5, 'VOLM', t_X3(5), t_Y3(4), t_Z3, irt_targ5, 'ONLY')
      CALL GSPOS('TVR3', 6, 'VOLM', t_X3(5), t_Y3(3), t_Z3, irt_targ6, 'ONLY')

      CALL GSPOS('TVR4', 1, 'VOLM', t_X4(1), t_Y4(1), t_Z4, irt_targ1, 'ONLY')
      CALL GSPOS('TVR4', 2, 'VOLM', t_X4(1), t_Y4_ON(2), t_Z4_ON, irt_targ2_ON, 'ONLY')
      CALL GSPOS('TVR4', 3, 'VOLM', t_X4(3), t_Y4(3), t_Z4, irt_targ3, 'ONLY')
      CALL GSPOS('TVR4', 4, 'VOLM', t_X4(3), t_Y4(4), t_Z4, irt_targ4, 'ONLY')
      CALL GSPOS('TVR4', 5, 'VOLM', t_X4(5), t_Y4(4), t_Z4, irt_targ5, 'ONLY')
      CALL GSPOS('TVR4', 6, 'VOLM', t_X4(5), t_Y4(3), t_Z4, irt_targ6, 'ONLY')

      CALL GSPOS('TALA', 1, 'VOLM', t_X5(1), t_Y5(1), t_Z5, circ_targ1, 'ONLY')
      CALL GSPOS('TF56', 1, 'VOLM', 0., -0.0623, 1.316, irt_tbox2_ON, 'MANY')
      CALL GSPOS('TCAR', 1, 'VOLM', t_X5(3), t_Y5(3), t_Z5, circ_targ3, 'ONLY')
      CALL GSPOS('TLED', 1, 'VOLM', t_X5(3), t_Y5(4), t_Z5, circ_targ4, 'ONLY')
      CALL GSPOS('TTIN', 1, 'VOLM', t_X5(5), t_Y5(4), t_Z5, circ_targ5, 'ONLY')
      CALL GSPOS('TALB', 1, 'VOLM', t_X5(5), t_Y5(3), t_Z5, circ_targ6, 'ONLY')

      elseif(itg_type(1).eq.27)then
      CALL GSPOS('WION', 1, 'VOLM', X7(1), Y7_ON(1), Z7_ON, irt_wire1_ON, 'ONLY')
      CALL GSPOS('WIRE', 1, 'VOLM', X7(1), Y7(2), Z7, irt_wire2, 'ONLY')
      CALL GSPOS('WIRE', 2, 'VOLM', X7(3), Y7(3), Z7, irt_wire3, 'ONLY')
      CALL GSPOS('WIRE', 3, 'VOLM', X7(3), Y7(4), Z7, irt_wire4, 'ONLY')
      CALL GSPOS('WIRE', 4, 'VOLM', X7(5), Y7(4), Z7, irt_wire5, 'ONLY')
      CALL GSPOS('WIRE', 5, 'VOLM', X7(5), Y7(3), Z7, irt_wire6, 'ONLY')

      CALL GSPOS('VAR7', 1, 'VOLM', X8(1), Y8_ON(1), Z8_ON, irt_level1_ON, 'ONLY')
      CALL GSPOS('VAR7', 2, 'VOLM', X8(1), Y8(2), Z8, irt_level2, 'ONLY')
      CALL GSPOS('VAR7', 3, 'VOLM', X8(3), Y8(3), Z8, irt_level3, 'ONLY')
      CALL GSPOS('VAR7', 4, 'VOLM', X8(3), Y8(4), Z8, irt_level4, 'ONLY')
      CALL GSPOS('VAR7', 5, 'VOLM', X8(5), Y8(4), Z8, irt_level5, 'ONLY')
      CALL GSPOS('VAR7', 6, 'VOLM', X8(5), Y8(3), Z8, irt_level6, 'ONLY')

      CALL GSPOS('VAR8', 1, 'VOLM', X9(1), Y9_ON(1), Z9_ON, circ_level1, 'ONLY')
      CALL GSPOS('VAR8', 2, 'VOLM', X9(1), Y9(2), Z9, circ_level2, 'ONLY')
      CALL GSPOS('VAR8', 3, 'VOLM', X9(3), Y9(3), Z9, circ_level3, 'ONLY')
      CALL GSPOS('VAR8', 4, 'VOLM', X9(3), Y9(4), Z9, circ_level4, 'ONLY')
      CALL GSPOS('VAR8', 5, 'VOLM', X9(5), Y9(4), Z9, circ_level5, 'ONLY')
      CALL GSPOS('VAR8', 6, 'VOLM', X9(5), Y9(3), Z9, circ_level6, 'ONLY')

      CALL GSPOS('VAR9', 1, 'VOLM', X10(1), Y10_ON(1), Z10_ON, irt_wire1_ON, 'ONLY')
      CALL GSPOS('VAR9', 2, 'VOLM', X10(1), Y10(2), Z10, irt_wire2, 'ONLY')
      CALL GSPOS('VAR9', 3, 'VOLM', X10(3), Y10(3), Z10, irt_wire3, 'ONLY')
      CALL GSPOS('VAR9', 4, 'VOLM', X10(3), Y10(4), Z10, irt_wire4, 'ONLY')
      CALL GSPOS('VAR9', 5, 'VOLM', X10(5), Y10(4), Z10, irt_wire5, 'ONLY')
      CALL GSPOS('VAR9', 6, 'VOLM', X10(5), Y10(3), Z10, irt_wire6, 'ONLY')

      CALL GSPOS('VA10', 1, 'VOLM', X11(1), Y11_ON(1), Z11_ON, irt_wire1_ON, 'ONLY')
      CALL GSPOS('VA10', 2, 'VOLM', X11(1), Y11(2), Z11, irt_wire2, 'ONLY')
      CALL GSPOS('VA10', 3, 'VOLM', X11(3), Y11(3), Z11, irt_wire3, 'ONLY')
      CALL GSPOS('VA10', 4, 'VOLM', X11(3), Y11(4), Z11, irt_wire4, 'ONLY')
      CALL GSPOS('VA10', 5, 'VOLM', X11(5), Y11(4), Z11, irt_wire5, 'ONLY')
      CALL GSPOS('VA10', 6, 'VOLM', X11(5), Y11(3), Z11, irt_wire6, 'ONLY')

      CALL GSPOS('TVR1', 1, 'VOLM', t_X1(1), t_Y1_ON(1), t_Z1_ON, irt_targ1_ON, 'ONLY')
      CALL GSPOS('TVR1', 2, 'VOLM', t_X1(1), t_Y1(2), t_Z1, irt_targ2, 'ONLY')
      CALL GSPOS('TVR1', 3, 'VOLM', t_X1(3), t_Y1(3), t_Z1, irt_targ3, 'ONLY')
      CALL GSPOS('TVR1', 4, 'VOLM', t_X1(3), t_Y1(4), t_Z1, irt_targ4, 'ONLY')
      CALL GSPOS('TVR1', 5, 'VOLM', t_X1(5), t_Y1(4), t_Z1, irt_targ5, 'ONLY')
      CALL GSPOS('TVR1', 6, 'VOLM', t_X1(5), t_Y1(3), t_Z1, irt_targ6, 'ONLY')

      CALL GSPOS('TVR2', 1, 'VOLM', t_X2(1), t_Y2_ON(1), t_Z2_ON, irt_targ1_ON, 'ONLY')
      CALL GSPOS('TVR2', 2, 'VOLM', t_X2(1), t_Y2(2), t_Z2, irt_targ2, 'ONLY')
      CALL GSPOS('TVR2', 3, 'VOLM', t_X2(3), t_Y2(3), t_Z2, irt_targ3, 'ONLY')
      CALL GSPOS('TVR2', 4, 'VOLM', t_X2(3), t_Y2(4), t_Z2, irt_targ4, 'ONLY')
      CALL GSPOS('TVR2', 5, 'VOLM', t_X2(5), t_Y2(4), t_Z2, irt_targ5, 'ONLY')
      CALL GSPOS('TVR2', 6, 'VOLM', t_X2(5), t_Y2(3), t_Z2, irt_targ6, 'ONLY')

      CALL GSPOS('TVR3', 1, 'VOLM', t_X3(1), t_Y3_ON(1), t_Z3_ON, irt_targ1_ON, 'ONLY')
      CALL GSPOS('TVR3', 2, 'VOLM', t_X3(1), t_Y3(2), t_Z3, irt_targ2, 'ONLY')
      CALL GSPOS('TVR3', 3, 'VOLM', t_X3(3), t_Y3(3), t_Z3, irt_targ3, 'ONLY')
      CALL GSPOS('TVR3', 4, 'VOLM', t_X3(3), t_Y3(4), t_Z3, irt_targ4, 'ONLY')
      CALL GSPOS('TVR3', 5, 'VOLM', t_X3(5), t_Y3(4), t_Z3, irt_targ5, 'ONLY')
      CALL GSPOS('TVR3', 6, 'VOLM', t_X3(5), t_Y3(3), t_Z3, irt_targ6, 'ONLY')

      CALL GSPOS('TVR4', 1, 'VOLM', t_X4(1), t_Y4_ON(1), t_Z4_ON, irt_targ1_ON, 'ONLY')
      CALL GSPOS('TVR4', 2, 'VOLM', t_X4(1), t_Y4(2), t_Z4, irt_targ2, 'ONLY')
      CALL GSPOS('TVR4', 3, 'VOLM', t_X4(3), t_Y4(3), t_Z4, irt_targ3, 'ONLY')
      CALL GSPOS('TVR4', 4, 'VOLM', t_X4(3), t_Y4(4), t_Z4, irt_targ4, 'ONLY')
      CALL GSPOS('TVR4', 5, 'VOLM', t_X4(5), t_Y4(4), t_Z4, irt_targ5, 'ONLY')
      CALL GSPOS('TVR4', 6, 'VOLM', t_X4(5), t_Y4(3), t_Z4, irt_targ6, 'ONLY')

      CALL GSPOS('TALA', 1, 'VOLM', 0., 0.0623, 1.316,  irt_tbox1_ON, 'MANY')
      CALL GSPOS('TF56', 1, 'VOLM', t_X5(1), t_Y5(2), t_Z5, circ_targ2, 'ONLY')
      CALL GSPOS('TCAR', 1, 'VOLM', t_X5(3), t_Y5(3), t_Z5, circ_targ3, 'ONLY')
      CALL GSPOS('TLED', 1, 'VOLM', t_X5(3), t_Y5(4), t_Z5, circ_targ4, 'ONLY')
      CALL GSPOS('TTIN', 1, 'VOLM', t_X5(5), t_Y5(4), t_Z5, circ_targ5, 'ONLY')
      CALL GSPOS('TALB', 1, 'VOLM', t_X5(5), t_Y5(3), t_Z5, circ_targ6, 'ONLY')

      elseif(itg_type(1).eq.12)then
      CALL GSPOS('WIRE', 1, 'VOLM', X7(1), Y7(1), Z7, irt_wire1, 'ONLY')
      CALL GSPOS('WIRE', 2, 'VOLM', X7(1), Y7(2), Z7, irt_wire2, 'ONLY')
      CALL GSPOS('WION', 1, 'VOLM', X7_ON(3), Y7_ON(3), Z7_ON, irt_wire3_ON, 'ONLY')
      CALL GSPOS('WIRE', 3, 'VOLM', X7(3), Y7(4), Z7, irt_wire4, 'ONLY')
      CALL GSPOS('WIRE', 4, 'VOLM', X7(5), Y7(4), Z7, irt_wire5, 'ONLY')
      CALL GSPOS('WIRE', 5, 'VOLM', X7(5), Y7(3), Z7, irt_wire6, 'ONLY')

      CALL GSPOS('VAR7', 1, 'VOLM', X8(1), Y8(1), Z8, irt_level1, 'ONLY')
      CALL GSPOS('VAR7', 2, 'VOLM', X8(1), Y8(2), Z8, irt_level2, 'ONLY')
      CALL GSPOS('VAR7', 3, 'VOLM', X8_ON(3), Y8_ON(3), Z8_ON, irt_level3_ON, 'ONLY')
      CALL GSPOS('VAR7', 4, 'VOLM', X8(3), Y8(4), Z8, irt_level4, 'ONLY')
      CALL GSPOS('VAR7', 5, 'VOLM', X8(5), Y8(4), Z8, irt_level5, 'ONLY')
      CALL GSPOS('VAR7', 6, 'VOLM', X8(5), Y8(3), Z8, irt_level6, 'ONLY')

      CALL GSPOS('VAR8', 1, 'VOLM', X9(1), Y9(1), Z9, circ_level1, 'ONLY')
      CALL GSPOS('VAR8', 2, 'VOLM', X9(1), Y9(2), Z9, circ_level2, 'ONLY')
      CALL GSPOS('VAR8', 3, 'VOLM', X9_ON(3), Y9_ON(3), Z9_ON, circ_level3, 'ONLY')
      CALL GSPOS('VAR8', 4, 'VOLM', X9(3), Y9(4), Z9, circ_level4, 'ONLY')
      CALL GSPOS('VAR8', 5, 'VOLM', X9(5), Y9(4), Z9, circ_level5, 'ONLY')
      CALL GSPOS('VAR8', 6, 'VOLM', X9(5), Y9(3), Z9, circ_level6, 'ONLY')

      CALL GSPOS('VAR9', 1, 'VOLM', X10(1), Y10(1), Z10, irt_wire1, 'ONLY')
      CALL GSPOS('VAR9', 2, 'VOLM', X10(1), Y10(2), Z10, irt_wire2, 'ONLY')
      CALL GSPOS('VAR9', 3, 'VOLM', X10_ON(3), Y10_ON(3), Z10_ON, irt_wire3_ON, 'ONLY')
      CALL GSPOS('VAR9', 4, 'VOLM', X10(3), Y10(4), Z10, irt_wire4, 'ONLY')
      CALL GSPOS('VAR9', 5, 'VOLM', X10(5), Y10(4), Z10, irt_wire5, 'ONLY')
      CALL GSPOS('VAR9', 6, 'VOLM', X10(5), Y10(3), Z10, irt_wire6, 'ONLY')

      CALL GSPOS('VA10', 1, 'VOLM', X11(1), Y11(1), Z11, irt_wire1, 'ONLY')
      CALL GSPOS('VA10', 2, 'VOLM', X11(1), Y11(2), Z11, irt_wire2, 'ONLY')
      CALL GSPOS('VA10', 3, 'VOLM', X11_ON(3), Y11_ON(3), Z11_ON, irt_wire3_ON, 'ONLY')
      CALL GSPOS('VA10', 4, 'VOLM', X11(3), Y11(4), Z11, irt_wire4, 'ONLY')
      CALL GSPOS('VA10', 5, 'VOLM', X11(5), Y11(4), Z11, irt_wire5, 'ONLY')
      CALL GSPOS('VA10', 6, 'VOLM', X11(5), Y11(3), Z11, irt_wire6, 'ONLY')

      CALL GSPOS('TVR1', 1, 'VOLM', t_X1(1), t_Y1(1), t_Z1, irt_targ1, 'ONLY')
      CALL GSPOS('TVR1', 2, 'VOLM', t_X1(1), t_Y1(2), t_Z1, irt_targ2, 'ONLY')
      CALL GSPOS('TVR1', 3, 'VOLM', t_X1_ON(3), t_Y1_ON(3), t_Z1_ON, irt_targ3_ON, 'ONLY')
      CALL GSPOS('TVR1', 4, 'VOLM', t_X1(3), t_Y1(4), t_Z1, irt_targ4, 'ONLY')
      CALL GSPOS('TVR1', 5, 'VOLM', t_X1(5), t_Y1(4), t_Z1, irt_targ5, 'ONLY')
      CALL GSPOS('TVR1', 6, 'VOLM', t_X1(5), t_Y1(3), t_Z1, irt_targ6, 'ONLY')

      CALL GSPOS('TVR2', 1, 'VOLM', t_X2(1), t_Y2(1), t_Z2, irt_targ1, 'ONLY')
      CALL GSPOS('TVR2', 2, 'VOLM', t_X2(1), t_Y2(2), t_Z2, irt_targ2, 'ONLY')
      CALL GSPOS('TVR2', 3, 'VOLM', t_X2_ON(3), t_Y2_ON(3), t_Z2_ON, irt_targ3_ON, 'ONLY')
      CALL GSPOS('TVR2', 4, 'VOLM', t_X2(3), t_Y2(4), t_Z2, irt_targ4, 'ONLY')
      CALL GSPOS('TVR2', 5, 'VOLM', t_X2(5), t_Y2(4), t_Z2, irt_targ5, 'ONLY')
      CALL GSPOS('TVR2', 6, 'VOLM', t_X2(5), t_Y2(3), t_Z2, irt_targ6, 'ONLY')

      CALL GSPOS('TVR3', 1, 'VOLM', t_X3(1), t_Y3(1), t_Z3, irt_targ1, 'ONLY')
      CALL GSPOS('TVR3', 2, 'VOLM', t_X3(1), t_Y3(2), t_Z3, irt_targ2, 'ONLY')
      CALL GSPOS('TVR3', 3, 'VOLM', t_X3_ON(3), t_Y3_ON(3), t_Z3_ON, irt_targ3_ON, 'ONLY')
      CALL GSPOS('TVR3', 4, 'VOLM', t_X3(3), t_Y3(4), t_Z3, irt_targ4, 'ONLY')
      CALL GSPOS('TVR3', 5, 'VOLM', t_X3(5), t_Y3(4), t_Z3, irt_targ5, 'ONLY')
      CALL GSPOS('TVR3', 6, 'VOLM', t_X3(5), t_Y3(3), t_Z3, irt_targ6, 'ONLY')

      CALL GSPOS('TVR4', 1, 'VOLM', t_X4(1), t_Y4(1), t_Z4, irt_targ1, 'ONLY')
      CALL GSPOS('TVR4', 2, 'VOLM', t_X4(1), t_Y4(2), t_Z4, irt_targ2, 'ONLY')
      CALL GSPOS('TVR4', 3, 'VOLM', t_X4_ON(3), t_Y4_ON(3), t_Z4_ON, irt_targ3_ON, 'ONLY')
      CALL GSPOS('TVR4', 4, 'VOLM', t_X4(3), t_Y4(4), t_Z4, irt_targ4, 'ONLY')
      CALL GSPOS('TVR4', 5, 'VOLM', t_X4(5), t_Y4(4), t_Z4, irt_targ5, 'ONLY')
      CALL GSPOS('TVR4', 6, 'VOLM', t_X4(5), t_Y4(3), t_Z4, irt_targ6, 'ONLY')

      CALL GSPOS('TALA', 1, 'VOLM', t_X5(1), t_Y5(1), t_Z5, circ_targ1, 'ONLY')
      CALL GSPOS('TF56', 1, 'VOLM', t_X5(1), t_Y5(2), t_Z5, circ_targ2, 'ONLY')
      CALL GSPOS('TCAR', 1, 'VOLM', 0.05395, 0.03115, 1.316,  irt_tbox3_ON, 'MANY')
      CALL GSPOS('TLED', 1, 'VOLM', t_X5(3), t_Y5(4), t_Z5, circ_targ4, 'ONLY')
      CALL GSPOS('TTIN', 1, 'VOLM', t_X5(5), t_Y5(4), t_Z5, circ_targ5, 'ONLY')
      CALL GSPOS('TALB', 1, 'VOLM', t_X5(5), t_Y5(3), t_Z5, circ_targ6, 'ONLY')

      elseif(itg_type(1).eq.207)then
      CALL GSPOS('WIRE', 1, 'VOLM', X7(1), Y7(1), Z7, irt_wire1, 'ONLY')
      CALL GSPOS('WIRE', 2, 'VOLM', X7(1), Y7(2), Z7, irt_wire2, 'ONLY')
      CALL GSPOS('WIRE', 3, 'VOLM', X7(3), Y7(3), Z7, irt_wire3, 'ONLY')
      CALL GSPOS('WION', 1, 'VOLM', X7_ON(3), Y7_ON(4), Z7_ON, irt_wire4_ON, 'ONLY')
      CALL GSPOS('WIRE', 4, 'VOLM', X7(5), Y7(4), Z7, irt_wire5, 'ONLY')
      CALL GSPOS('WIRE', 5, 'VOLM', X7(5), Y7(3), Z7, irt_wire6, 'ONLY')

      CALL GSPOS('VAR7', 1, 'VOLM', X8(1), Y8(1), Z8, irt_level1, 'ONLY')
      CALL GSPOS('VAR7', 2, 'VOLM', X8(1), Y8(2), Z8, irt_level2, 'ONLY')
      CALL GSPOS('VAR7', 3, 'VOLM', X8(3), Y8(3), Z8, irt_level3, 'ONLY')
      CALL GSPOS('VAR7', 4, 'VOLM', X8_ON(3), Y8_ON(4), Z8_ON, irt_level4_ON, 'ONLY')
      CALL GSPOS('VAR7', 5, 'VOLM', X8(5), Y8(4), Z8, irt_level5, 'ONLY')
      CALL GSPOS('VAR7', 6, 'VOLM', X8(5), Y8(3), Z8, irt_level6, 'ONLY')

      CALL GSPOS('VAR8', 1, 'VOLM', X9(1), Y9(1), Z9, circ_level1, 'ONLY')
      CALL GSPOS('VAR8', 2, 'VOLM', X9(1), Y9(2), Z9, circ_level2, 'ONLY')
      CALL GSPOS('VAR8', 3, 'VOLM', X9(3), Y9(3), Z9, circ_level3, 'ONLY')
      CALL GSPOS('VAR8', 4, 'VOLM', X9_ON(3), Y9_ON(4), Z9_ON, circ_level4, 'ONLY')
      CALL GSPOS('VAR8', 5, 'VOLM', X9(5), Y9(4), Z9, circ_level5, 'ONLY')
      CALL GSPOS('VAR8', 6, 'VOLM', X9(5), Y9(3), Z9, circ_level6, 'ONLY')

      CALL GSPOS('VAR9', 1, 'VOLM', X10(1), Y10(1), Z10, irt_wire1, 'ONLY')
      CALL GSPOS('VAR9', 2, 'VOLM', X10(1), Y10(2), Z10, irt_wire2, 'ONLY')
      CALL GSPOS('VAR9', 3, 'VOLM', X10(3), Y10(3), Z10, irt_wire3, 'ONLY')
      CALL GSPOS('VAR9', 4, 'VOLM', X10_ON(3), Y10_ON(4), Z10_ON, irt_wire4_ON, 'ONLY')
      CALL GSPOS('VAR9', 5, 'VOLM', X10(5), Y10(4), Z10, irt_wire5, 'ONLY')
      CALL GSPOS('VAR9', 6, 'VOLM', X10(5), Y10(3), Z10, irt_wire6, 'ONLY')

      CALL GSPOS('VA10', 1, 'VOLM', X11(1), Y11(1), Z11, irt_wire1, 'ONLY')
      CALL GSPOS('VA10', 2, 'VOLM', X11(1), Y11(2), Z11, irt_wire2, 'ONLY')
      CALL GSPOS('VA10', 3, 'VOLM', X11(3), Y11(3), Z11, irt_wire3, 'ONLY')
      CALL GSPOS('VA10', 4, 'VOLM', X11_ON(3), Y11_ON(4), Z11_ON, irt_wire4_ON, 'ONLY')
      CALL GSPOS('VA10', 5, 'VOLM', X11(5), Y11(4), Z11, irt_wire5, 'ONLY')
      CALL GSPOS('VA10', 6, 'VOLM', X11(5), Y11(3), Z11, irt_wire6, 'ONLY')

      CALL GSPOS('TVR1', 1, 'VOLM', t_X1(1), t_Y1(1), t_Z1, irt_targ1, 'ONLY')
      CALL GSPOS('TVR1', 2, 'VOLM', t_X1(1), t_Y1(2), t_Z1, irt_targ2, 'ONLY')
      CALL GSPOS('TVR1', 3, 'VOLM', t_X1(3), t_Y1(3), t_Z1, irt_targ3, 'ONLY')
      CALL GSPOS('TVR1', 4, 'VOLM', t_X1_ON(3), t_Y1_ON(4), t_Z1_ON, irt_targ4_ON, 'ONLY')
      CALL GSPOS('TVR1', 5, 'VOLM', t_X1(5), t_Y1(4), t_Z1, irt_targ5, 'ONLY')
      CALL GSPOS('TVR1', 6, 'VOLM', t_X1(5), t_Y1(3), t_Z1, irt_targ6, 'ONLY')

      CALL GSPOS('TVR2', 1, 'VOLM', t_X2(1), t_Y2(1), t_Z2, irt_targ1, 'ONLY')
      CALL GSPOS('TVR2', 2, 'VOLM', t_X2(1), t_Y2(2), t_Z2, irt_targ2, 'ONLY')
      CALL GSPOS('TVR2', 3, 'VOLM', t_X2(3), t_Y2(3), t_Z2, irt_targ3, 'ONLY')
      CALL GSPOS('TVR2', 4, 'VOLM', t_X2_ON(3), t_Y2_ON(4), t_Z2_ON, irt_targ4_ON, 'ONLY')
      CALL GSPOS('TVR2', 5, 'VOLM', t_X2(5), t_Y2(4), t_Z2, irt_targ5, 'ONLY')
      CALL GSPOS('TVR2', 6, 'VOLM', t_X2(5), t_Y2(3), t_Z2, irt_targ6, 'ONLY')

      CALL GSPOS('TVR3', 1, 'VOLM', t_X3(1), t_Y3(1), t_Z3, irt_targ1, 'ONLY')
      CALL GSPOS('TVR3', 2, 'VOLM', t_X3(1), t_Y3(2), t_Z3, irt_targ2, 'ONLY')
      CALL GSPOS('TVR3', 3, 'VOLM', t_X3(3), t_Y3(3), t_Z3, irt_targ3, 'ONLY')
      CALL GSPOS('TVR3', 4, 'VOLM', t_X3_ON(3), t_Y3_ON(4), t_Z3_ON, irt_targ4_ON, 'ONLY')
      CALL GSPOS('TVR3', 5, 'VOLM', t_X3(5), t_Y3(4), t_Z3, irt_targ5, 'ONLY')
      CALL GSPOS('TVR3', 6, 'VOLM', t_X3(5), t_Y3(3), t_Z3, irt_targ6, 'ONLY')

      CALL GSPOS('TVR4', 1, 'VOLM', t_X4(1), t_Y4(1), t_Z4, irt_targ1, 'ONLY')
      CALL GSPOS('TVR4', 2, 'VOLM', t_X4(1), t_Y4(2), t_Z4, irt_targ2, 'ONLY')
      CALL GSPOS('TVR4', 3, 'VOLM', t_X4(3), t_Y4(3), t_Z4, irt_targ3, 'ONLY')
      CALL GSPOS('TVR4', 4, 'VOLM', t_X4_ON(3), t_Y4_ON(4), t_Z4_ON, irt_targ4_ON, 'ONLY')
      CALL GSPOS('TVR4', 5, 'VOLM', t_X4(5), t_Y4(4), t_Z4, irt_targ5, 'ONLY')
      CALL GSPOS('TVR4', 6, 'VOLM', t_X4(5), t_Y4(3), t_Z4, irt_targ6, 'ONLY')

      CALL GSPOS('TALA', 1, 'VOLM', t_X5(1), t_Y5(1), t_Z5, circ_targ1, 'ONLY')
      CALL GSPOS('TF56', 1, 'VOLM', t_X5(1), t_Y5(2), t_Z5, circ_targ2, 'ONLY')
      CALL GSPOS('TCAR', 1, 'VOLM', t_X5(3), t_Y5(3), t_Z5, circ_targ3, 'ONLY')
      CALL GSPOS('TLED', 1, 'VOLM', 0.05395, -0.03115, 1.316,  irt_tbox4_ON, 'MANY')
      CALL GSPOS('TTIN', 1, 'VOLM', t_X5(5), t_Y5(4), t_Z5, circ_targ5, 'ONLY')
      CALL GSPOS('TALB', 1, 'VOLM', t_X5(5), t_Y5(3), t_Z5, circ_targ6, 'ONLY')

      elseif(itg_type(1).eq.119)then
      CALL GSPOS('WIRE', 1, 'VOLM', X7(1), Y7(1), Z7, irt_wire1, 'ONLY')
      CALL GSPOS('WIRE', 2, 'VOLM', X7(1), Y7(2), Z7, irt_wire2, 'ONLY')
      CALL GSPOS('WIRE', 3, 'VOLM', X7(3), Y7(3), Z7, irt_wire3, 'ONLY')
      CALL GSPOS('WIRE', 4, 'VOLM', X7(3), Y7(4), Z7, irt_wire4, 'ONLY')
      CALL GSPOS('WION', 1, 'VOLM', X7_ON(5), Y7_ON(4), Z7_ON, irt_wire5_ON, 'ONLY')
      CALL GSPOS('WIRE', 5, 'VOLM', X7(5), Y7(3), Z7, irt_wire6, 'ONLY')

      CALL GSPOS('VAR7', 1, 'VOLM', X8(1), Y8(1), Z8, irt_level1, 'ONLY')
      CALL GSPOS('VAR7', 2, 'VOLM', X8(1), Y8(2), Z8, irt_level2, 'ONLY')
      CALL GSPOS('VAR7', 3, 'VOLM', X8(3), Y8(3), Z8, irt_level3, 'ONLY')
      CALL GSPOS('VAR7', 4, 'VOLM', X8(3), Y8(4), Z8, irt_level4, 'ONLY')
      CALL GSPOS('VAR7', 5, 'VOLM', X8_ON(5), Y8_ON(4), Z8_ON, irt_level5_ON, 'ONLY')
      CALL GSPOS('VAR7', 6, 'VOLM', X8(5), Y8(3), Z8, irt_level6, 'ONLY')

      CALL GSPOS('VAR8', 1, 'VOLM', X9(1), Y9(1), Z9, circ_level1, 'ONLY')
      CALL GSPOS('VAR8', 2, 'VOLM', X9(1), Y9(2), Z9, circ_level2, 'ONLY')
      CALL GSPOS('VAR8', 3, 'VOLM', X9(3), Y9(3), Z9, circ_level3, 'ONLY')
      CALL GSPOS('VAR8', 4, 'VOLM', X9(3), Y9(4), Z9, circ_level4, 'ONLY')
      CALL GSPOS('VAR8', 5, 'VOLM', X9_ON(5), Y9_ON(4), Z9_ON, circ_level5, 'ONLY')
      CALL GSPOS('VAR8', 6, 'VOLM', X9(5), Y9(3), Z9, circ_level6, 'ONLY')

      CALL GSPOS('VAR9', 1, 'VOLM', X10(1), Y10(1), Z10, irt_wire1, 'ONLY')
      CALL GSPOS('VAR9', 2, 'VOLM', X10(1), Y10(2), Z10, irt_wire2, 'ONLY')
      CALL GSPOS('VAR9', 3, 'VOLM', X10(3), Y10(3), Z10, irt_wire3, 'ONLY')
      CALL GSPOS('VAR9', 4, 'VOLM', X10(3), Y10(4), Z10, irt_wire4, 'ONLY')
      CALL GSPOS('VAR9', 5, 'VOLM', X10_ON(5), Y10_ON(4), Z10_ON, irt_wire5_ON, 'ONLY')
      CALL GSPOS('VAR9', 6, 'VOLM', X10(5), Y10(3), Z10, irt_wire6, 'ONLY')

      CALL GSPOS('VA10', 1, 'VOLM', X11(1), Y11(1), Z11, irt_wire1, 'ONLY')
      CALL GSPOS('VA10', 2, 'VOLM', X11(1), Y11(2), Z11, irt_wire2, 'ONLY')
      CALL GSPOS('VA10', 3, 'VOLM', X11(3), Y11(3), Z11, irt_wire3, 'ONLY')
      CALL GSPOS('VA10', 4, 'VOLM', X11(3), Y11(4), Z11, irt_wire4, 'ONLY')
      CALL GSPOS('VA10', 5, 'VOLM', X11_ON(5), Y11_ON(4), Z11_ON, irt_wire5_ON, 'ONLY')
      CALL GSPOS('VA10', 6, 'VOLM', X11(5), Y11(3), Z11, irt_wire6, 'ONLY')

      CALL GSPOS('TVR1', 1, 'VOLM', t_X1(1), t_Y1(1), t_Z1, irt_targ1, 'ONLY')
      CALL GSPOS('TVR1', 2, 'VOLM', t_X1(1), t_Y1(2), t_Z1, irt_targ2, 'ONLY')
      CALL GSPOS('TVR1', 3, 'VOLM', t_X1(3), t_Y1(3), t_Z1, irt_targ3, 'ONLY')
      CALL GSPOS('TVR1', 4, 'VOLM', t_X1(3), t_Y1(4), t_Z1, irt_targ4, 'ONLY')
      CALL GSPOS('TVR1', 5, 'VOLM', t_X1_ON(5), t_Y1_ON(4), t_Z1_ON, irt_targ5_ON, 'ONLY')
      CALL GSPOS('TVR1', 6, 'VOLM', t_X1(5), t_Y1(3), t_Z1, irt_targ6, 'ONLY')

      CALL GSPOS('TVR2', 1, 'VOLM', t_X2(1), t_Y2(1), t_Z2, irt_targ1, 'ONLY')
      CALL GSPOS('TVR2', 2, 'VOLM', t_X2(1), t_Y2(2), t_Z2, irt_targ2, 'ONLY')
      CALL GSPOS('TVR2', 3, 'VOLM', t_X2(3), t_Y2(3), t_Z2, irt_targ3, 'ONLY')
      CALL GSPOS('TVR2', 4, 'VOLM', t_X2(3), t_Y2(4), t_Z2, irt_targ4, 'ONLY')
      CALL GSPOS('TVR2', 5, 'VOLM', t_X2_ON(5), t_Y2_ON(4), t_Z2_ON, irt_targ5_ON, 'ONLY')
      CALL GSPOS('TVR2', 6, 'VOLM', t_X2(5), t_Y2(3), t_Z2, irt_targ6, 'ONLY')

      CALL GSPOS('TVR3', 1, 'VOLM', t_X3(1), t_Y3(1), t_Z3, irt_targ1, 'ONLY')
      CALL GSPOS('TVR3', 2, 'VOLM', t_X3(1), t_Y3(2), t_Z3, irt_targ2, 'ONLY')
      CALL GSPOS('TVR3', 3, 'VOLM', t_X3(3), t_Y3(3), t_Z3, irt_targ3, 'ONLY')
      CALL GSPOS('TVR3', 4, 'VOLM', t_X3(3), t_Y3(4), t_Z3, irt_targ4, 'ONLY')
      CALL GSPOS('TVR3', 5, 'VOLM', t_X3_ON(5), t_Y3_ON(4), t_Z3_ON, irt_targ5_ON, 'ONLY')
      CALL GSPOS('TVR3', 6, 'VOLM', t_X3(5), t_Y3(3), t_Z3, irt_targ6, 'ONLY')

      CALL GSPOS('TVR4', 1, 'VOLM', t_X4(1), t_Y4(1), t_Z4, irt_targ1, 'ONLY')
      CALL GSPOS('TVR4', 2, 'VOLM', t_X4(1), t_Y4(2), t_Z4, irt_targ2, 'ONLY')
      CALL GSPOS('TVR4', 3, 'VOLM', t_X4(3), t_Y4(3), t_Z4, irt_targ3, 'ONLY')
      CALL GSPOS('TVR4', 4, 'VOLM', t_X4(3), t_Y4(4), t_Z4, irt_targ4, 'ONLY')
      CALL GSPOS('TVR4', 5, 'VOLM', t_X4_ON(5), t_Y4_ON(4), t_Z4_ON, irt_targ5_ON, 'ONLY')
      CALL GSPOS('TVR4', 6, 'VOLM', t_X4(5), t_Y4(3), t_Z4, irt_targ6, 'ONLY')

      CALL GSPOS('TALA', 1, 'VOLM', t_X5(1), t_Y5(1), t_Z5, circ_targ1, 'ONLY')
      CALL GSPOS('TF56', 1, 'VOLM', t_X5(1), t_Y5(2), t_Z5, circ_targ2, 'ONLY')
      CALL GSPOS('TCAR', 1, 'VOLM', t_X5(3), t_Y5(3), t_Z5, circ_targ3, 'ONLY')
      CALL GSPOS('TLED', 1, 'VOLM', t_X5(3), t_Y5(4), t_Z5, circ_targ4, 'ONLY')
      CALL GSPOS('TTIN', 1, 'VOLM', -0.05395, -0.03115, 1.316,  irt_tbox5_ON, 'MANY')
      CALL GSPOS('TALB', 1, 'VOLM', t_X5(5), t_Y5(3), t_Z5, circ_targ6, 'ONLY')

      elseif(itg_type(1).eq.28)then
      CALL GSPOS('WIRE', 1, 'VOLM', X7(1), Y7(1), Z7, irt_wire1, 'ONLY')
      CALL GSPOS('WIRE', 2, 'VOLM', X7(1), Y7(2), Z7, irt_wire2, 'ONLY')
      CALL GSPOS('WIRE', 3, 'VOLM', X7(3), Y7(3), Z7, irt_wire3, 'ONLY')
      CALL GSPOS('WIRE', 4, 'VOLM', X7(3), Y7(4), Z7, irt_wire4, 'ONLY')
      CALL GSPOS('WIRE', 5, 'VOLM', X7(5), Y7(4), Z7, irt_wire5, 'ONLY')
      CALL GSPOS('WION', 1, 'VOLM', X7_ON(5), Y7_ON(3), Z7_ON, irt_wire6_ON, 'ONLY')

      CALL GSPOS('VAR7', 1, 'VOLM', X8(1), Y8(1), Z8, irt_level1, 'ONLY')
      CALL GSPOS('VAR7', 2, 'VOLM', X8(1), Y8(2), Z8, irt_level2, 'ONLY')
      CALL GSPOS('VAR7', 3, 'VOLM', X8(3), Y8(3), Z8, irt_level3, 'ONLY')
      CALL GSPOS('VAR7', 4, 'VOLM', X8(3), Y8(4), Z8, irt_level4, 'ONLY')
      CALL GSPOS('VAR7', 5, 'VOLM', X8(5), Y8(4), Z8, irt_level5, 'ONLY')
      CALL GSPOS('VAR7', 6, 'VOLM', X8_ON(5), Y8_ON(3), Z8_ON, irt_level6_ON, 'ONLY')

      CALL GSPOS('VAR8', 1, 'VOLM', X9(1), Y9(1), Z9, circ_level1, 'ONLY')
      CALL GSPOS('VAR8', 2, 'VOLM', X9(1), Y9(2), Z9, circ_level2, 'ONLY')
      CALL GSPOS('VAR8', 3, 'VOLM', X9(3), Y9(3), Z9, circ_level3, 'ONLY')
      CALL GSPOS('VAR8', 4, 'VOLM', X9(3), Y9(4), Z9, circ_level4, 'ONLY')
      CALL GSPOS('VAR8', 5, 'VOLM', X9(5), Y9(4), Z9, circ_level5, 'ONLY')
      CALL GSPOS('VAR8', 6, 'VOLM', X9_ON(5), Y9_ON(3), Z9_ON, circ_level6, 'ONLY')

      CALL GSPOS('VAR9', 1, 'VOLM', X10(1), Y10(1), Z10, irt_wire1, 'ONLY')
      CALL GSPOS('VAR9', 2, 'VOLM', X10(1), Y10(2), Z10, irt_wire2, 'ONLY')
      CALL GSPOS('VAR9', 3, 'VOLM', X10(3), Y10(3), Z10, irt_wire3, 'ONLY')
      CALL GSPOS('VAR9', 4, 'VOLM', X10(3), Y10(4), Z10, irt_wire4, 'ONLY')
      CALL GSPOS('VAR9', 5, 'VOLM', X10(5), Y10(4), Z10, irt_wire5, 'ONLY')
      CALL GSPOS('VAR9', 6, 'VOLM', X10_ON(5), Y10_ON(3), Z10_ON, irt_wire6_ON, 'ONLY')

      CALL GSPOS('VA10', 1, 'VOLM', X11(1), Y11(1), Z11, irt_wire1, 'ONLY')
      CALL GSPOS('VA10', 2, 'VOLM', X11(1), Y11(2), Z11, irt_wire2, 'ONLY')
      CALL GSPOS('VA10', 3, 'VOLM', X11(3), Y11(3), Z11, irt_wire3, 'ONLY')
      CALL GSPOS('VA10', 4, 'VOLM', X11(3), Y11(4), Z11, irt_wire4, 'ONLY')
      CALL GSPOS('VA10', 5, 'VOLM', X11(5), Y11(4), Z11, irt_wire5, 'ONLY')
      CALL GSPOS('VA10', 6, 'VOLM', X11_ON(5), Y11_ON(3), Z11_ON, irt_wire6_ON, 'ONLY')

      CALL GSPOS('TVR1', 1, 'VOLM', t_X1(1), t_Y1(1), t_Z1, irt_targ1, 'ONLY')
      CALL GSPOS('TVR1', 2, 'VOLM', t_X1(1), t_Y1(2), t_Z1, irt_targ2, 'ONLY')
      CALL GSPOS('TVR1', 3, 'VOLM', t_X1(3), t_Y1(3), t_Z1, irt_targ3, 'ONLY')
      CALL GSPOS('TVR1', 4, 'VOLM', t_X1(3), t_Y1(4), t_Z1, irt_targ4, 'ONLY')
      CALL GSPOS('TVR1', 5, 'VOLM', t_X1(5), t_Y1(4), t_Z1, irt_targ5, 'ONLY')
      CALL GSPOS('TVR1', 6, 'VOLM', t_X1_ON(5), t_Y1_ON(3), t_Z1_ON, irt_targ6_ON, 'ONLY')

      CALL GSPOS('TVR2', 1, 'VOLM', t_X2(1), t_Y2(1), t_Z2, irt_targ1, 'ONLY')
      CALL GSPOS('TVR2', 2, 'VOLM', t_X2(1), t_Y2(2), t_Z2, irt_targ2, 'ONLY')
      CALL GSPOS('TVR2', 3, 'VOLM', t_X2(3), t_Y2(3), t_Z2, irt_targ3, 'ONLY')
      CALL GSPOS('TVR2', 4, 'VOLM', t_X2(3), t_Y2(4), t_Z2, irt_targ4, 'ONLY')
      CALL GSPOS('TVR2', 5, 'VOLM', t_X2(5), t_Y2(4), t_Z2, irt_targ5, 'ONLY')
      CALL GSPOS('TVR2', 6, 'VOLM', t_X2_ON(5), t_Y2_ON(3), t_Z2_ON, irt_targ6_ON, 'ONLY')

      CALL GSPOS('TVR3', 1, 'VOLM', t_X3(1), t_Y3(1), t_Z3, irt_targ1, 'ONLY')
      CALL GSPOS('TVR3', 2, 'VOLM', t_X3(1), t_Y3(2), t_Z3, irt_targ2, 'ONLY')
      CALL GSPOS('TVR3', 3, 'VOLM', t_X3(3), t_Y3(3), t_Z3, irt_targ3, 'ONLY')
      CALL GSPOS('TVR3', 4, 'VOLM', t_X3(3), t_Y3(4), t_Z3, irt_targ4, 'ONLY')
      CALL GSPOS('TVR3', 5, 'VOLM', t_X3(5), t_Y3(4), t_Z3, irt_targ5, 'ONLY')
      CALL GSPOS('TVR3', 6, 'VOLM', t_X3_ON(5), t_Y3_ON(3), t_Z3_ON, irt_targ6_ON, 'ONLY')

      CALL GSPOS('TVR4', 1, 'VOLM', t_X4(1), t_Y4(1), t_Z4, irt_targ1, 'ONLY')
      CALL GSPOS('TVR4', 2, 'VOLM', t_X4(1), t_Y4(2), t_Z4, irt_targ2, 'ONLY')
      CALL GSPOS('TVR4', 3, 'VOLM', t_X4(3), t_Y4(3), t_Z4, irt_targ3, 'ONLY')
      CALL GSPOS('TVR4', 4, 'VOLM', t_X4(3), t_Y4(4), t_Z4, irt_targ4, 'ONLY')
      CALL GSPOS('TVR4', 5, 'VOLM', t_X4(5), t_Y4(4), t_Z4, irt_targ5, 'ONLY')
      CALL GSPOS('TVR4', 6, 'VOLM', t_X4_ON(5), t_Y4_ON(3), t_Z4_ON, irt_targ6_ON, 'ONLY')

      CALL GSPOS('TALA', 1, 'VOLM', t_X5(1), t_Y5(1), t_Z5, circ_targ1, 'ONLY')
      CALL GSPOS('TF56', 1, 'VOLM', t_X5(1), t_Y5(2), t_Z5, circ_targ2, 'ONLY')
      CALL GSPOS('TCAR', 1, 'VOLM', t_X5(3), t_Y5(3), t_Z5, circ_targ3, 'ONLY')
      CALL GSPOS('TLED', 1, 'VOLM', t_X5(3), t_Y5(4), t_Z5, circ_targ4, 'ONLY')
      CALL GSPOS('TTIN', 1, 'VOLM', t_X5(5), t_Y5(4), t_Z5, circ_targ5, 'ONLY')
      CALL GSPOS('TALB', 1, 'VOLM', -0.05395, 0.03115, 1.316,  irt_tbox6_ON, 'MANY')
      endif

      
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      
      
*     target cell: 
      
      parm(1) =   0.
      parm(2) = 360.
      parm(3) = 10
      do i=1,10
         parm(1+i*3) =tg_lh2_pos(i)
         parm(2+i*3) =tg_lh2_r1(i)
         parm(3+i*3) =tg_lh2_r2(i)
      enddo
      
      np=parm(3)*3+3
      
      call gsvolu('TARG','PCON',tmed_tg,parm,np,ivol)
      call gspos('TARG',1,'VOLM',0.,0.,-6.196,0,'MANY')
      
*     cell walls - note: 0.992 is cosine of cell cone half angle
      
      parm(1) =   0.
      parm(2) = 360.
      parm(3) = 4
      
      do i=7,10
         parm(1+(i-6)*3) =tg_lh2_pos(i)
         parm(2+(i-6)*3) =tg_lh2_r2(i)
         parm(3+(i-6)*3) =parm(2+(i-6)*3)+thickcell/0.992
      enddo
      
      np=parm(3)*3+3
      call gsvolu('TGCL','PCON',tmed_cell,parm,np,ivol)
      call gspos('TGCL',1,'VOLM',0.,0.,-6.196,0,'MANY')
      
* glue joint for end cap      

      parm(1) =   0.
      parm(2) = 360.
      parm(3) = 5
      
      do i=1,5
         parm(1+i*3) =tg_epx_pos(i)
         parm(2+i*3) =tg_epx_r1(i)
         parm(3+i*3) =tg_epx_r2(i)
      enddo
      
      np=parm(3)*3+3
      call gsvolu('TGEP','PCON',clas_med_alumaseal,parm,np,ivol)
      call gspos('TGEP',1,'VOLM',0.,0.,-6.196,0,'MANY')
      
*     kapton glued to end of conical section
      
      parm(1) =   0.
      parm(2) = 360.
      parm(3) = 2
      
      do i=1,2
         parm(1+i*3) =tg_ecp_pos(i)
         parm(2+i*3) =tg_ecp_r1(i)
         parm(3+i*3) =tg_ecp_r2(i)
      enddo
      np=parm(3)*3+3
      call gsvolu('TGKP','PCON',clas_med_kapton,parm,np,ivol)
      call gspos('TGKP',1,'VOLM',0.,0.,-6.196,0,'MANY')
      
*     kapton part of end cap window
      
      parm(1) =0.2
      parm(2) =tg_ecp_r2(2)
      parm(3) =0.0025
      call gsvolu('TGEC','TUBE',clas_med_kapton,parm,3,ivol)
      call gspos('TGEC',1,'VOLM', 0.,0.,
     &     -6.196+tg_ecp_pos(2)+parm(3),0,'MANY')
      
*     window on upstream end of target cell (Al)

      parm(1) =0.0
      parm(2) =0.28
      parm(3) =thickwindow/2   
      call gsvolu('TGW1','TUBE',clas_med_alu,parm,3,ivol)
      call gspos('TGW1',1,'VOLM', 0.,0.,
     &     -6.196+tg_ecp_pos(2)-parm(3),0,'MANY')
*      print*,'///$#$#$#$#$#$#$#$#$#$',-6.196+tg_ecp_pos(2)-parm(3)
     
*     save parameters
      
      tgpar_save(1)= 2*tglenby2
      tgpar_save(2)= tgrad
      tgpar_save(6)=-tglenby2
      tgpar_save(7)= tg_pos_ff(3) +tgpar_save(6)
      tgpar_save(8) =tg_pos_ff(3) +tgpar_save(6)
      tgpar_save(9) =tg_pos_ff(3) +tgpar_save(7)
      tgpar_save(10)=xytg +tgrad

*     superinsulation
*     
      
      parm(1) =   0.
      parm(2) = 360.
      parm(3) = 8
      
      do i=7,10
         parm(1+(i-6)*3) =tg_lh2_pos(i)
         parm(2+(i-6)*3) =tg_lh2_r2(i)+thickcell/0.992+0.1
         parm(3+(i-6)*3) =parm(2+(i-6)*3)+thickrad
      enddo

      parm(13) = parm(13)+0.1
      
      parm(16) = parm(13)
      parm(17) = 0.
      parm(18) = parm(15)
      
      parm(19) = parm(16)+thickrad
      parm(20) = parm(17)
      parm(21) = parm(18)
      
      parm(22) = tg_lh2_pos(10)+0.1+thickrad
      parm(23) = tg_lh2_r2(10)+thickcell/0.992+0.1
      parm(24) = parm(23)+thickrad
      
      parm(25) = parm(22)+2.0
      parm(26) = parm(23)
      parm(27) = parm(24)
      
      call gsvolu('TGSI','PCON',tmed_ins,parm,27,ivol)
      call gspos('TGSI',1,'VOLM', 0.,0.,-6.196,0,'MANY')
      
      tg_ins_ff(2) =thickrad
      tg_ins_ff(4) =distrad

*     There was a `reference window' 2cm downstream of the target cell 
*     which, during e1-6a, was made of super insulation.  For later runs 
*     it was replaced with 15um of aluminium as the super insulation 
*     was burned through by the beam, (S. Morrow 12/10/02).

      parm(1) =   0.
      parm(2) = 360.
      parm(3) = 2
           
      parm(4) = parm(25)
      parm(5) = 0.
      parm(6) = parm(27)
      
      parm(7) = parm(4)+thickwindow
      parm(8) = 0.
      parm(9) = parm(6)
      
      call gsvolu('TGSA','PCON',clas_med_alu,parm,9,ivol)
      call gspos('TGSA',1,'VOLM',0.,0.,-6.196,0,'MANY')

*     window on upstream end of target cell (Al)

      parm(1) =0.0
      parm(2) =tg_turad(10) 
      parm(3) =thickwindow/2   
      call gsvolu('TGW2','TUBE',clas_med_alu,parm,3,ivol)
      call gspos('TGW2',1,'VOLM', 0.,0.,
     &     -6.196+tg_tupos(10)-parm(3),0,'MANY')
      
      wzpos=-6.196+tg_tupos(10)-parm(3)
      print*,'========WZPOS======',wzpos

*     outer base ring (torlon)

      parm(1) =0.
      parm(2) =360.
      parm(3) =10.
      do i=1,10
         parm(1+i*3) =tg_flpos(i)  
         parm(2+i*3) =tgrad + tg_fldrad(i)
         parm(3+i*3) =parm(2+i*3) + tg_flthick(i)
      enddo
      call gsvolu('TGFL','PCON',clas_med_kapton,parm,33,ivol)
      call gspos('TGFL',1,'VOLM', 0.,0.,-6.196,0,'MANY')

*    inner base tube (torlon)

      parm(1) =0.
      parm(2) =360.
      parm(3) =10.
      do i=1,10
         parm(1+i*3) =tg_tupos(i)  
         parm(2+i*3) =tg_turad(i)
         parm(3+i*3) =tg_turad(i) + tg_tuthick(i)
      enddo
      call gsvolu('TGTU','PCON',clas_med_kapton,parm,33,ivol)
      call gspos('TGTU',1,'VOLM', 0.,0.,-6.196,0,'MANY')


      end

C----------------------------------------------------------------

        subroutine vert_eg2(vx_poz)
c  include files:
c  --------------

#include "ffpar_tg.inc"
#include "ffpar_tg_e2.inc"
c_end_inc
      real vx_poz(3)
      real uran(3)
       call grndm(uran,3)
          if(ivx_eg2(1).eq.1)then

             vx_poz(1)=0.0
             vx_poz(2)=0.0
             vx_poz(3)=teg2_pos_ff(3)-6.0+2.*uran(3)

          elseif(ivx_eg2(1).eq.2)then
             
             if(itg_type(1).eq.56)then
                vx_poz(1)=0.0
                vx_poz(2)=0.0
                vx_poz(3)=teg2_pos_ff(3)-0.02+0.04*uran(3)
             elseif(itg_type(1).eq.27)then
                vx_poz(1)=0.0
                vx_poz(2)=0.0
                vx_poz(3)=teg2_pos_ff(3)-0.00075+0.0015*uran(3)
             elseif(itg_type(1).eq.12)then
                vx_poz(1)=0.0
                vx_poz(2)=0.0
                vx_poz(3)=teg2_pos_ff(3)-0.08615+0.1723*uran(3)
             elseif(itg_type(1).eq.207)then
                vx_poz(1)=0.0
                vx_poz(2)=0.0
                vx_poz(3)=teg2_pos_ff(3)-0.007+0.014*uran(3)
             elseif(itg_type(1).eq.119)then
                vx_poz(1)=0.0
                vx_poz(2)=0.0
                vx_poz(3)=teg2_pos_ff(3)-0.0156+0.0312*uran(3)
             elseif(itg_type(1).eq.28)then
                vx_poz(1)=0.0
                vx_poz(2)=0.0
                vx_poz(3)=teg2_pos_ff(3)-0.029+0.058*uran(3)
             endif
             
          elseif(ivx_eg2(1).eq.3)then
             vx_poz(1) = 0.0
             vx_poz(2) = 0.0
             vx_poz(3) = 0.0

          endif
        
             return
             end
*========================================================
