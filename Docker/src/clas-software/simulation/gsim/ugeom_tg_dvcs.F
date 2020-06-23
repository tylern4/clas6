      subroutine ugeom_tg_dvcs(itg_mate)
  
c_begin_doc
c
c  Documentation for subroutine ugeom_tg_e1dvcs
c  
c  Purpose:
c  --------
c     ******************************************************************
c     *                                                                *
c     *       e1e target implementation                                *
c     *       Author: C. Smith Adapted from S. Morrow e1-6 code        *
c     *       Date: 02/04/03                                           *
c     *                                                                *
c     ******************************************************************
c
c  for tg_type_ff=17
c   #   cell wall               end cap   length   diameter    name
c   1   Kapton,50mu(0.002 inch) Al 15mu   20mm    7.0-11.9mm  'e1e'
c       upstream entrance window 15um Al
c   group# 7
c
c  support structure type=Torlon (Kapton in block form)
c  superinsulation material #163.0 (Aluminized Mylar)
c  shape :  tube + base ring (upstream)
c
c  Taken from drawing 66850-A-03727
c
c     Rustam Niyazov,RPI, Feb-12-2008: e1dvcs target is based on e1e target.
c     Drawing 66840-E-04248 was used to implement the shape and sizes of the target cell
c     superinsulation volume around and behind the target cell.
c     Instead of conical insulation which ends at 1.4 cm after the target cell (see drawing) I used
c     conical insulation that ends with target cell (80 mkm) and I used the cylindrical mylar with thicker
c     material (80+50 mkm) till 1.4 cm and regular mylar (50 mkm) for the next 0.6 cm of the superinsulating
C     volume behind the target cell. 
C     Rustam Niyazov,RPI July-22-2008 fixed the vertex smearing bug 
c_end_doc

      implicit none
      character*(*) cfile,crevis,cstate,cdate,cautho,crname,crauth
      character*132 crcsid,crmess
      parameter (cfile=  '$RCSfile: ugeom_tg_dvcs.F,v $')
      parameter (crevis= '$Revision: 1.3 $')
      parameter (cstate= '$State: Exp $')
      parameter (cdate=  '$Date: 2008/10/03 13:45:26 $')
      parameter (cautho= '$Author: avakian $')
      parameter (crname= 'UGEOM_DVCS')
      parameter (crauth= 'sam')
      data crcsid/'
     1$Id: ugeom_tg_dvcs.F,v 1.3 2008/10/03 13:45:26 avakian Exp $
     1 '/
      save

c  include files:
c  --------------
#include "gcbank.inc"
#include "gcnum.inc"
#include "gsimpar.inc"
#include "gsimcontrol.inc"
#include "ffpar_foil.inc"
#include "ffpar_tg.inc"
#include "ffpar_mg.inc"
#include "clmate.inc"
#include "cltmed.inc"
#include "ffpar_dvcs.inc"
#include "ffpar_bckgr.inc"
c_end_inc

*
*  input parameters
*  ----------------

      integer itg_mate          !target material number
*
*  external functions
*  ------------------

      integer  iucomp, lnblnk, get_tmed, get_rotm

*  local variables:
*  ----------------
      real  twopiby3, piby2
      integer i, np
      integer istrlen
      integer ivol                      !volume#
      integer irot1, irot2, irot3       !rotation_matrix#
      integer i2                        !#plane in PolyCONe for target cell
      real  tg_zpos2                    !pos. of tube-like part of target cell
      real  dangle                      !step size for cone of target cell
      real  parm(50), pars(20)          !arrays for volume definitions
      real  xpos, ypos, zpos, xypos     !position of volumes
      real  xytg, ztg                   !position of target center
      real  tglenby2, tgrad, thickcell  !target info 
      real  tglenby2_eff                !effective target length
c                                        (part seen by the beam)
 
      real  tgoffset			!Distance from upstream cell to CLAS center
      real  thickwindow,radwindow       !Al window info
      integer tmed_tg                   !track.medium for target material
      integer imatcell, tmed_cell       !mate# and track.medium for target cell
      integer imatins, tmed_ins         !for superinsulation layers 
      real  distrad, distax             !insulation (distance from target)
      real  thickrad, thickax           !insulation (thickness)
      real  tmaxfd, stmax, deemax, epsil, stmin   !track.media cuts 
      real  aa,zz,dens,radl,absl                  !material constants
      character*20 cmat_tg, cmatcell, cmatins     !material names
      character*4  cmoth                          !mother volume
      real  z0pos                                 !z-position in mother volume
*
*  default values for target geometry  
*  ----------------------------------
*
      real tg_len(1:1)               !target length (depends on target type)
      real tg_diam(2)                !target diameter
      real tg_thickcell              !cell wall thickness  (material: Kapton)
      real tg_thickwindow            !Al window thickness
      real tg_radwindow              !Al window radius
      
      real tg_distrad                !radius of superinsulation
      real ins_dlen                  !length of layers in downstream dir.
      real tg_thickins               !total thickness of superinsulation 
                                     !layers (material: aluminized Mylar)
                                     !packed as solid block (ca. 3.5% of total)
      
      data tg_len                 / 3.621 /
      data tg_diam,tg_thickcell   / 1.609, 0.696, 0.0051 /
      data tg_thickwindow         / 0.0015 /
      data tg_radwindow           / 0.6 /
      data tg_distrad,tg_thickins / 5.2, 0.008 / 
      data ins_dlen               / 2.00 /
*
*  gas/liquid distribution pipes & support structures 
*  --------------------------------------------------
*
*  pcon:           base ring (torlon) at entry window position 
*  tube1 & tube2:  horizontal, connected to base ring and condenser
*
      real tg_flpos(10)             ! relative to right edge of flange
      real tg_flthick(10)           ! thickness of torlon ring
      real tg_fldrad(10)            ! relative inner radius wrt. cell radius
      real tg_floffset              ! offset wrt tg_fldrad values from e1e which give negative values of Rmin
      data tg_flpos /-4.78,-4.15,-3.53,-3.53,-2.84,-2.84,-1.47,-1.23,-0.33,0.0/
      data tg_flthick /0.330,1.75,1.75,0.26,0.26,0.41,0.41,0.244,0.244,0.0/
      data tg_fldrad  /-0.455,-0.455,-0.455,0.635,0.635,0.635,0.635,0.635,0.013,0.013/
      data tg_floffset /0.455/

      real tg_tupos(10)             !relative to left edge of flange
      real tg_tuthick(10)           !thickness of inner base tube (support)
      real tg_turad(10)             !thickness of inner base tube (support)
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

*  offset vs. volume GAS2 to position the target in CLAS center:
      real  eltg_zoff
      data  eltg_zoff /-5.06/

c_end_var

c  executable code:
c  ----------------
      piby2   =acos(0.)
      twopiby3=acos(-1.)*2./3.

      imatcell= imate_kapton 
      imatins = imate_cerexmyl1

      tmed_cell= clas_med_kapton    ! cell walls kapton
      tmed_ins = clas_med_cerexmyl1 ! superins. CEREX/Alum.mylar
      
      if(itg_mate.EQ.imate_vacuum) then ! Choose empty or full (H2) cell
         tmed_tg  = clas_med_vac
      elseif(itg_mate.EQ.imate_liq_h2) then
         tmed_tg  = clas_med_h2liq
      elseif(itg_mate.EQ.imate_liq_d2) then
         tmed_tg  = clas_med_d2liq
      else
         call recmes(crname,'W',
     &        'Only HYDR, DEUT or VAC targets supported for e1e')
         return
      endif

C      tglenby2 = tg_len(1)/2
C      tgrad    = tg_diam(1)/2
      tglenby2 = (tleng+1.67)/2 !total length is 4.17 cm
      tglenby2_eff = tleng/2
      tgrad    = tg_diam(2)/2


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
      tg_lh2_r2(6)  = 0.759 !determines the front conical radius
      tg_lh2_pos(6) = tg_tupos(7)
      tg_lh2_r1(7)  = tg_turad(7)+tg_tuthick(7)
      tg_lh2_r2(7)  = tg_lh2_r2(6)
      tg_lh2_pos(7) = tg_tupos(7)
      tg_lh2_r1(8)  = tg_turad(7)+tg_tuthick(7)
      tg_lh2_r2(8)  = 0.595 !the diameter of 11.9 mm at the beginning of interaction volume
      tg_lh2_pos(8) = tg_tupos(10)
      tg_lh2_r1(9)  = 0.0
      tg_lh2_r2(9)  = tg_lh2_r2(8)
      tg_lh2_pos(9) = tg_tupos(10)
      tg_lh2_r1(10)  = tg_lh2_r1(9)
      tg_lh2_r2(10)  = tg_diam(2)/2. 
      tg_lh2_pos(10) = tg_tupos(10)+tleng

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
*  stop if beam pipe volume not defined
*
*  - until 'FOIL' volumes are cleaned up, put target into 'GAS2' volume
*    which is positioned at z=5.06 (so: add an artificial target offset)
*  - If using the CHAM scattering volume, position TG at the centre of CHAM, 
*    otherwise put it in GAS2 (s.morrow).

      if (chamber_type_ff.le.0 .or. chamber_type_ff.ge.4) then
        call recmes(crname,'W',
     &        'Scattering chamber not defined -> no geometry')
        return
      elseif ( chamber_type_ff.eq.2 ) then
        cmoth='CHAM'
        z0pos=tg_pos_ff(3)
        tgpar_save(12)=upst_pos_ff(3)
      elseif ( chamber_type_ff.eq.3 ) then
        cmoth='DVCS'
        z0pos=0.325;
        tgpar_save(12)=0
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
      print *,'UGEOM_TG_DVCS chamber type:',chamber_type_ff,tleng,ivol
      call recmes(crname,'I',
     &     'Define geometry for electron target (shape "tube") ...')
*
***************************
*  define tracking media
***************************
*
*  these parameters are only(!) valid if  Data Card 'AUTO' = 0  !
      TMAXFD =  0.1       !max. angular deviation in one step [degree]
      STMAX  =  1.0       !max. step  [cm]
      DEEMAX =  0.01      !max. energy loss per step  [0;1]
      EPSIL  =  0.001     !boundary crossing precision
      STMIN  =  0.001     !min. step  [cm]
*
*  target material
*
      call GFMATE(itg_mate,cmat_tg,aa,zz,dens,radl,absl,parm,np)
      call GFMATE(imatcell,cmatcell,aa,zz,dens,radl,absl,parm,np)
      call GFMATE(imatins,cmatins,aa,zz,dens,radl,absl,parm,np)
      
*  print info

      istrlen=LNBLNK(cmat_tg)
      write(crmess,'(3A,F7.2,A)') 'set Target (material=',
     &      cmat_tg(1:istrlen),')  at z-pos.:',tg_pos_ff(3),'cm'
      call recmes(crname,'I',crmess)
        istrlen=LNBLNK(cmatcell)
        write(crmess,'(2(A,F7.3),A,F6.4,3A)') '   Target length:',
     &        tglenby2_eff*2,'cm;  radius:',tgrad,'cm;  cell thickness:',
     &        thickcell,'cm (',cmatcell(1:istrlen),')'
      call recmes(crname,'I',crmess)
*  
************************
*  geometry definitions
************************
*
*  mother volume (vacuum)
*  target and support structure for electron beam is positioned in 'UPST',
*  or 'GAS2' depending on which scattering chamber is being used.
*  
      pars(1)=0.0
      pars(2)=tg_tube1(2)+tg_tube2(3)/2.+tgrad
      pars(3)=tglenby2+distax/2+ABS(tg_flpos(1)/2)+0.5
      call gsvolu('TG  ','TUBE',clas_med_vac,pars,3,ivol)
      xytg=sqrt(tg_pos_ff(1)**2+tg_pos_ff(2)**2)
      ztg =pars(3)-tglenby2-distax
      call gspos('TG  ',1,cmoth, tg_pos_ff(1),tg_pos_ff(2),
     &     z0pos-ztg,0,'ONLY')

*
      print *,'UGEOM_TG_DVCS positioned in: ',cmoth,z0pos-ztg
*  target cell: 

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
      call gspos('TARG',1,'TG  ',0.,0.,ztg-tgoffset,0,'ONLY')
      
* cell walls - note: 0.992 is cosine of cell cone half angle

      parm(1) =   0.
      parm(2) = 360.
      parm(3) = 4
      
      do i=7,10
         parm(1+(i-6)*3) =tg_lh2_pos(i)
         parm(2+(i-6)*3) =tg_lh2_r2(i)
         parm(3+(i-6)*3) =parm(2+(i-6)*3)+thickcell/0.992
      enddo
      
      np=parm(3)*3+3
      call gsvolu('TGCL','PCON',tmed_cell,parm,np,ivol) !kapton cell walls
      call gspos('TGCL',1,'TG  ',0.,0.,ztg-tgoffset,0,'MANY')

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
      call gspos('TGEP',1,'TG  ',0.,0.,ztg-tgoffset,0,'MANY')

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
      call gspos('TGKP',1,'TG  ',0.,0.,ztg-tgoffset,0,'MANY')
     
*     kapton part of end cap window

      parm(1) =0.2
      parm(2) =tg_ecp_r2(2)
      parm(3) =0.0025
      call gsvolu('TGEC','TUBE',clas_med_kapton,parm,3,ivol) !endcap kapton
      call gspos('TGEC',1,'TG  ', 0.,0.,
     &     ztg-tgoffset+tg_ecp_pos(2)+parm(3),0,'MANY')
     
*     window on upstream end of target cell (Al)
*     or target cell exit window
 
      parm(1) =0.0
      parm(2) =0.2
      parm(3) =thickwindow/2   
      call gsvolu('TGW1','TUBE',clas_med_alu,parm,3,ivol)
      call gspos('TGW1',1,'TG  ', 0.,0.,
     &     ztg-tgoffset+tg_ecp_pos(2)-parm(3),0,'MANY')
*  save parameters
C   Rustam; fixed the vertex position     
      tgpar_save(1)= 2*tglenby2_eff
      tgpar_save(2)= tgrad
      tgpar_save(6)=-tglenby2_eff
      tgpar_save(7)= tg_pos_ff(3) +tgpar_save(6)
      tgpar_save(8) =tg_pos_ff(3) +tgpar_save(6)
      tgpar_save(9) =tg_pos_ff(3) +tgpar_save(7)
      tgpar_save(10)=xytg +tgrad

*  superinsulation
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
      
      parm(22) = tg_lh2_pos(10)+thickrad
      parm(23) = tg_lh2_r2(10)+thickcell/0.992
      parm(24) = parm(23)+thickrad
 
      parm(25) = parm(22)+1.4 !double layer (super_unsul+mylar)
      parm(26) = parm(23)
      parm(27) = parm(24)+tg_thickcell

      parm(28) = parm(22)+2.0 !only mylar layer for dz=0.6 cm according to 66840-E-04248
      parm(29) = parm(23)
      parm(30) = parm(23)+tg_thickcell

      call gsvolu('TGSI','PCON',tmed_ins,parm,30,ivol) !superinsulation
      call gspos('TGSI',1,'TG  ', 0.,0.,ztg-tgoffset,0,'MANY')

      tg_ins_ff(2) =thickrad
      tg_ins_ff(4) =distrad

*     There was a `reference window' 2cm downstream of the target cell 
*     which, during e1-6a, was made of super insulation.  For later runs 
*     it was replaced with 15um of aluminium as the super insulation 
*     was burned through by the beam, (S. Morrow 12/10/02).
*     insulation foil

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
      call gspos('TGSA',1,'TG  ',0.,0.,ztg-tgoffset,0,'MANY')
      
*     window on upstream end of target cell (Al)
*     or target cell inlet window

      parm(1) =0.0
      parm(2) =0.2
      parm(3) =thickwindow/2   
      call gsvolu('TGW2','TUBE',clas_med_alu,parm,3,ivol)
      call gspos('TGW2',1,'TG  ', 0.,0.,
     &     ztg-tgoffset+tg_ecp_pos(2)-parm(3)-tleng,0,'MANY')

*     outer base ring (torlon)

      parm(1) =0.
      parm(2) =360.
      parm(3) =10.
      do i=1,10
         parm(1+i*3) =tg_flpos(i)  
         parm(2+i*3) =tgrad + tg_floffset + tg_fldrad(i)
         parm(3+i*3) =parm(2+i*3) + tg_flthick(i)
      enddo

      call gsvolu('TGFL','PCON',clas_med_kapton,parm,33,ivol)
      call gspos('TGFL',1,'TG  ', 0.,0.,ztg-tgoffset,0,'MANY')

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
      call gspos('TGTU',1,'TG  ', 0.,0.,ztg-tgoffset,0,'MANY')

      

      end

