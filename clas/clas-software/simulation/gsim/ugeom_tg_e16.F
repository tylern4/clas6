      subroutine ugeom_tg_e16(itg_mate)
  
c_begin_doc
c
c  Documentation for subroutine ugeom_tg_e16
c  
c  Purpose:
c  --------
c     ******************************************************************
c     *                                                                *
c     *       e1-6a target implementation                              *
c     *       Author: S. Morrow                                        *
c     *       Date: 07/25/02                                           *
c     *                                                                *
c     ******************************************************************
c
c  for tg_type_ff=16
c   #   cell wall               end cap   length  diameter  name
c   1   Kapton,128mu(0.005inch) Al 15mu   50mm    15.5mm    'e1-6'/'e1-6a'
c       upstream entrance window 15um Al
c   group# 6
c
c  support structure type=Torlon (Kapton in block form)
c  superinsulation material #163.0 (Aluminized Mylar)
c  shape :  tube + base ring (upstream)
c
c  Taken from drawing 66850-A-03456
c
c_end_doc

      implicit none
      character*(*) cfile,crevis,cstate,cdate,cautho,crname,crauth
      character*132 crcsid,crmess
      parameter (cfile=  '$RCSfile: ugeom_tg_e16.F,v $')
      parameter (crevis= '$Revision: 1.5 $')
      parameter (cstate= '$State: Exp $')
      parameter (cdate=  '$Date: 2003/03/27 16:36:47 $')
      parameter (cautho= '$Author: sam $')
      parameter (crname= 'UGEOM_TG_E16')
      parameter (crauth= 'sam')
      data crcsid/'
     1$Id: ugeom_tg_e16.F,v 1.5 2003/03/27 16:36:47 sam Exp $
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
      real  thickwindow,radwindow       !Al window info
      integer tmed_tg                !track.medium for target material
      integer imatcell, tmed_cell    !mate# and track.medium for target cell
      integer imatins, tmed_ins      !for superinsulation layers 
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
      real tg_len_eff(1:1)
      real tg_diam                   !target diameter
      real tg_thickcell              !cell wall thickness  (material: Kapton)
      real tg_thickwindow            !Al window thickness
      real tg_radwindow              !Al window radius
      real tg_thickins               !total thickness of superinsulation 
                                     !layers (material: aluminized Mylar)
                                     !packed as solid block (ca. 3.5% of total)
      real tg_distrad                !radius of insulation
      real ins_dlen                  !length of layers in downstream dir.
      data tg_len                 / 7.20 /
      data tg_len_eff             / 5.00 /
      data tg_diam,tg_thickcell   / 1.55, 0.0128 /
      data tg_thickwindow         / 0.0015 /
      data tg_radwindow           / 0.6 /
      data tg_distrad,tg_thickins / 5.2, 0.00715 / 
      data ins_dlen               / 2.00 /
*
*  gas/liquid distribution pipes & support structures 
*  --------------------------------------------------
*
*  pcon:           base ring (torlon) at entry window position 
*  tube1 & tube2:  horizontal, connected to base ring and condenser
*
      real tg_flpos(5)             ! relative position wrt. entry window
      real tg_flthick(5)           ! thickness of torlon ring
      real tg_fldrad(5)            ! relative outer radius wrt. cell radius
      data tg_flpos   /-2.55,-1.80,-1.80,-1.275, 0.0 /
      data tg_flthick / 0.58, 1.33, 0.95, 1.480, 0.2 /
      data tg_fldrad  /-0.38,-0.38, 0.00, 0.00, 0.0 /
      real tg_floffset             ! distance by which the target cell
      data tg_floffset / 0.5 /     ! slides into the flange TGFL 

      real tg_tupos(4)             !relative position wrt. entry window
      real tg_tuthick(4)           !thickness of inner base tube (support)
      real tg_turad(4)             !thickness of inner base tube (support)
      data tg_tupos   /-2.55,1.56,1.56,1.71 /
      data tg_tuthick / 0.15,0.15,0.30,0.30 /

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

      tmed_cell= clas_med_kapton   ! cell walls kapton
      tmed_ins = clas_med_cerexmyl1 ! superins. CEREX/Alum.mylar
      if(itg_mate.EQ.imate_vacuum) then ! Choose empty or full (H2) cell
         tmed_tg  = clas_med_vac
      elseif(itg_mate.EQ.imate_liq_h2) then
         tmed_tg  = clas_med_h2liq
      else
         call recmes(crname,'W',
     &        'Only HYDR or VAC targets supported for e1-6a')
         return
      endif

      tglenby2 = tg_len(1)/2
      tglenby2_eff = tg_len_eff(1)/2
      tgrad    = tg_diam/2

      thickwindow = tg_thickwindow
      radwindow   = tg_radwindow
      thickcell = tg_thickcell
      thickrad  = tg_thickins
c     Thickness of axial part of super ins. 
c     (after the e1-6a run this was changed to 15um of Al).
      thickax   = tg_thickins
      distrad   = tg_distrad
      distax    = ins_dlen

      tg_turad(1)  = tgrad + tg_fldrad(1)
      tg_turad(2)  = tg_turad(1)
      tg_turad(3)  = tg_turad(1) + 0.05
      tg_turad(4)  = tg_turad(1) + 0.05
      do i =1,5
         tg_flpos(i) =tg_flpos(i)+tg_floffset
      enddo
      do i =1,4
         tg_tupos(i) =tg_tupos(i)+tg_floffset
      enddo
*
*  stop if beam pipe volume not defined
*
*  - until 'FOIL' volumes are not cleaned up, put target into 'GAS2' volume
*    which is positioned at z=5.06 (so: add an artificial target offset)
*  - If using the CHAM scattering volume, position TG at the centre of CHAM, 
*    otherwise put it in GAS2 (s.morrow).
      if (chamber_type_ff.le.0 .or. chamber_type_ff.ge.3) then
        call recmes(crname,'W',
     &        'Scattering chamber not defined -> no geometry')
        return
      elseif ( chamber_type_ff.eq.2 ) then
        cmoth='CHAM'
        z0pos=tg_pos_ff(3)-1.1
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
     &      cmat_tg(1:istrlen),')  at z-pos.:',
     &      tg_pos_ff(3)+tgpar_save(12),'cm'
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
*  (a) mother volume (vacuum)
*  -----------------
*  target and support structure for electron beam is positioned in 'UPST',
*  or 'GAS2' depending on which scattering chamber is being used.
*  
      pars(1)=0.0
      pars(2)=MAX((tgrad+distrad+thickrad)*1.05,
     &     tg_tube1(2)+tg_tube2(3)/2.+tgrad)
      pars(3)=tglenby2+distax/2+ABS(tg_flpos(2)/2)+tg_tube1(3)+tg_tube2(3)
      call gsvolu('TG  ','TUBE',clas_med_vac,pars,3,ivol)
      xytg=sqrt(tg_pos_ff(1)**2+tg_pos_ff(2)**2)
      ztg =pars(3)-tglenby2-distax
      call gspos('TG  ',1,cmoth, tg_pos_ff(1),tg_pos_ff(2),
     &     z0pos-ztg,0,'ONLY')
*
*  (b) target cell: 
*  ----------------
*  cell shape: tube  
*
      parm(1)=  0.
      parm(2)=360.
*  end position of tube
      tg_zpos2= tglenby2
      parm(3)=6
      parm(4) =-tglenby2             !upstream end
      parm(5) = tg_turad(1) 
      parm(6) = tgrad
      parm(7)  =tg_tupos(2)-thickcell-tglenby2 !Mould around shape of TGTU
      parm(8)  =tg_turad(2) 
      parm(9)  =tgrad
      parm(10) =tg_tupos(3)-thickcell-tglenby2
      parm(11) =tg_turad(3) 
      parm(12) =tgrad
      parm(13) =tg_tupos(4)-thickcell-tglenby2
      parm(14) =tg_turad(4) 
      parm(15) =tgrad
      parm(16) =tg_tupos(4)-thickcell-tglenby2
      parm(17) =0.0
      parm(18) =tgrad
      parm(19) =tg_zpos2              !downstream end
      parm(20) =0.0
      parm(21) =tgrad
      np=parm(3)*3+3

      call gsvolu('TARG','PCON',tmed_tg,parm,np,ivol)
      call gspos('TARG',1,'TG  ',0.0,0.0,ztg,0,'MANY')

*  save parameters
      tgpar_save(1)= 2*tglenby2_eff
      tgpar_save(2)= tgrad
      tgpar_save(6)=-tglenby2_eff
      tgpar_save(7)= tg_pos_ff(3) +tgpar_save(6)
      tgpar_save(8) =tg_pos_ff(3) +tgpar_save(6)
      tgpar_save(9) =tg_pos_ff(3) +tgpar_save(7)
      tgpar_save(10)=xytg +tgrad
*
*  (c) superinsulation
*  -------------------
*
      pars(1)=0.0               !radial superins.
      pars(2)=360.0
      pars(3)=4
      pars(4)= -tglenby2    -thickcell +tg_flthick(5)
     &         +tg_floffset -distrad  -distax*3.0
      pars(5)= tgrad +thickcell +distrad 
      pars(6)= tgrad +thickcell +thickrad +distrad
      pars(7)= pars(4) +distax*4.0 
      pars(8)= pars(5)
      pars(9)= pars(6)
      pars(10)=pars(7) +distrad
      pars(11)=pars(8) -distrad !inner radius: touching target wall
      pars(12)=pars(9) -distrad
      pars(13)=tglenby2 +distax*2.0      
      pars(14)=pars(11)
      pars(15)=pars(12)
      call gsvolu('TGSI','PCON',tmed_ins,pars,15,ivol)
      call gspos('TGSI',1,'TG  ', 0.,0.,ztg-distax,0,'ONLY')
      tg_ins_ff(2) =thickrad
      tg_ins_ff(4) =distrad

      tgpar_save(8) =tg_pos_ff(3) -pars(3)
      tgpar_save(9) =tg_pos_ff(3) +pars(3)
      tgpar_save(10)=xytg +pars(2)
*
*     There was a `reference window' 2cm downstream of the target cell 
*     which, during e1-6a, was made of super insulation.  For later runs 
*     it was replaced with 15um of aluminium as the super insulation 
*     was burned through by the beam, (S. Morrow 12/10/02).
*
      pars(1)=0.0               
      pars(2)=tgrad +thickcell
      pars(3)=thickax/2. 
      if(thickax.GT.0) then
         call gsvolu('TGSA','TUBE',tmed_ins,pars,3,ivol)
         call gspos('TGSA',1,'TG  ', 0.,0.,
     &        tglenby2+distax-pars(3)+ztg,0,'ONLY')
      endif
*
*
*  (d) target cell walls
*  ---------------------
*
*  planes for target cell walls:
      parm(3)=6          
      np=parm(3)*3+3
      parm(4)=-tglenby2-thickcell
      parm(5)=tg_turad(1)   
      parm(6)=tgrad+thickcell
      parm(7)=-tglenby2 
      parm(8)=tg_turad(1)   
      parm(9)=tgrad+thickcell
      parm(10)=-tglenby2
      parm(11)=tgrad
      parm(12)=tgrad+thickcell
      parm(np-8)=tglenby2
      parm(np-7)=tgrad
      parm(np-6)=tgrad+thickcell
      parm(np-5)=tglenby2
      parm(np-4)=radwindow
      parm(np-3)=tgrad+thickcell
      parm(np-2)=tglenby2+thickcell
      parm(np-1)=radwindow
      parm(np)  =tgrad+thickcell
      call gsvolu('TGCL','PCON',tmed_cell,parm,np,ivol)
      call gspos('TGCL',1,'TG  ',0.,0.,ztg,0,'MANY')

      tgpar_save(3)=thickcell
      tgpar_save(4)=thickcell
*
*     window on downstream end of target cell (Al)
*
      parm(1) =0.0
      parm(2) =radwindow
      parm(3) =thickwindow/2
      call gsvolu('TGW1','TUBE',clas_med_alu,parm,3,ivol)
      call gspos('TGW1',1,'TG  ',0.,0.,
     &     ztg+tglenby2+parm(3),0,'MANY')
*
*  (e) support structure 
*  ---------------------
*
*
*     virtual volume to hold the others
      parm(1) =0.0
      parm(2) =tg_flthick(4) +tg_fldrad(4) +tgrad +thickcell 
      parm(3) =0.3+(tg_tupos(4)-tg_tupos(1))/2
      call gsvolu('TGFA','TUBE',clas_med_vac,parm,3,ivol)
      call gspos('TGFA',1,'TG  ', 0.,0.,-tglenby2+ztg,0,'MANY')
*
*     window on upstream end of target cell (Al)
*
      parm(1) =0.0
      parm(2) =tg_turad(4)-tg_tuthick(4)
      parm(3) =thickwindow/2   
      call gsvolu('TGW2','TUBE',clas_med_alu,parm,3,ivol)
      call gspos('TGW2',1,'TGFA', 0.,0.,
     &     tg_tupos(4)-thickcell-parm(3),0,'MANY')

*
*     outer base ring (torlon)

      parm(1) =0.
      parm(2) =360.
      parm(3) =5.
      do i=1,5
         parm(1+i*3) =tg_flpos(i)- thickcell 
         parm(2+i*3) =tgrad + thickcell + tg_fldrad(i)
         parm(3+i*3) =parm(2+i*3) + tg_flthick(i)
      enddo
      call gsvolu('TGFL','PCON',clas_med_kapton,parm,18,ivol)
      call gspos('TGFL',1,'TGFA', 0.,0.,0.,0,'MANY')
*
*    inner base tube (torlon)
*
      parm(1) =0.
      parm(2) =360.
      parm(3) =4.
      do i=1,4
         parm(1+i*3) =tg_tupos(i)-thickcell 
         parm(2+i*3) =tg_turad(i) - tg_tuthick(i)
         parm(3+i*3) =tg_turad(i)
      enddo
      call gsvolu('TGTU','PCON',clas_med_kapton,parm,15,ivol)
      call gspos('TGTU',1,'TGFA', 0.,0.,0.,0,'MANY')
*     
*  gas distribution tubes (in vacuum box)
*       tube2 angled by 45deg. wrt. z-axis

      pars(1)=tg_tube1(2)
      pars(2)=tg_tube2(3)/2+tg_tube1(2)
      pars(3)=tg_tube1(3)+tg_tube2(3)
      call gsvolu('TGST','BOX ',clas_med_vac,pars,3,ivol)
      call gsvolu('TGS1','TUBE',clas_med_alu,tg_tube1,3,ivol)
      call gsvolu('TGS2','TUBE',clas_med_alu,tg_tube2,3,ivol)
      irot1=get_rotm()
      call gsrotm(irot1,90.,0.,230.,90.,135.,90.)
      call gspos('TGS1',1,'TGST', 0.,
     &     pars(2)-tg_tube1(2),1.0-tg_tube2(3),0,'MANY')
      call gspos('TGS2',1,'TGST', 0.,0.,tg_tube1(3),irot1,'MANY')

*  3 copies of gas pipe volumes (named 'TGST') are positioned in mother volume
*  (rotated by 120 deg.)
      irot2=get_rotm()
      call gsrotm(irot2,-90.,-120.,-90.,-210.,0.,0.)
      irot3=get_rotm()
      call gsrotm(irot3,90.,120.,90.,210.,0.,0.)
      xypos=pars(2)+tgrad+tg_fldrad(1)+tg_flthick(1)
      xpos=xypos*sin(twopiby3)
      ypos=xypos*cos(twopiby3)
      zpos=-tglenby2+tg_flpos(2)-pars(3)+ztg+1.0
      call gspos('TGST',1,'TG  ',  0.,  xypos, zpos,0,'MANY')
      call gspos('TGST',2,'TG  ',  xpos, ypos, zpos,irot2,'MANY')
      call gspos('TGST',3,'TG  ', -xpos, ypos, zpos,irot3,'MANY')

      tgpar_save(8) = tg_pos_ff(3) -zpos -pars(3) 
      tgpar_save(10)= MAX(tgpar_save(10),(xytg+xypos+pars(2))) 
*
*  support ring for the distribution tubes
*
      pars(1)=tg_tube3(1)
      pars(2)=tg_tube3(2)
      pars(3)=tg_tube3(3)
      call gsvolu('TGS3','TUBE',clas_med_alu,pars,3,ivol)
      call gspos('TGS3',1,'TG  ', 0.,0.,-11.0,0,'MANY')

      end

c-----------------------------------------------------------------------------
