
       subroutine geom_eltg2(itg_mate)
  
c_begin_doc

c  Documentation for subroutine geom_eltg2
c  
c  Purpose:
c  --------
c     ******************************************************************
c     *                                                                *
c     *      Defines the geometry of the CLAS - SACLAY Cryotarget      *
c     *       for electron target cells of SACLAY shape 'cuivre'       *
c     *                                                                *
c     ******************************************************************
c
c  for tg_type_ff=2
c   #   cell wall      end cap       length  diameter  names
*   2   Alu,120mu      Alu,120mu       38mm    8.6mm    'e1c'
c  support structure type=2, superinsulation material #163.0 (?) 
c
c  TO BE CHECKED !!!
c
c  Major revisions:
c  ----------------
c
c_end_doc


      implicit none
      character*(*) cfile,crevis,cstate,cdate,cautho,crname,crauth
      character*132 crcsid,crmess
      parameter (cfile=  '$RCSfile: geom_eltg2.F,v $')
      parameter (crevis= '$Revision: 1.2 $')
      parameter (cstate= '$State: Exp $')
      parameter (cdate=  '$Date: 2001/07/13 08:03:56 $')
      parameter (cautho= '$Author: fklein $')
      parameter (crname= 'GEOM_ELTG2')
      parameter (crauth= 'fklein')
      data crcsid/'
     1$Id: geom_eltg2.F,v 1.2 2001/07/13 08:03:56 fklein Exp $
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

      real  twopiby3, PIby2
      integer i, np
      integer istrlen
      integer ivol                      !volume#
      integer irot1, irot2, irot3       !rotation_matrix#
      real  parm(50), pars(5)           !arrays for volume definitions
      real  xpos, ypos, zpos, xypos     !position of volumes
      real  xytg, ztg                   !position of target center
      real  tglenby2                    !half length of target cell incl. end caps
      real  tgrad                       !target radius w/o walls 
      real  thickcell                   !thickness of cell walls
      real  thickrad, thickax           !thickness of superins.: radial; axial
      data  thickcell,thickrad,thickax /-1, -1, -1 /
      real  distax                      !dist. of axial superins. to end cap
      integer tmed_tg                   !track.medium for target material
      integer imatcell, tmed_cell       !mate# and track.medium for target cell
      integer tmed_support              !track.medium for flange and distr.tubes
      integer imatrad, tmed_rad         !for radial superinsulation layers 
      integer imatax, tmed_ax           !for axial superinsulation layers 
      data  imatcell,imatrad,imatax /-1, -1, -1 /       !take default
      integer  icellflg, h_cellflag(3)  !options: wall/superins. geom.
      data h_cellflag /4HCELL,4HOFF ,4HINS /
      real  tmaxfd, stmax, deemax, epsil, stmin   !track.media cuts 
      real  aa,zz,dens,radl,absl                  !material constants
      character*20 cmat_tg, cmatcell, cmatins     !material names
      character*4  cmoth                          !mother volume
      real  z0pos                                 !z-position in mother volume
*
*  default values for target geometry  
*  ----------------------------------
*
*   shape='cigarre' i.e. tube with spherical end cap in downstream dir.
*                         [cf. drawing for photon beam target: Barranger 8/93]
*
      real tg_len(2:2)             !target length (depends on target type)
      real tg_diam(2:2)            !target diameter
      real tg_thickcell(2:2)       !cell wall thickness  (material: alu)
      real tg_cap_rad,tg_cap_thick !radius,thickness of al.windows at end caps 
      real tg_cap2_len             !cone length of downstream end cap
*  position, thickness of superinsulation only my guess!!!
      real tg_thickrad,tg_thickax  !total thickness of superinsulation layers
                                   !          (material: aluminized Mylar)
                                   !packed as solid block (ca. 3.5% of total)
      real ins_rad1,ins_rad2       !radius of superins.at upstream/downstream end
      real ins_zpos1,ins_zpos2     !position of upstream/downstream end of ins.
      data tg_len                   /  3.8  /
      data tg_diam                  / 0.86  /
      data tg_thickcell             / 0.0120 /
      data tg_cap2_len              / 0.50 /
      data tg_cap_rad,tg_cap_thick  / 0.25 , 0.0016 /
      data tg_thickrad,tg_thickax   / 0.021 , 0.021  / 
      data ins_rad1,ins_rad2        /  4.5 ,  2.0 / 
      data ins_zpos1,ins_zpos2      /-15.0 ,  2.4 /      ! =dist. from end caps

*
*  offset vs. volume GAS2 to position the target in CLAS center:
      real  eltg_zoff
      data  eltg_zoff /-5.06/
*
*  gas/liquid i/o pipes & support structures 
*  ------------------------------------------
*  cone:   upstream flange to target cell (outer cone)
*  pcon:   upstream inner flange to cell (cone&tube)
*  tube1:  horizontal, to backward condenser
*  tube2:  angled by 45 deg., attached to flange
      real tg_cone(5), tg_tube1(3), tg_tube2(3)
      real tg_pcon_pos(4),tg_pcon_rad(4)
      data tg_cone   / 1.5, 1.28, 1.325, 0.51, 0.537 /
      data tg_tube1  / 0.25, 0.35,   5.0 /
      data tg_tube2  / 0.25, 0.35,   2.2 /
      data tg_pcon_pos / -3.2, -2.5, -0.9, 0.0 /
      data tg_pcon_rad / 0.89, 0.89, 0.34, 0.34 / 
      
c_end_var


c  executable code:
c  ----------------
      piby2   =acos(0.)
      twopiby3=acos(-1.)*2./3.
*
      if(tgpar_save(5).GE.0.0.AND.tgpar_save(5).LE.NMATE) 
     &          imatcell = IFIX(tgpar_save(5))
      if(tg_ins_ff(1).GT.0.0) imatrad = IFIX(tg_ins_ff(1))
      if(tg_ins_ff(2).GE.0.0) thickrad = tg_ins_ff(2)
      if(tg_ins_ff(3).GE.0.0) thickax  = tg_ins_ff(3)
      if(tg_ins_ff(5).GT.0.1) ins_rad2 = tg_ins_ff(5)    !at downstream end
      distax = ins_zpos2
      if(tg_ins_ff(6).GT.0.1) distax   = tg_ins_ff(6)
      if(thickrad.LT.0.0)    thickrad  = tg_thickrad
      if(thickax.LT.0.0)     thickax   = tg_thickax

*  correct material id in case that no target walls/superinsulation chosen
      icellflg=IUCOMP(tg_cell_ff,h_cellflag,3)
      if(icellflg.GT.0) then
        if(icellflg.GT.1) imatcell=0
        if(icellflg.LT.2) imatrad =0
      endif

*  axial heat shield: default=same material as radial heat shield
      imatax=imatrad
      if(imatrad.EQ.-1) imatrad = imate_cerexmyl1
      if(imatax.EQ.-1)  imatax  = imate_cerexmyl1

********************************************
*  stop here if 'tg_type_ff' out of range  *
*
      if(tg_type_ff.GE.2 .AND. tg_type_ff.LE.2) then

        tglenby2  = tg_len(tg_type_ff)/2.0
        tgrad     = tg_diam(tg_type_ff)/2.0
        thickcell = tg_thickcell(tg_type_ff)
        if(imatcell.EQ.-1)  imatcell = imate_aluminum

      elseif(tg_type_ff.EQ.-1) then             !user defined geom.

        tglenby2  = tg_user_ff(1)/2
        tgrad     = tg_user_ff(2)/2
        thickcell = tg_thickcell(2)             !default values for e1c target
        if(tg_user_ff(3).GT.0) thickcell = tg_user_ff(3)
        if(imatcell.EQ.-1)     imatcell  = imate_aluminum

      else
        call recmes(crname,'W','Target type not defined -> no target geometry')
        return
      endif
*
*  stop if beam pipe volume not defined
*
*  - until 'FOIL' volumes are not cleaned up, put target into 'GAS2' volume
*    which is positioned at z=5.06 (so: add an artificial target offset)
*  - If using the CHAM scattering volume, position TG at the centre of CHAM, 
*    otherwise put it in GAS2 (s.morrow).

      if (chamber_type_ff.le.0 .or. chamber_type_ff.ge.3) then
        call recmes(crname,'W','Scattering chamber not defined -> no geometry')
        return
      elseif ( chamber_type_ff.eq.2 ) then
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
        write(crmess,'(3A)') 'Scattering chamber volume "',cmoth,'" not defined -> no target geometry'
        call recmes(crname,'W',crmess)
        return
      endif
      call recmes(crname,'I','Define geometry for electron target (shape "cuivre") ...')
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
      if(aa.eq.-1) then
        call recmes(CRNAME,'W','Target material not defined -> NO TARGET!')
        return
      endif
      if(itg_mate.NE.imate_vacuum) then
         tmed_tg=get_tmed()
         CALL GSTMED( tmed_tg,cmat_tg,itg_mate,
     &             1, init_ifield,init_fieldm,tmaxfd,
     &             stmax, deemax, epsil, stmin, 0 , 0 )
      else
         tmed_tg=clas_med_vac
      endif
*
*  target cell walls:
*  
      if(imatcell.GT.0) then
         call GFMATE(imatcell,cmatcell,aa,zz,dens,radl,absl,parm,np)
         if(aa.gt.0) then
            tmed_cell=get_tmed()
            CALL GSTMED( tmed_cell,cmatcell,imatcell, 
     &           1, init_ifield,init_fieldm,tmaxfd,
     &           stmax, deemax, epsil, stmin, 0 , 0 )
         else
            imatcell=0
         endif
      endif
*
*  superinsulation
*
      if(imatrad.GT.0) then
        call GFMATE(imatrad,cmatins,aa,zz,dens,radl,absl,parm,np)
        if(aa.gt.0) then
          tmed_rad=get_tmed()
          CALL GSTMED( tmed_rad,cmatins,imatrad, 
     &             1, init_ifield,init_fieldm,tmaxfd,
     &             stmax, deemax, epsil, stmin, 0 , 0 )
        else
          imatrad=0
        endif
      endif

      if(imatax.GT.0) then
        if(imatax.EQ.imatrad) then
          tmed_ax = tmed_rad
        else
          call GFMATE(imatax,cmatins,aa,zz,dens,radl,absl,parm,np)
          if(aa.gt.0) then
            tmed_ax=get_tmed()
            CALL GSTMED( tmed_ax,cmatins,imatax, 
     &             1, init_ifield,init_fieldm,tmaxfd,
     &             stmax, deemax, epsil, stmin, 0 , 0 )
          else
            imatax=imatrad
            tmed_ax=tmed_rad
          endif
        endif
      endif
*
*  support structure

      if(imatcell.EQ.0) icellflg=2
      if(icellflg.LE.1) then
        tmed_support=get_tmed()              !default material aluminum
        CALL GSTMED(tmed_support,'ALUMINUM$', imate_aluminum, 0,
     &      init_ifield,init_fieldm,tmaxfd,stmax,deemax,epsil,stmin,0,0)
      endif

*  print info

      istrlen=LNBLNK(cmat_tg)
      write(crmess,'(3A,F7.2,A)') 'set Target (material=',
     &      cmat_tg(1:istrlen),')  at z-pos.:',tg_pos_ff(3)+tgpar_save(12),'cm'
      call recmes(crname,'I',crmess)
      if(imatcell.GT.0) then
        istrlen=LNBLNK(cmatcell)
        write(crmess,'(2(A,F7.3),A,F6.4,3A)') '   Target length:',
     &        tglenby2*2,'cm;  radius:',tgrad,'cm;  cell thickness:',
     &        thickcell,' (',cmatcell(1:istrlen),')'
      else
        write(crmess,'(2(A,F7.3),A)') '   Target length:',tglenby2*2,
     &        'cm;  radius:',tgrad,'cm'
      endif
      call recmes(crname,'I',crmess)

*  
************************
*  geometry definitions
************************
*
*  (a) mother volume (vacuum)
*  -----------------
*  target and support structure for electron beam is positioned in 'FOIL',
*  the whole structure is put into a vacuum cone

      pars(1)=(abs(ins_zpos1)+ins_zpos2+thickax)/2 +tglenby2
      pars(2)=0.0
      pars(3)=ins_rad1+thickrad
      pars(4)=0.0
      pars(5)=ins_rad2+thickrad*2
      call gsvolu('TG  ','CONE',clas_med_vac,pars,5,ivol)
      xytg=sqrt(tg_pos_ff(1)**2+tg_pos_ff(2)**2)
      ztg =(ins_zpos2+thickax+ins_zpos1)/2
      call gspos('TG  ',1,cmoth, tg_pos_ff(1),tg_pos_ff(2),z0pos+ztg,0,'ONLY')
*
*  (b) target cell: 
*  ----------------
*  target inlet window is moved 4mm inside target cell

      parm(1)=  0.
      parm(2)=360.
      parm(3)=5                       !#planes
      np=parm(3)*3+3
      do i=1,5
        parm(i*3+2)=0.0
      enddo
      parm(4)=-tglenby2
      parm(6)=tgrad
      parm(7)=tglenby2-tg_cap2_len
      parm(9)=tgrad
      parm(10)=tglenby2-tg_cap2_len*2/3
      parm(12)=tgrad*0.9
      parm(13)=tglenby2-tg_cap2_len/3
      parm(15)=(tg_cap_rad+tgrad)/2
      parm(16)=tglenby2
      parm(18)=tg_cap_rad
      call gsvolu('TARG','PCON',tmed_tg,parm,np,ivol)
      call gspos('TARG',1,'TG  ',0.0,0.0,-ztg,0,'ONLY')
*  save parameters
      tgpar_save(1) =tglenby2
      tgpar_save(2) =tgrad
      tgpar_save(6) =-tglenby2
      tgpar_save(7) =tg_pos_ff(3) +tgpar_save(6)
      tgpar_save(8) =tg_pos_ff(3) +tgpar_save(6)
      tgpar_save(9) =tg_pos_ff(3) +tgpar_save(7)
      tgpar_save(10)=xytg +tgrad
*
*  (c) superinsulation
*  -------------------
*
      if(imatrad.GT.0) then
        pars(1)=(abs(ins_zpos1)+ins_zpos2)/2 +tglenby2
        pars(2)=ins_rad1
        pars(3)=ins_rad1+thickrad
        pars(4)=ins_rad2
        pars(5)=ins_rad2+thickrad
        call gsvolu('TGSI','CONE',tmed_rad,pars,5,ivol)
        call gspos('TGSI',1,'TG  ', 0.,0.,-thickax/2,0,'ONLY')

        pars(2)=0.0
        pars(3)=ins_rad2+thickrad
        pars(4)=thickax/2.
        tg_ins_ff(2) =thickrad
        tgpar_save(8)=tg_pos_ff(3) -tglenby2 +ins_zpos1
        tgpar_save(9)=tg_pos_ff(3) +tglenby2 +ins_zpos2+pars(3)*2
        tgpar_save(10)=xytg +ins_rad1
        if(thickax.GT.0) then
          call gsvolu('TGSA','TUBE',tmed_ax,pars(2),3,ivol)
          call gspos('TGSA',1,'TG  ', 0.,0.,pars(1),0,'ONLY')
          tg_ins_ff(3)=thickax

*  superinsulation at upstream end (condenser side)
          pars(3)=ins_rad1
          call gsvolu('TGSC','TUBE',tmed_ax,pars(2),3,ivol)
          call gspos('TGSC',1,'TG  ', 0.,0.,-pars(1),0,'MANY')
        endif

      endif
*
*  (d) target cell walls
*  ---------------------
*
      if(imatcell.EQ.0) RETURN

*  planes for target cell walls:
      do i=1,5
        parm(i*3+2)=parm(i*3+3)
        parm(i*3+3)=parm(i*3+3) +thickcell
      enddo
      call gsvolu('TGCL','PCON',tmed_cell,parm,np,ivol)
      call gspos('TGCL',1,'TG  ',0.0,0.0,-ztg,0,'ONLY')
      tgpar_save(3)=thickcell
      tgpar_save(5)=imatcell

*  alu disks for end caps:  
      pars(1)=0.0
      pars(2)=tg_cap_rad
      pars(3)=tg_cap_thick/2
      call gsvolu('TGCC','TUBE',tmed_support,pars,3,ivol)
      call gspos('TGCC',1,'TG  ',0.0,0.0,tglenby2+pars(3)-ztg,0,'ONLY')
      call gspos('TGCC',2,'TG  ',0.0,0.0,-tglenby2-pars(3)-ztg,0,'ONLY')
      tgpar_save(4)=tg_cap_thick
*
*  (e) support structure 
*  ---------------------
*
      if(icellflg.GT.1) return
*
*  cone shaped flange (2 parts)

      call gsvolu('TGF1','CONE',tmed_support,tg_cone,5,ivol)
      zpos=-tglenby2-tg_cone(1)-ztg
      call gspos('TGF1',1,'TG  ', 0.,0.,zpos,0,'ONLY')
      parm(3) =4.
      do i=1,4
        parm(1+i*3) =tg_pcon_pos(i)
        parm(2+i*3) =tg_pcon_rad(i)
        parm(3+i*3) =parm(2+i*3)+0.025
      enddo
      call gsvolu('TGF2','PCON',tmed_support,parm,15,ivol)
      call gspos('TGF2',1,'TG  ', 0.,0.,-tglenby2-ztg,0,'ONLY')

*  distribution tubes in vacuum box (tube1 angled by 45deg. wrt. z-axis)
*   3 copies of gas pipe volumes (named 'TGDT') are rotated by 120 deg.

      irot1=get_rotm()
      call gsrotm(irot1,90.,0.,225.,90.,135.,90.)
      pars(1)=MAX(tg_tube1(2),tg_tube2(2))
      pars(2)=tg_tube1(2)*1.6+tg_tube2(3)/2
      pars(3)=tg_tube1(3) + pars(2) -tg_tube1(1)/2
*  if tube too long to fit into mother volume: cut the end!
      zpos=tg_pcon_pos(2) -ins_zpos1
      if(2*pars(3).GT.zpos) then
        pars(3)=zpos/2
        tg_tube1(3)=pars(3) -pars(2) +tg_tube1(1)/2
      endif
      call gsvolu('TGDT','BOX ',clas_med_vac,pars,3,ivol)
      call gsvolu('TGD1','TUBE',tmed_support,tg_tube1,3,ivol)
      call gsvolu('TGD2','TUBE',tmed_support,tg_tube2,3,ivol)
      call gspos('TGD2',1,'TGDT', 0.,0.,tg_tube1(3)-tg_tube1(1)/2,irot1,'ONLY')
      call gspos('TGD1',1,'TGDT', 0.,pars(2)-tg_tube1(2)*1.1,-pars(3)+tg_tube1(3),0,'ONLY')
*
      irot2=get_rotm()
      call gsrotm(irot2,-90.,-120.,-90.,-210.,0.,0.)
      irot3=get_rotm()
      call gsrotm(irot3,90.,120.,90.,210.,0.,0.)
      xypos=pars(2)+tgrad
      xpos=xypos*sin(twopiby3)
      ypos=xypos*cos(twopiby3)
      zpos=-tglenby2+tg_pcon_pos(2) -pars(3) -ztg
      call gspos('TGDT',1,'TG  ',  0.,  xypos, zpos,    0,'MANY')
      call gspos('TGDT',2,'TG  ',  xpos, ypos, zpos,irot2,'MANY')
      call gspos('TGDT',3,'TG  ', -xpos, ypos, zpos,irot3,'MANY')
      tgpar_save(8)=zpos -pars(3)

      end
