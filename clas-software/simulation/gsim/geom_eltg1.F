
       subroutine geom_eltg1(itg_mate)
  
c_begin_doc

c  Documentation for subroutine geom_eltg1
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
c  for tg_type_ff=1
c   #   cell wall      end cap       length  diameter  names
c   1   Kapton,120mu   Kapton,120mu    50mm    8.6mm    'e1a','e1b'
c  support structure type=1, superinsulation material #163.0 (?) 
c  shape "cuivre":  tube + half shell downstream + base ring (upstream)
c
c  TO BE CHECKED !!!
c
c    [cf. Catalogue des cibles realisees pour CLAS, 12/96;
c           Note on available targets (S.Cristo,C.Marchand,L.Farhi, 2/98]
c
c  Major revisions:
c  ----------------
c
c_end_doc


      implicit none
      character*(*) cfile,crevis,cstate,cdate,cautho,crname,crauth
      character*132 crcsid,crmess
      parameter (cfile=  '$RCSfile: geom_eltg1.F,v $')
      parameter (crevis= '$Revision: 1.4 $')
      parameter (cstate= '$State: Exp $')
      parameter (cdate=  '$Date: 2001/07/13 08:03:55 $')
      parameter (cautho= '$Author: fklein $')
      parameter (crname= 'GEOM_ELTG1')
      parameter (crauth= 'fklein')
      data crcsid/'
     1$Id: geom_eltg1.F,v 1.4 2001/07/13 08:03:55 fklein Exp $
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
      integer i2                        !#plane in PolyCONe for target cell
      real  tg_zpos2                    !pos. of tube-like part of target cell
      real  dangle                      !step size for cone of target cell
      real  parm(50), pars(3)           !arrays for volume definitions
      real  xpos, ypos, zpos, xypos     !position of volumes
      real  xytg, ztg                   !position of target center
      real  tglenby2, tgrad, thickcell  !target info 
      real  distrad, distax             !insulation (distance from target)
      real  thickrad, thickax           !insulation (thickness)
      integer tmed_tg                !track.medium for target material
      integer imatcell, tmed_cell    !mate# and track.medium for target cell
      integer imatins, tmed_ins      !for superinsulation layers 
      integer tmed_support           !track.medium for flansh and distr.pipes
      real  tmaxfd, stmax, deemax, epsil, stmin   !track.media cuts 
      real  aa,zz,dens,radl,absl                  !material constants
      character*20 cmat_tg, cmatcell, cmatins     !material names
      character*4  cmoth                          !mother volume
      real  z0pos                                 !z-position in mother volume
      integer  icellflg                           !options: wall/superins. geom.
      integer  h_cellflag(3)
      data h_cellflag /4HCELL,4HOFF ,4HINS /
*
*  default values for target geometry  
*  ----------------------------------
*
*   shape='cuivre' i.e. tube with spherical end cap in downstream dir.
*                         [cf. drawing for photon beam target: Barranger 8/93]
*
      real tg_len(1:1)               !target length (depends on target type)
      real tg_diam                   !target diameter
      real tg_thickcell              !cell wall thickness  (material: Mylar)
*  position, thickness of superinsulation only my guess!!!
      real tg_thickins               !total thickness of superinsulation layers
                                     !          (material: aluminized Mylar)
                                     !packed as solid block (ca. 3.5% of total)
      real tg_distrad                !distance betw. cell wall and insulation
      real ins_dlen                  !length of layers in downstream dir.
      data tg_len                 /  5.0 /
      data tg_diam,tg_thickcell   / 0.86 , 0.0120 /
      data tg_distrad,tg_thickins / 2.54 , 0.021 / 
      data ins_dlen               / 2.54 /
*
*  gas/liquid distribution pipes & support structures 
*  --------------------------------------------------
*
*  pcon:           base ring (copper) at entry window position 
*  tube1 & tube2:  horizontal, connected to base ring and condenser
*
      real  tg_flpos(5)             ! relative position wrt. entry window
      real  tg_flthick(5)           ! thickness of copper ring
      real  tg_fldrad(5)            ! relative outer radius wrt. cell radius
      data tg_flpos   / -2.8, -2.1,-1.41, -1.4, 0.0 /
      data tg_flthick /  0.7,  1.2,  1.2,  0.6, 0.6 /
      data tg_fldrad  /  0.2,  0.2,  0.0,  0.0, 0.0 /

      real tg_tube1(3), tg_tube2(3)
      data tg_tube1  / 0.45, 0.55, 5.0 /
      data tg_tube2  / 0.35, 0.45, 1.0 /

*  offset vs. volume GAS2 to position the target in CLAS center:
      real  eltg_zoff
      data  eltg_zoff /-5.06/

c_end_var


c  executable code:
c  ----------------
      piby2   =acos(0.)
      twopiby3=acos(-1.)*2./3.

      if(tgpar_save(5).GE.0.0 .AND. tgpar_save(5).LE.NMATE) then
        imatcell = IFIX(tgpar_save(5))
      elseif(tgpar_save(5).EQ.-1) then
        imatcell = imate_kapton
      else
        imatcell = 0
      endif
      thickcell = tg_thickcell
      if(tg_ins_ff(1).LE.1.0) then
        imatins = imate_alumylar
      else
        imatins = IFIX(tg_ins_ff(1))
      endif
      if(tg_ins_ff(2).GE.0.0) then
        thickrad = tg_ins_ff(2)
      else
        thickrad = tg_thickins
      endif
      if(tg_ins_ff(3).GE.0.0) then
        thickax = tg_ins_ff(3)
      else
        thickax = tg_thickins
      endif
      if(tg_ins_ff(5).GT.0.0) then
        distrad = tg_ins_ff(5)
      else
        distrad = tg_distrad
      endif
      if(tg_ins_ff(6).GT.0.0) then
        distax = tg_ins_ff(6)
      else
        distax = ins_dlen
      endif

*  correct material id in case that no target walls/superinsulation chosen
      icellflg=IUCOMP(tg_cell_ff,h_cellflag,3)
      if(icellflg.GT.0) then
         if(icellflg.GT.1) imatcell=0
         if(icellflg.LT.2) imatins =0
      endif

********************************************
*  stop here if 'tg_type_ff' out of range  *
*
      if(tg_type_ff.GE.1 .AND. tg_type_ff.LE.1) then
        tglenby2  = tg_len(tg_type_ff)/2.0
        tgrad     = tg_diam/2.0
      elseif(tg_type_ff.EQ.-1) then                     !user defined geom.
        tglenby2  = tg_user_ff(1)/2
        tgrad     = tg_user_ff(2)/2
        if(tg_user_ff(3).GT.0) thickcell=tg_user_ff(3)
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
      if(imatins.GT.0) then
        call GFMATE(imatins,cmatins,aa,zz,dens,radl,absl,parm,np)
        if(aa.gt.0) then
          tmed_ins=get_tmed()
          CALL GSTMED( tmed_ins,cmatins,imatins, 
     &             1, init_ifield,init_fieldm,tmaxfd,
     &             stmax, deemax, epsil, stmin, 0 , 0 )
        else
          imatins=0
        endif
      endif
*
*  support structure

      if(imatcell.EQ.0) icellflg=2
      if(icellflg.LE.1) then 
        tmed_support=get_tmed()              !default material copper
        CALL GSTMED(tmed_support,'COPPER$', imate_copper,
     &           0, 0, 0.0, tmaxfd,stmax, deemax, epsil, stmin, 0 , 0 )
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
*  the whole structure is put into a vacuum tube

      pars(1)=0.0
      pars(2)=MAX(tgrad+distrad+thickrad,tg_tube1(2)+tg_tube2(3)/2.+tgrad)
      pars(3)=tglenby2+distax/2+ABS(tg_flpos(2)/2)+tg_tube1(3)+tg_tube2(3)
      call gsvolu('TG  ','TUBE',clas_med_vac,pars,3,ivol)
      xytg=sqrt(tg_pos_ff(1)**2+tg_pos_ff(2)**2)
      ztg =pars(3)-tglenby2-distax
      call gspos('TG  ',1,cmoth, tg_pos_ff(1),tg_pos_ff(2),z0pos-ztg,0,'ONLY')
*
*  (b) target cell: 
*  ----------------
*  cell shape "cuivre": tube + half shell (downstream) 
*
      parm(1)=  0.
      parm(2)=360.
      i2=7                        !planes for downstream half sphere
      dangle=piby2/(i2+1)         !angular steps wrt. z-axis 
*  end position of tube part
      tg_zpos2= tglenby2-tgrad*sin(i2*dangle)
      parm(3)=2+i2                !#planes:  2(for tube part)+
                                  !         i2(for downstream end cap)
      parm(4) =-tglenby2          !tube like part (upstream end)
      parm(5) =0. 
      parm(6) =tgrad
      parm(7) =tg_zpos2           !tube like part (downstream end)
      parm(8) =0.
      parm(9) =tgrad
      do i=1,i2                   ! downstream half sphere
         parm(i*3+7)=tg_zpos2+tgrad*sin(i*dangle)
         parm(i*3+8)=0.
         parm(i*3+9)=tgrad*cos(i*dangle)
      enddo
      np=parm(3)*3+3
      call gsvolu('TARG','PCON',tmed_tg,parm,np,ivol)
      call gspos('TARG',1,'TG  ',0.0,0.0,ztg,0,'ONLY')

*  save parameters
      tgpar_save(1)= 2*tglenby2
      tgpar_save(2)= tgrad
      tgpar_save(6)=-tglenby2
      tgpar_save(7)= tg_pos_ff(3) +tgpar_save(6)
      tgpar_save(8) =tg_pos_ff(3) +tgpar_save(6)
      tgpar_save(9) =tg_pos_ff(3) +tgpar_save(7)
      tgpar_save(10)=xytg +tgrad
*
*  (c) superinsulation
*  -------------------
*
      if(imatins.GT.0) then
        pars(1)=tgrad +distrad            !inner radius: 1" from target wall
        pars(2)=tgrad +distrad +thickrad
        pars(3)=tglenby2 +distax*1.5
        call gsvolu('TGSI','TUBE',tmed_ins,pars,3,ivol)
        call gspos('TGSI',1,'TG  ', 0.,0.,ztg-distax/2,0,'ONLY')
        tg_ins_ff(2) =thickrad
        tg_ins_ff(4) =distrad
        tgpar_save(8) =tg_pos_ff(3) -pars(3)
        tgpar_save(9) =tg_pos_ff(3) +pars(3)
        tgpar_save(10)=xytg +pars(2)

        pars(1)=0.0                       !axial superins.
        pars(2)=tgrad +distrad
        pars(3)=thickax/2.
        if(thickax.GT.0) then
          call gsvolu('TGSA','TUBE',tmed_ins,pars,3,ivol)
          call gspos('TGSA',1,'TG  ', 0.,0.,tglenby2+distax-pars(3)+ztg,0,'ONLY')
          call gspos('TGSA',2,'TG  ', 0.,0.,-(tglenby2+2*distax-pars(3))+ztg,0,'MANY')
        endif
      endif
*
*
*  (d) target cell walls
*  ---------------------
*
      if(imatcell.EQ.0) RETURN

*  planes for target cell walls:
      do i=i2+2,1,-1
         parm(i*3+7)=parm(i*3+1)
         parm(i*3+8)=parm(i*3+3)
         parm(i*3+9)=parm(i*3+3) +thickcell
      enddo
      parm(3)=parm(3)+4
      np=parm(3)*3+3
      parm(4)=-tglenby2-thickcell
      parm(5)=0.
      parm(6)=parm(11)
      parm(7)=-tglenby2-0.0001
      parm(8)=0.
      parm(9)=parm(12)
      parm(np-5)=tglenby2+0.0001
      parm(np-4)=0.
      parm(np-3)=parm(np-6)
      parm(np-2)=tglenby2+thickcell
      parm(np-1)=0.
      parm(np)  =parm(np-7)
      call gsvolu('TGCL','PCON',tmed_cell,parm,np,ivol)
      call gspos('TGCL',1,'TG  ',0.,0.,ztg,0,'ONLY')
      tgpar_save(3)=thickcell
      tgpar_save(4)=thickcell
*
*  (e) support structure 
*  ---------------------
*
      if(icellflg.GT.1) return
*
*    base ring (copper)

      parm(1) =0.
      parm(2) =360.
      parm(3) =5.
      do i=1,5
         parm(1+i*3) =tg_flpos(i)-thickcell
         parm(2+i*3) =tgrad + tg_fldrad(i)
         parm(3+i*3) =parm(3+i*3) + tg_flthick(i)
      enddo
      call gsvolu('TGFL','PCON',tmed_support,parm,18,ivol)
      call gspos('TGFL',1,'TG  ', 0.,0.,-tglenby2+ztg,0,'MANY')
*     
*  gas distribution tubes (in vacuum box)
*       tube1 angled by 45deg. wrt. z-axis

      pars(1)=tg_tube1(2)
      pars(2)=tg_tube2(3)/2+tg_tube1(2)
      pars(3)=tg_tube1(3)+tg_tube2(3)
      call gsvolu('TGST','BOX ',clas_med_vac,pars,3,ivol)
      call gsvolu('TGS1','TUBE',tmed_support,tg_tube1,3,ivol)
      call gsvolu('TGS2','TUBE',tmed_support,tg_tube2,3,ivol)
      irot1=get_rotm()
      call gsrotm(irot1,90.,0.,230.,90.,140.,90.)
      call gspos('TGS1',1,'TGST', 0.,pars(2)-tg_tube1(2),0.1-tg_tube2(3),0,'ONLY')
      call gspos('TGS2',1,'TGST', 0.,0.,tg_tube1(3),irot1,'ONLY')

*  3 copies of gas pipe volumes (named 'TGST') are positioned in mother volume
*  (rotated by 120 deg.)
      irot2=get_rotm()
      call gsrotm(irot2,-90.,-120.,-90.,-210.,0.,0.)
      irot3=get_rotm()
      call gsrotm(irot3,90.,120.,90.,210.,0.,0.)
      xypos=pars(2)+tgrad+tg_fldrad(1)
      xpos=xypos*sin(twopiby3)
      ypos=xypos*cos(twopiby3)
      zpos=-tglenby2+tg_flpos(2)-pars(3)+ztg
      call gspos('TGST',1,'TG  ',  0.,  xypos, zpos,0,'MANY')
      call gspos('TGST',2,'TG  ',  xpos, ypos, zpos,irot2,'MANY')
      call gspos('TGST',3,'TG  ', -xpos, ypos, zpos,irot3,'MANY')

      tgpar_save(8) = tg_pos_ff(3) -zpos -pars(3) 
      tgpar_save(10)= MAX(tgpar_save(10),(xytg+xypos+pars(2))) 

      end

c-----------------------------------------------------------------------------
