
       subroutine geom_phtg5(itg_mate)
  
c_begin_doc

c  Documentation for subroutine geom_phtg5
c  
c  Purpose:
c  --------
c     ******************************************************************
c     *                                                                *
c     *      Defines the geometry of the CLAS - SACLAY Cryotarget      *
c     *          for Jlab photon beam target cells (large radius)      *
c     *                                                                *
c     ******************************************************************
c
c  for tg_type_ff=10,11:
c    #     cell wall      end cap        length   diameter   names
c    10    Kapt/Alu,241mu Kapt/Alu,241mu 240.0mm  40.0mm   'g10a'
c  support structure type=3:
c  shape=cone + upstream extension + 2 end caps (shell segments of +/-30deg.)
c  superinsulation mat.#  167.0   for tg_type_ff=10
c
c      [cf. drawings 66850-C-03985/03986/03987 (g10a)
c
c  Major revisions:
c  ----------------
c
c_end_doc


      implicit none
      character*(*) cfile,crevis,cstate,cdate,cautho,crname,crauth
      character*132 crcsid,crmess
      parameter (cfile=  '$RCSfile: geom_phtg5.F,v $')
      parameter (crevis= '$Revision: 1.5 $')
      parameter (cstate= '$State: Exp $')
      parameter (cdate=  '$Date: 2004/09/17 03:19:52 $')
      parameter (cautho= '$Author: baltzell $')
      parameter (crname= 'GEOM_PHTG5')
      parameter (crauth= 'baltzell')
      data crcsid/'
     1$Id: geom_phtg5.F,v 1.5 2004/09/17 03:19:52 baltzell Exp $
     1 '/
      save


c  include files:
c  --------------
#include "gcbank.inc"
#include "gcnum.inc"
#include "gsimpar.inc"
#include "gsimcontrol.inc"
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

      integer  iucomp, lnblnk, get_rotm, get_tmed

*  local variables:
*  ----------------

      real  twopiby3, PIby2
      integer i, istrlen                !loop parameter, string length
      integer np, ind, im               !#parameters in PCON and plane index
      integer ivol                      !volume#
      integer irot1, irot2, irot3       !rotation_matrix#
      integer i1,i2                     !#planes in PolyCONe for target cell
      real  tg_zpos1, tg_zpos2          !pos. of tube-like part of target cell
      real  dangle1, dangle2,dangle3    !step size for cones of target cell
      real  parm(50), pars(5), parc(50) !arrays for volume definitions
      real  xpos,ypos,zpos,xypos        !position of volumes
      real  xytg, ztg                   !position of target center
      real  tglenby2                    !half length of target cell incl. end caps
      real  tglen_out                   !length of outer tube
      real  tgtan                       !tangent of angle of cell wall wrt z-axis
      real  tgrad,tgdiam               !target radius,diameter w/o walls 
      real  z0_cap                      !center of circle segment for end caps
      real  cellrad                     !radius of target cell+wall
      real  thickcell                   !thickness of cell walls
      real  thickatt                    !thickness of superinsulation: attached
      data  thickcell,thickatt  / -1, -1 /    !default
      real  distax                      !dist. of axial superins. to end cap
      integer tmed_tg                   !track.medium for target material
      integer imatcell, tmed_cell       !mate# and track.medium for target cell
      integer tmed_cellins              !medium for combined cell wall+superins.
      integer tmed_support              !track.medium for flange and distr.tubes
      integer imatatt, tmed_att         !for attached superinsulation layers
      data  imatcell,imatatt    / -1, -1 /    !default
      integer  icellflg, h_cellflag(3)  !options: wall/superins. geom.
      data h_cellflag /4HCELL,4HOFF ,4HINS /
      real    tmaxfd, stmax, deemax, epsil, stmin   !track.media cuts 
      real    aa,zz,dens,radl,absl                  !material constants
      character*20  cmat_tg, cmatcell, cmatins      !material names
*
*  default values for target geometry  
*  ----------------------------------
*
*   shape: tube with 2 shell segments as end caps
*
      real tg_len(10:10)             !target length (depends on target type)
      real tg_diam(10:10)            !target diameter of downstream and upstream endcaps
      real tg_tan(10:10)             !tangent of angle of cell wall wrt z-axis
      real tg_thickcell(10:10)       !cell wall thickness
      real tg_thickatt(10:10)        !thick.of attached superinsulation
      real tg_cap_thick,tg_cap_rad   !thickness & radius of beam windows
      real tg_mother_rad1,tg_mother_rad2   !radius of target's mother volume (vacuum)
      real tg_mother_zpos1,tg_mother_zpos2 !z-pos. of target's mother volume (vacuum)
      data tg_len        / 24.0 /
      data tg_diam       / 4.0  /
      data tg_tan        / 0.0297 / !theta=1.7degrees
      data tg_thickcell  / 0.0127 /
      data tg_thickatt   / 0.011 /
      data tg_cap_thick / 0.0015 /
      data tg_cap_rad   / 0.5    /
      data tg_mother_rad1,tg_mother_rad2    /  8.5 ,  4.0 /
      data tg_mother_zpos1,tg_mother_zpos2  /-15.0 ,  4.4 /  ! =dist. from end caps

*  gas/liquid distribution pipes & support structures 
*  --------------------------------------------------
*
*  pcon1: and outer base tube (cone&tube)    (#66850-A-02981)
*  pcon2: inner base tube     (cone&tube)    (#66850-A-03986)
*  tube1:  horizontal, to backward condenser
*  tube2:  angled by 45 deg., attached to base tube

      real tg_pcon1_pos(3)
      real tg_pcon1_rad(3)       ! inner radius above cell radius
      real tg_pcon1_thick(3)     ! thickness of collar and outer base tube
      real tg_pcon2_pos(4)
      real tg_pcon2_rad(4)
      real tg_pcon2_thick        ! thickness of inner base tube
      data tg_pcon1_rad   / 0.72, 0.75, 0.75 /
      data tg_pcon1_thick / 0.25,  1.3, 0.06 /
      data tg_pcon1_pos   /-5.17, -3.9, -2.63 /
      data tg_pcon2_pos   /-5.17, -1.96, 0.0,  0.5 /
      data tg_pcon2_rad   / 0.35,  0.35, 0.05, 0.05 /
      data tg_pcon2_thick / 0.10 /
      real tg_tube1(3), tg_tube2(3)
      data tg_tube1  / 0.4, 0.5, 6.0 /
      data tg_tube2  / 0.4, 0.5, 1.0 /

c_end_var


c  executable code:
c  ----------------
      piby2   =acos(0.)
      twopiby3=acos(-1.)*2./3.

      if(tgpar_save(5).GE.0.0.AND.tgpar_save(5).LE.NMATE) 
     &          imatcell = IFIX(tgpar_save(5))
      if(tg_ins_ff(3).GE.0.0) thickatt = tg_ins_ff(4)

*  correct material id in case that no target walls/superinsulation chosen
      icellflg=IUCOMP(tg_cell_ff,h_cellflag,3)
      if(icellflg.GT.0) then
        if(icellflg.GT.1) imatcell=0
        if(icellflg.LT.2) then
          imatatt =0
        endif
      endif

********************************************
*  stop here if 'tg_type_ff' out of range  *
*
      if(tg_type_ff.EQ.10) then

        tglenby2  = tg_len(tg_type_ff)/2.0
        tgrad     = tg_diam(tg_type_ff)/2.0
        tgtan     = tg_tan(tg_type_ff)
        thickcell = tg_thickcell(tg_type_ff)
        if(imatcell.EQ.-1)  imatcell = imate_kapton
        if(thickatt.LT.0.0) thickatt = tg_thickatt(tg_type_ff)
        if(imatatt .EQ.-1)  imatatt  = imate_cerexmyl1

      elseif(tg_type_ff.EQ.-1) then              ! user defined geom.
        tglenby2  = tg_user_ff(1)/2
        tgrad     = tg_user_ff(2)/2
        thickcell = tg_thickcell(10)             ! default values for g10 target
        if(tg_user_ff(3).GT.0) thickcell = tg_user_ff(3)
        if(thickatt.LT.0.0)    thickatt  = tg_thickatt(10)
        if(imatcell.EQ.-1)     imatcell  = imate_kapton
        if(imatatt .EQ.-1)     imatatt   = imate_cerexmyl1

      else
        call recmes(crname,'W',
     &     'Target type not defined -> no target geometry')
        return
      endif

*
*  stop if beam pipe volume not defined
      call GLOOK('BEAM', IQ(jvolum+1), NVOLUM, ivol)
      if(ivol.LE.0) then
        call recmes(crname,'W',
     &     'Beam pipe volume not defined -> no target geometry')
        return
      endif
      if (tg_type_ff.EQ.10 .OR. tg_type_ff.EQ.-1) then
        call recmes(crname,'I',
     &     'Define geometry for photon target ("Jlab cell g10a") ...')
      endif
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
        call recmes(CRNAME,'W',
     &     'Target material not defined -> NO TARGET!')
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
*  target walls:
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
      if(imatatt.GT.0) then
        call GFMATE(imatatt,cmatins,aa,zz,dens,radl,absl,parm,np)
        if(aa.gt.0) then
          tmed_att=get_tmed()
          CALL GSTMED( tmed_att,cmatins,imatatt, 
     &             1, init_ifield,init_fieldm,tmaxfd,
     &             stmax, deemax, epsil, stmin, 0 , 0 )
          tmed_cellins=tmed_att
        else
          imatatt=0
        endif
      endif
      if(imatatt.EQ.0) then
        tmed_cellins=tmed_cell
        thickatt=0.0
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
        write(crmess,'(2(A,F7.3),A,F6.4,3A)') 'length:',
     &        tglenby2*2,'cm;  radius:',tgrad,'cm;  cell thickness:',
     &        thickcell,' (',cmatcell(1:istrlen),')'
      else
        write(crmess,'(2(A,F7.3),A)') 'length:',tglenby2*2,
     &        'cm;  radius:',tgrad,'cm'
      endif
      call recmes(crname,'I',crmess)
*  
************************
*  geometry definitions
************************
*
*  (a) mother volume (vacuum)
*  --------------------------
*  target for photon beam is positioned in 'BEAM' (beam pipe)
*  put the whole structure into a vacuum cone

      pars(1)=(abs(tg_mother_zpos1)+tg_mother_zpos2)/2 +tglenby2
      pars(2)=0.0
      pars(3)=tg_mother_rad1
      pars(4)=0.0
      pars(5)=tg_mother_rad2
      call gsvolu('TG  ','CONE',clas_med_vac,pars,5,ivol)
      xytg=sqrt(tg_pos_ff(1)**2+tg_pos_ff(2)**2)
      ztg =(tg_mother_zpos2+tg_mother_zpos1)/2
      call gspos('TG  ',1,'BEAM', tg_pos_ff(1),tg_pos_ff(2),tg_pos_ff(3)+ztg,0,'ONLY')
*
*  (b) target cell: 
*  ----------------
*  cell shape "cone + 2 shell segments + upstream extension
* 
      tgdiam = 2*tgrad
      z0_cap = tglenby2-tgdiam    !center for circle segment for end caps
      parm(1)=  0.
      parm(2)=360.
      i1=6                        !planes for upstream half sphere (seperate volume)
      i2=6                        !planes for downstream half sphere
      parm(3)=6+i2                !#planes: i2+6(4 around base, 2 solid tube)
      np=parm(3)*3+3
*  angular steps wrt. z-axis
      dangle1=piby2/3/i1
      dangle2=piby2/3/i2
*  start/end position of tube part (at end caps)
      tg_zpos1=-z0_cap - tgdiam*cos(i1*dangle1)
      tg_zpos2= z0_cap + tgdiam*cos(i2*dangle2)
*  upstream target extension up to distribution tubes (4sections; inner radius>0)
*        planes defined w.r.t. to inner(outer) base tube(ring) positions
      ind=4
      parm(ind)    = tg_zpos1 - abs(tg_pcon1_pos(3)-tg_pcon2_pos(3))
      parm(ind+1)  = tgrad + tg_pcon2_rad(1) + tg_pcon2_thick
      parm(ind+2)  = tgrad + tg_pcon1_rad(3)

      parm(ind+3)  = tg_zpos1-abs(tg_pcon2_pos(2)-tg_pcon2_pos(3))
      parm(ind+4)  = tgrad + tg_pcon2_rad(2) + tg_pcon2_thick
      parm(ind+5)  = parm(ind+2) - abs((parm(ind)-parm(ind+3)))*tgtan

      parm(ind+6)  = tg_zpos1+abs(tg_pcon2_pos(3)-tg_pcon2_pos(3))
      parm(ind+7)  = tgrad + tg_pcon2_rad(3) + tg_pcon2_thick
      parm(ind+8)  = parm(ind+2) - abs((parm(ind)-parm(ind+6)))*tgtan

      parm(ind+9)  = tg_zpos1+abs(tg_pcon2_pos(4)-tg_pcon2_pos(3))
      parm(ind+10) = tgrad + tg_pcon2_rad(4) + tg_pcon2_thick
      parm(ind+11) = parm(ind+2) - abs((parm(ind)-parm(ind+9)))*tgtan

*  conical part of target (inner radius == 0)
      ind=16
      parm(ind)  =tg_zpos1+tg_pcon2_pos(4) ! (upstream end at z of cap)
      parm(ind+1)=0.
      parm(ind+2)=parm(ind-1)
      parm(ind+3)=tg_zpos2                 ! (downstream end at cap)
      parm(ind+4)=0.
      parm(ind+5)=tgrad
*  downstream end cap
      do i=1,i2
        parm(i*3+ind+3)=z0_cap +tgdiam*cos((i2-i)*dangle2)
        parm(i*3+ind+4)=0.
        parm(i*3+ind+5)=tgdiam*sin((i2-i)*dangle2)
      enddo
      call gsvolu('TARG','PCON',tmed_tg,parm,np,ivol)
      call gspos('TARG',1,'TG  ',0.0,0.0,-ztg,0,'ONLY')
*
*  upstream end cap is a seperate volume:
      i1=6                        !planes for upstream half sphere
      parc(1)=  0.
      parc(2)=360.
      parc(3)=2+i1                !#planes: i1+2(for short tube part)
      np=parc(3)*3+3
      dangle1=piby2/3/i1
      do i=0,i1-1
        parc(i*3+4)=-z0_cap -tgdiam*cos(i*dangle1)
        parc(i*3+5)=0.
        parc(i*3+6)=tgdiam*sin(i*dangle1)
      enddo
      ind=3+i1*3+1  !index of 1.entry of short tube part (header=3 entries & 3 entries/plane)
      parc(ind)=tg_zpos1
      parc(ind+1)=0.
      parc(ind+2)=tgrad
      parc(ind+3)=tg_zpos1+tg_pcon2_pos(4)
      parc(ind+4)=0.
      parc(ind+5)=tgrad
      np=6*3+2*3+3
      parc(3)=2+i1
      call gsvolu('TGCP','PCON',tmed_tg,parc,np,ivol)
      call gspos('TGCP',1,'TG  ',0.0,0.0,-ztg,0,'ONLY')

*  save parameters
      tgpar_save(1) =tglenby2*2
      tgpar_save(2) =tgrad
      tgpar_save(6) =-tglenby2
      tgpar_save(7) =tg_pos_ff(3) +tgpar_save(6)
      tgpar_save(8) =tg_pos_ff(3) +tgpar_save(6)
      tgpar_save(9) =tg_pos_ff(3) +tgpar_save(6) +tgpar_save(1)
      tgpar_save(10)=xytg +tgrad
*
*  (c) superinsulation
*  -------------------
*
      if(imatatt.GT.0 .AND. thickatt.GT.0) then
        cellrad = tgdiam+thickcell
        parc(1)=   0.
        parc(2)= 360.
        parc(3)=5
        np=parc(3)*3+3
        ind=4                           ! upstream end
        parc(4)=parm(ind)
        parc(5)=parm(ind+2) +thickcell
        parc(6)=parm(ind+2) +thickcell +thickatt
        ind=13                          ! intermediate point
        parc(7)=parm(ind)
        parc(8)=parm(ind+2) +thickcell
        parc(9)=parm(ind+2) +thickcell +thickatt
        ind=16                          ! downstream end
        parc(10)=tglenby2 +thickcell +tg_cap_thick
        parc(11)=parm(ind+5) +thickcell
        parc(12)=parm(ind+5) +thickcell +thickatt
        parc(13)=parc(10)
        parc(14)=0.
        parc(15)=parc(12)
        parc(16)=parc(10) +thickatt
        parc(17)=0.
        parc(18)=parc(12)
        call gsvolu('TGSU','PCON',tmed_cellins,parc,np,ivol)
        call gspos('TGSU',1,'TG  ',0.0,0.0,-ztg,0,'ONLY')
      endif

*
*
*  (d) target cell walls
*  ---------------------
*
      if(imatcell.EQ.0) RETURN
*
*  planes for target cell walls:
      cellrad = tgdiam+thickcell
      parm(1)=   0.
      parm(2)= 360.
      parm(3)=3+i2
      np=parm(3)*3+3
      ind=4                           ! upstream end
      parm(4)=parm(ind)
      parm(5)=parm(ind+2)
      parm(6)=parm(ind+2) +thickcell
      ind=13                          ! intermediate point
      parm(7)=parm(ind)
      parm(8)=parm(ind+2)
      parm(9)=parm(ind+2) +thickcell
      ind=16                          ! downstream end
      parm(10)=parm(ind+3)
      parm(11)=parm(ind+5)
      parm(12)=parm(ind+5) +thickcell
      do i=1,i2                       ! downstream end cap
        parm(i*3+10)=parm(i*3+ind+3)
        parm(i*3+11)=parm(i*3+ind+5)
        dangle3=i2/(i2-1)*acos(tgdiam/cellrad*cos((i2-i)*dangle2))
        parm(i*3+12)=cellrad*sin(dangle3)
      enddo

      call gsvolu('TGCL','PCON',tmed_cellins,parm,np,ivol)
      call gspos('TGCL',1,'TG  ',0.0,0.0,-ztg,0,'ONLY')
      tgpar_save(3)=thickcell
      tgpar_save(4)=thickcell
      tgpar_save(5)=imatcell

*  upstream end cap is a seperate volume:  
      cellrad = tgdiam+thickcell        ! no superinsulation
      parm(3)=i1+3
      np=parm(3)*3+3
      do i=0,i1-1
        parm(i*3+10)=-z0_cap -tgdiam*cos(i*dangle1)
        parm(i*3+11)=tgdiam*sin(i*dangle1)
        dangle3=i1/(i1-1)*acos(tgdiam/cellrad*cos(i*dangle1))
        parm(i*3+12)=cellrad*sin(dangle3)
      enddo
      parm(11)=parm(14)*0.8              !adjust cell radius at z=-tglenby2
      parm(7)=-tglenby2-0.0001
      parm(8)=0.
      parm(9)=parm(12)
      parm(4)=-tglenby2-thickcell
      parm(5)=0.
      parm(6)=parm(12)*0.8
      parm(np-2)= tg_zpos1 -0.0001
      parm(np-1)= tgrad
      parm(np)  = tgrad +thickcell
      call gsvolu('TGCI','PCON',tmed_cell,parm,np,ivol)
      call gspos('TGCI',1,'TG  ',0.0,0.0,-ztg,0,'ONLY')
      tgpar_save(6)=tg_pos_ff(3) -parm(4)

* 2 aluminum beam windows, one on each end cap:
      parc(1)=0.0                                           !inner radius
      parc(2)=tg_cap_rad                                    !outer radius
      parc(3)=tg_cap_thick/2                                !halfthickness
      parc(4)=tglenby2+parc(3)+thickcell                    !z-position
      call gsvolu('TGCC','TUBE',tmed_support,parc,3,ivol)
      call gspos('TGCC',1,'TG  ',0.0,0.0,-ztg-parc(4),0,'ONLY')
      call gspos('TGCC',2,'TG  ',0.0,0.0,-ztg+parc(4),0,'ONLY')
*
*  (e) support structure 
*  ---------------------
*
      if(icellflg.GT.1) return
*
*  cell wall collar and outer base ring
      parm(1) =0.
      parm(2) =360.
      parm(3) =3.
      np=parm(3)*3+3
      do i=1,3
         parm(1+i*3) =tg_pcon1_pos(i)
         parm(2+i*3) =tg_pcon1_rad(i) +tgrad
         parm(3+i*3) =parm(2+i*3) +tg_pcon1_thick(i)
      enddo
      call gsvolu('TGF1','PCON',tmed_support,parm,np,ivol)
      call gspos('TGF1',1,'TG  ', 0.,0.,tg_zpos1-ztg,0,'ONLY')

*  outer base ring
      parm(3) =4.
      np=parm(3)*3+3
      do i=1,4
         parm(1+i*3) =tg_pcon2_pos(i)
         parm(2+i*3) =tg_pcon2_rad(i) +tgrad
         parm(3+i*3) =parm(2+i*3) +tg_pcon2_thick
      enddo
      call gsvolu('TGF2','PCON',tmed_support,parm,np,ivol)
      call gspos('TGF2',1,'TG  ', 0.,0.,tg_zpos1-ztg,0,'ONLY')

*  distribution tubes in vacuum box (tube1 angled by 45deg. wrt. z-axis)
*   3 copies of gas pipe volumes (named 'TGDT') are rotated by 120 deg.

      irot1=get_rotm()
      call gsrotm(irot1,90.,0.,225.,90.,135.,90.)
      pars(1)=MAX(tg_tube1(2),tg_tube2(2))
      pars(2)=tg_tube2(3)/2+tg_tube1(2)
      pars(3)=tg_tube1(3) +tg_tube2(3)/1.5
*  if tube too long to fit into mother volume: cut the end!
      zpos=tg_pcon1_pos(2) +tg_zpos1 -tg_mother_zpos1 +tglenby2
      if(2*pars(3).GT.zpos) then
        pars(3)=zpos/2
        tg_tube1(3)=pars(3) -tg_tube2(3)/1.5
      endif
      call gsvolu('TGDT','BOX ',clas_med_vac,pars,3,ivol)
      call gsvolu('TGD1','TUBE',tmed_support,tg_tube1,3,ivol)
      call gsvolu('TGD2','TUBE',tmed_support,tg_tube2,3,ivol)
      call gspos('TGD2',1,'TGDT', 0.,0.,tg_tube1(3)-tg_tube2(3)*0.65,irot1,'ONLY')
      call gspos('TGD1',1,'TGDT', 0.,pars(2)-tg_tube1(2),-tg_tube2(3)/1.5,0,'MANY')
*
      irot2=get_rotm()
      call gsrotm(irot2,-90.,-120.,-90.,-210.,0.,0.)
      irot3=get_rotm()
      call gsrotm(irot3,90.,120.,90.,210.,0.,0.)
      xypos=pars(2)+tgrad+tg_pcon1_rad(2)
      xpos=xypos*sin(twopiby3)
      ypos=xypos*cos(twopiby3)
      zpos=tg_zpos1+tg_pcon1_pos(2) -pars(3) -ztg
      call gspos('TGDT',1,'TG  ',  0.,  xypos, zpos,    0,'MANY')
      call gspos('TGDT',2,'TG  ',  xpos, ypos, zpos,irot2,'MANY')
      call gspos('TGDT',3,'TG  ', -xpos, ypos, zpos,irot3,'MANY')
      tgpar_save(8)=zpos -pars(3)

      end
