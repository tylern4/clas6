
       subroutine geom_phtg8(itg_mate)
  
c_begin_doc

c  Documentation for subroutine geom_phtg8
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
c    11    Kapt/Alu,241mu Kapt/Alu,241mu 400.0mm  40.0mm   'g11a'
c  support structure type=3:
c  shape=cone + upstream extension + 2 end caps
c  superinsulation mat.#  167.0 cerex-mylar-aluminum
c  scattering chamber.#   173.0 low-density foam (Rohacell 110WF - 107 mg/cm^2)
c          
c      [ cf. drawings 66850-C-04070,04071,04072 ]
c
c  The user-defined target's z-position is applied to the beam volume in
c  geom_tg.F such that the new beam pipe attaches to the target properly.
c
c  Major revisions:
c  ----------------
c
c Nov 9,2004 - Add ffread flag SCHDLEN to add length to scattering chamber
c               default value is 0, which results in g11 specifications.
c
c July 2009 (fk) define BEAM and BPIP (upstream vacuum pipe) in this routine instead
c               of geom_tg.F to prevent overlapping volumes!
c_end_doc


      implicit none
      character*(*) cfile,crevis,cstate,cdate,cautho,crname,crauth
      character*132 crcsid,crmess
      parameter (cfile=  '$RCSfile: geom_phtg8.F,v $')
      parameter (crevis= '$Revision: 1.7 $')
      parameter (cstate= '$State: Exp $')
      parameter (cdate=  '$Date: 2009/08/14 22:30:46 $')
      parameter (cautho= '$Author: fklein $')
      parameter (crname= 'GEOM_PHTG8')
      parameter (crauth= 'baltzell')
      data crcsid/'
     1$Id: geom_phtg8.F,v 1.7 2009/08/14 22:30:46 fklein Exp $
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
#include "ffpar_st.inc"
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
      real  para(12)
      real  xpos,ypos,zpos,xypos        !position of volumes
      real  xytg, ztg, ztg2             !position of target center, mother center
      real  tglenby2                    !half length of target cell incl. end caps
      real  tglen_out                   !length of outer tube
      real  tgtan                       !tangent of angle of cell wall wrt z-axis
      real  tgrad,tgdiam                !target radius,diameter w/o walls 
      real  z0_cap                      !center of circle segment for end caps
      real  cellrad                     !radius of target cell+wall
      real  thickcell                   !thickness of cell walls
      real  thickrad,thickax,thickatt   !thickness of superinsulation: radial, axial, attached
      data  thickcell,thickrad,thickax,thickatt  / -1, -1, -1, -1 /    !default
      real  radrad                      !distance from cell wall to outer radius of scattering chamber
      data  radrad /-1/                 !default
      real  distax                      !distance from cell wall to axial part of scattering chamber
      data  distax /-1/                 !default
      integer tmed_tg                   !track.medium for target material
      integer imatcell, tmed_cell       !mate# and track.medium for target cell
      integer tmed_cellins              !medium for combined cell wall+superins.
      integer tmed_support              !track.medium for flange and distr.tubes
      integer imatrad, tmed_rad         !for radial superinsulation layers 
      integer imatax, tmed_ax           !for axial superinsulation layers 
      integer imatatt, tmed_att         !for attached superinsulation layers
      data  imatcell,imatrad,imatax,imatatt    /-1, -1, -1, -1/       !default
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
      real tg_len(11:11)          !target length (depends on target type)
      real tg_diam(11:11)         !target diameter of downstream and upstream endcaps
      real tg_tan(11:11)          !tangent of angle of cell wall wrt z-axis
      real tg_thickcell(11:11)    !cell wall thickness
      real tg_thickrad(11:11)     !total thickness of scattering chamber
      real tg_radrad(11:11)       !distance from cell wall to outer radius of scattering chamber
      real tg_thickax(11:11)      !thick.of downstream scattering chamber
      real tg_distax(11:11)       !distance from attached s.i. to scattering chamber
      real tg_thickatt(11:11)     !thick.of attached superinsulation
      real tg_cap_thick,tg_cap_rad  !thickness & radius of target cell beam windows
      real tg_scatcap_thick,tg_scatcap_rad !thickness & radius of scattering chamber beam window
      real tg_mother_rad1,tg_mother_rad2   !radius of target's mother volume (vacuum)
      real tg_mother_zpos1,tg_mother_zpos2 !z-dist from endcaps to mother volume ends
      data tg_len        / 40.0   /
      data tg_diam       / 4.0    /
      data tg_tan        / 0.015  /
      data tg_thickcell  / 0.0127 /
      data tg_thickrad   / 0.978  /
      data tg_radrad     / 2.724  /
      data tg_thickax    / 1.369  /
      data tg_distax     / 1.916  /
      data tg_thickatt   / 0.011  /
      data tg_cap_thick  / 0.0015 /
      data tg_cap_rad    / 0.5    /
      data tg_scatcap_thick / 0.007 /
      data tg_scatcap_rad   / 2.0   /
      data tg_mother_rad1,tg_mother_rad2     /  4.73, 4.73 /
c      data tg_mother_zpos1  / -15.0 / !upstream endcap z to end of mother 
c (fk 7/09 changed to prevent overlapping volumes: note some cannot be created with MANY)
      data tg_mother_zpos1  / -13.1 / !upstream endcap z to end of mother

*  new upstream beam pipe for new start counter and g11 target (N.Baltzell, Nov.04)
*  1.plane upstream outside of CLAS
*  2.plane at upstream end of cone
*  3.plane at downstream end of cone = upstream end of scattering chamber
* fk 7/09: move beam pipe upstream to prevent overlapping volumes

      real bpip_newst_thick
      real bpip_newst_pln(3), bpip_newst_rad(3)
c      data bpip_newst_pln  / -300., -39.2, -27.88 /
      data bpip_newst_pln  / -300., -39.2, -28.2 /
      data bpip_newst_rad  /  9.6,   9.6,   4.73 /
      data bpip_newst_thick /0.2/
*
*  upstream support structures 
*  ---------------------------
*
*  pcon1: and outer base tube (cone&tube)    (#66850-A-02981,sht.2)
*  pcon2: inner base tube     (cone&tube)    (g11#66850-A-04072)

      real tg_pcon1_pos(3)
      real tg_pcon1_rad(3)       ! inner radius above cell radius
      real tg_pcon1_thick(3)     ! thickness of collar and outer base tube
      real tg_pcon2_pos(4)
      real tg_pcon2_rad(4)
      real tg_pcon2_thick        ! thickness of inner base tube
      data tg_pcon1_rad   / 0.72, 0.75, 0.75 /
      data tg_pcon1_thick / 0.25,  1.3, 0.06 /
      data tg_pcon1_pos   / -13.5,  -12.23, -10.96 /
      data tg_pcon2_pos   / -13.17, -10.37, 0.0,  0.5  /
      data tg_pcon2_rad   /  0.35,  0.35, 0.05, 0.05 /
      data tg_pcon2_thick / 0.10 /

c_end_var

c  executable code:
c  ----------------
      piby2   =acos(0.)
      twopiby3=acos(-1.)*2./3.

      if(tgpar_save(5).GE.0.0.AND.tgpar_save(5).LE.NMATE) 
     &          imatcell = IFIX(tgpar_save(5))
      if(tg_ins_ff(1).GT.0.0) then
        imatrad = IFIX(tg_ins_ff(1))
      endif
      if(tg_ins_ff(2).GE.0.0) thickrad = tg_ins_ff(2)
      if(tg_ins_ff(3).GE.0.0) thickax  = tg_ins_ff(3)
      if(tg_ins_ff(3).GE.0.0) thickatt = tg_ins_ff(4)

*  correct material id in case that no target walls/superinsulation chosen
      icellflg=IUCOMP(tg_cell_ff,h_cellflag,3)
      if(icellflg.GT.0) then
        if(icellflg.GT.1) imatcell=0
        if(icellflg.LT.2) then
          imatrad =0
          imatatt =0
        endif
      endif

*  axial heat shield: default=same material as radial heat shield
      imatax=imatrad

********************************************
*  stop here if 'tg_type_ff' out of range  *
*
      if(tg_type_ff.EQ.11) then

        tglenby2  = tg_len(tg_type_ff)/2.0
        tgrad     = tg_diam(tg_type_ff)/2.0
        tgtan     = tg_tan(tg_type_ff)
        thickcell = tg_thickcell(tg_type_ff)
        if(imatcell.EQ.-1)  imatcell = imate_kapton
        if(thickrad.LT.0.0) thickrad = tg_thickrad(tg_type_ff)
        if(radrad.LT.0.0)   radrad   = tg_radrad(tg_type_ff)
        if(thickax .LT.0.0) thickax  = tg_thickax(tg_type_ff)
        if(distax.LT.0.0)   distax   = tg_distax(tg_type_ff)
        if(thickatt.LT.0.0) thickatt = tg_thickatt(tg_type_ff)
        if(imatrad .EQ.-1)  imatrad  = imate_lowdfoam
        if(imatax  .EQ.-1)  imatax   = imate_lowdfoam
        if(imatatt .EQ.-1)  imatatt  = imate_cerexmyl1

      elseif(tg_type_ff.EQ.-1) then            ! user defined geom.
        tglenby2  = tg_user_ff(1)/2
        tgrad     = tg_user_ff(2)/2
        thickcell = tg_thickcell(11)           ! default values for g11 target
        if(tg_user_ff(3).GT.0) thickcell = tg_user_ff(3)
        if(thickrad.LT.0.0)    thickrad  = tg_thickrad(11)
        if(radrad.LT.0.0)      radrad    = tg_radrad(11)
        if(thickax .LT.0.0)    thickax   = tg_thickax(11)
        if(thickatt.LT.0.0)    thickatt  = tg_thickatt(11)
        if(imatcell.EQ.-1)     imatcell  = imate_kapton
        if(imatrad .EQ.-1)     imatrad   = imate_lowdfoam
        if(imatax  .EQ.-1)     imatax    = imate_lowdfoam
        if(imatatt .EQ.-1)     imatatt   = imate_cerexmyl1

      else
        call recmes(crname,'W',
     &     'Target type not defined -> no target geometry')
        return
      endif

      if (tg_type_ff.EQ.11 .OR .tg_type_ff.EQ.-1) then
        call recmes(crname,'I',
     &     'Define geometry for photon target ("Jlab cell g11a") ...')
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
     &      cmat_tg(1:istrlen),')  at z-pos.:',tg_pos_ff(3)+st_zoff_ff,'cm'
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

      tg_mother_zpos2 = distax + thickax + tg_scatcap_thick !downstream endcap to mother
      tg_mother_zpos2 = tg_mother_zpos2 + tg_scatz_ff !add ffread addition to scat.chamber length
      pars(1)= abs( tg_mother_zpos1 - tg_mother_zpos2 )/2 + tglenby2 !halflength of mother
      pars(2)=0.0
      pars(3)=tgrad+radrad
      pars(4)=0.0
      pars(5)=tgrad+radrad
      call gsvolu('TG  ','CONE',clas_med_vac,pars,5,ivol)
      xytg=sqrt(tg_pos_ff(1)**2+tg_pos_ff(2)**2)
      ztg2 = ( tg_mother_zpos1 + tg_mother_zpos2 )/2 +    !mother center position in CLAS
     &               st_zoff_ff + tg_pos_ff(3)
      call gspos('TG  ',1,'CLAS',0.0,0.0,ztg2,0,'ONLY')
      ztg = pars(1) - tglenby2 - tg_mother_zpos2        !target center position in mother

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
      ind=3+1
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
      ind=3+4*3+1
      parm(ind)  =tg_zpos1+tg_pcon2_pos(4) ! (upstream end at cap)
      parm(ind+1)=0.
      parm(ind+2)=parm(ind-1)
      parm(ind+3)=tg_zpos2             ! (downstream end at cap)
      parm(ind+4)=0.
      parm(ind+5)=tgrad
*  downstream end cap
      do i=1,i2
        parm(i*3+ind+3)=z0_cap +tgdiam*cos((i2-i)*dangle2)
        parm(i*3+ind+4)=0.
        parm(i*3+ind+5)=tgdiam*sin((i2-i)*dangle2)
      enddo
      call gsvolu('TARG','PCON',tmed_tg,parm,np,ivol)
      call gspos('TARG',1,'TG  ',0.0,0.0,ztg,0,'ONLY')
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
      parc(6)=0.1*tgrad
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
      call gspos('TGCP',1,'TG  ',0.0,0.0,ztg,0,'ONLY')

*  save parameters
      tgpar_save(1) =tglenby2*2
      tgpar_save(2) =tgrad
      tgpar_save(6) =-tglenby2
      tgpar_save(7) =tg_pos_ff(3) +tgpar_save(6)
      tgpar_save(8) =tg_pos_ff(3) +tgpar_save(6)
      tgpar_save(9) =tg_pos_ff(3) +tgpar_save(6) +tgpar_save(1)
      tgpar_save(10)=xytg +tgrad
      tgpar_save(12)=st_zoff_ff+tg_pos_ff(3)
*
*  (c) upstream beampipe, scattering chamber and superinsulation
*  -------------------------------------------------------------
*
*New Beam Pipe for new start counter
      para(1)=0.
      para(2)=360.
      para(3)=3.                !number of planes (perp.to z) with different radii
      do i=1,3
         para(1+i*3)=bpip_newst_pln(i) !plane position in z 
         para(2+i*3)=0.         !inner radius
         para(3+i*3)=bpip_newst_rad(i) !outer radius 
      enddo
      call gsvolu('BEAM','PCON',clas_med_vac,para,12,ivol)
      call gspos('BEAM',1,'CLAS',0.0,0.0,ztg2, 0,'ONLY')

*  define beam pipe, ends upstream of scattering chamber
      do i=1,3
         para(2+i*3)=para(3+i*3)-bpip_newst_thick !inner radius
      enddo
      call gsvolu('BPIP','PCON',clas_med_alu,para,12,ivol)
      call gspos('BPIP',1,'BEAM',0.,0.,0.,0,'ONLY')
*
* attached insulation
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
        call gspos('TGSU',1,'TG  ',0.0,0.0,ztg,0,'ONLY')
      endif

* foam scattering chamber (dwg. 66850-D-04076)
      if(thickrad.GT.0.AND.thickax.GT.0.AND.imatrad.GT.0.AND.imatax.GT.0) then
        parc(1)=0.
        parc(2)=360.
        parc(3)=4
        np=parc(3)*3+3
        parc(4)=tg_zpos1+tg_pcon1_pos(3)+abs(tg_pcon1_pos(1)-tg_pcon1_pos(3))
        parc(5)=(radrad+tgrad)-thickrad
        parc(6)=(radrad+tgrad)
        parc(7)=tglenby2 + distax + tg_scatz_ff ! ffread flag to add length to scat.chamber
        parc(8)=parc(5)
        parc(9)=parc(6)
        parc(10)=parc(7)
        parc(11)=tg_scatcap_rad  !hole for aluminum window
        parc(12)=parc(6)
        parc(13)=parc(7)+thickax
        parc(14)=tg_scatcap_rad  !hole for aluminum window
        parc(15)=parc(6)
        call gsvolu('TGSC','PCON',tmed_rad,parc,np,ivol)
        call gspos('TGSC',1,'TG  ',0.0,0.0,ztg,0,'ONLY')
* aluminum window on scattering chamber
        parc(1)=0.0
        parc(2)=tg_scatcap_rad
        parc(3)=tg_scatcap_thick/2
        parc(4)=parc(13)+parc(3)
        call gsvolu('TGSW','TUBE',tmed_support,parc,3,ivol)
        call gspos('TGSW',1,'TG  ',0.0,0.0,ztg+parc(4),0,'ONLY')
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
      call gspos('TGCL',1,'TG  ',0.0,0.0,ztg,0,'ONLY')
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
      parm(6)=parm(12)*0.5
      parm(np-2)= tg_zpos1 -0.0001
      parm(np-1)= tgrad
      parm(np)  = tgrad +thickcell
      call gsvolu('TGCI','PCON',tmed_cell,parm,np,ivol)
      call gspos('TGCI',1,'TG  ',0.0,0.0,ztg,0,'ONLY')
      tgpar_save(6)=tg_pos_ff(3) -parm(4)

* 2 aluminum beam windows, one on each end cap:
      parc(1)=0.0                                           !inner radius
      parc(2)=tg_cap_rad                                    !outer radius
      parc(3)=tg_cap_thick/2                                !halfthickness
      parc(4)=tglenby2+parc(3)+thickcell                    !z-position
      call gsvolu('TGCC','TUBE',tmed_support,parc,3,ivol)
      call gspos('TGCC',1,'TG  ',0.0,0.0,ztg-parc(4),0,'ONLY')
      call gspos('TGCC',2,'TG  ',0.0,0.0,ztg+parc(4),0,'ONLY')
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
      call gspos('TGF1',1,'TG  ', 0.,0.,tg_zpos1+ztg,0,'ONLY')

*  outer base ring
      parm(3) =4.
      np=parm(3)*3+3
      do i=1,4
         parm(1+i*3) =tg_pcon2_pos(i)
         parm(2+i*3) =tg_pcon2_rad(i) +tgrad
         parm(3+i*3) =parm(2+i*3) +tg_pcon2_thick
      enddo
      call gsvolu('TGF2','PCON',tmed_support,parm,np,ivol)
      call gspos('TGF2',1,'TG  ', 0.,0.,tg_zpos1+ztg,0,'ONLY')

      end
