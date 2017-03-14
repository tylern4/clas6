
       subroutine geom_phtg3(itg_mate)
  
c_begin_doc

c  Documentation for subroutine geom_phtg3
c  
c  Purpose:
c  --------
c     ******************************************************************
c     *                                                                *
c     *      Defines the geometry of the CLAS - SACLAY Cryotarget      *
c     *          for Jlab photon beam target cells                     *
c     *                                                                *
c     ******************************************************************
c
c  for tg_type_ff=6,7,8,9:
c    #    cell wall      end cap        length   diameter   names
c    6    Mylar,127mu    Mylar,127mu    100.0mm  40.0mm    'g2a'    
c    7    Kapton,127mu   Kapton,127mu   180.0mm  40.0mm    'g1c'
c    8    Kapt/Alu,241mu Kapt/Alu,241mu 180.0mm  40.0mm    'g3a'
c  support structure type=3:
c  shape="platted cigare": tube + 2 end caps (shell segments of +/-30deg.)
c  superinsulation mat.#  167.0 (?) for tg_type_ff=6,7
c                         165.164   for tg_type_ff=8
c
c      [cf. drawings 66850-A-02981/02982 (g2a target); 
c                    66850-A-03024 (g1c target);
c                    66850-A-03051 (g3a target) ]
c
c  Major revisions:
c  ----------------
c
c_end_doc


      implicit none
      character*(*) cfile,crevis,cstate,cdate,cautho,crname,crauth
      character*132 crcsid,crmess
      parameter (cfile=  '$RCSfile: geom_phtg3.F,v $')
      parameter (crevis= '$Revision: 1.3 $')
      parameter (cstate= '$State: Exp $')
      parameter (cdate=  '$Date: 2001/08/22 16:11:36 $')
      parameter (cautho= '$Author: fklein $')
      parameter (crname= 'GEOM_PHTG3')
      parameter (crauth= 'fklein')
      data crcsid/'
     1$Id: geom_phtg3.F,v 1.3 2001/08/22 16:11:36 fklein Exp $
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
      real  parm(50), pars(5)           !arrays for volume definitions
      real  xpos,ypos,zpos,xypos        !position of volumes
      real  xytg, ztg                   !position of target center
      real  tglenby2                    !half length of target cell incl. end caps
      real  tgrad,tgdiam                !target radius,diameter w/o walls 
      real  z0_cap                      !center of circle segment for end caps
      real  cellrad                     !radius of target cell+wall
      real  thickcell                   !thickness of cell walls
*  thickness of superinsulation: radial; axial; attached to cell wall
      real  thickrad,thickax,thickatt
      data  thickcell,thickrad,thickax,thickatt  / -1, -1, -1, -1 /
      real  distax                      !dist. of axial superins. to end cap
      integer tmed_tg                   !track.medium for target material
      integer imatcell, tmed_cell       !mate# and track.medium for target cell
      integer tmed_cellins              !medium for combined cell wall+superins.
      integer tmed_support              !track.medium for flange and distr.tubes
      integer imatrad, tmed_rad         !for radial superinsulation layers 
      integer imatax, tmed_ax           !for axial superinsulation layers 
      integer imatatt, tmed_att         !for superins. attached to cell walls
      data  imatcell,imatrad,imatax,imatatt    /-1, -1, -1, -1 /       !default
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
      real tg_len(6:8)            !target length (depends on target type)
      real tg_diam                !target diameter
      real tg_thickcell(6:8)      !cell wall thickness
      real tg_thickrad(6:8)       !total thickness of superinsulation layers
                                  !packed as solid block (ca. 3.5% of total)
      real tg_thickax(6:8)        !thick.of upstream axial superinsulation
                                  !packed as solid block (ca. 3.5% of total)
      real tg_thickatt(6:8)       !thick.of superins. surrounding the cell
                                  !packed as solid block (ca. 3.5% of total)
      real ins_rad1,ins_rad2      !radius of superins.at upstream/downstream end
      real ins_zpos1,ins_zpos2    !position of upstream/downstream end of ins.
*                 tg_type=   6       7       8  
      data tg_len        /  10.0 ,  18.0 ,  18.0 /
      data tg_diam       / 4.0 /
      data tg_thickcell  / 0.0127, 0.0127, 0.0241/
      data tg_thickrad   / 0.011 , 0.011 , 0.0222/
      data tg_thickax    / 0.011 , 0.011 , 0.0444/
      data tg_thickatt   /  0.0  , 0.0   , 0.0085/
      data ins_rad1,ins_rad2    /  8.5 ,  4.0 / 
      data ins_zpos1,ins_zpos2  /-15.0 ,  4.4 /      ! =dist. from end caps

*  gas/liquid distribution pipes & support structures 
*  --------------------------------------------------
*
*  pcon1: cell wall collar (outer cone)        (#66850-A-02981,sht.4)
*            and outer base tube (cone&tube)    (#66850-A-02981,sht.2)
*  pcon2: inner base tube (cone&tube)       (#66850-A-02981,sht.3)
*  tube1:  horizontal, to backward condenser
*  tube2:  angled by 45 deg., attached to base tube

      real tg_pcon1_pos(8)       ! wrt. edge of end cap
      real tg_pcon1_rad(8)       ! inner radius above cell radius
      real tg_pcon1_thick(8)     ! thickness of collar and outer base tube
      real tg_pcon2_pos(4)       ! wrt. edge of end cap 
      real tg_pcon2_rad(4)       ! inner radius above cell radius
      real tg_pcon2_thick        ! thickness of inner base tube
      data tg_pcon1_pos   /-8.20,-6.93,-5.66,-0.51,-0.50, 0.13, 1.03, 1.53 /
      data tg_pcon1_rad   / 0.72, 0.75, 0.75, 0.75, 0.75, 0.75, 0.0,  0.0 / 
      data tg_pcon1_thick / 0.25,  1.3, 0.06, 0.06, 0.11, 0.11, 0.11, 0.10 /
      data tg_pcon2_pos   / -8.2,-0.30,  0.0,  0.5 /
      data tg_pcon2_rad   / 0.35, 0.35, 0.05, 0.05 / 
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
      if(tg_ins_ff(1).GT.0.0) then
        imatrad = IFIX(tg_ins_ff(1))
        imatatt = IFIX((tg_ins_ff(1)-imatrad)*1000)
      endif
      if(tg_ins_ff(2).GE.0.0) thickrad = tg_ins_ff(2)
      if(tg_ins_ff(3).GE.0.0) thickax  = tg_ins_ff(3)
      if(tg_ins_ff(4).GE.0.0) thickatt = tg_ins_ff(4)
      if(tg_ins_ff(5).GT.0.1) ins_rad2 = tg_ins_ff(5)    !at downstream end
      distax = ins_zpos2
      if(tg_ins_ff(6).GT.0.1) distax = tg_ins_ff(6)

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
      if(tg_type_ff.GE.6 .AND. tg_type_ff.LE.8) then

        tglenby2  = tg_len(tg_type_ff)/2.0
        tgrad     = tg_diam/2.0
        thickcell = tg_thickcell(tg_type_ff)
        if(imatcell.EQ.-1) then
          if(tg_type_ff.EQ.6) then              ! g2a target
            imatcell = imate_mylar
          elseif(tg_type_ff.EQ.7) then          ! g1c target
            imatcell = imate_kapton
          elseif(tg_type_ff.EQ.8) then          ! g3a target
            imatcell = imate_comp_alumaseal
          endif
        endif
        if(thickrad.LT.0.0) thickrad = tg_thickrad(tg_type_ff)
        if(thickax.LT.0.0)  thickax  = tg_thickax(tg_type_ff)
        if(thickatt.LT.0.0) thickatt = tg_thickatt(tg_type_ff)
        if(imatrad.EQ.-1) then
          if(tg_type_ff.EQ.8) then              ! g3a target
            if(imatrad.EQ.-1) imatrad = imate_cerexalu1
            if(imatax.EQ.-1)  imatax  = imate_cerexalu2
            if(imatatt.EQ.-1) imatatt = imate_cerex
          else
            if(imatrad.EQ.-1) imatrad = imate_cerexmyl1
            if(imatax.EQ.-1)  imatax  = imate_cerexmyl1
            imatatt = 0
          endif
        endif

      elseif(tg_type_ff.EQ.-1) then             !user defined geom.

        tglenby2  = tg_user_ff(1)/2
        tgrad     = tg_user_ff(2)/2
        thickcell = tg_thickcell(7)             !default values for g1c target
        if(tg_user_ff(3).GT.0) thickcell = tg_user_ff(3)
        if(thickrad.LT.0.0)    thickrad  = tg_thickrad(7)
        if(thickax.LT.0.0)     thickax   = tg_thickax(7)
        if(thickatt.LT.0.0)    thickatt  = tg_thickatt(7)
        if(imatcell.EQ.-1)     imatcell  = imate_kapton
        if(imatrad.EQ.-1)      imatrad   = imate_cerexmyl1
        if(imatax.EQ.-1)       imatax    = imate_cerexmyl1
        if(imatatt.EQ.-1)      imatatt   = 0

      else
        call recmes(crname,'W','Target type not defined -> no target geometry')
        return
      endif

*
*  stop if beam pipe volume not defined
      call GLOOK('BEAM', IQ(jvolum+1), NVOLUM, ivol)
      if(ivol.LE.0) then
        call recmes(crname,'W','Beam pipe volume not defined -> no target geometry')
        return
      endif
      call recmes(crname,'I','Define geometry for photon target ("Jlab cell") ...')
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
*  attached superins. and cell wall as single volume
          tmed_cellins=tmed_att
        else
          imatatt=0
        endif
      endif
      if(imatatt.EQ.0) then
        tmed_cellins=tmed_cell
        thickatt    =0.0
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

      pars(1)=(abs(ins_zpos1)+ins_zpos2+thickax)/2 +tglenby2
      pars(2)=0.0
      pars(3)=ins_rad1+thickrad
      pars(4)=0.0
      pars(5)=ins_rad2+thickrad*2
      call gsvolu('TG  ','CONE',clas_med_vac,pars,5,ivol)
      xytg=sqrt(tg_pos_ff(1)**2+tg_pos_ff(2)**2)
      ztg =(ins_zpos2+thickax+ins_zpos1)/2
      call gspos('TG  ',1,'BEAM', tg_pos_ff(1),tg_pos_ff(2),tg_pos_ff(3)+ztg,0,'ONLY')
*
*  (b) target cell: 
*  ----------------
*  cell shape "platted cigare": tube + 2 shell segments (+/- 30deg.) 
* 
      tgdiam = 2*tgrad
      z0_cap = tglenby2-tgdiam    !center for circle segment for end caps
      parm(1)=  0.
      parm(2)=360.
* NOTE: max. #planes=14  (cf. gsvolu.F)
      i1=6                        !planes for upstream half sphere
      i2=6                        !planes for downstream half sphere
      parm(3)=2+i1+i2             !#planes: i1+i2+2(for tube part)
      np=parm(3)*3+3
*  angular steps wrt. z-axis 
      dangle1=piby2/3/i1
      dangle2=piby2/3/i2
*  ind=index of 1.entry for tube part (header=3 entries & 3 entries per plane)
      ind=3+i1*3+1
*  start/end position of tube part
      tg_zpos1=-z0_cap - tgdiam*cos(i1*dangle1)
      tg_zpos2= z0_cap + tgdiam*cos(i2*dangle2)
      do i=0,i1-1                 !upstream end cap
        parm(i*3+4)=-z0_cap -tgdiam*cos(i*dangle1)
        parm(i*3+5)=0.
        parm(i*3+6)=tgdiam*sin(i*dangle1)
      enddo
      parm(ind)  =tg_zpos1        !tube like part (upstream end)
      parm(ind+1)=0. 
      parm(ind+2)=tgrad
      parm(ind+3)=tg_zpos2        !tube like part (downstream end)
      parm(ind+4)=0.
      parm(ind+5)=tgrad
      do i=1,i2                   !downstream end cap
        parm(i*3+ind+3)=z0_cap +tgdiam*cos((i2-i)*dangle2)
        parm(i*3+ind+4)=0.
        parm(i*3+ind+5)=tgdiam*sin((i2-i)*dangle2)
      enddo
      parm(6)=parm(9)*0.8         !adjust entries at z=+-tglenby2 (were 0.0)
      parm(np)=parm(np-3)*0.8
      call gsvolu('TARG','PCON',tmed_tg,parm,np,ivol)
      call gspos('TARG',1,'TG  ',0.0,0.0,-ztg,0,'ONLY')
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
*
*  (d) target cell walls
*  ---------------------
*  - in case of attached superinsulation 
*    cell wall and superinsulation is taken together as single volume
*
      if(imatcell.EQ.0 .AND. imatatt.EQ.0) RETURN

*  planes for target cell walls:
      cellrad = tgdiam+thickcell+thickatt
      if(icellflg.LE.1) then         !cell wall collar covers part of cell wall
        parm(ind) = parm(ind) + tg_pcon1_pos(8)
      endif
      do i=0,1
        parm(i*3+4)=parm(ind+i*3)
        parm(i*3+5)=parm(ind+i*3+2)
        parm(i*3+6)=parm(ind+i*3+2) +thickcell +thickatt
      enddo
      do i=1,i2                      !downstream end cap
        parm(i*3+7)=parm(i*3+ind+3)
        parm(i*3+8)=parm(i*3+ind+5)
        dangle3=i2/(i2-1)*acos(tgdiam/cellrad*cos((i2-i)*dangle2))
        parm(i*3+9)=cellrad*sin(dangle3)
      enddo
      parm(i2*3+10)=tglenby2 +0.0001
      parm(i2*3+11)=0.0
      parm(i2*3+12)=parm(i2*3+9)
      im=1
      if(thickatt.GT.thickcell) im=5
      dangle3 = dangle3/im            !angle for outer segment at tglenby2
      do i=1,im
        parm((i2+i)*3+10)=z0_cap +cellrad*cos((im-i)*dangle3)
        parm((i2+i)*3+11)=0.0
        parm((i2+i)*3+12)=cellrad*sin((im-i)*dangle3)
      enddo
      parm(3)=2+i2+im+1
      np=parm(3)*3+3
      parm(np)=parm(np-3)*0.8           !adjust outermost entry (was 0.0)
      call gsvolu('TGCL','PCON',tmed_cellins,parm,np,ivol)
      call gspos('TGCL',1,'TG  ',0.0,0.0,-ztg,0,'ONLY')
      tg_ins_ff(4) =thickatt
      tgpar_save(3)=thickcell
      tgpar_save(4)=thickcell
      tgpar_save(5)=imatcell

*  planes for upstream end cap:  
      cellrad = tgdiam+thickcell        !upstream end cap without any superins.
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
*
*  (e) support structure 
*  ---------------------
*
      if(icellflg.GT.1) return
*
*  cell wall collar and outer base ring
      parm(1) =0.
      parm(2) =360.
      parm(3) =8.
      np=parm(3)*3+3
      do i=1,8
         parm(1+i*3) =tg_pcon1_pos(i)
         parm(2+i*3) =tg_pcon1_rad(i) +tgrad
         parm(3+i*3) =parm(2+i*3) +tg_pcon1_thick(i)
      enddo
      call gsvolu('TGF1','PCON',tmed_support,parm,np,ivol)
      call gspos('TGF1',1,'TG  ', 0.,0.,tg_zpos1-ztg,0,'ONLY')

*  upstream part of attached superinsulation
      if(imatatt.GT.0) then
         parm(3) =7.
         np=parm(3)*3+3
         do i=1,7
            parm(1+i*3) =parm(4+i*3)
            parm(2+i*3) =parm(6+i*3)
            parm(3+i*3) =parm(6+i*3) +thickatt
         enddo
         call gsvolu('TGSU','PCON',tmed_att,parm,np,ivol)
         call gspos('TGSU',1,'TG  ', 0.,0.,tg_zpos1-ztg,0,'ONLY')
      endif

*  inner base ring
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
      zpos=tg_pcon1_pos(2) +tg_zpos1 -ins_zpos1 +tglenby2
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
