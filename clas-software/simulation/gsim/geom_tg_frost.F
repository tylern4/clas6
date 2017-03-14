
       subroutine geom_tg_frost(itg_mate)
  
c_begin_doc

c  Documentation for subroutine geom_tg_frost
c  
c  Purpose:
c  --------
c     ******************************************************************
c     *                                                                *
c     *      Defines the geometry of a CLAS frozen spin target         *
c     *                                                                *
c     ******************************************************************
c
c  for tg_type_ff=12:
c    #    cell wall         end cap         length  diameter   names
c   12    Alu-seal,241mu    Alu-seal,241mu   50.0mm  15.0mm    'polt','frost'
c  shape=cylinder
c
c  Major revisions:
c  ----------------
c Fall 2009: start counter support, CH2 and C targets, 
c            corrected FROST geometry
c            CH2 target thickness can be set by card: TGUSER 5=... (in cm)
c               (switch off CH2 target by card TGUSER 5=-1.)
c            Carbon target thickness can be set by card: TGUSER 6=... (in cm)
c               (switch off Carbon target by card TGUSER 6=-1.)
c_end_doc


      implicit none
      character*(*) cfile,crevis,cstate,cdate,cautho,crname,crauth
      character*132 crcsid,crmess
      parameter (cfile=  '$RCSfile: geom_tg_frost.F,v $')
      parameter (crevis= '$Revision: 1.7 $')
      parameter (cstate= '$State: Exp $')
      parameter (cdate=  '$Date: 2010/01/09 01:46:29 $')
      parameter (cautho= '$Author: fklein $')
      parameter (crname= 'GEOM_TG_FROST')
      parameter (crauth= 'fklein')
      data crcsid/'
     1$Id: geom_tg_frost.F,v 1.7 2010/01/09 01:46:29 fklein Exp $
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

      integer  iucomp, lnblnk, get_rotm, get_tmed, get_mate

*  local variables:
*  ----------------

      real  twopiby3, PIby2
      integer i, istrlen                !loop parameter, string length
      integer ivol                      !volume#
      real  parm(3), pars(3),parvol(3)  !arrays for volume definitions
      real  xpos,ypos,zpos,xypos,zposm  !position of volumes
      real  xytg, ztg                   !position of target center
      real  tglenby2                    !half length of target cell incl. end caps
      real  tgrad                       !target radius w/o walls 
      integer imat_jacket, tmed_jacket
      integer imat_scatchamber, tmed_scatchamber 
      integer imat_bpip, tmed_bpip
      integer imat_ipip, tmed_ipip
      integer imat_carbon, tmed_carbon
      integer imat_ch2, tmed_ch2
      integer imat_mixchamber, tmed_mixchamber 
      integer imat_he3he4, tmed_he3he4
      integer imat_mandrel, tmed_mandrel
      integer imat_coils, tmed_coils
      integer imat_tgcell, tmed_tgcell
      integer tmed_tg                   !track.medium for target material
      integer  icellflg, h_cellflag(5)  !options: wall/superins. geom.
      data h_cellflag /4HCELL,4HOFF ,4HNONE,4HINS , 4HCOIL /
*              flag#     -2     0       0     -3       -4
      real    tmaxfd, stmax, deemax, epsil, stmin   !track.media cuts 
      real    aa,zz,dens,radl,absl                  !material constants
      character*20  cmat_tg, cmatcell, cmate      !material names
      integer np                 !#parameters 
      real  par(30)              !dummy parameter list

*
*  default values for target geometry  
*  ----------------------------------
*
* target walls: KEL-F 
      real A_kelf(3), Z_kelf(3), W_kelf(3), D_kelf
      data A_kelf / 12.01, 18.9984, 35.4527 /
      data Z_kelf /  6.0,   9.0,    17.0 /
      data W_kelf /  2.0,   3.0,    1.0  /
      data D_kelf /  2.12 /

      real tg_len, tg_diam, thickcell                      ! thickness=0.050"
      data tg_len,tg_diam,thickcell / 5.20, 1.50, 0.127 /

*  upstream end of cell: KEL-F cylinder D_int=0.67", D_out=0.75", L=1.1"
      real cup_len, cup_diam, cup_thick
      data cup_len,cup_diam,cup_thick / 2.79, 1.70, 0.203 / 
*
*  mixing chamber (Al) with He3/He4 bath 
*  cylinder D_int=0.75", thick=0.065", L=4.25"=51mm(upstream)+57mm(target+downstream)
*   (to prevent overlapping volumes: shorten upstream length=27.8mm)
      real mixer_diam, mixer_len, mixer_len0,mixer_thick,mixer_zoff
*      data mixer_diam,mixer_len,mixer_len0 / 1.905,10.8,5.1 /
      data mixer_diam,mixer_len,mixer_len0 / 1.905,8.49,2.78 /
      data mixer_thick,mixer_zoff / 0.165, 0.3 /
*
* He3/He4 bath in upstream part
*  material: He3/He4 bath
      real A_he3he4(2),Z_he3he4(2),W_he3he4(2),D_he3he4(2),dens_he3he4 
      data A_he3he4 / 3.0 , 4.0 /
      data Z_he3he4 / 2.0 , 2.0 /
      data W_he3he4 / 0.5 , 0.5 /
c      data D_he3he4 / 0.07, 0.125 /
c  13-sep-2005 update He density  for very low temperatures
      data D_he3he4 / 0.082, 0.145 /

*  insert pipe (starts 1.1" upstream of target):
*  stainless steel: L=10.375", D_int=0.555", D_out=0.625" 
      real ipip_len, ipip_diam, ipip_thick, ipip_pos
c      data ipip_len,ipip_diam,ipip_thick,ipip_pos / 33.97, 1.41, 0.18, -2.8 /
      data ipip_len,ipip_diam,ipip_thick,ipip_pos / 37.77, 1.41, 0.18, -2.8 /

* mandrel + 1K heat shield: Al
* mandrel: L=7.25", D_out = 1.969"=5.0cm, thick=0.057"

      real mandrel_diam, mandrel_len, mandrel_thick
      data mandrel_diam,mandrel_len,mandrel_thick / 5.00, 18.415, 0.1448 /

* Coil center 4.75" from upstream end of mandrel, L=4.327"
c The wire itself is 0.005" thick (127 micrometers).
c there is also an isolation (varnish) 0.0055" thick
c total thickness in center 0.017" (3 layers), 0.893" from ends 0.022" (4 layers)
c
c as polycone (relative to upstream end of mandrel), grooved into mandrel
c      real coil_pos(6), coil_thick(6)
c      data coil_pos   /  6.60, 8.87, 8.90, 15.29, 15.32, 17.59 /
c      data coil_thick / 0.056,0.056, 0.043,0.043, 0.056,0.056 /
c simpler: 4 layers throughout (same for racetrack coil), pos=offset from downstream end
      real coil_len, coil_thick, coil_pos
      data coil_len,coil_thick,coil_pos / 10.99, 0.056, 0.855 /

* Monofilament Superconductor Nb 45 to 47 weight % Ti alloy placed
* in copper matrix (Cu to SC ratio is about 1.5 : 1.0).
      real A_coils(3), Z_coils(3), W_coils(3), D_coils(3), dens_coils
*                   Nb    Ti    Cu
      data A_coils / 92.9, 47.87, 63.54 /
      data Z_coils / 41. , 22.  , 29. /
      data W_coils / 0.47,  1.0 , 2.2 /     !Cu: 1.5x(0.47+1.0)
      data D_coils / 8.57, 4.54 , 8.96 / 
 
*  vacuum jacket (simplified: cylinder)
*  upstream part starts -42cm and ends -7cm from target center, 
*  downstream part starts -7cm and ends 16cm from tg center
*  flange for dilution refrigerator ~1.8cm upstream of flange connecting the scat.chamber
      real jacket_diam0, jacket_len0, jacket_end0        !upstream part
      data jacket_diam0,jacket_len0,jacket_end0 / 12.1, 35., -7. /  
      real jacket_diam, jacket_len,jacket_end            !downstream part
      data jacket_diam,jacket_len,jacket_end / 6.2, 23., 16. / 
      real jacket_thick, jacket_thickcap
      data jacket_thick,jacket_thickcap / 0.071, 0.1 /  !inch or cm?
      real jacket_thickflange,jacket_rfloff,jacket_thickwindow
      data jacket_thickflange,jacket_rfloff / 1.27, -5.1 /
      data jacket_thickwindow /0.02/

*  carbon and CH2 targets
      real carb_thick, carb_diam, ch2_thick, ch2_diam
      data carb_thick,carb_diam / 0.277, 1.5 /   !on mandrel cap
      data ch2_thick,ch2_diam / 0.345, 1.5 /     !on jacket cap
*
*  1k shield and beam pipe: 
*  stainless steel: D_out=1.5", thick=0.07" starts 13.8cm upstream of target center
      real bpip_len, bpip_diam, bpip_thick, bpip_pos
      data bpip_len,bpip_diam,bpip_thick,bpip_pos / 186., 3.8, 0.18, -13.8 /

*  NOTE: increase mother volume for 'CLAS' in order to use He bag volume
*  He bag between frost (-2.0m from tg center) to pair spect. L=1019cm
      real bag_len, bag_diam, bag_pos
      data bag_len,bag_diam,bag_pos / 1019., 15.2, -200. /
c
c  scattering chamber with bolting ring (drawing 66850-D-04532/3) 
c  (exit window 25um Al at position +21.61cm not in model)
c  (pos. wrt. tg center)
      real scatring_zpos(6),scatring_orad(6) 
      data scatring_zpos / -7.0, -6.68,-6.675, -5.61,-5.605, -5.1 /
      data scatring_orad / 6.03,  6.03,  4.60,  4.60,  5.56, 5.56 /
      real scat_zpos(5), scat_irad(5), scat_orad(5)
c      data scat_zpos / -5.1, 19.47, 20.02, 21.75, 22.11 /
      data scat_zpos / -5.1, 19.47, 20.22, 21.25, 22.11 /
      data scat_irad /  4.45, 4.45,  3.04,  1.27,  1.27 /
      data scat_orad /  5.56, 5.56,  5.56,  3.71,  2.22 /

c
c  start counter support (drawing 66814-D-04678/04683)
c  - support arm (simplified as PCON section)   (NOTE: units=inch)
      real st_arm_zoff(9), st_arm_irad(9), st_arm_orad(9)
      data st_arm_zoff /-46.9, 22.4,0.1, 27.24, 20.72,0.2, 3.0,0.1,1.74 / !additive in inch
      data st_arm_irad / 13.5, 13.5,16.0, 16.0,  5.6, 2.5, 2.5,2.5,2.5 /
      data st_arm_orad / 22.2, 22.2,22.2, 22.2, 10.1,10.05,8.5,3.5,3.5 /

c  - downstream tube connected to DC R1 support and shielding pipe
      real st_apex_rad, st_apex_thick, st_apex_len
      data st_apex_rad   / 2.5 /   ! NOTE: all in inch
      data st_apex_thick / 0.5 /   
      data st_apex_len   / 11.88 /  
      
c_end_var


c  executable code:
c  ----------------
      piby2   =acos(0.)
      twopiby3=acos(-1.)*2./3.

      imat_carbon = imate_carbon
      imat_ch2    = imate_ch2
      if(tg_user_ff(5).lt.0) then
         imat_carbon = 0
      elseif(tg_user_ff(5).gt.0) then
         carb_thick = tg_user_ff(5)
      endif
      if(tg_user_ff(6).lt.0) then
         imat_ch2    = 0
      elseif(tg_user_ff(6).gt.0) then
         ch2_thick = tg_user_ff(6)
      endif

      imat_jacket    = imate_stainless_steel
      imat_scatchamber=imate_lowdfoam
      imat_mandrel   = imate_aluminum
      imat_ipip      = imate_stainless_steel
      imat_bpip      = imate_stainless_steel

c skip that for now (only switch on/off of materials via tg_cell_ff
c      if(tgpar_save(5).GE.0.0.AND.tgpar_save(5).LE.NMATE) 
c     &          imat_tgcell = IFIX(tgpar_save(5))
c      if(tg_ins_ff(1).GT.0.0) imat_jacket  = IFIX(tg_ins_ff(1))
c      if(tg_ins_ff(2).GT.0.0) imat_ipip    = IFIX(tg_ins_ff(2))
c      if(tg_ins_ff(3).GT.0.0) imat_mandrel = IFIX(tg_ins_ff(3))
c      if(tg_ins_ff(4).GT.0.0) imat_he3he4  = IFIX(tg_ins_ff(4))

*  correct material id in case that no target walls/superinsulation chosen
      icellflg=IUCOMP(tg_cell_ff,h_cellflag,5)
      if(icellflg.GT.0) then
         if(icellflg.GT.1) then
            imat_tgcell=0
            imat_he3he4=0
         endif
         if(icellflg.NE.3) then
            imat_jacket =0
            imat_ipip   =0
         endif
         if(icellflg.LT.4) imat_mandrel =0
      endif

********************************************
*  stop here if 'tg_type_ff' out of range  *
*
      if(tg_type_ff.GE.12 .AND. tg_type_ff.LE.12) then

      tglenby2  = tg_len/2
      tgrad     = tg_diam/2

      elseif(tg_type_ff.EQ.-1) then             !user defined geom.

        tglenby2  = tg_user_ff(1)/2
        tgrad     = tg_user_ff(2)/2
        if(tg_user_ff(3).GT.0)  thickcell = tg_user_ff(3)

      else
        call recmes(crname,'W',
     &                 'Target type not defined -> no target geometry')
        return
      endif

*
*  stop if beam pipe volume not defined
*      call GLOOK('BEAM', IQ(jvolum+1), NVOLUM, ivol)
*      if(ivol.LE.0) then
*        call recmes(crname,'W','Beam pipe volume not defined -> no target geometry')
*        return
*      endif
      call recmes(crname,'I','Define geometry for FROzen Spin Target...')
*
***************************
*  define tracking media
***************************
*
*  these parameters are only(!) valid if  Data Card 'AUTO' = 0  !
      TMAXFD =  0.2       !max. angular deviation in one step [degree]
      STMAX  =  0.1       !max. step  [cm]
      DEEMAX =  0.01      !max. energy loss per step  [0;1]
      EPSIL  =  0.001     !boundary crossing precision
      STMIN  =  0.0001    !min. step  [cm]
*
*  target material
*
      call GFMATE(itg_mate,cmat_tg,aa,zz,dens,radl,absl,parm,np)
      if(aa.eq.-1) then
        write(crmess,'(A,I4,A)')'Target material (id=',itg_mate,
     &     ') not defined -> NO TARGET!'
        call recmes(CRNAME,'W',crmess)
        return
      endif
      if(itg_mate.EQ.imate_vacuum) then
         tmed_tg=clas_med_vac
      else
         tmed_tg=get_tmed()
         CALL GSTMED( tmed_tg,cmat_tg,itg_mate,
     &             1, init_ifield,init_fieldm,tmaxfd,
     &             stmax, deemax, epsil, stmin, 0 , 0 )
         call set_tpar(tmed_tg,tg_tmedcuts_ff)
      endif
*
*  target cell: Kel-F
*  
      imat_tgcell=get_mate()
      if(imat_tgcell.gt.0) then
         cmatcell='KEL-F  '
         call GSMIXT(imat_tgcell,cmatcell,A_kelf,Z_kelf,D_kelf,-3,W_kelf)
         tmed_tgcell=get_tmed()
         CALL GSTMED(tmed_tgcell,cmatcell,imat_tgcell, 
     &           1, init_ifield,init_fieldm,tmaxfd,
     &           stmax, deemax, epsil, stmin, 0 , 0 )
         call set_tpar(tmed_tgcell,tg_tmedcuts_ff)  
         imat_mixchamber=imat_tgcell
         tmed_mixchamber=tmed_tgcell
      else
         icellflg=2
      endif
*
*  carbon and CH2 targets
*
      if(imat_carbon.GT.0) then
         call GFMATE(imat_carbon,cmate,aa,zz,dens,radl,absl,parm,np)
         if(aa.gt.0) then
            tmed_carbon=get_tmed()
            CALL GSTMED( tmed_carbon,cmate,imat_carbon,
     &           1, init_ifield,init_fieldm,tmaxfd,
     &           stmax, deemax, epsil, stmin, 0 , 0 )
c            call set_tpar(tmed_carbon,tg_tmedcuts_ff)
         else
            imat_carbon=0
         endif
      endif
      if(imat_ch2.GT.0) then
         call GFMATE(imat_ch2,cmate,aa,zz,dens,radl,absl,parm,np)
         if(aa.gt.0) then
            tmed_ch2=get_tmed()
            CALL GSTMED( tmed_ch2,cmate,imat_ch2,
     &           1, init_ifield,init_fieldm,tmaxfd,
     &           stmax, deemax, epsil, stmin, 0 , 0 )
c            call set_tpar(tmed_ch2,tg_tmedcuts_ff)
         else
            imat_ch2=0
         endif
      endif
*
*  holding coils:
*  
      if(imat_mandrel.GT.0) then
         call GFMATE(imat_mandrel,cmate,aa,zz,dens,radl,absl,parm,np)
         if(aa.gt.0) then
            tmed_mandrel=get_tmed()
            CALL GSTMED( tmed_mandrel,cmate,imat_mandrel,
     &           1, init_ifield,init_fieldm,tmaxfd,
     &           stmax, deemax, epsil, stmin, 0 , 0 )
            call set_tpar(tmed_mandrel,tg_tmedcuts_ff)

            imat_coils=get_mate()
            dens_coils=(W_coils(1)*D_coils(1)+W_coils(2)*D_coils(2)+
     &           W_coils(3)*D_coils(3))/(W_coils(1)+W_coils(2)+W_coils(3))
            call gsmixt(imat_coils,'HOLDING COILS$',
     &           A_coils,Z_coils,dens_coils,-3,W_coils)
            tmed_coils=get_tmed()
            CALL GSTMED( tmed_coils,'HOLDING COILS$',imat_coils, 
     &           1, init_ifield,init_fieldm,tmaxfd,
     &           stmax, deemax, epsil, stmin, 0 , 0 )
            call set_tpar(tmed_coils,tg_tmedcuts_ff)
         else
            imat_mandrel=0
         endif
      endif

*  helium bath + mixing chamber
*  
      if(imat_mixchamber.GT.0) then
         imat_he3he4=get_mate()
*     call gsmate(imat_he3he4,'HELIUM3/4$',3.5,2.,0.0975 ,755.5,545.,UBUF,0)
         dens_he3he4 = W_he3he4(1)*D_he3he4(1)+W_he3he4(2)*D_he3he4(2)
         call gsmixt(imat_he3he4,'HELIUM3/4$',
     &           A_he3he4,Z_he3he4,dens_he3he4,-2,W_he3he4)
         tmed_he3he4=get_tmed()
         CALL GSTMED( tmed_he3he4,'HELIUM3/4$',imat_he3he4, 
     &           1, init_ifield,init_fieldm,tmaxfd,
     &           stmax, deemax, epsil, stmin, 0 , 0 )
         call set_tpar(tmed_he3he4,tg_tmedcuts_ff)
      endif
*
*  vacuum jacket, scatter chamber, beam pipe
*
      if(imat_jacket.GT.0) then
         call GFMATE(imat_jacket,cmate,aa,zz,dens,radl,absl,parm,np)
         if(aa.gt.0) then
            tmed_jacket=get_tmed()
            CALL GSTMED( tmed_jacket,cmate,imat_jacket,
     &           1, init_ifield,init_fieldm,tmaxfd,
     &           stmax, deemax, epsil, stmin, 0 , 0 )
c            call set_tpar(tmed_jacket,tg_tmedcuts_ff)
         else
            imat_jacket=0
         endif
      endif
      if(imat_scatchamber.GT.0) then
         call GFMATE(imat_scatchamber,cmate,aa,zz,dens,radl,absl,parm,np)
         if(aa.gt.0) then
            tmed_scatchamber=get_tmed()
            CALL GSTMED( tmed_scatchamber,cmate,imat_scatchamber,
     &           1, init_ifield,init_fieldm,tmaxfd,
     &           stmax, deemax, epsil, stmin, 0 , 0 )
c            call set_tpar(tmed_scatchamber,tg_tmedcuts_ff)
         else
            imat_scatchamber=0
         endif
      endif
      if(imat_ipip.GT.0) then
         call GFMATE(imat_ipip,cmate,aa,zz,dens,radl,absl,parm,np)
         if(aa.gt.0) then
            tmed_ipip=get_tmed()
            CALL GSTMED( tmed_ipip,cmate,imat_ipip,
     &           1, init_ifield,init_fieldm,tmaxfd,
     &           stmax, deemax, epsil, stmin, 0 , 0 )
c           call set_tpar(tmed_ipip,tg_tmedcuts_ff)
         else
            imat_ipip=0
         endif
      endif
      if(imat_bpip.GT.0) then
         call GFMATE(imat_bpip,cmate,aa,zz,dens,radl,absl,parm,np)
         if(aa.gt.0) then
            tmed_bpip=get_tmed()
            CALL GSTMED( tmed_bpip,cmate,imat_bpip,
     &           1, init_ifield,init_fieldm,tmaxfd,
     &           stmax, deemax, epsil, stmin, 0 , 0 )
c            call set_tpar(tmed_bpip,tg_tmedcuts_ff)
         else
            imat_bpip=0
         endif
      endif
*
*  print info
*
      istrlen=LNBLNK(cmat_tg)
      write(crmess,'(3A,F7.2,A)') 'set Target (material=',
     &      cmat_tg(1:istrlen),')  at z-pos.:',tg_pos_ff(3)+tgpar_save(12),'cm'
      call recmes(crname,'I',crmess)
      if(imat_tgcell.GT.0) then
        istrlen=LNBLNK(cmatcell)
        write(crmess,'(2(A,F7.3),A,F6.4,3A)') 'length:',
     &        tglenby2*2,'cm;  radius:',tgrad,'cm;  cell thickness:',
     &        thickcell,' (',cmatcell(1:istrlen),')'
      else
        write(crmess,'(2(A,F7.3),A)') 'length:',tglenby2*2,
     &        'cm;  radius:',tgrad,'cm'
      endif
      call recmes(crname,'I',crmess)
      if(imat_mixchamber.GT.0 .and. imat_mandrel.GT.0) then
           write(crmess,'(2A)') 'including mixing chamber, He3/He4 bath',
     &        ' and holding coil'
           call recmes(crname,'I',crmess)
      endif
*  
************************
*  geometry definitions
************************
*
* start counter support (drawing 66814-D-04675 to 04689)
* support arm (between sectors 5&6)
      par(1)=267.8
      par(2)=4.4    ! thickness of support arm is 3.175cm <=> 4.45deg at distance 16"
      par(3)=9      ! number of planes
      np=3+3*par(3)
      par(4)= st_arm_zoff(1)*2.54
      par(5)= st_arm_irad(1)*2.54
      par(6)= st_arm_orad(1)*2.54
      do i=2,9
        par(1+i*3)= par(i*3-2)+st_arm_zoff(i)*2.54
        par(2+i*3)= st_arm_irad(i)*2.54
        par(3+i*3)= st_arm_orad(i)*2.54
      enddo
      call gsvolu('SARM','PCON',clas_med_alu, par,np,ivol)
      call gspos('SARM',1,'CLAS', 0.0,0.0,0.0, 0,'ONLY')

* apex support (tube)
      parvol(1)= (st_apex_rad - st_apex_thick)*2.54
      parvol(2)= st_apex_rad*2.54
      parvol(3)= st_apex_len/2*2.54
      call gsvolu('SAPE','TUBE',clas_med_iron, parvol,3,ivol)
      call gspos('SAPE',1,'CLAS', 0.0,0.0,par(28), 0,'MANY')
*                                         ^^^^^^last position of support arm definition
*
*  (a) mother volume (vacuum)
*  --------------------------
*  put the whole target_cryostat structure into a tube (might consider polycone ...)
      parvol(1)=0.0
      parvol(2)=jacket_diam0/2.
      parvol(3)=(jacket_len0+scat_zpos(5)-scatring_zpos(1))/2.
      call gsvolu('TG  ','TUBE',clas_med_vac,parvol,3,ivol)
      xytg=sqrt(tg_pos_ff(1)**2+tg_pos_ff(2)**2)
      ztg =scat_zpos(5)-parvol(3)
      call gspos('TG  ',1,'CLAS', tg_pos_ff(1),tg_pos_ff(2),tg_pos_ff(3)+ztg,0,'ONLY')
*
*  scatter chamber
*
      if(imat_scatchamber.gt.0) then
         par(1)=0.
         par(2)=360.
         par(3)=5.             !number of planes (perp.to z) with different radii
         do i=1,5
            par(1+i*3)=scat_zpos(i)    !plane position in z 
            par(2+i*3)=scat_irad(i)    !inner radius
            par(3+i*3)=scat_orad(i)    !outer radius 
         enddo
         call gsvolu('SCAT','PCON',tmed_scatchamber,par,18,ivol)
         call gspos('SCAT',1,'TG  ',0.0,0.0,-ztg,0,'ONLY')

         if(imat_jacket.gt.0) then
           par(3)=6.             !number of planes (perp.to z) with different radii
           do i=1,6
              par(1+i*3)=scatring_zpos(i)    !plane position in z 
              par(2+i*3)=scat_irad(1)        !inner radius
              par(3+i*3)=scatring_orad(i)    !outer radius 
           enddo
           call gsvolu('SCTR','PCON',tmed_jacket,par,21,ivol)
           call gspos('SCTR',1,'TG  ',0.0,0.0,-ztg,0,'ONLY')
         endif
      endif
 

*  beam pipe and He bag (upstream of TG volume)
      if(imat_bpip.gt.0) then
         parm(1)=0.0
         parm(2)=bpip_diam/2
         parm(3)=bpip_len/2
         call gsvolu('BEAM','TUBE',clas_med_vac,parm,3,ivol)
         zpos=ztg-parm(3)-parvol(3)
         call gspos('BEAM',1,'CLAS',0.,0.,zpos,0,'ONLY')
         if(imat_jacket.gt.0)then
            parm(1)=parm(2)-bpip_thick
            call gsvolu('BPIP','TUBE',tmed_jacket,parm,3,ivol)
            call gspos('BPIP',1,'BEAM',0.,0.,0.,0,'ONLY')
         endif
         par(1)=0.0
         par(2)=bag_diam/2
         par(3)=bag_len/2
         call gsvolu('HEBG','TUBE',clas_med_helium,par,3,ivol)
         call gspos('HEBG',1,'CLAS',0.,0.,zpos-parm(3)-par(3),0,'ONLY')
      endif
      
* vacuum jacket/heat shield

      if(imat_jacket.GT.0) then
         parm(2)=jacket_diam/2
         parm(1)=parm(2) - jacket_thick           ! downstream vacuum jacket
         parm(3)=jacket_len/2.
         call gsvolu('JACK','TUBE',tmed_jacket,parm,3,ivol)
         zpos=jacket_end-parm(3)-ztg
         call gspos('JACK',1,'TG  ', 0.,0.,zpos,0,'ONLY')
         
         parm(2)=parm(1)                            !vacuum jacket (downstream cap) 
         parm(1)=ch2_diam/2.   
         parm(3)=jacket_thickcap/2 
         call gsvolu('JACD','TUBE',tmed_jacket,parm,3,ivol)
         zpos=jacket_end-parm(3)-ztg
         call gspos('JACD',1,'TG  ', 0.,0.,zpos,0,'ONLY')
         parm(2)=parm(1)                            !downstream beam window
         parm(1)=0.0
         parm(3)=jacket_thickwindow/2
         call gsvolu('JACW','TUBE',tmed_mandrel,parm,3,ivol)
         call gspos('JACW',1,'TG  ', 0.,0.,zpos-4*parm(3),0,'ONLY')

         parm(2)=jacket_diam0/2                         !upstream vacuum jacket
         parm(1)=parm(2)-jacket_thick
         parm(3)=jacket_len0/2
         call gsvolu('JACU','TUBE',tmed_jacket,parm,3,ivol)
         zpos=jacket_end0-parm(3)-ztg
         call gspos('JACU',1,'TG  ', 0.,0.,zpos,0,'ONLY')

         parm(1)=jacket_diam/2-jacket_thick
         parm(2)=jacket_diam0/2-jacket_thick           !upstream flange of vacuum jacket
         parm(3)=jacket_thickflange/2
         call gsvolu('JACF','TUBE',tmed_jacket,parm,3,ivol)
         zpos=jacket_end0-parm(3)-ztg
         call gspos('JACF',1,'TG  ', 0.,0.,zpos,0,'ONLY')

         parm(1)=ipip_diam/2+ipip_thick
         parm(2)=jacket_diam0/2-jacket_thick       !downstream flange of dilution refrigerator
         parm(3)=jacket_thickflange/2
         call gsvolu('JACR','TUBE',tmed_jacket,parm,3,ivol)
         call gspos('JACR',1,'TG  ', 0.,0.,zpos+jacket_rfloff,0,'ONLY')

         parm(1)=ipip_diam/2+ipip_thick
         parm(2)=jacket_diam0/2-jacket_thick       !in(upstream) flange of dilution fridge
         parm(3)=jacket_thickflange/2
         call gsvolu('JACI','TUBE',tmed_jacket,parm,3,ivol)
         zpos=parm(3)-parvol(3)
         call gspos('JACI',1,'TG  ', 0.,0.,zpos,0,'ONLY')
      endif
      if(imat_ipip.gt.0) then
         parm(1)=0.0
         parm(2)=ipip_diam/2
         parm(3)=ipip_len/2
         call gsvolu('IBEM','TUBE',clas_med_vac,parm,3,ivol)
         zpos=parm(3)-parvol(3)
         call gspos('IBEM',1,'TG  ',0.,0.,zpos,0,'ONLY')
         par(1)=parm(2)
         par(2)=parm(2)+ipip_thick
         par(3)=parm(3)
         call gsvolu('IPIP','TUBE',tmed_ipip,par,3,ivol)
         call gspos('IPIP',1,'TG  ',0.,0.,zpos,0,'ONLY')
      endif
         
*
*  mixing chamber (adjacent to cell walls (if no walls=adjacent to target)
*
      if(imat_mixchamber.GT.0) then
         parm(1)=0.0
         parm(2)=mixer_diam/2                 ! He3/He4 bath
         parm(3)=mixer_len/2                  ! extends to upstream end of TG volume
         zposm=-parm(3)+mixer_len0-ztg +mixer_zoff
         call gsvolu('HE34','TUBE',tmed_he3he4,parm,3,ivol)
         call gspos('HE34',1,'TG  ', 0.,0.,zposm,0,'ONLY')

         parm(1)=parm(2)
         parm(2)=parm(2)+mixer_thick
         parm(3)=parm(3)
         call gsvolu('TMIX','TUBE',tmed_mixchamber,parm,3,ivol)
         call gspos('TMIX',1,'TG  ', 0.,0.,zposm,0,'ONLY')

         par(2)=parm(2)                     ! mixing chamber downstream cap
         par(1)=0.0
         par(3)=thickcell/4
         call gsvolu('TMXC','TUBE',tmed_mixchamber,par,3,ivol)
         call gspos('TMXC',1,'TG  ', 0.,0.,zposm+parm(3)+par(3),0,'ONLY')
      endif
*
*  butanol target
*
      pars(1)=0.0
      pars(2)=tgrad
      pars(3)=tglenby2
      call gsvolu('TARG','TUBE',tmed_tg,pars,3,ivol)

      if(imat_tgcell.GT.0) then
         pars(1)=0.0
         pars(2)=pars(2)+thickcell          ! target walls
         pars(3)=pars(3)+thickcell/2
         call gsvolu('TCEL','TUBE',tmed_tgcell,pars,3,ivol)
         parm(1)=cup_diam/2
         parm(2)=parm(1)+cup_thick
         parm(3)=cup_len/2
         call gsvolu('TCUP','TUBE',tmed_tgcell,parm,3,ivol)
         if(imat_mixchamber.gt.0) then
            zposm=-mixer_len/2+mixer_len0
            zpos=zposm+pars(3)
            call gspos('TCEL',1,'HE34', 0.,0.,zpos,0,'ONLY')
            call gspos('TARG',1,'TCEL', 0.,0.,0.,0,'ONLY')
            zpos=zposm-parm(3)
            call gspos('TCUP',1,'HE34', 0.,0.,zpos,0,'ONLY')
         else
            call gspos('TCEL',1,'TG  ', 0.,0.,-ztg,0,'ONLY')
            call gspos('TARG',1,'TCEL', 0.,0.,0.,0,'ONLY')
            zpos=-ztg-pars(3)-parm(3)-0.3
            call gspos('TCUP',1,'TG  ', 0.,0.,zpos,0,'ONLY')
         endif
      else
         if(imat_mixchamber.gt.0) then
            zposm=-mixer_len/2+mixer_len0+pars(3) 
            call gspos('TARG',1,'HE34', 0.,0.,zpos,0,'ONLY')
         else
            call gspos('TARG',1,'TG  ', 0.,0.,-ztg,0,'ONLY')
         endif
      endif
      tgpar_save(1) =tglenby2*2
      tgpar_save(2) =tgrad
      tgpar_save(6) =-tglenby2
      tgpar_save(7) =tg_pos_ff(3) +tgpar_save(6)
      tgpar_save(8) =tg_pos_ff(3) +tgpar_save(6)
      tgpar_save(9) =tg_pos_ff(3) +tgpar_save(7)
      tgpar_save(10)=xytg +tgrad

* mandrel and holding coils

      if(imat_mandrel.GT.0) then
         parm(2)=mandrel_diam/2
         parm(1)=parm(2) - mandrel_thick            ! mandrel
         parm(3)=mandrel_len/2     
         call gsvolu('TMAN','TUBE',tmed_mandrel,parm,3,ivol)
         zposm=coil_len/2+coil_pos-mandrel_len/2
         call gspos('TMAN',1,'TG  ', 0.,0.,zposm-ztg,0,'ONLY')

         par(1)=parm(2)-coil_thick                 ! coils grooved into mandrel
         par(2)=parm(2)
         par(3)=coil_len/2 
         call gsvolu('TCOI','TUBE',tmed_coils,par,3,ivol)
         call gspos('TCOI',1,'TMAN', 0.,0.,-zposm,0,'ONLY')

         pars(2)=parm(1)                    ! downstream cap
         pars(1)=carb_diam/2
         pars(3)=jacket_thickcap/2
         zposm=zposm+mandrel_len/2-pars(3)-ztg
         call gsvolu('TMNC','TUBE',tmed_mandrel,pars,3,ivol)
         call gspos('TMNC',1,'TG  ', 0.,0.,zposm,0,'ONLY')
         pars(2)=pars(1)                    ! downstream beam window
         pars(1)=0.0
         pars(3)=jacket_thickwindow/2
         call gsvolu('TMNW','TUBE',tmed_mandrel,pars,3,ivol)
         call gspos('TMNW',1,'TG  ', 0.,0.,zposm-3*pars(3),0,'ONLY')
      endif         

* carbon and ch2 targets
      if(imat_carbon.gt.0) then
        par(1)=0.0                          ! carbon target
        par(2)=carb_diam/2
        par(3)=carb_thick/2
        call gsvolu('TCAR','TUBE',tmed_carbon,par,3,ivol)
        zposm=coil_len/2+coil_pos+jacket_thickcap-ztg
        call gspos('TCAR',1,'TG  ', 0.,0.,zposm,0,'ONLY')
      endif
      if(imat_ch2.gt.0) then
         par(1)=0.0                                 !CH2 target
         par(2)=ch2_diam/2.   
         par(3)=ch2_thick/2
         call gsvolu('TCH2','TUBE',tmed_ch2,par,3,ivol)
         zpos=jacket_end+jacket_thickcap-ztg
         call gspos('TCH2',1,'TG  ', 0.,0.,zpos,0,'ONLY')
      endif

*
        
      call gsatt('TMAN','FILL',1)
      call gsatt('TMAN','COLO',4)
      call gsatt('HE34','FILL',1)
      call gsatt('HE34','COLO',7)
      call gsatt('TARG','FILL',1)
      call gsatt('TARG','COLO',2)

      
      end
