
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
c
c_end_doc


      implicit none
      character*(*) cfile,crevis,cstate,cdate,cautho,crname,crauth
      character*132 crcsid,crmess
      parameter (cfile=  '$RCSfile: geom_tg_frost.F,v $')
      parameter (crevis= '$Revision: 1.3 $')
      parameter (cstate= '$State: Exp $')
      parameter (cdate=  '$Date: 2004/11/01 20:13:09 $')
      parameter (cautho= '$Author: pcollins $')
      parameter (crname= 'GEOM_TG_FROST')
      parameter (crauth= 'fklein')
      data crcsid/'
     1$Id: geom_tg_frost.F,v 1.3 2004/11/01 20:13:09 pcollins Exp $
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
      real  xpos,ypos,zpos,xypos        !position of volumes
      real  xytg, ztg                   !position of target center
      real  tglenby2                    !half length of target cell incl. end caps
      real  tgrad                       !target radius w/o walls 
      integer imat_jacket, tmed_jacket
      integer imat_heatshield, tmed_heatshield
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
      real  par(20)              !dummy parameter list

*
*  default values for target geometry  
*  ----------------------------------
*
*target walls: alumaseal 241mu

      real tg_len, tg_diam, thickcell
      data tg_len,tg_diam,thickcell / 5.0, 1.5, 0.0241 /

*
*  mixing chamber with He3/He4 bath 

      real bath_thick,mixer_thick
      data bath_thick,mixer_thick / 0.5, 0.1 /
*
*  material: He3/He4 bath
      real A_he3he4(2),Z_he3he4(2),W_he3he4(2),D_he3he4(2),dens_he3he4 
      data A_he3he4 / 3.0 , 4.0 /
      data Z_he3he4 / 2.0 , 2.0 /
      data W_he3he4 / 0.5 , 0.5 /
      data D_he3he4 / 0.07, 0.125 /

*
*mandrel + heat shield: Al
*mandrel: D_ext = 40.0 mm, D_int = 38.0 mm, Coil length L = 250.0 mm << change to 21cm

      real mandrel_diam, mandrel_len, mandrel_thick 
      data mandrel_diam,mandrel_len,mandrel_thick / 4.0, 21.0, 0.1 /
      real coil_len, coil_thick
      data coil_len,coil_thick / 20.0, 0.05 /

* Monofilament Superconductor Nb 45 to 47 weight % Ti alloy placed
* in copper matrix (Cu to SC ratio is about 1.5 : 1.0).
      real A_coils(3), Z_coils(3), W_coils(3), D_coils(3), dens_coils
*                   Nb    Ti    Cu
      data A_coils / 92.9, 47.87, 63.54 /
      data Z_coils / 41. , 22.  , 29. /
      data W_coils / 0.47,  1.0 , 2.2 /     !Cu: 1.5x(0.47+1.0)
      data D_coils / 8.57, 4.54 , 8.96 / 

*
* 10K heat shield
      real shield_diam, shield_len, shield_thick
      data shield_diam,shield_len,shield_thick  / 5.8, 26.0, 0.1 /
      real shield_thickcap
      data shield_thickcap  / 0.1 /
      real shield_diamflange, shield_thickflange
      data shield_diamflange,shield_thickflange  / 10.0, 2.4 / 


*  vacuum jacket
*  chosen volume is 15cm upstream and 15cm downstream of target center
      real jacket_diam0, jacket_len0                !upstream part
      data jacket_diam0,jacket_len0 / 12.0, 30.0 /  
      real jacket_diam, jacket_len                  !downstream part
      data jacket_diam,jacket_len / 9.0, 25.0 / 
      real jacket_thick, jacket_thickcap, jacket_thickflange
      data jacket_thick,jacket_thickcap / 0.1, 0.1 /
      data jacket_thickflange / 2.0 /

c_end_var


c  executable code:
c  ----------------
      piby2   =acos(0.)
      twopiby3=acos(-1.)*2./3.

      imat_jacket    = imate_aluminum
      imat_heatshield= imate_aluminum
      imat_tgcell    = imate_alumaseal
      imat_mandrel   = imate_aluminum
      imat_mixchamber= imate_aluminum

c skip that for now (only switch on/off of materials via tg_cell_ff
c      if(tgpar_save(5).GE.0.0.AND.tgpar_save(5).LE.NMATE) 
c     &          imat_tgcell = IFIX(tgpar_save(5))
c      if(tg_ins_ff(1).GT.0.0) imat_jacket  = IFIX(tg_ins_ff(1))
c      if(tg_ins_ff(2).GT.0.0) imat_heatshield = IFIX(tg_ins_ff(2))
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
            imat_heatshield =0
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
      call recmes(crname,'I','Define geometry for FROzen Spin Target ...')
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
*  target walls:
*  
      if(imat_tgcell.GT.0) then
         call GFMATE(imat_tgcell,cmatcell,aa,zz,dens,radl,absl,parm,np)
         if(aa.gt.0) then
            tmed_tgcell=get_tmed()
            CALL GSTMED(tmed_tgcell,cmatcell,imat_tgcell, 
     &           1, init_ifield,init_fieldm,tmaxfd,
     &           stmax, deemax, epsil, stmin, 0 , 0 )
            call set_tpar(tmed_tgcell,tg_tmedcuts_ff)
         else
            imat_tgcell=0
         endif
      endif
      if(imat_tgcell.EQ.0) icellflg=2
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
         if(imat_mandrel.GT.0) then
            imat_mixchamber=imat_mandrel
            tmed_mixchamber=tmed_mandrel
         else
            call GFMATE(imat_mixchamber,cmate,aa,zz,dens,radl,absl,parm,np)
            if(aa.gt.0) then
               tmed_mixchamber=get_tmed()
               CALL GSTMED( tmed_mixchamber,cmate,imat_mixchamber,
     &              1, init_ifield,init_fieldm,tmaxfd,
     &              stmax, deemax, epsil, stmin, 0 , 0 )
               call set_tpar(tmed_mixchamber,tg_tmedcuts_ff)
            else
               imat_mixchamber=0
            endif
         endif
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
      endif
*
*  heat shield and vacuum jacket
*
      if(imat_heatshield.GT.0) then
         call GFMATE(imat_heatshield,cmate,aa,zz,dens,radl,absl,parm,np)
         if(aa.gt.0) then
            tmed_heatshield=get_tmed()
            CALL GSTMED( tmed_heatshield,cmate,imat_heatshield,
     &           1, init_ifield,init_fieldm,tmaxfd,
     &           stmax, deemax, epsil, stmin, 0 , 0 )
            call set_tpar(tmed_heatshield,tg_tmedcuts_ff)
         else
            imat_heatshield=0
         endif
      endif
      if(imat_jacket.GT.0) then
         call GFMATE(imat_jacket,cmate,aa,zz,dens,radl,absl,parm,np)
         if(aa.gt.0) then
            tmed_jacket=get_tmed()
            CALL GSTMED( tmed_jacket,cmate,imat_jacket,
     &           1, init_ifield,init_fieldm,tmaxfd,
     &           stmax, deemax, epsil, stmin, 0 , 0 )
            call set_tpar(tmed_jacket,tg_tmedcuts_ff)
         else
            imat_jacket=0
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
      if(imat_mixchamber.GT.0 .and. imat_heatshield.GT.0) then
           write(crmess,'(2A)') 'including mixing chamber, He3/He4 bath',
     &        ' and heat shield'
           call recmes(crname,'I',crmess)
      endif
*  
************************
*  geometry definitions
************************
*
*  (a) mother volume (vacuum)
*  --------------------------
*  target for photon beam is positioned in 'BEAM' (beam pipe)
*  put the whole target_cryostat structure into a vacuum tube
*  volume is chosen to cover 15cm upstream and 15cm downstream from target center
      parvol(1)=0.0
      parvol(2)=jacket_diam0/2       ! mother volume covers up+downstream vacuum jackets
      parvol(3)=jacket_len0/2
      call gsvolu('TG  ','TUBE',clas_med_vac,parvol,3,ivol)
      xytg=sqrt(tg_pos_ff(1)**2+tg_pos_ff(2)**2)
      ztg =0.0
      call gspos('TG  ',1,'BEAM', tg_pos_ff(1),tg_pos_ff(2),tg_pos_ff(3)+ztg,0,'ONLY')

      if(imat_jacket.GT.0) then
         parm(2)=jacket_diam/2
         parm(1)=parm(2) - jacket_thick           ! downstream vacuum jacket
         parm(3)=jacket_len/2.
         call gsvolu('JACK','TUBE',tmed_jacket,parm,3,ivol)
         zpos=parvol(3)-parm(3)
         call gspos('JACK',1,'TG  ', 0.,0.,zpos,0,'ONLY')
         
         parm(2)=parm(1)                            !vacuum jacket (downstream cap) 
         parm(1)=0.0   
         parm(3)=jacket_thickcap/2 
         call gsvolu('JACD','TUBE',tmed_jacket,parm,3,ivol)
         zpos=parvol(3)-parm(3)
         call gspos('JACD',1,'TG  ', 0.,0.,zpos,0,'ONLY')

         parm(1)=jacket_diam/2
         parm(2)=jacket_diam0/2                    !flange of vacuum jacket
         parm(3)=jacket_thickflange/2
         call gsvolu('JACF','TUBE',tmed_jacket,parm,3,ivol)
         zpos=parm(3)-parvol(3)+jacket_len0-jacket_len
         call gspos('JACF',1,'TG  ', 0.,0.,zpos,0,'ONLY')
      endif

      if(imat_heatshield.GT.0) then
         parm(2)=shield_diam/2
         parm(1)=parm(2) - shield_thick        ! 10K heat shield
         parm(3)=shield_len/2 
         call gsvolu('HTSH','TUBE',tmed_heatshield,parm,3,ivol)
         zpos=0.0
         call gspos('HTSH',1,'TG  ', 0.,0.,zpos,0,'ONLY')
         
         parm(1)=0.0
         parm(2)=parm(1)
         parm(3)=shield_thickcap/2         ! 10K heat shield (downstream cap)
         call gsvolu('HTSD','TUBE',tmed_heatshield,parm,3,ivol)
         zpos=shield_len/2
         call gspos('HTSD',1,'TG  ', 0.,0.,zpos,0,'ONLY')

         parm(1)=shield_diam/2             ! downstream flange of heat shield
         parm(2)=shield_diamflange/2
         parm(3)=shield_thickflange/2
         call gsvolu('HTSF','TUBE',tmed_heatshield,parm,3,ivol)
         zpos=parm(3)-parvol(3)+2.0
         call gspos('HTSF',1,'TG  ', 0.,0.,zpos,0,'ONLY')

      endif

*
*  butanol target
*
      pars(1)=0.0
      pars(2)=tgrad
      pars(3)=tglenby2
      call gsvolu('TARG','TUBE',tmed_tg,pars,3,ivol)
      zpos=0.0

      if(imat_tgcell.GT.0) then
         pars(1)=0.0
         pars(2)=pars(2)+thickcell          ! target walls
         pars(3)=pars(3)+thickcell
         call gsvolu('TCEL','TUBE',tmed_tgcell,pars,3,ivol)
         call gspos('TCEL',1,'TG  ', 0.,0.,zpos,0,'ONLY')
         call gspos('TARG',1,'TCEL', 0.,0.,0.,0,'ONLY')
      else
         call gspos('TARG',1,'TG  ', 0.,0.,zpos,0,'ONLY')
      endif
      tgpar_save(1) =tglenby2*2
      tgpar_save(2) =tgrad
      tgpar_save(6) =-tglenby2
      tgpar_save(7) =tg_pos_ff(3) +tgpar_save(6)
      tgpar_save(8) =tg_pos_ff(3) +tgpar_save(6)
      tgpar_save(9) =tg_pos_ff(3) +tgpar_save(7)
      tgpar_save(10)=xytg +tgrad

*
*  mixing chamber (adjacent to cell walls (if no walls=adjacent to target)
*
      if(imat_mixchamber.GT.0) then
         parm(1)=pars(2)
         parm(2)=pars(2)+bath_thick                 ! He3/He4 bath
         parm(3)=(pars(3)+bath_thick+parvol(3))/2   ! extends to upstream end of TG volume
         call gsvolu('HE34','TUBE',tmed_he3he4,parm,3,ivol)
         zpos=pars(3)+bath_thick-parm(3)
         call gspos('HE34',1,'TG  ', 0.,0.,zpos,0,'ONLY')

         parm(1)=parm(2)
         parm(2)=parm(2)+mixer_thick
         parm(3)=parm(3)
         call gsvolu('TMIX','TUBE',tmed_mixchamber,parm,3,ivol)
         call gspos('TMIX',1,'TG  ', 0.,0.,zpos,0,'ONLY')

         parm(1)=0.0
         parm(2)=pars(2)                     ! He3/He4 bath downstream end
         parm(3)=bath_thick/2
         call gsvolu('HF34','TUBE',tmed_he3he4,parm,3,ivol)
         zpos=parm(3)+pars(3)
         call gspos('HF34',1,'TG  ', 0.,0.,zpos,0,'ONLY')
      endif

      if(imat_mandrel.GT.0) then
         parm(2)=mandrel_diam/2
         parm(1)=parm(2) - mandrel_thick            ! mandrel
         parm(3)=(mandrel_len/2 + parvol(3))/2     
         call gsvolu('TMAN','TUBE',tmed_mandrel,parm,3,ivol)
         zpos=mandrel_len/2 - parm(3)
         call gspos('TMAN',1,'TG  ', 0.,0.,zpos,0,'ONLY')

         parm(1)=parm(2)
         parm(2)=parm(2)+coil_thick                 ! coils
         parm(3)=coil_len/2 
         call gsvolu('TCOI','TUBE',tmed_coils,parm,3,ivol)
         zpos=0.0
         call gspos('TCOI',1,'TG  ', 0.,0.,zpos,0,'ONLY')
      endif         

      call gsatt('TMAN','FILL',1)
      call gsatt('TMAN','COLO',4)
      call gsatt('HE34','FILL',1)
      call gsatt('HE34','COLO',7)
      call gsatt('HF34','FILL',1)
      call gsatt('HF34','COLO',7)
      call gsatt('TARG','FILL',1)
      call gsatt('TARG','COLO',2)
      
      end
