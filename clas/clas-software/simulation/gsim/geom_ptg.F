      subroutine geom_ptg
c     ==============================
c
c_begin_doc

      implicit none
      character*(*) cfile,crevis,cstate,cdate,cautho,crname,crauth
      character*132 crcsid
      parameter (cfile=  '$RCSfile: geom_ptg.F,v $')
      parameter (crevis= '$Revision: 1.9 $')
      parameter (cstate= '$State: Exp $')
      parameter (cdate=  '$Date: 2006/02/07 23:10:36 $')
      parameter (cautho= '$Author: avakian $')
      parameter (crname= 'GEOM_PTG')
      parameter (crauth= 'Elliott Wolin')
      data crcsid/'
     1$Id: geom_ptg.F,v 1.9 2006/02/07 23:10:36 avakian Exp $
     1 '/
      save


c  include files:
c  --------------
#include "gsimpar.inc"
#include "gsimcontrol.inc"
#include "gcunit.inc"
#include "gclist.inc"
#include "gconst.inc"
#include "ffpar.inc"
#include "ffpar_mg.inc"
#include "ffpar_ptg.inc"
#include "ffpar_tg.inc"
#include "ptgtmed.inc"
#include "lcgeom.inc"
#include "cltmed.inc"
c_end_inc


c  local variables:
c  ----------------
      real tube(3)

c  Geometry parameters (it should be the same geometry as in the field
c  calculation routine PTF)

       integer ivol,i,mate,get_mate,get_tmed,get_rotm,ptg_r1,ptg_r2,
     1         ptg_r3,ptg_r4,ptg_r5,foir_3,foir_4, rot_tarh,imax
       real tube_he_foil(3), cup_mini(3)
       real big_case(3)
       real ptg1(3), wndga(3), wndgb(3), casea(3), caseb(5), casec(5)
       real cased(5), casee(3), targ1(3), targ2(3), prad1(3),zz
       real pcon1(3), pcon2(3), shela(3), shelb(3), shelc(3)
       real sheld(3), shele(3), shelf(3), prad9(3), prad10(5), prad11(5)
       real prad12(5),prad13(5),prad14(5),prad15(5),prad16(5), pspa1(3)
       real pspa2(5), pspa3(5), pspa4(5), pspa5(5), pspa6(5), pspa7(5)
       real pspa8(5), ptied(3), ptie1(3), ptie2(3), pspb2(5), prac10(5)
       real pftar(3), targ3(3), pvac1(5), tcell(3), tcell_dvcs(3)
       real targ_cyl(3),targ_cyl_dvcs(3),targ_fw(3),targ_fw_dvcs(3) 
       real targ_bw(3), targ_bw_dvcs(3), tube_he(3), tgv1(3), tgc1(3)
       real tgw1(3), bwnd1(3), bwnd2(3), bwnd3(3), bwnd4(3), par(10)
       real banjo_he_dvcs(3),banjo_entr_win_dvcs(3),banjo_exit_win_dvcs(3)
       real pvac1_dvcs(5),bwnd1_dvcs(3),bwnd2_dvcs(3),bwnd3_dvcs(3)
       real bwnd4_dvcs(3),bwnd5_dvcs(3),bwnd6_dvcs(3),sup2_dvcs(3)
       real sup4_dvcs(3), sup_lateral_dvcs(5)
       real sup5_dvcs(3),sup6_dvcs(3),shield_dvcs(3)
       real ovc_dvcs(5),vac_alu_dvcs(5),vac_cu_dvcs(5)
       real dz,rmin1,rmax1,rmin2,rmax2,pos,beta
       real radius,alpha0,alpha,pi2,x,ab,delta,gamma,dztot
       character*4 name
       character*3 nome
       character*2 nome1
       character*2 nome2
       character*2 nome3

c   
c       data ptg1 /0.,100.,31./
c__       data ptg1 /0.,50.,23./
       data ptg1 /0.,50.,28./
       data tcell /0., 0.75, 0.5 /
       data targ_cyl /0.751, 0.756,0.5/ ! 50 mk Al tube
       data targ_fw /0., 0.756,0.0025/ ! 50 mk Al window
       data targ_bw /0., 0.756,0.0025/ ! 50 mk Al window
       data tube_he /0., 0.95, 20./    !  He tube around the target
       data tube_he_foil /0.951, 0.958,20./ ! Al around He tube
       data cup_mini /0.9323, 0.945,8./ ! kapton minicup
       data tgv1 /0., 0.8, 9.0/      ! vacuume beam pipe before target
       data tgc1 /0.801, 0.808, 9.0/  !  beam pipe before target
       data tgw1 /0., 0.808, 0.0035/  !  beam pipe before target
c
       data pvac1 /11.25, 0., 1.5, 0., 24.0/  ! banjo vacuume volume
       data bwnd1 /0., 1.49, 0.0035/  ! banjo window1
       data bwnd2 /0., 6.49, 0.001/  ! banjo window2
       data bwnd3 /0., 13.49, 0.0025/  ! banjo window3
       data bwnd4 /0., 21.4, 0.0035/  ! banjo window4

       data tcell_dvcs /0., 0.725, 0.75 /
       data targ_cyl_dvcs /0.726, 0.75,0.75/ ! Kapton tube
       data targ_fw_dvcs /0., 0.75,0.00165/ ! Kapton window
       data targ_bw_dvcs /0., 0.75,0.00165/ ! Kapton window

       data banjo_he_dvcs /0., 7.939,3.809/ ! Helium4 inside banjo
       data banjo_entr_win_dvcs /0.,1.1,0.00355/ ! Banjo entrance window
       data banjo_exit_win_dvcs /0.,2.1,0.00355/ ! Banjo exit window

       data pvac1_dvcs /2.8, 0., 2.1, 0., 7.939/
       data vac_alu_dvcs /10.89645, 2.1,2.105,24.9722,24.975/
       data vac_cu_dvcs /8.2,10.1,10.26,29.24,29.4/
       data sup_lateral_dvcs /8.2,10.099,9.949,29.2399,29.089/
       data bwnd1_dvcs /0., 2.1, 0.00355/
       data bwnd2_dvcs /0., 7.94, 0.0003/
       data bwnd3_dvcs /0., 9., 0.00125/
       data bwnd4_dvcs /0., 1.9, 0.005/
       data bwnd5_dvcs /0., 1.9, 0.006/
       data bwnd6_dvcs /0., 1.25, 0.0025/

       data sup2_dvcs /1.905,8.,0.006/
       data sup4_dvcs /1.905,10.7,0.006/

       data sup5_dvcs /0.,21.35,0.003/
       data sup6_dvcs /0.,24.06,0.0045/
       data shield_dvcs /0.,23.07,0.0057/

       data big_case /35.94,37.53,23.96/

       data wndga /16.5,21.9,2.38/
       data wndgb /11.6,15.1,1.5/
       data casea /10.4,25.,2.64/
       data caseb /2.88,10.4,25.,17.5,25./
       data casec /2.88,17.5,25.,10.4,25./
c    Modified by S. Niccolai for eg1-dvcs 
c      data cased /3.1,17.5,17.75,24.75,25./
       data cased /5.75,10.3,10.6,24.5,24.7/
       data casee /24.5,25.,3.1/
       data targ1 /.953,9.53,.635/
       data targ2 /7.94,9.53,3.81/
       data prad1 /30.5,32.5,1.0/
       data prad9 /29.5,30.,7.75/
       data prad10 /29.5,30.,5.,0.,80./
       data prac10 /29.5,30.,5.,100.,360./
       data prad11 /29.5,30.,10.,25.,35./
       data prad12 /29.5,30.,10.,75.,105./
       data prad13 /29.5,30.,10.,145.,155./
       data prad14 /29.5,30.,10.,205.,215./
       data prad15 /29.5,30.,10.,265.,275./
       data prad16 /29.5,30.,10.,325.,335./
       data pcon1 /34.,44.,1.25/
       data pcon2 /38.,44.,1.25/
       data pspa1 /32.25,32.5,11/
       data pspa2 /32.25,32.5,11,0.,80./
       data pspb2 /32.25,32.5,11,100.,360./
       data pspa3 /32.25,32.5,9.2,25.,35./
       data pspa4 /32.25,32.5,9.2,75.,105./
       data pspa5 /32.25,32.5,9.2,145.,155./
       data pspa6 /32.25,32.5,9.2,205.,215./
       data pspa7 /32.25,32.5,9.2,265.,275./
       data pspa8 /32.25,32.5,9.2,325.,335./
       data shela /10.,1.,29./
       data shelb /1.,2.75,11./
       data shelc /1.,21.5,9./
       data sheld /2.75,1.,11./
       data shele /1.5,1.,11./
       data shelf /15.75,1.,9./
       data ptied /.9,.9,19./
       data ptie1 /.9,.9,1.25/
       data ptie2 /0.,.6,.35/
       data pftar /.2,.2,.2/

c_end_var

       print *,'geom_ptg test:',
     6 clas_med_helium4,clas_med_copper,clas_med_iron,clas_med_alu,
     6 clas_med_air, clas_med_vac, ptg_med_nh3, ptg_med_nd3,clas_med_kapton,
     6 tg_type_ff, clas_med_stainless_steel

c
c define rotation for faces of outside shield
       ptg_r1=get_rotm()
       call gsrotm(ptg_r1,90.,30.,90.,120.,0.,0.)
       ptg_r2=get_rotm()
       call gsrotm(ptg_r2,90.,330.,90.,60.,0.,0.)
c define rotation for tie downs on coil casing
       ptg_r3=get_rotm()
       call gsrotm(ptg_r3,90.,0.,40.,90.,50.,270.)
       ptg_r4=get_rotm()
       call gsrotm(ptg_r4,90.,120.,40.,210.,50.,30.)
       ptg_r5=get_rotm()
       call gsrotm(ptg_r5,90.,240.,40.,330.,50.,150.)
c define rotations for shield fins
      foir_3=get_rotm()
      CALL GSROTM(foir_3,90.,60.,90.,150.,0.,0.)
      foir_4=get_rotm()
      CALL GSROTM(foir_4,90.,120.,90.,210.,0.,0.)
      rot_tarh = get_rotm()
      CALL GSROTM(rot_tarh, 90., 0.,180.,0.,90., 90.)

      if(tg_type_ff.ne.28.and.tg_type_ff.ne.31..and.tg_type_ff.ne.32) call gsvolu('PTG ','TUBE',clas_med_air,ptg1,3,IVOL)
      if(tg_type_ff.eq.28.or.tg_type_ff.eq.31.or.tg_type_ff.eq.32) call gsvolu('PTG ','TUBE',clas_med_vac,ptg1,3,IVOL)

c
c put the PTG volume in mg_pos_ff (the same as tg_pos for PTG
c
      if(mg_pos_ff(3).ne.tg_pos_ff(3)) then
       print *,'GEOM_PTG: ERROR wrong position',tg_pos_ff(3),mg_pos_ff(3)
       stop
      endif

      call gspos('PTG ',1,'CLAS',tg_pos_ff(1),tg_pos_ff(2),
     6tg_pos_ff(3),0,'ONLY')

      if(tg_type_ff.ne.28.and.tg_type_ff.ne.31.and.tg_type_ff.ne.32) then

c
c     He around the target :
c
      call gsvolu('TARH', 'TUBE', clas_med_helium4, tube_he, 3,IVOL)
      call gspos('TARH',1,'PTG ', 0.,0.,0.,rot_tarh,'MANY')
      call gsvolu('TGHA', 'TUBE', clas_med_alu, tube_he_foil, 3,IVOL)
      call gspos('TGHA',1,'PTG ', 0.,0.,0.,rot_tarh,'ONLY')
      call gsvolu('MCUP', 'TUBE', clas_med_kapton, cup_mini, 3,IVOL)
      call gspos('MCUP',1,'PTG ', 0.,0.,0.,rot_tarh,'ONLY')
c     ========================= 
c
c       Target cell itself
c
      call gsvolu('TARG', 'TUBE', ptg_med_nh3, tcell, 3,IVOL)
      call gspos('TARG',1,'PTG ', 0.,0.,0.,0,'ONLY')
c
      call gsvolu('TARC', 'TUBE', clas_med_alu, targ_cyl, 3,IVOL)
      call gspos('TARC',1,'PTG ', 0.,0.,0.,0,'ONLY')
c
      call gsvolu('TARF', 'TUBE', clas_med_alu, targ_fw, 3,IVOL)
      call gspos('TARF',1,'PTG ', 0.,0.,0.5035,0,'ONLY')
c
      call gsvolu('TARB', 'TUBE', clas_med_alu, targ_bw, 3,IVOL)
      call gspos('TARB',1,'PTG ', 0.,0.,-0.5035,0,'ONLY')
c    ===========================
c
c   Beam pipe before the target
c
      call gsvolu('TGV1','TUBE',clas_med_vac, tgv1,3,IVOL)
      call gspos('TGV1',1,'PTG ', 0., 0., -10.0, 0, 'ONLY')
      call gsvolu('TGC1','TUBE',clas_med_alu, tgc1,3,IVOL)
      call gspos('TGC1',1,'PTG ', 0., 0., -10.0, 0, 'ONLY')
      call gsvolu('TGW1','TUBE',clas_med_alu, tgw1,3,IVOL)
      call gspos('TGW1',1,'PTG ', 0., 0., -1.0036, 0, 'ONLY')
c
c    ===========================
c
c     Banjo ( Gitara? ) vacuume volume ;-)
c
      call gsvolu('VACT','CONE',clas_med_vac,pvac1,5,IVOL)
      call gspos('VACT',1,'PTG ',0.,0.,12.25,0,'ONLY')
      call gsvolu('BWN1','TUBE',clas_med_alu,bwnd1,3,IVOL)
      call gspos('BWN1',1,'VACT',0.,0.,-9.964,0,'ONLY')
      call gsvolu('BWN2','TUBE',clas_med_alu,bwnd2,3,IVOL)
      call gspos('BWN2',1,'VACT',0.,0.,-5.0,0,'ONLY')
      call gsvolu('BWN3','TUBE',clas_med_alu,bwnd3,3,IVOL)
      call gspos('BWN3',1,'VACT',0.,0.,2.0,0,'ONLY')
      call gsvolu('BWN4','TUBE',clas_med_alu,bwnd4,3,IVOL)
      call gspos('BWN4',1,'VACT',0.,0., 9.8, 0,'ONLY')

      endif

      if(tg_type_ff.eq.28.or.tg_type_ff.eq.31.or.tg_type_ff.eq.32) then

c
c       Target cell itself
c
      if(tg_type_ff.ne.32)call gsvolu('PTAR', 'TUBE', ptg_med_nh3, tcell_dvcs, 3,IVOL)
      if(tg_type_ff.eq.32)call gsvolu('PTAR', 'TUBE', ptg_med_nd3, tcell_dvcs, 3,IVOL)
      call gspos('PTAR',1,'PTG ', 0.,0.,0.,0,'ONLY')
c
      call gsvolu('TARC', 'TUBE', clas_med_kapton, targ_cyl_dvcs, 3,IVOL)
      call gspos('TARC',1,'PTG ', 0.,0.,0.,0,'ONLY')
c
      call gsvolu('TARF', 'TUBE', clas_med_kapton, targ_fw_dvcs, 3,IVOL)
      call gspos('TARF',1,'PTG ', 0.,0.,0.751,0,'ONLY')
c
      call gsvolu('TARB', 'TUBE', clas_med_kapton, targ_bw_dvcs, 3,IVOL)
      call gspos('TARB',1,'PTG ', 0.,0.,-0.751,0,'ONLY')

c     Banjo
      call gsvolu('BANJ', 'TUBE', clas_med_helium4, banjo_he_dvcs,3,IVOL)
      call gspos('BANJ',1,'PTG ', 0.,0.,0.,0,'MANY')
      call gsvolu('BENW', 'TUBE', clas_med_alu, banjo_entr_win_dvcs,3,IVOL)
      call gspos('BENW',1,'PTG ', 0.,0.,-1.3,0,'ONLY')
      call gsvolu('BEXW', 'TUBE', clas_med_alu, banjo_exit_win_dvcs,3,IVOL)
      call gspos('BEXW',1,'PTG ', 0.,0.,0.9,0,'ONLY')

c
c     Banjo vacuum volume
c
      if(tg_type_ff.eq.28) then 
         call gsvolu('VACU','CONE',clas_med_copper,vac_cu_dvcs,5,IVOL)
         call gspos('VACU',1,'PTG ',0.,0.,17.5,0,'ONLY')
         call gsvolu('SUPL','CONE',clas_med_cerexmyl1,sup_lateral_dvcs,5,IVOL)
         call gspos('SUPL',1,'PTG ',0.,0.,17.5,0,'ONLY')
      endif

      call gsvolu('VACT','CONE',clas_med_vac,pvac1_dvcs,5,IVOL)
      call gspos('VACT',1,'PTG ',0.,0.,3.7,0,'ONLY')
      call gsvolu('BWN2','TUBE',clas_med_alumylar,bwnd2_dvcs,3,IVOL)
      call gspos('BWN2',1,'PTG ',0.,0.,6.501,0,'ONLY')
      call gsvolu('SUP2','TUBE',clas_med_cerexmyl1,sup2_dvcs,3,IVOL)
      call gspos('SUP2',1,'PTG ',0.,0.,7.1,0,'ONLY')
      call gsvolu('BWN3','TUBE',clas_med_alu,bwnd3_dvcs,3,IVOL)
      call gspos('BWN3',1,'PTG ',0.,0.,8.,0,'ONLY')
      call gsvolu('BWN4','TUBE',clas_med_alumylar,bwnd4_dvcs,3,IVOL)
      call gspos('BWN4',1,'PTG ',0.,0.,7.1,0,'ONLY')

      if(tg_type_ff.eq.28) then 
         call gsvolu('SUP4','TUBE',clas_med_cerexmyl1,sup4_dvcs,3,IVOL)
         call gspos('SUP4',1,'PTG ',0.,0.,10.1,0,'ONLY')
         call gsvolu('BWN5','TUBE',clas_med_alumylar,bwnd5_dvcs,3,IVOL)
         call gspos('BWN5',1,'PTG ',0.,0.,10.1,0,'ONLY')
      endif

      imax = 20
      x = 0.
      beta = 22.44
      alpha0 = (180.-beta/imax)/2.
      radius = 92.2422
      pi2=acos(0.)
      ab = 2*radius*sin(pi2/90.*beta/(2.*imax))
      rmin2 = 0
      pos = 22.7
      dztot = 0
      do i=1,imax
         alpha = alpha0 - x
         gamma = 90. - alpha
         delta = alpha0 - gamma
         x = 90. - delta
         dz = ab*cos(pi2/90.*alpha)/2
         rmin1 = rmin2
         rmax1 = rmin1+0.05
         rmin2= rmax1+ab*sin(pi2/90.*alpha)
         rmax2=rmin2+0.05
         dztot = dztot+dz*2
         if(i.lt.10)then
            nome='CON'
            write(nome2,'(I1)') i
            name=nome // nome2
         elseif(i.ge.10)then
            nome1='CO'
            write(nome3,'(I2)') i
            name=nome1 // nome3
         endif
         ovc_dvcs(1) = dz
         ovc_dvcs(2) = rmin1
         ovc_dvcs(3) = rmax1
         ovc_dvcs(4) = rmin2
         ovc_dvcs(5) = rmax2
         pos = pos+2*dz

         call gsvolu(name,'CONE',clas_med_alu,ovc_dvcs,6,IVOL)
         call gspos(name,1,'PTG ',0.,0.,pos,0,'ONLY')
      enddo

      if(tg_type_ff.eq.31.or.tg_type_ff.eq.32) then 
      
c  superinsulation blanket 10 layers, placed at z=18
         call gsvolu('SUP5','TUBE',clas_med_alumylar,sup5_dvcs,3,IVOL)
         call gspos('SUP5',1,'PTG ',0.,0.,18.,0,'ONLY')
         
c    80 K heat shield (aluminum), placed at z=19.4, 3 layers, 38 microns/layer thick
         call gsvolu('SHIE','TUBE',clas_med_alu,shield_dvcs,3,IVOL)
         call gspos('SHIE',1,'PTG ',0.,0.,19.4,0,'ONLY')

c  superinsulation blanket (aluminized mylar and 2 ply cerex), placed at z=20.1, 15 layers
         call gsvolu('SUP6','TUBE',clas_med_alumylar,sup6_dvcs,3,IVOL)
         call gspos('SUP6',1,'PTG ',0.,0.,20.1,0,'ONLY')
      endif
      endif
c    ===========================
c
c    Magnet settings ?
c
c Modifications, affecting all configurations, by S. Niccolai:
c added MBOX, which is the outer case of the megnet
c replaced the iron elements with stainless steel ones
      call gsvolu('MBOX','TUBE',clas_med_stainless_steel,big_case,3,IVOL)
      call gspos('MBOX',1,'PTG ',0.,0.,0.,0,'ONLY')
      call gsvolu('WDG1','TUBE',clas_med_copper,wndga,3,IVOL)
      call gspos('WDG1',1,'PTG ',0.,0.,-9.58,0,'ONLY')
      call gspos('WDG1',2,'PTG ',0.,0.,9.58,0,'ONLY')
      call gsvolu('WDG2','TUBE',clas_med_copper,wndgb,3,IVOL)
      call gspos('WDG2',1,'PTG ',0.,0.,-6.68,0,'ONLY')
      call gspos('WDG2',2,'PTG ',0.,0.,6.68,0,'ONLY')
      call gsvolu('CAS1','TUBE',clas_med_stainless_steel,casea,3,IVOL)
      call gspos('CAS1',1,'PTG ',0.,0.,-5.88,0,'MANY')
      call gspos('CAS1',2,'PTG ',0.,0.,5.88,0,'MANY')
      call gsvolu('CAS2','CONE',clas_med_stainless_steel,caseb,5,IVOL)
      call gspos('CAS2',1,'PTG ',0.,0.,11.4,0,'MANY')
      call gsvolu('CAS3','CONE',clas_med_stainless_steel,casec,5,IVOL)
      call gspos('CAS3',1,'PTG ',0.,0.,-11.4,0,'MANY')
      call gsvolu('CAS4','CONE',clas_med_stainless_steel,cased,5,IVOL)
      call gspos('CAS4',1,'PTG ',0.,0.,14.8,0,'MANY')
      call gsvolu('CAS5','TUBE',clas_med_stainless_steel,casee,3,IVOL)
      call gspos('CAS5',1,'PTG ',0.,0.,17.4,0,'MANY')
      call gsvolu('TARR','TUBE',clas_med_stainless_steel,targ1,3,IVOL)
      call gspos('TARR',1,'PTG ',0.,0.,-4.445,0,'ONLY')
      call gsvolu('TAR2','TUBE',clas_med_stainless_steel,targ2,3,IVOL)
      call gspos('TAR2',1,'PTG ',0.,0.,0.,0,'ONLY')
c ========================================================
      call gsvolu('RAD1','TUBE',clas_med_alu,prad1,3,IVOL)
      call gspos('RAD1',1,'PTG ',0.,0.,27.0,0,'ONLY')
      call gsvolu('RAD9','TUBE',clas_med_alu,prad9,3,IVOL)
      call gspos('RAD9',1,'PTG ',0.,0.,17.75,0,'ONLY')
      call gsvolu('RA10','TUBS',clas_med_alu,prad10,5,IVOL)
      call gspos('RA10',1,'PTG ',0.,0.,-17.7,0,'ONLY')
      call gsvolu('RB10','TUBS',clas_med_alu,prac10,5,IVOL)
      call gspos('RB10',1,'PTG ',0.,0.,-17.7,0,'ONLY')
      call gsvolu('RA11','TUBS',clas_med_alu,prad11,5,IVOL)
      call gspos('RA11',1,'PTG ',0.,0.,0.,0,'ONLY')
      call gsvolu('RA12','TUBS',clas_med_alu,prad12,5,IVOL)
      call gspos('RA12',1,'PTG ',0.,0.,0.,0,'ONLY')
      call gsvolu('RA13','TUBS',clas_med_alu,prad13,5,IVOL)
      call gspos('RA13',1,'PTG ',0.,0.,0.,0,'ONLY')
      call gsvolu('RA14','TUBS',clas_med_alu,prad14,5,IVOL)
      call gspos('RA14',1,'PTG ',0.,0.,0.,0,'ONLY')
      call gsvolu('RA15','TUBS',clas_med_alu,prad15,5,IVOL)
      call gspos('RA15',1,'PTG ',0.,0.,0.,0,'ONLY')
      call gsvolu('RA16','TUBS',clas_med_alu,prad16,5,IVOL)
      call gspos('RA16',1,'PTG ',0.,0.,0.,0,'ONLY')
c ========================================================
      call gsvolu('SPA3','TUBS',clas_med_alu,pspa3,5,IVOL)
      call gspos('SPA3',1,'PTG ',0.,0.,0.,0,'ONLY')
      call gsvolu('SPA4','TUBS',clas_med_alu,pspa4,5,IVOL)
      call gspos('SPA4',1,'PTG ',0.,0.,0.,0,'ONLY')
      call gsvolu('SPA5','TUBS',clas_med_alu,pspa5,5,IVOL)
      call gspos('SPA5',1,'PTG ',0.,0.,0.,0,'ONLY')
      call gsvolu('SPA6','TUBS',clas_med_alu,pspa6,5,IVOL)
      call gspos('SPA6',1,'PTG ',0.,0.,0.,0,'ONLY')
      call gsvolu('SPA7','TUBS',clas_med_alu,pspa7,5,IVOL)
      call gspos('SPA7',1,'PTG ',0.,0.,0.,0,'ONLY')
      call gsvolu('SPA8','TUBS',clas_med_alu,pspa8,5,IVOL)
      call gspos('SPA8',1,'PTG ',0.,0.,0.,0,'ONLY')
c **************************************************************
 11    continue
      print *, 'Polarized Target Geometry is defined'
      end
