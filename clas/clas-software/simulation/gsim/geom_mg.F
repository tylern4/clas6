      subroutine geom_mg

c_begin_doc

c  Documentation for subroutine geom_mg

c  $Id: geom_mg.F,v 1.10 2000/01/12 20:07:05 feuerbac Exp $

c  Purpose:
c  --------
c     ******************************************************************
c     *                                                                *
c     *      Defines geometrie of the torus and mini-torus coils       *
c     *                                                                *
c     *       Author    M.Guckes  *******      (Oct 1987)              *
c     *                                                                *
c     *               MODIFIED: M.Guidal       (May 1994)              *	
c     *               ==================                               *
c     *                                                                *
c     *       Removal of the support structures                        *
c     *                                                                *
c     ******************************************************************


c  Major revisions:
c  ----------------
c     Elliott Wolin, College of William and Mary, 7-dec-95
c     Elliott Wolin, College of William and Mary, 5-dec-94
c_end_doc


      implicit none
      character*(*) cfile,crevis,cstate,cdate,cautho,crname,crauth
      character*132 crcsid
      parameter (cfile=  '$RCSfile: geom_mg.F,v $')
      parameter (crevis= '$Revision: 1.10 $')
      parameter (cstate= '$State: Exp $')
      parameter (cdate=  '$Date: 2000/01/12 20:07:05 $')
      parameter (cautho= '$Author: feuerbac $')
      parameter (crname= 'GEOM_MG')
      parameter (crauth= 'Elliott Wolin')
      data crcsid/'
     1$Id: geom_mg.F,v 1.10 2000/01/12 20:07:05 feuerbac Exp $
     1 '/
      save


c  include files:
c  --------------
#include "gsimpar.inc"
#include "gsimcontrol.inc"
#include "gcunit.inc"
#include "gclist.inc"
#include "gconst.inc"
#include "gcflag.inc"
#include "lcgeom.inc"
#include "cltmed.inc"
#include "bgrid.inc"
#include "ffpar_mg.inc"
#include "mgtmed.inc"
c_end_inc


c  local variables:
c  ----------------
      integer mxpgon,mxsml,ndata,tmed

#ifdef USE_OLD_TORUS
      parameter   (mxpgon=21, mxsml=22)
      parameter   (ndata=3*(mxsml-mxpgon))
      
      real amgm(5), trap(11), pgcoil(3,mxsml), pgsml(3,mxsml),
     1            tumir(3),zmir
#else
      parameter   (mxpgon=31, mxsml=22)   
      parameter   (ndata=3*(mxpgon-mxsml))   
      real amgm(5), trap(11), pgcoil(3,mxpgon), pgsml(3,mxpgon)

#endif

      character*4 conly, cname

#ifdef USE_OLD_TORUS

c  PGCOIL : Array of parameters for polygone of one coil:
c           Z-coordinate, minimum Y-coordinate, maximum Y-coordinate
      data pgcoil /  -148.5,  153.1,  153.1,   -125.8,  104.7,  201.6,
     1               -103.0,   95.6,  218.5,    -80.2,   99.4,  232.4,
     2                -57.4,  113.0,  243.5,    -34.6,  122.1,  252.2,
     3                -11.8,  125.5,  258.6,     10.9,  126.5,  262.9,
     4                 33.7,  122.1,  265.2,     56.5,  113.1,  265.4,
     5                 79.3,   98.3,  263.7,    102.1,   75.5,  259.9,
     6                125.4,   39.3,  254.0,    147.6,   22.4,  245.9,
     7                170.4,   19.6,  235.4,    193.2,   19.6,  222.3,
     8                216.0,   19.6,  206.2,    238.8,   19.4,  186.5,
     9                261.6,   19.6,  162.5,    284.3,   28.9,  132.5,
     9                307.1,   77.4,   77.4,
     9     ndata* 0./

#else

c  PGCOIL : Array of parameters for the polygons that form the outside
c           of the coil vacuum case, instead of only the coil itself.
c           Coordinates are derived from Oxford Autocad files:
c           0100-1.dxf 0200-1.dxf 0350-1.dxf
c           Z-coordinate, minimum Y-coordinate, maximum Y-coordinate
      data pgcoil /
     1     -175.0,  105.0,   168.98,
     2     -146.68, 105.0,   200.37,
     3     -133.0,   94.95,  214.37,
     4     -120.0,   87.74,  224.64,
     5     -100.0,   84.14,  237.90,
     6     -80.0,   87.66,  248.06,
     7     -60.0,   98.82,  258.75,
     8     -40.0,  108.18,  264.87,
     9     -20.0,  113.46,  272.00,
     &       0.0,  115.20,  277.61,
     1      20.0,  113.56,  278.61,
     2      40.0,  108.40,  280.63,
     3      60.0,   99.17,  279.01,
     4      80.0,   84.64,  278.78,
     5     100.0,   61.33,  274.54,
     6     120.0,   26.68,  271.21,
     7     128.5,   18.91,  269.05,
     8     128.5,   12.5,   269.05,
     9     141.0,   12.5,   266.40,
     &     141.0,    9.9,   266.40,
     1     148.5,    9.9,   261.91,
     2     148.5,    5.3,   261.91,
     3     160.0,    5.49,  257.74,
     4     180.0,    5.82,  248.26,
     5     200.0,    6.16,  235.29,
     6     220.0,    6.49,  223.21,
     7     240.0,    6.82,  207.16,
     8     260.75,   7.16,  187.42,
     9     293.0,    7.70,  155.17,
     &     293.0,   18.0 ,  155.17,
     1     326.0,   18.0 ,  120.10/

      real supp_ring1(5),supp_ring2(5),supp_ring3a(5),supp_ring3b(5)
      
c Big support ring on the up-stream end of clas. (drawing 0100-1.dxf )

      data supp_ring1 /105.0,136.5,27.5,-30.,30./   

c Small support ring at the down stream end of clas. (drawing 0426-1.dxf )

      data supp_ring2 /8.00,17.995,16.275,-30.,30./

c Small support ring just down stream of target. Consists of two rings (drawing 0426-1.dxf )

      data supp_ring3a /5.300,12.250,5.450,-30.,30./
      data supp_ring3b /5.600, 9.762,3.600,-30.,30./

#endif

c  PGSML  : Array of parameters for polygone of one coil:
c           Z-coordinate, minimum Y-coordinate, maximum Y-coordinate
      data pgsml  /  -200.0,    6.0,   25.5,    -65.5,    6.0,   25.5,
     1                -56.0,    6.0,   32.9,    -49.5,    6.0,   38.7,
     2                -42.9,    6.0,   43.3,    -36.3,    6.0,   46.9,
     3                -29.7,    6.0,   49.9,    -23.1,    6.0,   52.2,
     4                -16.5,    7.2,   53.8,     -9.9,   13.0,   55.0,
     5                 -3.2,   16.6,   55.5,      3.3,   16.6,   55.4,
     6                  9.9,   13.4,   55.9,     16.5,    7.1,   54.0,
     7                 23.1,    3.6,   52.4,     29.7,    3.3,   50.2,
     8                 36.3,    3.3,   47.2,     42.9,    3.3,   43.6,
     9                 49.5,    3.3,   39.9,     56.1,    3.3,   33.1,
     &                 62.7,    3.3,   25.8,     69.0,   10.0,   12.6,
     1               ndata*0./

c
c  Carbon fiber support tubes attached to the torus
c   x,z positions at endplate
      real carbon_tube(2,5), ct_radius, par(3)
c
      data carbon_tube /
     1     232.6,  -112.0,
     2     279.3,     7.5,
     3     268.7,   135.9,
     4     204.6,   244.3,
     5     148.1,   294.6 /
     
      data ct_radius/1.25/
      real cos30
      integer ct_rotm

c Data for the mini torus
c -----------------------

      REAL PMN1(5),PMN9(4),PMN8(3),PMN2(5),PFT1(3),PMN3(4),PMN4(3),PMIN(3),
     1     PMN5(5),PMN6(5),PMN7(3),PM10(3),PM11(5),PM12(5),PMA1(5),PMB1(5),
     1     PMC1(5),PMD1(5),PME1(5),PMA2(5),PMB2(5),PMC2(5),PMD2(5),PME2(5),
     1     PM13(5),PM14(3),PM15(5),PPSI(3),shl10(5)
      DATA PMIN/0.,65.,120./
      DATA PPSI/120.,60.,50./
      DATA PFT1/50.,60.,120./
      DATA PMN1/15.05,24.86,1.01,31.4,88./
      DATA PMA1/16.68,24.86,.05,31.4,115./
      DATA PMB1/18.316,24.86,.05,31.4,125./
      DATA PMC1/19.952,24.86,.05,31.4,130./
      DATA PMD1/21.588,24.86,.05,31.4,130./
      DATA PME1/23.224,24.86,.05,31.4,130./
      DATA PMA2/16.68,24.86,.05,31.4,115./
      DATA PMB2/18.316,24.86,.05,31.4,125./
      DATA PMC2/19.952,24.86,.05,31.4,130./
      DATA PMD2/21.588,24.86,.05,31.4,130./
      DATA PME2/23.224,24.86,.05,31.4,130./
      DATA PMN5/3.81,13.62,2.75,270.,40./
      DATA PMN9/2.75,1.01,4.908,21.295/
      DATA PMN3/.6,2.75,4.908,9.135/
      DATA PMN4/2.75,4.908,4.738/
      DATA PMN8/2.75,4.908,4.825/
      DATA PMN2/3.81,13.62,1.01,211.,270./
      DATA PMN6/73.75,83.56,2.75,41.,90./
      DATA PMN7/2.75,4.908,25.56/
      DATA PM10/1.,8.713,25.56/
      DATA PM11/57.75,73.75,1.,38.,90./
      DATA PM12/47.75,58.,1.,48.,90./
      DATA PM13/2.75,2.75,13.566,18.5288,6.3125/
      DATA PM14/2.75,13.56,22.15/
      DATA PM15/6.3,6.3,23.8,26.3,2.165/
      data shl10 /2.75, 2.75, 4.8, 10.4, 6.6/

c  TUMIR  : Array of parameters for the inner support ring (TUBE):
c  ZMIR   : Z-position of this ring
c      data     tumir,       zmir
C     1   /  10., 15., 75.,  200. /
c
c  The outer support structure is built up of two rings of trapezoids (TRD1)
c  ZMOR1/2: Z-position of the middle of first/second ring
c  XMOR1/2: X-position of the first/second ring, i.e. distance from the middle
c           of the detector to the middle of each trapezoid
c  HTHMOR : Half thickness of these rings (it's assumed to be the same for bot
C      real zmor1, zmor2, xmor1, xmor2, hthmor
C      data zmor1, zmor2, xmor1, xmor2, hthmor
C     1    /-150.0, 250.0, 165.0, 165.0,  4.0/

      integer get_rotm,coil_2,coil_rotm(10:10+nsect-1)

c these integers are needed for rotations of the mini torus

      integer minr_1,minr_2,minr_3,minr_4,minr_5,minr_6,minr_7

      real dangel,phi2,phi3
C
c  the following was automatically generated...ejw, W&M
      integer i,j,ivol,icoil,nmg,npgon
      real angel,x0,y0
C
      integer get_tmed,mate
c_end_var


c  executable code:
c  ----------------

#ifdef USE_OLD_TORUS
      call recmes(crname,'I','*** Old Torus Geometry.')
#else
      call recmes(crname,'I','*** New Torus Geometry.')
#endif

c  rotm around z with deg90 :  no 2.
      coil_2=get_rotm()
      call gsrotm(coil_2,90.,90.,90.,180.,0.,0.)

c  rotm around y with deg90 and z with i*degdangel
      dangel = 360.*rnsect
      do i=0,nsect-1
        phi2 = amod(90.+float(i)*dangel,360.)
        phi3 = amod(float(i)*dangel,360.)
        coil_rotm(10+i)=get_rotm()
        call gsrotm(coil_rotm(10+i),180.,90.,90.,phi2,90.,phi3)
      enddo

c rotation matrix for the mini torus
c ----------------------------------
      minr_1=get_rotm()
      CALL GSROTM(minr_1,0.,0.,90.,90.,90.,180.)
      minr_2=get_rotm()
      CALL GSROTM(minr_2,90.,60.,90.,150.,0.,0.)
      minr_3=get_rotm()
      CALL GSROTM(minr_3,90.,120.,90.,210.,0.,0.)
      minr_4=get_rotm()
      CALL GSROTM(minr_4,90.,180.,90.,270.,0.,0.)
      minr_5=get_rotm()
      CALL GSROTM(minr_5,90.,240.,90.,330.,0.,0.)
      minr_6=get_rotm()
      CALL GSROTM(minr_6,90.,300.,90.,30.,0.,0.)
      minr_7=get_rotm()
      CALL GSROTM(minr_7,90.,0.,120.5,90.,30.5,90.)

c  define magnet media

      torus_med_copper = get_tmed()
      if(nomate(toru))then
         mate=16
      else
         mate=11
      endif
      call gstmed(torus_med_copper,'Torus copper$', mate,
     1     0,init_ifield,init_fieldm,
     1     0.5,   0.5,   0.05, 0.005, 0.003, 0, 0)
	
      mini_med_copper = get_tmed()
      if(nomate(mini))then
         mate=16
      else
         mate=11
      endif
      call gstmed(mini_med_copper,'Mini-torus copper$', mate,
     1     0,init_ifield,init_fieldm,
     1     0.5,   0.5,   0.05, 0.005, 0.003, 0, 0)	



c  Find maximal boundaries of coil for mother volume
      amgm(1) = big
      amgm(2) = 0.
      amgm(4) = big
      amgm(5) = 0.

c  Big coil
      do  i=1,mxpgon
         amgm(1) = amin1(amgm(1), pgcoil(1,i)**2+pgcoil(2,i)**2)
         amgm(2) = amax1(amgm(2), pgcoil(1,i)**2+pgcoil(3,i)**2)
         do  j=1,2
            angel   = atan2(pgcoil(1,i),pgcoil(j+1,i)) + piby2
            amgm(4) = amin1(amgm(4), angel)
            amgm(5) = amax1(amgm(5), angel)
         enddo
      enddo

      amgm(1) = sqrt(amgm(1))
      amgm(2) = sqrt(amgm(2))
      amgm(3) = hthmg
      amgm(4) = raddeg*amgm(4)
      amgm(5) = raddeg*amgm(5)


c  Define and position mother volume at both sides of one sector
c  with minimal boundary.
c  Its exiting the mother volume but that doesn't matter,
c  since it's overlapping itself from the  next sector.
      if(helium_only)then
         tmed=clas_med_helium
      else
         tmed=clas_med_air
      endif
      call gsvolu('MG  ', 'TUBS',tmed, amgm, 5, ivol)
      do  i=1,2
         Call gspos('MG  ',I,'LSS-',0.,0.,0.,coil_rotm(13+I),'MANY')
      enddo

c
c     define and place the carbon fiber tubes -- going to use gsposp
c     because of differing lengths
c
      cos30 = cos(30.*acos(-1.)/180.)
      par(1) = 0.
      par(2) = ct_radius
      par(3) = 300.            ! to be changed with gsposp
      if (nomate(toru)) then
         tmed=clas_med_vac
      else
         tmed=clas_med_carbon
      endif

      call gsvolu('MGCT','TUBE',tmed,par,0,ivol)
      if (ivol.le.0) then
         call recmes(crname,'W','Cannot build MGCT geometry!')
      endif
      ct_rotm = get_rotm()
      call gsrotm(ct_rotm,0.,0.,90.,0.,90.,90.)
      do i=1,5
c        Determine 1/2 length of tube -- an equilateral triagle with
c        x as the length of one side.
         par(3) = carbon_tube(1,i)/2.
         call gsposp('MGCT', i, 'LSS-', carbon_tube(1,i)*cos30, 0.,
     $        carbon_tube(2,i), ct_rotm, 'ONLY', par, 3)
      enddo

c Place the three support rings.

#ifndef USE_OLD_TORUS
C
C Position the rings in LSS-, since they stick out of the MG mother volume.
C This has the added benefit that they don't need rotation.
C The 4 z-positions are the midpoint of the tube section. (from drawing
C  66100-E-02499 C.G. Riggs )
C
      call gsvolu('MGSR',   'TUBS',clas_med_iron,supp_ring1,0,IVOL)
      call gsposp('MGSR', 1,'LSS-',0.,0.,-202.5,0,'ONLY',supp_ring1,5)
      call gsposp('MGSR', 2,'LSS-',0.,0.,309.5,0,'ONLY' ,supp_ring2,5)
      call gsposp('MGSR', 3,'LSS-',0.,0.,134.4,0,'ONLY' ,supp_ring3a,5)
      call gsposp('MGSR', 4,'LSS-',0.,0.,144.7,0,'ONLY' ,supp_ring3b,5)
#endif
        
c geometry calls for the mini torus
c ---------------------------------

      if (.not.nogeom(mini)) then

         if(helium_only)then
            tmed=clas_med_helium
         else
            tmed=clas_med_air
         endif


c    these are mother volumes of the mini torus
c   --------------------------------------------
         CALL GSVOLU ('MINI','TUBE',tmed,PMIN,3,ivol)
         CALL GSPOS ('MINI',1,'CLAS',0.,0.,0.,0,'MANY')
         CALL GSVOLU ('FIT1','BOX ',tmed,PFT1,3,ivol)
         CALL GSPOS ('FIT1',1,'MINI',0.,0.,0.,0,'MANY')
         CALL GSVOLU ('FIT2','BOX ',tmed,PFT1,3,ivol)
         CALL GSPOS ('FIT2',1,'FIT1',0.,0.,0.,0,'MANY')
         CALL GSVOLU('CPSI', 'BOX ', tmed, PPSI, 3,IVOL)
         CALL GSPOS ('CPSI',1,'FIT1',0.,0.,0.,minr_1,'MANY')


         mini_med_aluminum = get_tmed()
         if(nomate(mini))then
            mate=16
         else
            mate=9
         endif
         call gstmed(mini_med_aluminum,'Mini-torus aluminum$', mate,
     1        0,init_ifield,init_fieldm,
     1        0.5,   0.5,   0.05, 0.005, 0.003, 0, 0)
         tmed=mini_med_aluminum

C   Each of these volumes is taken from Casin's drawings printed at 1:1 scale.
C   the reason for the large number of volumes is to accomplish the tapering 
C   that occurs in the coil near the target. This taper is accomplished with
C   volumes MI(A-E)(1-2).  It was felt that being this near to the target
C   required as accurate modeling as paossible.


         CALL GSVOLU ('MIN1','TUBS',tmed,PMN1,5,ivol)
         CALL GSPOS ('MIN1',1,'CPSI',.1,1.9,0.,0,'ONLY')
         CALL GSVOLU ('MIA1','TUBS',tmed,PMA1,5,ivol)
         CALL GSPOS ('MIA1',1,'CPSI',.1,1.9,1.06,0,'ONLY')
         CALL GSVOLU ('MIB1','TUBS',tmed,PMB1,5,ivol)
         CALL GSPOS ('MIB1',1,'CPSI',.1,1.9,1.16,0,'ONLY')
         CALL GSVOLU ('MIC1','TUBS',tmed,PMC1,5,ivol)
         CALL GSPOS ('MIC1',1,'CPSI',.1,1.9,1.26,0,'ONLY')
         CALL GSVOLU ('MID1','TUBS',tmed,PMD1,5,ivol)
         CALL GSPOS ('MID1',1,'CPSI',.1,1.9,1.36,0,'ONLY')
         CALL GSVOLU ('MIE1','TUBS',tmed,PME1,5,ivol)
         CALL GSPOS ('MIE1',1,'CPSI',.1,1.9,1.46,0,'ONLY')
         CALL GSVOLU ('MIA2','TUBS',tmed,PMA2,5,ivol)
         CALL GSPOS ('MIA2',1,'CPSI',.1,1.9,-1.06,0,'ONLY')
         CALL GSVOLU ('MIB2','TUBS',tmed,PMB2,5,ivol)
         CALL GSPOS ('MIB2',1,'CPSI',.1,1.9,-1.16,0,'ONLY')
         CALL GSVOLU ('MIC2','TUBS',tmed,PMC2,5,ivol)
         CALL GSPOS ('MIC2',1,'CPSI',.1,1.9,-1.26,0,'ONLY')
         CALL GSVOLU ('MID2','TUBS',tmed,PMD2,5,ivol)
         CALL GSPOS ('MID2',1,'CPSI',.1,1.9,-1.36,0,'ONLY')
         CALL GSVOLU ('MIE2','TUBS',tmed,PME2,5,ivol)
         CALL GSPOS ('MIE2',1,'CPSI',.1,1.9,-1.46,0,'ONLY')
         CALL GSVOLU ('MIN5','TUBS',tmed,PMN5,5,ivol)
         CALL GSPOS ('MIN5',1,'CPSI',52.3,16.92,0.,0,'ONLY')
         CALL GSVOLU ('MIN9','TRD1',tmed,PMN9,4,ivol)
         CALL GSPOS ('MIN9',1,'FIT1',0.,21.901,-20.735,0,'ONLY')
         CALL GSVOLU ('MIN8','BOX ',tmed,PMN8,3,ivol)
         CALL GSPOS ('MIN8',1,'FIT1',0.,21.901,-46.295,0,'ONLY')
         CALL GSVOLU ('MIN2','TUBS',tmed,PMN2,5,ivol)
         CALL GSPOS ('MIN2',1,'CPSI',24.56,16.92,0.,0,'MANY')
         CALL GSVOLU ('MIN3','TRD1',tmed,PMN3,4,ivol)
         CALL GSPOS ('MIN3',1,'FIT1',0.,8.205,33.689,0,'ONLY')
         CALL GSVOLU ('MIN4','BOX ',tmed,PMN4,3,ivol)
         CALL GSPOS ('MIN4',1,'FIT1',0.,8.205,47.562,0,'ONLY')
         CALL GSVOLU ('MIN6','TUBS',tmed,PMN6,5,ivol)
         CALL GSPOS ('MIN6',1,'CPSI',0.,-29.52,0.,0,'MANY')
         CALL GSVOLU ('MIN7','BOX ',tmed,PMN7,3,ivol)
         CALL GSPOS ('MIN7',1,'FIT1',0.,49.367,-25.56,0,'ONLY')
         CALL GSVOLU ('MI10','BOX ',tmed,PM10,3,ivol)
         CALL GSPOS ('MI10',1,'FIT1',0.,35.521,-25.56,0,'ONLY')
         CALL GSVOLU ('MI11','TUBS',tmed,PM11,5,ivol)
         CALL GSPOS ('MI11',1,'CPSI',0.,-29.52,0.,0,'MANY')
         CALL GSVOLU ('MI12','TUBS',tmed,PM12,5,ivol)
         CALL GSPOS ('MI12',1,'CPSI',0.,-29.95,0.,0,'MANY')
         CALL GSVOLU ('MI13','TRD2',tmed,PM13,5,ivol)
         CALL GSPOS ('MI13',1,'FIT1',0.,35.52125,-57.4325,0,'ONLY')
         CALL GSVOLU ('MI14','BOX ',tmed,PM14,3,ivol)
         CALL GSPOS ('MI14',1,'FIT1',0.,35.52125,-85.895,0,'ONLY')
         CALL GSVOLU ('MI15','TRD2',tmed,PM15,5,ivol)
         CALL GSPOS ('MI15',1,'CPSI',-74.8,0.,43.39,0,'MANY')
         CALL gsvolu('SH10','TRD2',clas_med_lead,shl10,5,IVOL)
         CALL gspos('SH10',1,'FIT2',0.,13.,63.6,minr_7,'MANY')
C   Now that one coil is defined in the FIT1 volume the other five
C   coils can be placed with a rotation matrix.

         CALL GSPOS ('FIT1',2,'MINI',0.,0.,0.,minr_2,'MANY')
         CALL GSPOS ('FIT1',3,'MINI',0.,0.,0.,minr_3,'MANY')
         CALL GSPOS ('FIT1',4,'MINI',0.,0.,0.,minr_4,'MANY')
         CALL GSPOS ('FIT1',5,'MINI',0.,0.,0.,minr_5,'MANY')
         CALL GSPOS ('FIT1',6,'MINI',0.,0.,0.,minr_6,'MANY')

      endif

c  coil built out of trapezoids.
      trap(1) = hthmg
      trap(2) =  0.
      trap(3) =  0.

c  Set parameters for big coil
      npgon = mxpgon-1
      conly = 'ONLY'
      cname = 'TORU'
      tmed=torus_med_copper


c  loop over both coils, including geom only if requested
      nmg=1
c      if (.not.nogeom(mini)) nmg=2

      do icoil=1,nmg
         if(((icoil.eq.1).and..not.nogeom(toru)) .or.
     1      ((icoil.eq.2).and..not.nogeom(mini)))
     1        call gsvolu(cname,'TRAP',tmed,trap,0,ivol)

         do  i=1,npgon
            trap(4) = (pgcoil(1,i+1) - pgcoil(1,i)) * 0.5
            trap(5) = (pgcoil(3,i) - pgcoil(2,i)) * 0.5
            trap(6) = (pgcoil(3,i+1) - pgcoil(2,i+1)) * 0.5
            trap(7) = raddeg * atan2(pgcoil(2,i+1) + pgcoil(3,i+1)
     1           - pgcoil(2,i)-pgcoil(3,i), 4.*trap(4))
            trap(8) = trap(4)
            trap(9) = trap(5)
            trap(10) = trap(6)
            trap(11) = trap(7)
            x0 = -0.5*(pgcoil(1,i)+pgcoil(1,i+1))
            y0 = 0.25* (pgcoil(2,i) + pgcoil(3,i) + pgcoil(2,i+1)
     1           + pgcoil(3,i+1))
         if( (((icoil.eq.1).and..not.nogeom(toru)) .or.
     1        ((icoil.eq.2).and..not.nogeom(mini)) ) .AND.
     1        (trap(4).ne.0.0) )
     1      call  gsposp(cname,i,'MG  ',x0,y0,0.,coil_2,conly,trap,11)
         enddo
         
         call ucopy(pgsml, pgcoil, 3*mxsml)
           
c   set parameters for small coil
         npgon = mxsml-1
         conly = 'MANY'
         cname = 'MINI'
         tmed=mini_med_aluminum
           
      enddo
      return
      end

c---------------------------------------------------------------------------------
