C======================================================================
      SUBROUTINE trk_geom
C----------------------------------------------------------------------
*_begin_doc
*  RCS ID string
*  $Id: trk_geom.F,v 1.20 2008/04/21 00:52:01 fklein Exp $
*
*  Documentation for subroutine TRK_GEOM.F
*
*  Purpose:  This routine reads all detector Geometry parameters
*  -------   and fills the TRK geometry array. 
*            (old SDA Geometry initialization and/or Write BOS bank)
*
*  Input Parameters:  Block DATA in this routine
*  ----------------
*
*  Output Parameters: COMMON/sdaswim/
*  -----------------
*
*  Other routines: sda_scgeom
*  ---------------
*
*  Notes:   This routine is the sda_geom routine modified for recsis.
*  ------
*
*  Author:         Francois Roudot 
*  -------         
*
*  Creation Date:  July 15, 1996
*  --------------
*
*  Major revisions:
*  ----------------
*     6/10/98:  no offsets for planes! distance always from (0.,0.,0.)
*
*_end_doc
*
      Implicit None
      Save
*
*
*_begin_inc
*  include files :
*  ---------------------
#include "sdaswim.inc"
#include "bcs.inc"
#include "wcs.inc"
#include "trktcl_var.inc"
#include "clas_offsets.inc"
*
*_end_inc
*
*_begin_var
*  input/output variables:
*  -----------------------
*
*  Local pre-defined variables:
*  ---------------------------
*  RCS information: 
      CHARACTER*132  CFILE, CREVIS, CSTATE, CDATE, CAUTHO, CRCSID
      PARAMETER ( CFILE = '$RCSfile: trk_geom.F,v $')
      PARAMETER ( CREVIS = '$Revision: 1.20 $')
      PARAMETER ( CSTATE = '$State: Exp $')
      PARAMETER ( CDATE = '$Date: 2008/04/21 00:52:01 $')
      PARAMETER ( CAUTHO = '$Author: fklein $')
      DATA CRCSID/
     1'$Id: trk_geom.F,v 1.20 2008/04/21 00:52:01 fklein Exp $
     3'/
*  Module information:
      CHARACTER*132  CRNAME, CRAUTH
      CHARACTER*256  CRMESS
      PARAMETER (CRNAME='TRK_GEOM')
      PARAMETER (CRAUTH='Francois H. Roudot')
*
*  Local User defined variables:
*  -----------------------------
C
      INTEGER MLINK, MAMIND
      INTEGER ilun,iret,isec
      INTEGER i, ip, ind_sc, j, k,ipl, il, iplane, icoor

c BOS manipulation for DCGM bank
      INTEGER ind_dcgm, ncol_dcgm, nami_dcgm, ipt_gm
      data nami_dcgm/0/

      REAL dist, ort(3), dcs_ort(3), degrad, psi, len_posZ
      REAL pi, twopi2six, sinsec(6), cossec(6)
      real phi_mid, phi_min, phi_max
      CHARACTER*3 DET(npln)
      DATA DET /'VTX','ST1','ST2',
     &		6*'DC1',6*'DC2',6*'DC3',6*'DC4',6*'DC5',6*'DC6',
     &		'CC ','SC1','SC2','SC3','SC4','EC1','EC2'/

C Target&Start Counter
      REAL r_tgst(3),d_tgst(3), x0_tgst(3,3)
      REAL phi_tgst_ort(3), phi_tgst_min(3),phi_tgst_max(3)
      integer ind_stgm,nami_stgm, ind_stgm_sec
      data ind_stgm,nami_stgm /0,0/
C Cerenkov Counter
      REAL r_cc,d_cc, x0_cc(3), phi_cc_ort,phib_cc_min, phib_cc_max
C Scint.Counter
      INTEGER sc_nslab(6)
      REAL sc_xymid(4,6), phi_sc_ort(6), sc_T(6)
C Electromagnetic Counter plane
      REAL r_ec(2),d_ec(2),dd_ec(2), x0_ec(3,2), phi_ec_ort(2)
     1                                , phi_ec_min(2),phi_ec_max(2)        
C
C=================== DATA BLOCK ========================================        
C
C Target&Start Counter
      DATA r_tgst       / 0.00, 0.00,  0.00 /
c      DATA d_tgst       / 0.05,11.00, 20.026/
      DATA d_tgst       / 0.05, 10.2, 19.783/	        !corrected geom for ST
c      DATA d_tgst       / 0.05,10.2, 18.783/	        !corrected geom for ST
      DATA x0_tgst      / 0.,0.,0., 2.,0.,0., 2.,0.,0./ !corrected geom for ST

      DATA phi_tgst_ort /90.00, 90.00, 60.00/

      real st_zoff(6), st1_len, st2_len
      DATA st_zoff /6*2.0/
      DATA st1_len, st2_len /398, 13.9 /
c      DATA phi_tgst_min /0.1,   27.646,  7.00 /
c      DATA phi_tgst_max /179.9, 152.354, 27.646/
c      DATA phi_tgst_min /0.1,   27.138,  5.810 /   !corrected geom for ST
c      DATA phi_tgst_max /179.9, 152.862, 27.138/   !corrected geom for ST
      
C
C Drift Chambers 
C
C All done in dc/dc_geom.c
C
C Cerenkov Counter
C fr 06-11-96      DATA r_cc       /394.0/         ! radius of front arc (from Bogdan Center)
      DATA r_cc       /390.0/         ! radius of front arc (from Bogdan Center)
      DATA d_cc       /480.0/         ! distance to back plane
      DATA x0_cc      /48.0,-70.0, 0./! Bogdan Center
      DATA phi_cc_ort /25.00/         ! ortvector to the back place
      DATA phib_cc_min /18.0/         ! PHIBmin (relative to Bogdan Center)
      DATA phib_cc_max /57.0/         ! PHIBmax (relative to Bogdan Center)
C
C Scintilator Counter 
C
C *** Elton parameters of January 7, 1994 for SC counters
C
cc      REAL tn30,pht
cc      DATA tn30 /0.57735/                ! TAN(30deg)
cc      DATA PHT   /30.0  /                ! Space for Photo Tube

C                    Xright  Yright   Xleft   Yleft (inner corners)
      DATA sc_xymid/
     1               510.79,  69.42, 363.65, 384.95,
     2               357.37, 372.14, 120.72, 435.55,
     3               124.68, 441.43,  17.11, 412.61,
     +                17.11, 412.61,-111.97, 378.02,
     4              -112.24, 383.89,-248.79, 269.31,
     +              -248.79, 269.31,-295.59, 230.04/

      DATA sc_T      /  5.08,  5.08,  5.08,  5.08,  5.08,  5.08/
      DATA sc_nslab  /    23,    11,     5,     3,     4,     2/
      DATA phi_sc_ort/  25.0,  75.0, 105.0, 105.0, 130.0, 130.0/
C
C Electromagnetic Counter
      DATA r_ec       /  0.00,   0.00/
      DATA d_ec       / 510.54, 490.2/
      DATA dd_ec      /  47.60, 55.90/       ! ~ 39x1.00(sc) + 38x0.23(Pb) and
C                                            ! ~ 33x1.50(sc) + 32x0.20(Pb)
      DATA x0_ec      /0.,0.,0., 0.,0.,0./
      DATA phi_ec_ort / 25.00,  75.00/
      DATA phi_ec_min /  5.00, 49.53/
      DATA phi_ec_max / 45.00, 72.99/
C
C====================End of DATA BLOCK ================================ 
      write(crmess,99)
      call recmes(crname,'I',crmess)
C
      pi        = ACOS(-1.)
      twopi2six = 2.*pi/6.
      degrad    = pi/180.

C Initialize array for rotation to sectors
      DO i=1,6
        sinsec(i) = SIN(FLOAT(i-1)*twopi2six)
        cossec(i) = COS(FLOAT(i-1)*twopi2six)
      ENDDO


C Target plane (1-nd plane used in fitting)
C Start counter planes (two planes)

c     initialize local variables from STG bank

      if(nami_stgm.eq.0) nami_stgm=MAMIND(jw,'STG ')
      if(nami_stgm.ne.0) ind_stgm=jw(nami_stgm)

c     SDA coordinate system, x along beam line
 
      do isec=1,6

         if(ind_stgm.ne.0) then  
            ind_stgm_sec=ind_stgm+jw(ind_stgm-5)*(isec-1)
            d_tgst(2)=rjw(ind_stgm_sec+2)
CCCC TO BE CHECKED: use of abs may be wrong
C        d_tgst(3)=abs(rjw(ind_stgm+6)+(rjw(ind_stgm+2)/
C     $     tan(rjw(ind_stgm+14)*degrad)))*sin(rjw(ind_stgm+14)*degrad)
C So why not removing it ??? because that's wrong!
* fk 4/08: originally the center of the ST leg was used as reference and the ST nose
*          defined as rotated plane about (CLAS_x=0,CLAS_y=0,CLAS_z=STleg_center).
*          Since the ST leg is parallel to the beamline, the 'center' or CLAS_z offset
*          of the ST leg is arbitrary (like R.de Vita: set it to zero), however the
*          'center' (rotation axis) for the ST nose is NOT at CLAS Center:
*     my change: take intersection (corner) of ST leg and ST nose as reference to
*                define the rotation axis and distance:
            d_tgst(3)=d_tgst(2)/cos(rjw(ind_stgm_sec+14)*degrad)

ccc WRONG DISTANCE BETWEEN CENTER AND NOSE PLANE -corrected May 10th 2004, R. De Vita
c start counter geometry in STG bank is defined in respect to CLAS CENTER
c therefore the CLAS center has to be used as a center for the SDA planes
ccc that correction doesn't make sense!
c     x0_tgst(1,2) = rjw(ind_stgm+7)+rjw(ind_stgm+15)/2.0!leg plane
c     x0_tgst(1,3) = rjw(ind_stgm+7)+rjw(ind_stgm+15)/2.0!nose plane
            x0_tgst(1,2)    = 0.
            x0_tgst(1,3)    = rjw(ind_stgm_sec+13) -
     &                        d_tgst(2)*tan(rjw(ind_stgm_sec+14)*degrad)
            phi_tgst_ort(3) = 90-rjw(ind_stgm_sec+14)

c edges (Z pos.) of ST paddles +/-delta: 
c  st_Zpos(1:2) for leg, st_Zpos(3:4) for nose
            st_Zpos(1,isec) = rjw(ind_stgm_sec+7) - 0.5
            st_Zpos(2,isec) = rjw(ind_stgm_sec+6)
            st_Zpos(3,isec) = rjw(ind_stgm_sec+13) - 0.2
            st_Zpos(4,isec) = rjw(ind_stgm_sec+12) + 5.
         else  
            st_Zpos(1,isec) = -st1_len/2.+st_zoff(isec) -0.5
            st_Zpos(2,isec) =  st1_len/2.+st_zoff(isec)
            st_Zpos(3,isec) =  st1_len/2.+st_zoff(isec) -0.2
            st_Zpos(4,isec) =  st_Zpos(3,isec)+
     &           st2_len*SIN(phi_tgst_ort(3)*degrad) +5.
         endif

         do ip = 1,nst_max
C Center of curvature for arc or offset for plane
          sda_pln(1,ip,isec) = x0_tgst(1,ip)
          sda_pln(2,ip,isec) = x0_tgst(2,ip)
          sda_pln(3,ip,isec) = x0_tgst(3,ip)
C Unit vector normal to the plane 
          sda_pln(4,ip,isec) = COS(phi_tgst_ort(ip)*degrad)
          sda_pln(5,ip,isec) = SIN(phi_tgst_ort(ip)*degrad)
          sda_pln(6,ip,isec) = 0.
C
          sda_pln(7,ip,isec) = r_tgst(ip)          ! Radius
          sda_pln(8,ip,isec) = d_tgst(ip)          ! Distance

         enddo 
      enddo

C Geometry of Drift Chamber
      if (nami_dcgm.le.0) nami_dcgm = mamind(jw,'DCGM')
      ind_dcgm = jw(nami_dcgm)
      if (ind_dcgm.le.0) then
         call recmes(crname,'W','DCGM Bank does not exist!')
      else
         ncol_dcgm = JW(ind_dcgm-5) ! number of columns of dcgm bank
         
         do isec = 1,6
            do while (jw(ind_dcgm-2).lt.isec.       !Never need to loop more
     &           AND.jw(ind_dcgm-1).gt.0)           !than once if normal.
               ind_dcgm = jw(ind_dcgm-1)
            enddo
            if (jw(ind_dcgm-2).ne.isec) then
               write(crmess,401)isec
               call recmes(crname,'W',crmess)
            endif
            
            do il = 1,36
               ipl = il + nst_max             ! DC starts after START counter
               ipt_gm = ind_dcgm + (il-1)*ncol_dcgm
               sda_pln(1,ipl,isec) = RJW(ipt_gm+DCGM_Z_CURVE) !cntr of curva.
               sda_pln(2,ipl,isec) = RJW(ipt_gm+DCGM_X_CURVE)
               sda_pln(3,ipl,isec) = RJW(ipt_gm+DCGM_Y_CURVE)
               sda_pln(4,ipl,isec) = RJW(ipt_gm+DCGM_Z_NORM)  !normal to plane
               sda_pln(5,ipl,isec) = RJW(ipt_gm+DCGM_X_NORM)
               sda_pln(6,ipl,isec) = RJW(ipt_gm+DCGM_Y_NORM)
               sda_pln(7,ipl,isec) = RJW(ipt_gm+DCGM_R_CURVE) !radius of cyl
               sda_pln(8,ipl,isec) = RJW(ipt_gm+DCGM_R_CURVE) !cntr to plane
            enddo
         enddo
      endif
            

C Geometry of Cerenkov Counter (entering arc only)

C Center of curvature for arc or offset for plane
      do isec=1,6
        ip = ncc_min
        sda_pln(1,ip,isec) = x0_cc(1)
        sda_pln(2,ip,isec) = x0_cc(2)
        sda_pln(3,ip,isec) = x0_cc(3)
C
C ??(fk) Unit vector normal to the plane CC like SC1 
        sda_pln(4,ip,isec) = COS(phi_cc_ort*degrad)
        sda_pln(5,ip,isec) = SIN(phi_cc_ort*degrad)
        sda_pln(6,ip,isec) = 0.
C was:
c        sda_pln(4,ip,isec) = 0.
c        sda_pln(5,ip,isec) = 0.
c        sda_pln(6,ip,isec) = 0.

        sda_pln(7,ip,isec) = r_cc            ! Radius (irrelevent for plane)
        sda_pln(8,ip,isec) = d_cc            ! Distance (irrelevent for arc)

cc      sda_pln(9,ip,isec) = phi_cc_ort*degrad
cc      sda_pln(16,ip,isec)= phib_cc_min*degrad
cc      sda_pln(17,ip,isec)= phib_cc_max*degrad        
cc      sda_pln(12,ip,isec) = x0_cc(1) + r_cc*COS(sda_pln(16,ip,isec))
cc      sda_pln(13,ip,isec) = x0_cc(2) + r_cc*SIN(sda_pln(16,ip,isec))
cc      sda_pln(14,ip,isec) = x0_cc(1) + r_cc*COS(sda_pln(17,ip,isec))
cc      sda_pln(15,ip,isec) = x0_cc(2) + r_cc*SIN(sda_pln(17,ip,isec))        
cc      sda_pln(10,ip,isec)= ATAN2(sda_pln(13,ip,isec),sda_pln(12,ip,isec))
cc      sda_pln(11,ip,isec)= ATAN2(sda_pln(15,ip,isec),sda_pln(14,ip,isec))
cc      sda_pln(18,ip,isec)= 2.0
cc      sda_pln(19,ip,isec)= 18.

      enddo ! isec

C Geometry of Scintilator Counter

      do isec=1,6
        ind_sc  = MLINK(jw,'SCP ',isec)

        DO ip = 1,npl_sc
          ipl=ncc_max+ip

          if(ind_sc.GT.0) then
c Q: you might ask, why do we switch x y z around, when SCP is such
c    a nice bank?
c(fk)  ^^^^^^^^^might be! but not useful for track_swimming!
c A: to get into the Bogdan coordinate system with beam in the
c    x direction (I think)         <<< more important: rotate to SCS (fk)
c BTW this is where C style indicies come in handy - JM 
c                   ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^that's true allthough...
c   ...it would have been better to define the bank with 4 columns!  (fk)

           do i=1,3
             dcs_ort(i) = rjw(ind_sc+((ip-1)*4)+i)
           enddo
           dist = rjw(ind_sc+((ip-1)*4)+4)
C
C translate to Bogdan's Sector Coord.System
           ort(1) = dcs_ort(3)
           ort(2) = dcs_ort(1)*cossec(isec) + dcs_ort(2)*sinsec(isec)
           ort(3) =-dcs_ort(1)*sinsec(isec) + dcs_ort(2)*cossec(isec)
          else					! take defaults
           if(ip.EQ.4) then
            i=ip+1
           else
            i=ip
           endif
           ort(1) = COS(phi_sc_ort(i)*degrad)
           ort(2) = SIN(phi_sc_ort(i)*degrad)
           ort(3) = 0.
           dist = sc_xymid(1,i)*ort(1)+sc_xymid(2,i)*ort(2)
          endif

C Center of curvature for arc or offset for plane  (not relevant for SC)
          sda_pln(1,ipl,isec) = 0.
          sda_pln(2,ipl,isec) = 0.
          sda_pln(3,ipl,isec) = 0.
C
C Store ort-vectors for four SC-planes
          sda_pln(4,ipl,isec) = ort(1)
          sda_pln(5,ipl,isec) = ort(2)
          sda_pln(6,ipl,isec) = ort(3)
C
          sda_pln(7,ipl,isec) = 0.              ! Radius (irrelevent for plane)
          sda_pln(8,ipl,isec) = dist            ! Distance (irrelevent for arc)
        ENDDO		! sc_planes
      enddo 		! isec

C Geometry of Electromagnetic Counter (entering plane only)

      i = 0
      DO ip = nec_min,nec_max
        i = i + 1
        do isec=1,6
C Center of curvature for arc or offset for plane
          sda_pln(1,ip,isec) = x0_ec(1,i)
          sda_pln(2,ip,isec) = x0_ec(2,i)
          sda_pln(3,ip,isec) = x0_ec(3,i)
C Unit vector normal to the plane (irrelevent for arc)
          sda_pln(4,ip,isec) = COS(phi_ec_ort(i)*degrad)
          sda_pln(5,ip,isec) = SIN(phi_ec_ort(i)*degrad)
          sda_pln(6,ip,isec) = 0.
C
          sda_pln(7,ip,isec) = r_ec(i)          ! Radius (irrelevent for plane)
          sda_pln(8,ip,isec) = d_ec(i)          ! Distance (irrelevent for arc)

cc        sda_pln(9,ip,isec) = phi_ec_ort(i)*degrad
cc        sda_pln(10,ip,isec)= phi_ec_min(i)*degrad
cc        sda_pln(11,ip,isec)= phi_ec_max(i)*degrad
ccC Xright,Yright and Xleft,Yleft
cc        psi = sda_pln(9,ip,isec) - sda_pln(10,ip,isec)
cc        sda_pln(12,ip,isec) = COS(sda_pln(10,ip,isec))*sda_pln(8,ip,isec)/COS(psi)
cc        sda_pln(13,ip,isec) = SIN(sda_pln(10,ip,isec))*sda_pln(8,ip,isec)/COS(psi)
cc        psi = sda_pln(9,ip,isec) - sda_pln(11,ip,isec)
cc        sda_pln(14,ip,isec) = COS(sda_pln(11,ip,isec))*sda_pln(8,ip,isec)/COS(psi)
cc        sda_pln(15,ip,isec) = SIN(sda_pln(11,ip,isec))*sda_pln(8,ip,isec)/COS(psi)
cc        IF(ip.EQ.nec_min) THEN
cc          sda_pln(16,ip,isec) = 39.0         ! No of layers {SC + Pb}
cc          sda_pln(17,ip,isec) = 36.0         ! No of SC slabs in a layer 
cc          sda_pln(18,ip,isec) = 54.22*degrad ! Opening angle
cc          sda_pln(19,ip,isec) = 1.00         ! Tsc
cc          sda_pln(20,ip,isec) = 0.23         ! Tpb
cc        ENDIF
cc        IF(ip.EQ.nec_max) THEN
cc          sda_pln(16,ip,isec) = 33.0         ! No of layers {SC + Pb}
cc          sda_pln(17,ip,isec) = 36.0         ! No of SC slabs in a layer 
cc          sda_pln(18,ip,isec) = 0.0          ! ?
cc          sda_pln(19,ip,isec) = 1.50         ! Tsc 
cc          sda_pln(20,ip,isec) = 0.20         ! Tpb
cc        ENDIF

        enddo  		! isec
      ENDDO		! ec_planes

C Print Geometry Tables in log file if linfo is true (first word)

      if(ipar_trk_print(1).GT.0) then
       WRITE(crmess,100)
       call recmes(crname,'I',crmess)
cc       WRITE(crmess,111)
cc       call recmes(crname,'I',crmess)
       WRITE(crmess,112)
       call recmes(crname,'I',crmess)
       WRITE(crmess,111)
       call recmes(crname,'I',crmess)
       isec=ipar_trk_print(1)
       WRITE(crmess,110)
       call recmes(crname,'I',crmess)
       DO ip = 1,npln
        IF(ip.EQ.4 .OR. ip.EQ.16 .OR. ip.EQ.28 .OR. ip.EQ.40) then
         WRITE(crmess,*)
         call recmes(crname,'I',crmess)
         endif              
        IF(ip.EQ.41 .OR. ip.EQ.45) then
         WRITE(crmess,*)              
         call recmes(crname,'I',crmess)
         endif              
        WRITE(crmess,101) ip,DET(ip),(sda_pln(i,ip,isec),i=1,8)
        call recmes(crname,'I',crmess)
       ENDDO

cc       write(6,'(A,I2)')'DC Sector',isec
cc       write(6,*)'ip dPHIm  dX0   dY0  PHBmin    DPHI   '//
cc     &'    Nw   Nwmin   Nwmax   Nwcyl AStereo    Dmax    Wcut'
cc       do ip=1,36
cc        write(6,'(I3,5F8.4,4F5.0,3F8.4)')ip,(dc_pln(i,ip,isec),i=9,20)
cc       enddo

c       write(crmess,'(3(A,3F7.2))')'XYpos VX',(XYpos_tgst(i,1,isec),i=1,3),
c     & '; ST1',(XYpos_tgst(i,2,isec),i=1,3),'; ST2',(XYpos_tgst(i,3,isec),i=1,3)
c       call recmes(crname,'I',crmess)
c
c         call recmes(crname,'I',crmess)

cc       WRITE(crmess,102)
cc       WRITE(crmess,113)
cc       call recmes(crname,'I',crmess)
cc       WRITE(crmess,111)
cc       call recmes(crname,'I',crmess)
cc       WRITE(crmess,107)
cc       call recmes(crname,'I',crmess)
cc       DO ip = 1,npln
cc        IF(ip.EQ.4) THEN
cc          WRITE(crmess,*)
cc          call recmes(crname,'I',crmess)
cc          WRITE(crmess,103)
cc          call recmes(crname,'I',crmess)
cc        ENDIF
cc        IF(ip.EQ.16 .OR. ip.EQ.28 .OR. ip.EQ.40 .OR. ip.EQ.45) then
cc         WRITE(crmess,*)
cc        endif                           
cc        IF(ip.EQ.40) then 
cc         WRITE(crmess,109)
cc         call recmes(crname,'I',crmess) 
cc         endif
cc        IF(ip.EQ.41) then
cc         WRITE(crmess,*) 
cc         call recmes(crname,'I',crmess) 
cc         endif
cc        IF(ip.EQ.41) then
cc         WRITE(crmess,106)
cc         call recmes(crname,'I',crmess) 
cc         endif
cc        IF(ip.EQ.45) then
cc         WRITE(crmess,108)
cc         call recmes(crname,'I',crmess) 
cc         endif
cc        IF(ip.GT.3 .AND. ip.LT.40) THEN
cc          WRITE(crmess,104) ip,DET(ip),(sda_pln(i,ip,isec),i=12,20)
cc          call recmes(crname,'I',crmess) 
cc        ELSE
cc          WRITE(crmess,105) ip,DET(ip),(sda_pln(i,ip,isec),i=12,20)
cc          call recmes(crname,'I',crmess) 
cc        ENDIF
cc       ENDDO
       endif

  99  FORMAT(' TRK fill SDA Global Geometry array')
 100  FORMAT(' LEVEL=-1 (SDA Geometry a la TRK, columns: 1 - 8)')
 110  FORMAT(
     1' ip',1X,'      Offset Center',1X,'    Normal Unit Vector'        
     2,1X,'     R      D   ')
 111  FORMAT(' ---------------------------------------------------------
     2----------------------')
cc 113  FORMAT(' Col -->    12      13       14      15      16      17       
cc     1   18      19      20') 
 112  FORMAT(' Col -->   1     2   3     4      5      6          7        
     1 8  ') 
 101  FORMAT(I3,1X,A3,2F6.1,F4.1,2X,3F7.4,2X,2F7.2)
cc 102  FORMAT(' LEVEL=-1 (SDA Geometry a la TRK, columns: 12 - 20)')
cc 103  FORMAT(
cc     1'   ',1X,'     PHBmin    DPHI       Nw   Nwmin   Nwmax   Nwcyl ASt
cc     2ereo    Dmax    Wcut')
cc 104  FORMAT(I3,1X,A3,2F8.4,1X,4F8.1,3F8.4)
cc 105  FORMAT(I3,1X,A3,2F8.2,1X,4F8.2,3F8.2)
cc 106  FORMAT('   ',1X,'     Xright  Yright    Xleft   Yleft    Xmid    Y
cc     1mid    Nsc1    Nsc2     Tsc')
cc 107  FORMAT(' ip',1X,'     Xright  Yright    Xleft   Yleft
cc     1        Nsc    RadL     Tsc')
cc 108  FORMAT('   ',1X,'     Xright  Yright    Xleft   Yleft    Nlay
cc     1Nsc     Psi     Tsc     Tpb')
cc 109  FORMAT('   ',1X,'     Xright  Yright    Xleft   Yleft  PHBmin  PHB
cc     1max    Nlay     Npt        ')

 401  format('Error getting DCGM bank # ',i1,'.')

      RETURN
      END
