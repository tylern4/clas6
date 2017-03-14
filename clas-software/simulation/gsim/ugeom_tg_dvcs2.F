      subroutine ugeom_tg_dvcs2(itg_mate,experiment)

****************************************************************
*
* Subroutine ugeom_tg_dvcs2, describing the target geometries for 
* two experiments:
*
* e1-DVCS-I
* e1-DVCS-II
*
* Based on subroutine ugeom_tg_dvcs, but including improvements 
* fixes to some minor bugs in e1-DVCS-I geometry of ugeom_tg_dvcs
* 
* Target group 13
*
* Includes geometries for the following independent volumes:
*
* inside of target cell occupied by target material
* additional volume of target material filling the end of the cell, 
*                                                   inside end cap
* base ring and cone (supporting structure for the cell)
* base tube (for beam)
* epoxy between base tube and target entrance window
* target entrance window (Al)
* target cell (conical, kapton)
* end cap
* target cell exit window (Al)
* epoxy attaching exit window to end cap and end cap to target cell
* foil cap at the very end
* superinsulation wrapped around conical target cell and part-way 
*                                                    along foil cap
*
*
* All drawings used are listed in the comments to each section
*
* Daria Sokhan, July 2010
*
*********************************************************************



*     Some important stuff at the start

      implicit none
      character*(*) cfile,crevis,cstate,cdate,cautho,crname,crauth
      character*132 crcsid,crmess
      parameter (cfile=  '$RCSfile: ugeom_tg_dvcs.F,v $')
      parameter (crevis= '$Revision: 1.3 $')
      parameter (cstate= '$State: Exp $')
      parameter (cdate=  '$Date: 2008/10/03 13:45:26 $')
      parameter (cautho= '$Author: avakian $')
      parameter (crname= 'UGEOM_DVCS2')
      parameter (crauth= 'sam')
      data crcsid/'
     1$Id: ugeom_tg_dvcs2.F,v 1.3 2008/10/03 13:45:26 avakian Exp $
     1 '/
      save     
      
********* Include files ***********

#include "gcbank.inc"
#include "gcnum.inc"
#include "gsimpar.inc"
#include "gsimcontrol.inc"
#include "ffpar_foil.inc"
#include "ffpar_tg.inc"
#include "ffpar_mg.inc"
#include "clmate.inc"
#include "cltmed.inc"
#include "ffpar_dvcs.inc"
#include "ffpar_bckgr.inc"

******** end of include **********
      
      
*************************     
*   Input parameters    *
*************************
      
      integer itg_mate          !target material number
      integer experiment        !number for dvcs experiment to choose target  (29: e1-dvcs1, 30: e1-dvcs2)

**************************     
*   External functions   *
**************************
      
      integer  lnblnk


****************************************************
*   Local variables (all lengths, distances in cm) *
****************************************************

***   Materials
***   ---------
      
      integer tmed_tg                             ! tracking medium for target material
      integer imatcell, tmed_cell                 ! material number and tracking medium for target cell
      integer imatins, tmed_ins                   ! same as above for superinsulation layers 
      character*20 cmat_tg, cmatcell, cmatins     ! material names
      real  aa,zz,dens,radl,absl                  ! material constants
      real  tmaxfd, stmax, deemax, epsil, stmin   ! tracking media cuts 


***   Geometry parameters
***   -------------------

      integer ivol                 ! volume number
      integer istrlen              ! not sure what this is, from old code!
      
      integer i                    ! counting integer
      real par(50)                 ! array for volume parameters
      integer Npar                 ! number of parameters
      real z_pos                   ! positioning of volumes within mother volume

      character*4 cmoth            ! mother volume

      real xytg                    ! x-y position of target center
      real moth_dz                 ! half-length of mother volume tube

      real z_mother                 ! z co-ordinate of positioning of the mother volume in the scattering chamber volume

*
*  Target cell
*
      real tg_length             ! interaction volume -- from end of entrance window to end of exit window (what's meant by tleng in ffread cards)
      real tg_rad                ! inner radius of target cell at downstream (narrow) end
      real cell_rad_max          ! inner radius of target cell at upstream (wide) end
      real z_cell                ! dz: half-length of target cell cone
      real tube_l                ! length of base tube

      real cell_thick            ! 51 microns thick kapton for cell wall (0.002")
      real cap_thick             ! thickness of kapton cell end-cap (0.003")

      real tan_theta             ! half-cone angle of cell, theta = 6.390 degrees
      real cos_theta             ! cosine of the above angle 

      real wind_thick            ! thickness of Al entrance and exit windows, 15 microns
      real glue_thick            ! thickness of epoxy, 50 microns
      real ins_thick             ! total thickness of superinsulation 
                                 ! layers (material: aluminized Mylar)
                                 ! packed as solid block (ca. 3.5% of total)
       

*     Dimensions which are experiment-specific (to be changed further down):
      
      data tg_length / 2.5 /       ! interaction volume -- from end of entrance window to end of exit window
      data z_cell / 2.038 /        ! dz: half-length of target cell cone
      data tan_theta / 0.1120 /    ! half-cone angle of cell, theta = 6.390 degrees
      data cos_theta / 0.9938 /    ! cosine of the above angle 
      
*

      data tg_rad  / 0.348 /    ! inner radius of target cell at downstream (narrow) end
      data cell_rad_max / 0.8045 / ! inner radius of target cell at upstream (wide) end
      data tube_l / 6.26 /      ! length of base tube
      
      data cell_thick  / 0.0051 /   ! 51 microns thick kapton for cell wall (0.002")
      data cap_thick  / 0.0076 /    ! thickness of kapton cell end-cap (0.003")

      data wind_thick / 0.0015 /    ! thickness of Al entrance and exit windows, 15 microns
      data glue_thick / 0.0050 /    ! average thickness of epoxy (0.001" - 0.003") 
      data ins_thick / 0.008 /      ! unclear why this value - taken from old dvcs code
                                    ! total thickness of superinsulation 
                                    ! layers (material: aluminized Mylar)
                                    ! packed as solid block (ca. 3.5% of total)



      real tg_z(10)
      real tg_r1(10)
      real tg_r2(10)

      real tg_end_z(5)
      real tg_end_r1
      real tg_end_r2(5)

      real wind_z(7)
      real wind_r1(7)
      real wind_r2(7)
      
      real cap_z(8)
      real cap_r1(8)
      real cap_r2(8)
      
      real glue_z(8)
      real glue_r1(8)
      real glue_r2(8)

      real base_z(10)
      real base_r1(10)
      real base_r2(10)

      real tube_z(8)
      real tube_r1(8)
      real tube_r2(8)

      real ins_z(3)
      real ins_r1(3)
      real ins_r2(3)



*******************************************************
*     Set parameters for geometries here
*******************************************************

*   First change these values depending on the experiment chosen:

      if (experiment.EQ.29) then       ! DVCS-I
         tg_length = 2.5             ! interaction volume -- from end of entrance window to end of exit window
         z_cell = 2.038              ! dz: half-length of target cell cone
         tan_theta = 0.1120          ! half-cone angle of cell, theta = 6.390 degrees
         cos_theta = 0.9938          ! cosine of the above angle 

      elseif (experiment.EQ.30) then ! DVCS-II
         tg_length = 5.0
         z_cell = 3.288
         tan_theta = 0.0694
         cos_theta = 0.9976

      endif




*     inside target volume
*     z-positions for the planes in PCON geometry of the target volume:

      tg_z(1) =  1.25                ! from left edge of base ring - start of volume
      tg_z(2) =  2.54                ! change in inner diameter -- narrows
      tg_z(3) =  tg_z(2)  
      tg_z(4) =  3.55                ! outer diameter begins to taper in diagonally
      tg_z(5) =  4.45                ! outer diameter reaches narrowest point
      tg_z(6) =  4.574               ! conical kapton target cell starts, inner diam. narrows again
      tg_z(7) =  tg_z(6) 
      tg_z(8) =  tube_l + glue_thick + wind_thick  ! end of base tube + glue + window, start of interaction volume
      tg_z(9) =  tg_z(8) 
      tg_z(10) = tg_z(6) + (z_cell * 2)     ! this bit of volume stops at the end of the conical cell

*     inner radius at above positions in PCON geomentry of the target volume:

      tg_r1(1) =  0.445
      tg_r1(2) =  tg_r1(1)
      tg_r1(3) =  0.35
      tg_r1(4) =  tg_r1(3)
      tg_r1(5) =  tg_r1(3)
      tg_r1(6) =  tg_r1(3)
      tg_r1(7) =  0.325
      tg_r1(8) =  tg_r1(7)
      tg_r1(9) =  0.0
      tg_r1(10) = 0.0

*     outer radius at above positions in PCON geomentry of the target volume:      
      
      tg_r2(1) =  1.44
      tg_r2(2) =  tg_r2(1)  
      tg_r2(3) =  tg_r2(1)
      tg_r2(4) =  tg_r2(1)
      tg_r2(5) =  0.8
      tg_r2(6) =  tg_r2(5)
      tg_r2(7) =  0.805              ! conical inner radius at the start of the kapton target cell

      if (experiment.EQ.29) then
         tg_r2(8) =  0.615           ! conical radius at the start of the interaction volume (after Al window and epoxy) - calculated from trig
      elseif (experiment.EQ.30) then   
         tg_r2(8) =  0.687
      endif

      tg_r2(9) =  tg_r2(8)
      tg_r2(10) = tg_rad             ! inner target radius at the very end of the kapton cell.
 

*     the very end bit of target volume, curved

      tg_end_z(1) = 0.
      tg_end_z(2) = 0.025
      tg_end_z(3) = 0.055
      tg_end_z(4) = 0.080
      tg_end_z(5) = 0.1024
      
      tg_end_r1 = 0.
      
      tg_end_r2(1) = 0.3485
      tg_end_r2(2) = 0.335
      tg_end_r2(3) = 0.315
      tg_end_r2(4) = 0.270
      tg_end_r2(5) = 0.200
      

*     aluminium exit window (downstream) -- r2 calculated with trig
*     follows outlines of end bit of target volume

      do i=1,5
         wind_z(i) = tg_end_z(i)
         wind_r1(i) = tg_end_r2(i)
      enddo
      
      wind_z(6) = wind_z(5)
      wind_z(7) = wind_z(5) + wind_thick

      wind_r1(6) = 0.
      wind_r1(7) = 0.

      wind_r2(1) = tg_rad + cell_thick / cos_theta ! To coincide with outer radius of kapton cell cone

      do i=2,4
         wind_r2(i) = wind_r1(i) + (wind_thick /  cos( atan( (wind_r1(i) 
     &        - wind_r1(i+1)) / (wind_z(i+1) - wind_z(i)) ) ) )
      enddo
      
      wind_r2(5) = wind_r1(5) + (wind_r2(4) - wind_r1(4)) 
      wind_r2(6) = wind_r2(5)
      wind_r2(7) = wind_r2(5) - (wind_thick * ((wind_r1(4) - wind_r1(5))
     &     / (wind_z(5) - wind_z(4))))
      

*     Kapton cell-cap:
*     Follows the outlines of Al cell, with space for epoxy

      do i=2,5
         cap_z(i) = wind_z(i-1)
         cap_r1(i) = wind_r2(i-1) + glue_thick
      enddo
       
      cap_z(6) = wind_z(7) + glue_thick
      cap_z(7) = cap_z(6)
      cap_z(8) = cap_z(7) + cap_thick
      cap_z(1) = cap_z(8) - 0.24        ! length of cap is 0.24 cm
    
      cap_r1(1) = ((cap_z(2) - cap_z(1)) * tan_theta) + tg_rad 
     &     + (cell_thick / cos_theta) + glue_thick
      cap_r1(6) = wind_r2(7)
      cap_r1(7) = 0.2                   ! downstream end of cap, given hole radius
      cap_r1(8) = 0.2                   ! as above
      
      do i=1,5
         cap_r2(i) = cap_r1(i) + (cap_thick /  cos( atan( (cap_r1(i)
     &        - cap_r1(i+1)) / (cap_z(i+1) - cap_z(i)) ) ) )
      enddo

      cap_r2(6) = cap_r1(6) + (cap_r2(5) - cap_r1(5))
      cap_r2(7) = cap_r2(6)
      cap_r2(8) = 0.2


*     Epoxy glue:
*     fills the space between cell-cap and Aluminium exit window / kapton cell
           
      do i=1,5
         glue_z(i) = cap_z(i)
      enddo

      glue_z(6) = wind_z(7)
      glue_z(7) = glue_z(6)
      glue_z(8) = glue_z(7) + glue_thick

      do i=2,5
         glue_r1(i) = wind_r2(i-1)
         glue_r2(i) = cap_r1(i)
      enddo
      
      glue_r1(1) = cap_r1(1) - glue_thick
      glue_r1(6) = wind_r2(7)
      glue_r1(7) = 0.2
      glue_r1(8) = 0.2
      
      glue_r2(1) = cap_r1(1)
      glue_r2(6) = cap_r1(5) - ( (glue_z(6) - glue_z(5)) * 
     &     ( (cap_r1(5) - cap_r1(6)) / (cap_z(6) - cap_z(5)) ) ) 
      glue_r2(7) = glue_r2(6)
      glue_r2(8) = cap_r1(6)


*     Base ring and base cone 

      base_z(1) = 0.0
      base_z(2) = 0.687         ! Measured to the first bend in outer surface
      base_z(3) = tg_z(1)       ! Target volume starts
      base_z(4) = base_z(3)
      base_z(5) = 1.94          ! Start of the base cone
      base_z(6) = base_z(5)
      base_z(7) = 3.31          ! Outer surface of base cone starts to slope inwards
      base_z(8) = tg_z(4)       ! Inner surface of base cone starts to slope inwards
      base_z(9) = tg_z(5)       ! Inner surface of base cone reaches narrowest point
      base_z(10) = 4.78         ! End of base cone

      base_r1(1) = tg_r1(1)     ! Widest part surrounding inner tube
      base_r1(2) = base_r1(1)   
      base_r1(3) = base_r1(1)
      base_r1(4) = tg_r2(1)
      base_r1(5) = base_r1(4)
      base_r1(6) = base_r1(4)
      base_r1(7) = base_r1(4)
      base_r1(8) = base_r1(4)
      base_r1(9) = cell_rad_max + cell_thick / cos_theta    ! upstream outer radius of kapton cell cone
      base_r1(10) = base_r1(9)

      base_r2(1) = 0.68
      base_r2(2) = 2.10
      base_r2(3) = base_r2(2)
      base_r2(4) = 1.70
      base_r2(5) = base_r2(4) 
      base_r2(6) = 1.85
      base_r2(7) = base_r2(6)
      base_r2(8) = 1.68                 ! Calculated from trig
      base_r2(9) = base_r1(9) + 0.234   ! Calculated from trig
      base_r2(10) = base_r1(10)


*     Base tube

      tube_z(1) = 0.0
      tube_z(2) = tg_z(2)       ! Base ring inner part finishes
      tube_z(3) = tube_z(2)
      tube_z(4) = tg_z(6)       ! Base tube thickness narrows
      tube_z(5) = tube_z(4)
      tube_z(6) = 6.19          ! Start of tapering at very end of base tube
      tube_z(7) = tube_z(6)
      tube_z(8) = 6.26          ! End of base tube, start of window
      
      do i=1,6
         tube_r1(i) = 0.3       ! Inner diameter of base tube
      enddo
      
      tube_r1(7) = 0.25
      tube_r1(8) = 0.2          ! Radius of hole in the end of base tube
      
      tube_r2(1) = base_r1(1)
      tube_r2(2) = tube_r2(1)
      tube_r2(3) = tg_r1(3)
      tube_r2(4) = tube_r2(3)
      tube_r2(5) = tg_r1(7)
      tube_r2(6) = tube_r2(5)
      tube_r2(7) = tube_r2(5)
      tube_r2(8) = tube_r2(5)
      

*     superinsulation

      ins_z(1) = tg_z(6) + 0.206                               ! 0.206 cm downstream from start of conical target cell (where base cone stops)
      ins_z(2) = tg_z(10)
      ins_z(3) = ins_z(2) + tg_end_z(5) + 2.0 - glue_thick     ! Al foil glued to end of superinsulation is 2 cm downstream from exit window

      ins_r1(1) = cell_rad_max + cell_thick / cos_theta - 0.023     ! Upstream outer radius of kapton cone - 0.023
                                                                    ! 0.023 determined with trig
      ins_r1(2) = cap_r2(2)                                    ! Goes over the surface of the end cap
      ins_r1(3) = 0.35 - ins_thick                             ! Foil diameter 7 mm epoxied to end


      ins_r2(1) = ins_r1(1) + ins_thick
      ins_r2(2) = ins_r1(2) + ins_thick
      ins_r2(3) = 0.35                                         ! Foil diameter


*      
*     Material selection (all of the marials values must presumably come from an include file)     
*     ------------------

 
      imatcell  = imate_kapton 
      imatins   = imate_cerexmyl1

      tmed_cell = clas_med_kapton          ! cell walls kapton
      tmed_ins  = clas_med_cerexmyl1       ! superinsulation CEREX / Alum.-mylar
      
      if(itg_mate.EQ.imate_vacuum) then ! Choose empty or full (H2) cell
         tmed_tg  = clas_med_vac
      elseif(itg_mate.EQ.imate_liq_h2) then
         tmed_tg  = clas_med_h2liq
      elseif(itg_mate.EQ.imate_liq_d2) then
         tmed_tg  = clas_med_d2liq
      else
         call recmes(crname,'W',
     &        'Only HYDR, DEUT or VAC targets supported for e1e')
         return
         
      endif

*     
*     Chamber selection
*     -----------------   
*         
*     Use custom DVCS volume (correct scattering chamber is chosen in it with CHAMBER ffkey card).   
*
*     stop if beam pipe volume not defined

      cmoth = 'DVCS'

      tgpar_save(12) = 0.0
    
      call GLOOK(cmoth, IQ(jvolum+1), NVOLUM, ivol)

      if(ivol.LE.0) then
         write(crmess,'(3A)') 'Scattering chamber volume "',cmoth,
     &        '" not defined -> no target geometry'
        call recmes(crname,'W',crmess)
        return
      endif

      call recmes(crname,'I',
     &     'Define geometry for dvcs target ...')

      
*     
***************************
*     define tracking media
***************************
*     
*     these parameters are only(!) valid if  Data Card 'AUTO' = 0!
      
      TMAXFD =  0.1             !max. angular deviation in one step [degree]
      STMAX  =  1.0             !max. step  [cm]
      DEEMAX =  0.01            !max. energy loss per step  [0;1]
      EPSIL  =  0.001           !boundary crossing precision
      STMIN  =  0.001           !min. step  [cm]
      
*     
*     target material
*     
      call GFMATE(itg_mate,cmat_tg,aa,zz,dens,radl,absl,par,Npar)
      call GFMATE(imatcell,cmatcell,aa,zz,dens,radl,absl,par,Npar)
      call GFMATE(imatins,cmatins,aa,zz,dens,radl,absl,par,Npar)
      
*     print info

      istrlen=LNBLNK(cmat_tg)
      
      write(crmess,'(3A,F7.2,A)') 'set Target (material=',
     &     cmat_tg(1:istrlen),')  at z-pos.:',tg_pos_ff(3),'cm'
      
      call recmes(crname,'I',crmess)
      
      istrlen=LNBLNK(cmatcell)
      
      write(crmess,'(2(A,F7.3),A,F6.4,3A)') '   Target length:',
     &     tg_length,'cm;  end radius:',tg_rad,'cm;  cell thickness:',
     &     cell_thick,'cm (',cmatcell(1:istrlen),')'
      
      call recmes(crname,'I',crmess)
      

******************************************
*              GEOMETRIES                *
******************************************

*  All measurements given in cm

*      
*  Mother volume (vacuum)
*  ----------------------

*  Mother volume placed inside DVCS to make sure center of target coincides with center of DVCS (which is placed at target center position in CLAS)
*  

      if (experiment.EQ.29) then
         moth_dz = 5.5165        ! (from start of base ring to end of superinsulation
                                !    + foil window thickness) / 2  = 5.377, but making longer. Gives a -2cm offset for TG pos inside DVCS
      elseif (experiment.EQ.30) then
         moth_dz = 6.7665          ! the equivalent half-length for e1-dvcs-II is 6.627. 6.7665 to give easy offset for TG wrt DVCS volumes

      endif


      par(1) = 0.0
      par(2) = 2.1        ! radius of widest part of base ring  ! dvcs geom: 2.298 But seems need just 2.1 cm to fit base ring
      par(3) = moth_dz

      call gsvolu('TG  ','TUBE',clas_med_vac,par,3,ivol)

      xytg = sqrt(tg_pos_ff(1)**2 + tg_pos_ff(2)**2)
      z_mother = -2.0         ! To make sure center of target, which is 7.5165 cm from left edge of
                              ! mother volume for DVCS-I, and 8.7665 cm from left edge of mother vol for DVCS-II,
                              ! is placed at the correct tg_pos_ff(3) position wrt center of CLAS.                          
      
      call gspos('TG  ',1,cmoth,tg_pos_ff(1),tg_pos_ff(2),z_mother,0,'ONLY')     

*
      print *,'UGEOM_TG_DVCS2 positioned in: ',cmoth,z_pos

      print *,'WOOF WOOF WOOF WOOF WOOF WOOF WOOF WOOF!!!'
  
*      
*  Target cell insides (the actual target material)
*  ------------------------------------------------ 

*  Dimensions taken from this combination of drawings:
*
*  66850-D-04250 - target assembly for dvcs1, 
*  66850-A-04253 - inside of target cell for dvcs1, 
*  66850-A-03730 - base cone (dvcs1, dvcs2, dvcs-eg1) 
*  66850-A-03729 - base tube (dvcs1, dvcs2, dvcs-eg1)
*  66850-A-03728 - base ring (dvcs1, dvcs2, dvcs-eg1)

      
      par(1) =   0.
      par(2) = 360.
      par(3) = 10
      do i=1,10
         par(1+i*3) =tg_z(i)
         par(2+i*3) =tg_r1(i)
         par(3+i*3) =tg_r2(i)
      enddo
      
      Npar=par(3)*3+3

      z_pos = -1 * moth_dz
      
      call gsvolu('TARG','PCON',tmed_tg,par,Npar,ivol)
      call gspos('TARG',1,'TG  ',0.,0.,z_pos,0,'ONLY')     
      

*      
*  The curved end bit inside the target cell (the actual target material)
*  ------------------------------------------------ 

*  Dimensions deduced and estimated from drawings:
*
*  66850-D-04250 - target assembly for dvcs1, 
*  66850-A-03513 - cell end cap (dvcs1, dvcs2 and eg1-dvcs)

*  Length 0.1024 cm, at downstream end straight edge 0.4 cm diam. 
*  Approximate the rest with polycone, having 5 planes. 
      
      par(1) =   0.
      par(2) = 360.
      par(3) = 5

      do i=1,5
         par(1+i*3) =tg_end_z(i)
         par(2+i*3) =tg_end_r1
         par(3+i*3) =tg_end_r2(i)
      enddo
      
      Npar=par(3)*3+3

      z_pos = tg_z(6) + (z_cell * 2) - moth_dz    ! upstream edge of mother volume + start of kapton cell + entire length of cell
      
      call gsvolu('TGED','PCON',tmed_tg,par,Npar,ivol)
      call gspos('TGED',1,'TG  ',0.,0.,z_pos,0,'ONLY')     
      
*
*  Conical kapton cell wall
*  ------------------------

*  Dimensions from drawings:
*
*  66850-D-04250 - target assembly for dvcs1, 
*  66850-A-04253 - inside of target cell for dvcs1, 
        
    
      par(1) = z_cell                               ! half-length     
      par(2) = cell_rad_max                         ! upstream inner radius
      par(3) = par(2) + cell_thick / cos_theta      ! upstream outer radius 
      par(4) = tg_rad                               ! downstream inner radius
      par(5) = par(4) + cell_thick / cos_theta      ! downstream outer radius
         
      z_pos = tg_z(6) + z_cell - moth_dz            ! upstream edge of mother volume + start of kapton cell + cell half-length
   
      call gsvolu('TGCL','CONE',tmed_cell,par,5,ivol) 
      call gspos('TGCL',1,'TG  ',0.,0.,z_pos,0,'ONLY')
      
  
*
*   Alumium exit window (downstream end)
*   ------------------------------------

*   Follows outlines of the end bit of target volume (TGED)
     
      par(1) =   0.
      par(2) = 360.
      par(3) = 7

      do i=1,7
         par(1+i*3) = wind_z(i)
         par(2+i*3) = wind_r1(i)
         par(3+i*3) = wind_r2(i)
      enddo
      
      Npar=par(3)*3+3

      z_pos = tg_z(6) + (z_cell * 2) - moth_dz    ! starts in same place as the end bit of target volume
      
      call gsvolu('TGW1','PCON',clas_med_alu,par,Npar,ivol)
      call gspos('TGW1',1,'TG  ', 0.,0.,z_pos,0,'ONLY')



    
*     
*   Kapton cell end-cap
*   -------------------
* 
*   Parameters (rather awkward - three straight parts, co-ordinates given at 
*   upstream end and at two planes at each bend and at the downstream end)
*   Measurements taken from 66850-A-03513 (dvcs1, dvcs2, eg1-dvcs)
      
      par(1) = 0.
      par(2) = 360.
      par(3) = 8
      
      do i=1,8
         par(1+i*3) = cap_z(i)
         par(2+i*3) = cap_r1(i)
         par(3+i*3) = cap_r2(i)
      enddo
      
      Npar = 3 * par(3) + 3

      z_pos = tg_z(6) + (z_cell * 2) - moth_dz    ! starts in same place as the end bit of target volume
      
      call gsvolu('TGKP','PCON',clas_med_kapton,par,Npar,ivol)
      call gspos('TGKP',1,'TG  ',0.,0.,z_pos,0,'ONLY')
      
      
*
*   Glue joint 
*   ----------
*   Between cap and kapton cell wall cone parameters
*   These are determined entirely by space available between target cell wall cone, 
*   cell-cap and the Al exit window -- but should be 50 microns thick

      par(1) =   0.
      par(2) = 360.
      par(3) = 8
      
      do i=1,8
         par(1+i*3) = glue_z(i)
         par(2+i*3) = glue_r1(i)
         par(3+i*3) = glue_r2(i)
      enddo
      
      Npar = 3 * par(3) + 3

      z_pos = tg_z(6) + (z_cell * 2) - moth_dz     ! starts in same place as the end bit of target volume
      
      call gsvolu('TGEP','PCON',clas_med_alumaseal,par,Npar,ivol)
      call gspos('TGEP',1,'TG  ',0.,0.,z_pos,0,'ONLY')
      


     

************************************ 
*          Save parameters         *
************************************
 
*  Note from old dvcs code:  Rustam has fixed the vertex position     

      tgpar_save(1) = tg_length
      tgpar_save(2) = tg_rad
      tgpar_save(6) = tg_length / -2
      tgpar_save(7) = tg_pos_ff(3) + tgpar_save(6)
      tgpar_save(8) = tg_pos_ff(3) + tgpar_save(6)
      tgpar_save(9) = tg_pos_ff(3) + tgpar_save(7)
      tgpar_save(10)= xytg +tg_rad    
 

************************************ 
*        Support structures        *
************************************

*  These are taken from the drawings 
*
*  66850-A-03730 - base cone (dvcs1, dvcs2, dvcs-eg1) 
*  66850-A-03729 - base tube (dvcs1, dvcs2, dvcs-eg1)
*  66850-A-03728 - base ring (dvcs1, dvcs2, dvcs-eg1)
*

*     outer base ring and base cone (torlon)

      par(1) = 0.
      par(2) = 360.
      par(3) = 10

      do i=1,10
         par(1+i*3) = base_z(i)  
         par(2+i*3) = base_r1(i)
         par(3+i*3) = base_r2(i)
      enddo

      Npar = 3 * par(3) + 3

      z_pos = -1 * moth_dz

      call gsvolu('TGRG','PCON',clas_med_kapton,par,Npar,ivol)
      call gspos('TGRG',1,'TG  ', 0.,0.,z_pos,0,'ONLY')


*    inner base tube (torlon)

      par(1) = 0.
      par(2) = 360.
      par(3) = 8

      do i=1,8
         par(1+i*3) = tube_z(i)  
         par(2+i*3) = tube_r1(i)
         par(3+i*3) = tube_r2(i)
      enddo

      Npar = 3 * par(3) + 3

      z_pos = -1 * moth_dz

      call gsvolu('TGTU','PCON',clas_med_kapton,par,Npar,ivol)
      call gspos('TGTU',1,'TG  ', 0.,0.,z_pos,0,'ONLY')


*     epoxy glue joint between base tube and Al inlet window


      par(1) = 0.2
      par(2) = tube_r2(8)
      par(3) = glue_thick / 2.

      z_pos = tube_z(8) + par(3) - moth_dz 
   
      call gsvolu('TGGL','TUBE',clas_med_alumaseal,par,3,ivol)
      call gspos('TGGL',1,'TG  ', 0.,0.,z_pos,0,'ONLY')



*     window on upstream end of target cell (Al)
*     or target cell inlet window
      
      par(1) = 0.0
      par(2) = 0.35
      par(3) = wind_thick / 2.
      
      z_pos = tube_z(8) + glue_thick + par(3) - moth_dz
   
      call gsvolu('TGW2','TUBE',clas_med_alu,par,3,ivol)
      call gspos('TGW2',1,'TG  ', 0.,0.,z_pos,0,'ONLY')


******************************
*        Insulation          *
******************************

*     There was a `reference window' 2cm downstream of the target cell 
*     which, during e1-6a, was made of super insulation.  For later runs 
*     it was replaced with 15um of aluminium as the super insulation 
*     was burned through by the beam, (S. Morrow 12/10/02).

*     superinsulation
*     modelled only from start of kapton cell onwards although it actually stretches back around base cone, ring, etc, too.
 
      par(1) = 0.
      par(2) = 360.
      par(3) = 3

      do i=1,3
         par(1+i*3) = ins_z(i)  
         par(2+i*3) = ins_r1(i)
         par(3+i*3) = ins_r2(i)
      enddo
      
      Npar = 3 * par(3) + 3
      
      z_pos = -1 * moth_dz
      
      call gsvolu('TGSI','PCON',tmed_ins,par,Npar,ivol) !superinsulation
      call gspos('TGSI',1,'TG  ', 0.,0.,z_pos,0,'ONLY')


*     epoxy glue joint between superinsulation and Al foil

      par(1) = 0.35 - ins_thick
      par(2) = 0.35
      par(3) = glue_thick / 2.

      z_pos = ins_z(3) + par(3) - moth_dz                  ! upstream edge of mother volume + end of superinsulation
   
      call gsvolu('TGE2','TUBE',clas_med_alumaseal,par,3,ivol)
      call gspos('TGE2',1,'TG  ', 0.,0.,z_pos,0,'ONLY')



*     Al foil (exactly the same as the inlet window)

      par(1) = 0.0
      par(2) = 0.35
      par(3) = wind_thick / 2.
            
      z_pos = ins_z(3) + glue_thick + par(3) - moth_dz    ! upstream edge of mother volume + end of superinsulation + end of glue
      
      call gsvolu('TGFL','TUBE',clas_med_alu,par,3,ivol)
      call gspos('TGFL',1,'TG  ',0.,0.,z_pos,0,'ONLY')



      end     
