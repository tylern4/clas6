      SUBROUTINE StGeom_initialize(irun)
c
c
c_begin_doc
c  RCS ID string
c  $Id: stgeom_initialize.F,v 1.9 2008/03/16 04:33:24 fklein Exp $
c
c  Documentation for subroutine StGeom_initialize
c
c  Purpose: 
c  --------
c
c  Calling Sequence:
c  ----------------
c
c  Input Parameters:  NONE
c  ----------------
c
c  Output Parameters:  NONE
c  -----------------
c
c  Called from:
c  ------------
c
c  Other routines:
c  ---------------
c
c  Notes:
c  ------
c
c  Author:   Francois ROUDOT      Created:  Thu Jun 19 12:00:00 EST 1997
c  -------
c
c  Major revisions:
c  ----------------
c     - calculate the boundary for ST :Burin Feb98
c
c_end_doc
c
      IMPLICIT NONE
      SAVE
c
c_begin_inc
c  include files :
c  ---------------------
c BOS common block  uncomment the next line for BOS include file
c
#include "bcs.inc"
#include "wcs.inc"
#include "st_general.PAR"
#include "stgeom.CMN"
#include "sttcl.CMN"
c_end_inc
c
c_begin_var
c  input/output variables:
c  -----------------------
c
c
c  Local pre-defined variables:
c  ---------------------------
c  RCS information: 
      CHARACTER*(*)  CFILE, CREVIS, CSTATE, CDATE, CAUTHO 
      PARAMETER (CFILE=  '$RCSfile: stgeom_initialize.F,v $')
      PARAMETER (CREVIS= '$Revision: 1.9 $')
      PARAMETER (CSTATE= '$State: Exp $')
      PARAMETER (CDATE=  '$Date: 2008/03/16 04:33:24 $')
      PARAMETER (CAUTHO= '$Author: fklein $')
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='STGeom_initialize')
      PARAMETER (CRAUTH='Francois Roudot')
      
      INTEGER NWRITE
c
c  Local st defined variables:
c  ----------------------------
      integer idx
      integer name_i,name_in,mamind
      integer isec, irun
      integer beam_save
      integer get_code_type
      REAL pi
      DATA pi/3.14159265/	
c_end_var
c
c  executable code for routine STGeom_initialize:
c  ------------------------------------
c
      IF (NWRITE .LT. 1) THEN
        NWRITE = NWRITE + 1
        CRMESS='initialize ST geometry'
        CALL RECMES(CRNAME,'I',CRMESS)
      ENDIF

*** choose between  0 = old ST Fortran  1 = new ST Fortran    2 = new ST C++ 
      if(st_type.LT.0 .OR. st_type.GT.2) then
         StType = get_code_type(irun)
      else
         stType=st_type
      endif
      if (StType.lt.0) then     ! STN_CALIB code_type value not in CALDB

*** choose between old and new ST geometry based on RUNC info
         call get_beam_type(beam_save)
         if(beam_save.ne.4) then
            StType=0               ! old start counter
         else           
            StType=1               ! new start counter
         endif
      endif     
      write(crmess,'(A,I2)') 'Initialize ST geometry for type',stType
      call recmes(crname,'W',crmess)
      if (StType.eq.2) return   ! We use C++, no Fortran constants needed

c  initialize array of sector id - pair id correspondance
c
      pair_id_2_sect_id(1,1)=1
      pair_id_2_sect_id(1,2)=2
      pair_id_2_sect_id(2,1)=3
      pair_id_2_sect_id(2,2)=4
      pair_id_2_sect_id(3,1)=5
      pair_id_2_sect_id(3,2)=6

c  Creating ST geomtry bank

c      call make_stg_banks(irun);

      call StConfigGeom(irun)

c  ST plane info. from trk_geom.F 
c  The Sector Coor. System used in trk_geom.F has x along the beamline,
c  y along the midplane of DC BUT the SCS used here has z along the beamline 
c  and x along the midplane of DC sector 1. To get the Detector Coor. System
c  one has to rotate 60 degrees for each sector.   
c$$$      do isec=1,St_max_sector
c$$$         St_Normal_vect(isec,X,leg) = cx_leg*cos((isec-1)*pi/3.)
c$$$     1    			      - cy_leg*sin((isec-1)*pi/3.)	
c$$$         St_Normal_vect(isec,Y,leg) = cx_leg*sin((isec-1)*pi/3.)
c$$$     1    			      + cy_leg*cos((isec-1)*pi/3.)
c$$$         St_Normal_vect(isec,Z,leg) = cz_leg
c$$$         St_Rnormal_dist(isec,leg)  = D_leg
c$$$         St_Normal_vect(isec,X,nose) = cx_nose*cos((isec-1)*pi/3.)
c$$$     1    			       - cy_nose*sin((isec-1)*pi/3.)
c$$$         St_Normal_vect(isec,Y,nose) = cx_nose*sin((isec-1)*pi/3.)
c$$$     1    			       + cy_nose*cos((isec-1)*pi/3.)
c$$$         St_Normal_vect(isec,Z,nose) = cz_nose
c$$$         St_Rnormal_dist(isec,nose) = D_nose
 
c  set the boundaries for ST geometry, use SCS in sector 1
           
c$$$        leg_x_max =  D_leg 
c$$$        leg_y_min = -W_leg/2.
c$$$        leg_y_max =  W_leg/2.
c$$$        leg_z_min = -L_leg/2. + z_offset
c$$$        leg_z_max =  L_leg/2. + z_offset
c$$$        
c$$$        nose_x_min = D_leg - L_nose*sin(pi/6.)
c$$$        nose_x_max = D_leg
c$$$        nose_z_min = leg_z_max 
c$$$        nose_z_max = leg_z_max + L_nose*cos(pi/6.)
        
c  nose_y = a_nose*nose_x + b_nose*nose_z + c_nose
c$$$	b_nose = (W_leg/2.-(W_leg-W_nose)/2./L_nose/sin(pi/6.)*D_leg)
c$$$     1           / (L_leg/2. + z_offset + D_leg*tan(pi/3.))
c$$$     	a_nose = (W_leg-W_nose)/2./L_nose/sin(pi/6.) + b_nose*tan(pi/3.)
c$$$        c_nose = W_leg/2. - a_nose*nose_x_max - b_nose*nose_z_min
c$$$      enddo  
	
      RETURN
      END
c
c------------------------------------------------------------------------------
