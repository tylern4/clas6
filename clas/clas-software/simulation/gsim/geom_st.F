

       subroutine geom_st
  

c_begin_doc

c  Documentation for subroutine geom_st

c  

c  Purpose:
c  --------
c     ******************************************************************
c     *                                                                *
c     *      Defines the geometry of the Start Counter                 *
c     *                                                                *
c     *                                                                *
c     ******************************************************************

c  Major revisions:
c  ----------------
c    Simon Taylor 7/22/96
c    Burin Asavapibhop 11/20/97 update volume

c_end_doc


      implicit none
      character*(*) cfile,crevis,cstate,cdate,cautho,crname,crauth
      character*132 crcsid
      parameter (cfile=  '$RCSfile: geom_st.F,v $')
      parameter (crevis= '$Revision: 1.9 $')
      parameter (cstate= '$State: Exp $')
      parameter (cdate=  '$Date: 2001/04/23 17:53:14 $')
      parameter (cautho= '$Author: fklein $')
      parameter (crname= 'GEOM_ST')
      parameter (crauth= 'Simon Taylor')
      data crcsid/'
     1$Id: geom_st.F,v 1.9 2001/04/23 17:53:14 fklein Exp $
     1 '/
      save


c  include files:
c  --------------
#include "gsimpar.inc"
#include "gsimcontrol.inc"
#include "gcunit.inc"
#include "gconst.inc"
#include "gclist.inc"
#include "lcgeom.inc"
#include "sttmed.inc"
#include "cltmed.inc"
#include "stgeom.inc"
#include "ffpar_st.inc"
c_end_inc

c  local variables:
c  ----------------

cyun29/9/97 Linux/OSF1


#ifdef Linux
#ifndef OSF1

        real sind, tand, cosd
        external sind, tand, cosd
#endif
#endif

#ifdef USEGNU 
        real sind, tand, cosd
        external sind, tand, cosd
#endif

	integer ivol,tmed
      	integer rot_matrix, get_rotm

	real temp1,temp2,temp3,temp4,temp5
c	real tand
c       	external tand

c_end_var


c  executable code:
c  ----------------
      
      call recmes(crname,'I','ST geometry routine begun...')

c reads geometry file and fills common	
     	call geom_st_init()

	if(helium_only)then
        	 tmed=clas_med_helium
      	else
        	 tmed=clas_med_air
     	endif

c  Define mother volume for scintillators (PGON)

      	call gsvolu('ST  ','PGON',tmed,mother,16,ivol)
	call gspos('ST  ',1,'CLAS',0.,0.,zoffset+st_zoff_ff,0,'ONLY')
		
c Copy the basic mother volume to the two other 120 degree pie sections
 
	rot_matrix=get_rotm()
	call gsrotm(rot_matrix,90.,-120.,90.,-30.,0.,0.)
	call gspos('ST  ',3,'CLAS',0.,0.,zoffset+st_zoff_ff,rot_matrix,'ONLY')
	rot_matrix=get_rotm()
	call gsrotm(rot_matrix,90.,120.,90.,210.,0.,0.)
	call gspos('ST  ',2,'CLAS',0.,0.,zoffset+st_zoff_ff,rot_matrix,'ONLY')

	
c create mother volumes for each side of the coupled paddle

	mother(2)=60.
	mother(3)=1.
c	write(6,*) mother(1),mother(2),mother(3),mother(4)
	call gsvolu('ST1 ','PGON',tmed,mother,16,ivol)
	call gsvolu('ST2 ','PGON',tmed,mother,16,ivol)

c create mother volume for nose

	call gsvolu('NOSM','TRD1',tmed,nose_mother,4,ivol)
c The nose itself is made up of two trapezoids.

	call gsvolu('NOS1','TRAP',st_med_scint,nose1_parms,11,ivol)
	call gsvolu('NOS2','TRD1',st_med_scint,nose2_parms,4,ivol)

c which are positioned inside the 'nose-mother':

	temp1 = nose2_parms(4)-nose_mother(4)
	temp2 = nose1_parms(1)*tand(nose1_parms(2))
	
	call gspos('NOS2',1,'NOSM',0.,0.,temp1,0,'MANY')
	call gspos('NOS1',1,'NOSM',temp2,0.,nose2_parms(4),0,'MANY')

c The legs for each piece are composed of BOXes

	call gsvolu('LEG1','BOX ', st_med_scint,leg_parms,3,ivol)
	call gsvolu('LEG2','BOX ',st_med_scint,leg_parms,3,ivol)

c Position each leg inside the 'Side-mother' ST1 or ST2

	call gspos('LEG1',1,'ST1 ',dist_to_bl,0.,0.,0,'MANY')
	call gspos('LEG2',1,'ST2 ',dist_to_bl,0.,0.,0,'MANY')

c Position the nose pieces inside the side-mothers.

	rot_matrix=get_rotm()
	temp3 = leg_parms(3)+nose_mother(4)*cosd(30.)
	temp4 = dist_to_bl - nose_mother(4)*sind(30.)
	temp5 = nose_mother(1)-leg_parms(2)

	call gsrotm(rot_matrix,90.,90.,60.,0.,30.,180.)
	call gspos('NOSM',1,'ST1 ',temp4,temp5,temp3,rot_matrix,'MANY')
	rot_matrix=get_rotm()
	call gsrotm(rot_matrix,90.,270.,60.,0.,30.,180.)
	
	call gspos('NOSM',2,'ST2 ',temp4,-temp5,temp3,rot_matrix,'MANY')

c Position each side inside the coupled-paddle mother

	call gspos('ST1 ',1,'ST  ',0.,0.,0.,0,'MANY')
	rot_matrix=get_rotm()
	call gsrotm(rot_matrix,90.,60.,90.,150.,0.,0.)
	call gspos('ST2 ',1,'ST  ',0.,0.,0.,rot_matrix,'MANY')

      return
      end

c----------------------------------------------------------------------------------


















