

       subroutine geom_stnew
  

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
      parameter (cfile=  '$RCSfile: geom_stnew.F,v $')
      parameter (crevis= '$Revision: 1.2 $')
      parameter (cstate= '$State: Exp $')
      parameter (cdate=  '$Date: 2005/02/07 13:40:47 $')
      parameter (cautho= '$Author: devita $')
      parameter (crname= 'GEOM_ST')
      parameter (crauth= 'Simon Taylor')
      data crcsid/'
     1$Id: geom_stnew.F,v 1.2 2005/02/07 13:40:47 devita Exp $
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
		
c Copy the basic mother volume to the  other 5 60 degree pie sections
 
	rot_matrix=get_rotm()
	call gsrotm(rot_matrix,90.,60.,90.,150.,0.,0.)
	call gspos('ST  ',2,'CLAS',0.,0.,zoffset+st_zoff_ff,rot_matrix,'ONLY')
	rot_matrix=get_rotm()
	call gsrotm(rot_matrix,90.,120.,90.,210.,0.,0.)
	call gspos('ST  ',3,'CLAS',0.,0.,zoffset+st_zoff_ff,rot_matrix,'ONLY')
	rot_matrix=get_rotm()
	call gsrotm(rot_matrix,90.,180.,90.,270.,0.,0.)
	call gspos('ST  ',4,'CLAS',0.,0.,zoffset+st_zoff_ff,rot_matrix,'ONLY')
	rot_matrix=get_rotm()
	call gsrotm(rot_matrix,90.,240.,90.,330.,0.,0.)
	call gspos('ST  ',5,'CLAS',0.,0.,zoffset+st_zoff_ff,rot_matrix,'ONLY')
	rot_matrix=get_rotm()
	call gsrotm(rot_matrix,90.,-60.,90.,30.,0.,0.)
	call gspos('ST  ',6,'CLAS',0.,0.,zoffset+st_zoff_ff,rot_matrix,'ONLY')
	
c create mother volumes for each side of the coupled paddle

	mother(2)=60.
	mother(3)=1.
c	write(6,*) mother(1),mother(2),mother(3),mother(4)
c	call gsvolu('ST1 ','PGON',tmed,mother,16,ivol)
c	call gsvolu('ST2 ','PGON',tmed,mother,16,ivol)

c create volume of support structure
        foam_tick1=1.07   ! equivalent tickness of material BELOW the ST paddles
        foam_tick2=0.81   ! equivalent tickness of material ABOVE the ST paddles
        support_mother( 1)=-30.0
        support_mother( 2)=60.
        support_mother( 3)=1
        support_mother( 4)=5
        support_mother( 5)=-leg_parms(3)                                    ! first point in z
        support_mother( 6)=dist_to_bl-foam_tick1
        support_mother( 7)=dist_to_bl+foam_tick2
        support_mother( 8)=leg_parms(3)-foam_tick1/tan(135/2/57.29)            ! second point in z
        support_mother( 9)=dist_to_bl-foam_tick1
        support_mother(10)=dist_to_bl+foam_tick2
        support_mother(11)=leg_parms(3)+foam_tick2/tan(135/2/57.29)            ! third point in z
        support_mother(12)=dist_to_bl-foam_tick1-(foam_tick1+foam_tick2)/tan(135/2/57.29)
        support_mother(13)=dist_to_bl+foam_tick2
        support_mother(14)=support_mother(8)+support_mother(9)-1.5   ! fourth point in z
        support_mother(15)=1.5
        support_mother(16)=1.5+support_mother(13)-support_mother(12)
        support_mother(17)=support_mother(11)+support_mother(13)-1.5   ! fifth point in z
        support_mother(18)=1.5
        support_mother(19)=1.5
        call gsvolu('STFM','PGON',clas_med_lowdfoam,support_mother,19,ivol)
        print *,support_mother

c create mother volume for nose

	call gsvolu('NOSM','TRD1',tmed,nose_mother,4,ivol)
c The nose itself is made up of two trapezoids.

c	call gsvolu('NOS1','TRAP',st_med_scint,nose1_parms,11,ivol)
c	call gsvolu('NOS2','TRD1',st_med_scint,nose2_parms,4,ivol)

ccccccccccc
        nose1_parms(1)=nose2_parms(4)/(nose2_parms(1)-nose2_parms(2))*nose2_parms(1)/2
        nose1_parms(2)=57.29*atan(nose2_parms(1)/nose1_parms(1)/8)
        nose1_parms(3)=0.
        nose1_parms(4)=nose2_parms(3)
        nose1_parms(5)=nose2_parms(1)/4
        nose1_parms(6)=nose2_parms(1)/4
        nose1_parms(7)=0.
        nose1_parms(8)=nose2_parms(3)
        nose1_parms(9)=0.
        nose1_parms(10)=0.
        nose1_parms(11)=0.
	call gsvolu('NOS1 ','TRAP',st_med_scint,nose1_parms,11,ivol)
        nose1_parms(3)=180.
	call gsvolu('NOS4 ','TRAP',st_med_scint,nose1_parms,11,ivol)

        nose3_parms(1)=nose2_parms(1)/4
        nose3_parms(2)=nose2_parms(3)
        nose3_parms(3)=nose1_parms(1)
	call gsvolu('NOS2','BOX ',st_med_scint,nose3_parms,3,ivol)
        nose3_parms(1)=nose2_parms(1)/4
        nose3_parms(2)=nose2_parms(3)
        nose3_parms(3)=nose1_parms(1)
	call gsvolu('NOS3 ','BOX ',st_med_scint,nose3_parms,3,ivol)

        nose5_parms(1)=nose2_parms(4)-
     %     nose2_parms(4)/(nose2_parms(1)-nose2_parms(2))*nose2_parms(1)/2
        nose5_parms(2)=57.29*atan((nose3_parms(1)-nose2_parms(2)/2)/nose5_parms(1)/2)
        nose5_parms(3)=0.
        nose5_parms(4)=nose2_parms(3)
        nose5_parms(5)=nose3_parms(1)
        nose5_parms(6)=nose3_parms(1)
        nose5_parms(7)=0.
        nose5_parms(8)=nose2_parms(3)
        nose5_parms(9)=nose2_parms(2)/2
        nose5_parms(10)=nose2_parms(2)/2
        nose5_parms(11)=0.
	call gsvolu('NNO2','TRAP',st_med_scint,nose5_parms,11,ivol)
        nose5_parms(3)=180.
	call gsvolu('NNO3','TRAP',st_med_scint,nose5_parms,11,ivol)


c which are positioned inside the 'nose-mother':

c	temp1 = nose2_parms(4)-nose_mother(4)
c	temp2 = nose1_parms(1)*tand(nose1_parms(2))
c 	call gspos('NOS2',1,'NOSM',0.,0.,temp1,0,'MANY')
c	call gspos('NOS1',1,'NOSM',temp2,0.,nose2_parms(4),0,'MANY')
ccccccccccccccc
        temp1 = -nose_mother(1)/2-(nose1_parms(1)*tan(nose1_parms(2)/57.29))
        temp2 = -nose_mother(4)+nose1_parms(1)
	call gspos('NOS1',1,'NOSM',temp1,0.,temp2,0,'ONLY')
        temp1 = +nose_mother(1)/2+(nose1_parms(1)*tan(nose1_parms(2)/57.29))
        temp2 = -nose_mother(4)+nose1_parms(1)
	call gspos('NOS4',1,'NOSM',temp1,0.,temp2,0,'ONLY')
        temp1 = -nose3_parms(1)
        temp2 = -nose_mother(4)+nose1_parms(1)
	call gspos('NOS2',1,'NOSM',temp1,0.,temp2,0,'ONLY')
        temp1 = nose3_parms(1)
        temp2 = -nose_mother(4)+nose1_parms(1)
	call gspos('NOS3',1,'NOSM',temp1,0.,temp2,0,'ONLY')
        temp1 = -nose2_parms(2)/2-(nose3_parms(1)-nose2_parms(2)/2)/2
        temp2 = -nose_mother(4)+nose1_parms(1)*2+nose5_parms(1)
	call gspos('NNO2',1,'NOSM',temp1,0.,temp2,0,'ONLY')
        temp1 = +nose2_parms(2)/2+(nose3_parms(1)-nose2_parms(2)/2)/2
        temp2 = -nose_mother(4)+nose1_parms(1)*2+nose5_parms(1)
	call gspos('NNO3',1,'NOSM',temp1,0.,temp2,0,'ONLY')




 
c The legs for each piece are composed of BOXes
        leg_parms(2)=leg_parms(2)/4
	call gsvolu('LEG1','BOX ', st_med_scint,leg_parms,3,ivol)
	call gsvolu('LEG2','BOX ', st_med_scint,leg_parms,3,ivol)
	call gsvolu('LEG3','BOX ', st_med_scint,leg_parms,3,ivol)
	call gsvolu('LEG4','BOX ', st_med_scint,leg_parms,3,ivol)
        leg_parms(2)=leg_parms(2)*4
c	call gsvolu('LEG2','BOX ',st_med_scint,leg_parms,3,ivol)

c Position each leg inside the 'Side-mother' ST1 or ST2

	call gspos('LEG1',1,'STFM',dist_to_bl,-3*leg_parms(2)/4,0.,0,'ONLY')
	call gspos('LEG2',1,'STFM',dist_to_bl,-leg_parms(2)/4,0.,0,'ONLY')
	call gspos('LEG3',1,'STFM',dist_to_bl,leg_parms(2)/4,0.,0,'ONLY')
	call gspos('LEG4',1,'STFM',dist_to_bl,3*leg_parms(2)/4,0.,0,'ONLY')

c	call gspos('LEG2',1,'ST2 ',dist_to_bl,0.,0.,0,'MANY')

c Position the nose pieces inside the side-mothers.

	rot_matrix=get_rotm()
	temp3 = leg_parms(3)+nose_mother(4)*cosd(45.)
	temp4 = dist_to_bl - nose_mother(4)*sind(45.)
	temp5 = nose_mother(1)-leg_parms(2)

c	call gsrotm(rot_matrix,90.,90.,60.,0.,30.,180.)
	call gsrotm(rot_matrix,90.,90.,45.,0.,45.,180.)
	call gspos('NOSM',1,'STFM',temp4,temp5,temp3,rot_matrix,'MANY')
	rot_matrix=get_rotm()
c	call gsrotm(rot_matrix,90.,270.,60.,0.,30.,180.)
	
c	call gspos('NOSM',2,'ST2 ',temp4,-temp5,temp3,rot_matrix,'MANY')

c Position each side inside the coupled-paddle mother

c	call gspos('ST1 ',1,'ST  ',0.,0.,0.,0,'MANY')
c	rot_matrix=get_rotm()
c	call gsrotm(rot_matrix,90.,60.,90.,150.,0.,0.)
c	call gspos('ST2 ',1,'ST  ',0.,0.,0.,rot_matrix,'MANY')

c        call gsdvn ('NLEG', 'LEG1', 4, 2)

	call gspos ('STFM',1,'ST  ',0.,0.,0.,0,'ONLY')        

      return
      end 

c----------------------------------------------------------------------------------


















