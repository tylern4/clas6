	subroutine geom_st_init()

c_begin_doc

c  Documentation for subroutine geom_st_init

c  $Id: geom_st_init.F,v 1.2 2004/09/07 16:47:32 devita Exp $

c  Purpose:
c  --------
c   fills the common stgeom with data from file stgeom.dat

c  Author:
c  ----------------
c     Etienne Burtin, University of South Carolina, 01.24.95 

c_end_doc


      implicit none
      character*(*) cfile,crevis,cstate,cdate,cautho,crname,crauth
      character*132 crcsid
      parameter (cfile=  '$RCSfile: geom_st_init.F,v $')
      parameter (crevis= '$Revision: 1.2 $')
      parameter (cstate= '$State: Exp $')
      parameter (cdate=  '$Date: 2004/09/07 16:47:32 $')
      parameter (cautho= '$Author: devita $')
      parameter (crname= 'GEOM_ST_INIT')
      parameter (crauth= 'Simon Taylor')
      data crcsid/'
     1$Id: geom_st_init.F,v 1.2 2004/09/07 16:47:32 devita Exp $
     1 '/
      save


c  include files:
c  --------------
#include "gsimpar.inc"
#include "stgeom.inc"
#include "ffpar_st.inc"
c  local variables:
c  ----------------
	integer icounter,unit,iret
	character*120 filename
	character*20 end_stat
c_end_var


c  executable code:
c23456  ----------------

      call recmes(crname,'I','Reading ST geometry parameters...')





c  translate env var name and get input geometry file name
	if(st_type.eq.0) then
	   call revinm(clas_parms,'stgeom.dat',filename)
	else
	   call revinm(clas_parms,'stnewgeom.dat',filename)   
	endif
	call reslun(crname,unit,iret)

c I changed the following line 11/18/96 S.T.
        call reopen(filename,unit,'old',iret)
	if(iret.eq.-1) then
		call recmes(crname,'F','?unable to open st geom file')
	else
		read(unit,*)		! Skip comment line
		do icounter=1,16
			read(unit,*)mother(icounter)
		enddo
		read(unit,*)		! Skip comment line
		do icounter=1,3
			read(unit,*)leg_parms(icounter)
		enddo	
		read(unit,*)		! Skip comment line
		do icounter=1,4
			read(unit,*)nose_mother(icounter)
		enddo
		read(unit,*)		! Skip comment line
		do icounter=1,11
			read(unit,*)nose1_parms(icounter)
		enddo
		read(unit,*)		! Skip comment line
		do icounter=1,4
			read(unit,*)nose2_parms(icounter)
		enddo
		read(unit,*)
		read(unit,*)dist_to_bl
		read(unit,*)
		read(unit,*)zoffset
		read(unit,'(a)')end_stat
		if (end_stat.eq.'end') then
	   	 call recmes(crname,'I','Start Counter geometry read normally')
		else
	   	 call recmes(crname,'I','?error in Start Counter geometry')
		endif      
		close(unit)
		call reslun(crname,-unit,iret)
	endif
	return
	end
	
c------------------------------------------------------------------------------


