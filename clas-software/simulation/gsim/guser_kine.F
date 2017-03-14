	subroutine guser_kine

c_begin_doc

c  Documentation for subroutine guser_kine

c  $Id: guser_kine.F,v 1.14 2009/03/14 20:27:52 fklein Exp $

c  Purpose:
c  --------
c     default user kinematics generator
c     stores kinematics data in both zebra and bos MC banks

c  Notes:
c  ------
c     implements just a 4-particle gun...no physics, no energy conservation, etc.
c     vertex is random in 4mm square box

c  Author:
c  -------
c     Elliott Wolin, College of William and Mary, 27-apr-95

c_end_doc

      implicit none
      character*(*) cfile,crevis,cstate,cdate,cautho,crname,crauth
      character*132 crcsid
      parameter (cfile=  '$RCSfile: guser_kine.F,v $')
      parameter (crevis= '$Revision: 1.14 $')
      parameter (cstate= '$State: Exp $')
      parameter (cdate=  '$Date: 2009/03/14 20:27:52 $')
      parameter (cautho= '$Author: fklein $')
      parameter (crname= 'GUSER_KINE')
      parameter (crauth= 'Elliott Wolin')
      data crcsid/'
     1$Id: guser_kine.F,v 1.14 2009/03/14 20:27:52 fklein Exp $
     1 '/
      save


c  include files:
c  --------------
#include "bcs.inc"
#include "gconst.inc"
#include "processevt.inc"
#include "gcflag.inc"
#include "ffpar_tg.inc"
c_end_inc

c  local variables:
c  ----------------

#ifdef Linux
#ifndef OSF1
        real sind, cosd, tand
        external sind, cosd, tand
#endif
#endif
#ifdef USEGNU
        real sind, cosd, tand
        external sind, cosd, tand
#endif
	logical first_time
	data first_time/.true./

	real pmom,plab(4),amass,charge,tlife,ub,theta,phi
	integer tknum,id,itrtyp,nub,primary_vertex_id
	real primary_vertex(3)
        character*20 napart
	real uran(4),acosd
        integer itk,mctk,mcvx,geant_to_pdg,mbank
c_end_var


c  executable code:
c  ----------------

	if(first_time)then
		first_time=.false.
		call recmes(crname,'I',
     1             ' *** Default version of guser_kine called ***')
	endif

	process_evt=.TRUE.
	
c  fill bos header bank...15 is for physics events
      call bos_head(15,idrun,idevt)

c  get mcvx,mctk bank pointers
        mcvx=mbank(iw,'MCVX',0,5,1)
        mctk=mbank(iw,'MCTK',0,11,4)

c  primary vertex in (0,0,0), may be smeared if ffread cards 'POSBEAM' or
c                             'SIGBEAM' are set
	call kine_smear_vx(primary_vertex)

c  store primary vertex in zebra
        call grndm(uran,4)
        primary_vertex(1)=0.
        primary_vertex(2)=0.  
        primary_vertex(3)=-2.+4.*uran(4)
      
	if(tg_type_ff.EQ.26)call vert_eg2(primary_vertex)
c	write(11,*)'vert=', primary_vertex(3),' uran',uran(4)
c
cc      primary_vertex(1)=0.
cc      primary_vertex(2)=0.
cc      primary_vertex(3)=5.
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

	call gsvert(primary_vertex,0,0,0,0,primary_vertex_id)
        rw(mcvx+1)=primary_vertex(1)
        rw(mcvx+2)=primary_vertex(2)
        rw(mcvx+3)=primary_vertex(3)
	write(12,*)'vert', rw(mcvx+3) 
        rw(mcvx+4)=0.0
        iw(mcvx+5)=0

c  random electron, p=0.3 to 4 gev, 0 to 45 degrees around z
	id=3
	pmom=0.3+uran(1)*3.7 
	theta=uran(2)*45.       !not really correct, but simple...ejw
	phi=360.*uran(3)

c  get input particle mass
	call gfpart(id,napart,itrtyp,amass,charge,tlife,ub,nub)

c  calculate particle 4-vector
	plab(1)=pmom*sind(theta)*cosd(phi)
	plab(2)=pmom*sind(theta)*sind(phi)
	plab(3)=pmom*cosd(theta)
	plab(4)=sqrt(pmom**2+amass**2)

c  store electron in zebra,bos
	call gskine(plab,id,primary_vertex_id,0,0,tknum)
        rw(mctk+1)=plab(1)/pmom
        rw(mctk+2)=plab(2)/pmom
        rw(mctk+3)=plab(3)/pmom
        rw(mctk+4)=pmom
        rw(mctk+5)=amass
        rw(mctk+6)=charge
        iw(mctk+7)=geant_to_pdg(3)
        iw(mctk+8)=0
        iw(mctk+9)=1
        iw(mctk+10)=0
        iw(mctk+11)=0

c  now make three other pions
        do itk=1,3

c  random pi0,pion+,pi-, p=0.1 to 2 gev, uniform in angle
           id=6+itk
           call grndm(uran,3)
           pmom=0.1+uran(1)*1.9 
           theta=acosd(2.*uran(2)-1.)
           phi=360.*uran(3)
           
c  get input particle mass
           call gfpart(id,napart,itrtyp,amass,charge,tlife,ub,nub)
           
c  calculate particle 4-vector
           plab(1)=pmom*sind(theta)*cosd(phi)
           plab(2)=pmom*sind(theta)*sind(phi)
           plab(3)=pmom*cosd(theta)
           plab(4)=sqrt(pmom**2+amass**2)

c  store particle in zebra,bos
           call gskine(plab,id,primary_vertex_id,0,0,tknum)
           mctk=mctk+11
           rw(mctk+1)=plab(1)/pmom
           rw(mctk+2)=plab(2)/pmom
           rw(mctk+3)=plab(3)/pmom
           rw(mctk+4)=pmom
           rw(mctk+5)=amass
           rw(mctk+6)=charge
           iw(mctk+7)=geant_to_pdg(id)
           iw(mctk+8)=0
           iw(mctk+9)=1
           iw(mctk+10)=0
           iw(mctk+11)=0

        enddo


	return
	end
	
c------------------------------------------------------------------------------
