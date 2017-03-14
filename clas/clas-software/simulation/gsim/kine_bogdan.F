	subroutine kine_bogdan

c_begin_doc

c  Documentation for subroutine kine_bogdan

c  $Id: kine_bogdan.F,v 1.11 2009/03/14 20:27:52 fklein Exp $

c  Purpose:
c  --------
c     For SDA testing...writes both MCIN and MCVX,MCTK banks

c  Notes:
c  ------
c     particle id is int(pkine(1)) from kine card, default is e-,
c       0.8<p<3.5, 30<theta<40, phi=+-10 degrees

c  Author:
c  -------
c     Elliott Wolin, College of William and Mary, 22-aug-95

c_end_doc

      implicit none
      character*(*) cfile,crevis,cstate,cdate,cautho,crname,crauth
      character*132 crcsid
      parameter (cfile=  '$RCSfile: kine_bogdan.F,v $')
      parameter (crevis= '$Revision: 1.11 $')
      parameter (cstate= '$State: Exp $')
      parameter (cdate=  '$Date: 2009/03/14 20:27:52 $')
      parameter (cautho= '$Author: fklein $')
      parameter (crname= 'KINE_BOGDAN')
      parameter (crauth= 'Elliott Wolin')
      data crcsid/'
     1$Id: kine_bogdan.F,v 1.11 2009/03/14 20:27:52 fklein Exp $
     1 '/
      save


c  include files:
c  --------------
#include "bcs.inc"
#include "gcbank.inc"
#include "gcnum.inc"
#include "gconst.inc"
#include "gckine.inc"
#include "gsimpar.inc"
#include "gsimcontrol.inc"
#include "gcflag.inc"
c_end_inc

c  local variables:
c  ----------------


#ifdef Linux
#ifndef OSF1
	real acosd, sind, cosd
	external acosd, sind, cosd
#endif
#endif
#ifdef USEGNU
	real acosd, sind, cosd
	external acosd, sind, cosd
#endif
	real pmom,plab(4),mass,chg,lifetime,ub,theta,phi
	integer tknum,id,ttyp,nub,primary_vertex_id,i
	real primary_vertex(3),buf(1)
        character*20 name
	real uran(3), vx_off(3)
	real cmin,cmax,cos_theta
        integer mctk,mcvx,geant_to_pdg,mbank,mcin
c_end_var

c  executable code:
c  ----------------

c  fill bos header bank...15 is for physics events
      call bos_head(15,idrun,idevt)

c  smear vertex position if ffread cards 'POSBEAM' or 'SIGBEAM' are set
	call kine_smear_vx(vx_off)

c  store primary vertex (0,0,0) in zebra
	do i=1,3
	   primary_vertex(i) = pkine(7+i) + vx_off(i)
	enddo
	call gsvert(primary_vertex,0,0,0,0,primary_vertex_id)

c  generate kinematics according to pkine parameters (not really correct, but...)
c
c Fixed to give a uniform distribution over spherical coordinates. (Maurik 3/10/97)
c
	cmin = cosd(pkine(4))
	cmax = cosd(pkine(5))

        id=int(pkine(1))
        call grndm(uran,3)
	pmom =pkine(2)+uran(1)*(pkine(3)-pkine(2))

	cos_theta= cmin+uran(2)*(cmax-cmin)
	theta = acosd(cos_theta)

        phi=mod(pkine(6)+uran(3)*(pkine(7)-pkine(6)),360.)

c  get input particle mass
	call gfpart(id,name,ttyp,mass,chg,lifetime,ub,nub)

c  calculate particle 4-vector
	plab(1)=pmom*sind(theta)*cosd(phi)
	plab(2)=pmom*sind(theta)*sind(phi)
	plab(3)=pmom*cos_theta
	plab(4)=sqrt(pmom**2+mass**2)

c  store electron in zebra
	buf(1)=1
	call gskine(plab,id,primary_vertex_id,buf,1,tknum)

c  store in BOS, incl. obsolete MCIN bank for Bogdan
        if(.not.nobosout(all))then
           mcvx=mbank(iw,'MCVX',0,5,1)
           mctk=mbank(iw,'MCTK',0,11,1)
           mcin=mbank(iw,'MCIN',0,10,1)

           call ucopy(primary_vertex,rw(mcvx+1),3)
           rw(mcvx+4)=0.0
           iw(mcvx+5)=0

           rw(mctk+1)=plab(1)/pmom
           rw(mctk+2)=plab(2)/pmom
           rw(mctk+3)=plab(3)/pmom
           rw(mctk+4)=pmom
           rw(mctk+5)=mass
           rw(mctk+6)=chg
           iw(mctk+7)=geant_to_pdg(id)
           iw(mctk+8)=0
           iw(mctk+9)=1
           iw(mctk+10)=0
           iw(mctk+11)=0

           call ucopy(primary_vertex,rw(mcin+1),3)
           rw(mcin+4)=plab(1)/pmom
           rw(mcin+5)=plab(2)/pmom
           rw(mcin+6)=plab(3)/pmom
           rw(mcin+7)=pmom
           rw(mcin+8)=mass
           rw(mcin+9)=chg
           iw(mcin+10)=geant_to_pdg(id)
        endif

	return
	end
	
c------------------------------------------------------------------------------
