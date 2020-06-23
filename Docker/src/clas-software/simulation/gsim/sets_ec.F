      subroutine sets_ec

c_begin_doc

c  Documentation for subroutine sets_ec

c  $Id: sets_ec.F,v 1.3 2010/01/09 01:48:21 fklein Exp $

c  Purpose:
c  --------
c      ******************************************************************
c      *                                                                *
c      *      Define the shower calorimeter as a sensitive set,         *
c      *      define the hit and digitization parameters.               *
c      *                                                                *
c      *       Author    M.Guckes       Oct.  87                        *
c      *                                                                *
c      *                                                                *
c      *          MODIFIED by M. Guidal March 93                        *
c      *          ==============================                        *
c      ******************************************************************

c  Other routines:
c  ---------------
c     called by user_geom

c  Major revisions:
c  ----------------
c     Elliott Wolin, College of William and Mary, 16-nov-94

c_end_doc


      implicit none
      character*(*) cfile,crevis,cstate,cdate,cautho,crname,crauth
      character*132 crcsid
      parameter (cfile=  '$RCSfile: sets_ec.F,v $')
      parameter (crevis= '$Revision: 1.3 $')
      parameter (cstate= '$State: Exp $')
      parameter (cdate=  '$Date: 2010/01/09 01:48:21 $')
      parameter (cautho= '$Author: fklein $')
      parameter (crname= 'SETS_EC')
      parameter (crauth= 'Elliott Wolin')
      data crcsid/'
     1$Id: sets_ec.F,v 1.3 2010/01/09 01:48:21 fklein Exp $
     1 '/
      save


c  include files:
c  --------------
#include "gcflag.inc"
#include "gcunit.inc"
#include "gclist.inc"
#include "gcsets.inc"
#include "gconst.inc"
#include "ecgeom.inc"
#include "ecsets.inc"
c_end_inc

c  local variables:
c  ----------------
      character*4 namesv(nvec), namesh(nhec), namesd(ndec)

c  the following was automatically generated...ejw
      integer i
      integer nbitsv,nbitsd,nbitsh
      real fact,orig
      dimension   nbitsv(nvec),nbitsh(nhec),orig(nhec),fact(nhec)
     1           ,nbitsd(ndec)


      data namesv/'LSS-','ECAL','ECUI','CSUI'/
      data nbitsv /        3,     3,     6,     6/
      data namesh /'X   ','Y   ','Z   ','PTOT','TIME','NOTH','ELOS'/

      data nbitsh /    0,     0,     0,     0,     0,0,0/
      data orig   /  500.,  500.,  500.,    0.,    0.,0.,0./
      data fact   /  100.,  100.,  100.,  1.e5,  1.e4,1.e4,1.e6/
      data namesd /'ADC ','TDC '/

c  use one full word
      data nbitsd /0,0/

c  for ECH hits
      character*4 nameshh(nhits_ech)
      integer nbitshh(nhits_ech)
      real facth(nhits_ech),origh(nhits_ech)

      data nameshh/'X','Y','Z','CX','CY','CZ','PTOT','PART','TOF'/
      data nbitshh/32,32,32,32,32,32,32,32,64/
      data origh  /500.,500.,500.,1.,1.,1.,0.,0.,0./
      data facth   /100.,100.,100.,100.,100.,100.,1.E+4,1.,1.E+4/

      integer get_idtype
c_end_var


c  executable code:
c  ----------------

c  define ec hits,digi for raw data
      idtype_ec=get_idtype()
      do i=1,6
         namesv(3)=cplate_ec(i)
         namesv(4)=cdet_ec(i)

         call gsdet('EC  ',cdet_ec(i),nvec,namesv,nbitsv,
     1        idtype_ec,1000,1000,set_ec,det_ec(i))
         call gsdeth('EC  ',cdet_ec(i),nhec,namesh,nbitsh,orig,fact)
         call gsdetd('EC  ',cdet_ec(i),ndec,namesd,nbitsd)
      enddo


c  define ec mc hits;  (x,y,z,cx,cy,cz,p,ipart,tof)
      idtype_ech=get_idtype()
      call gsdetv('ECH ','EC  ',idtype_ech,1000,1000,set_ech,det_ech)
      call gsdeth('ECH ','EC  ',nhits_ech,nameshh,nbitshh,origh,facth)

      return
      end

c-------------------------------------------------------------------------


