      subroutine sets_sc

c_begin_doc

c  Documentation for subroutine sets_ec

c  $Id: sets_sc.F,v 1.4 1998/04/26 09:51:06 holtrop Exp $

c  Purpose:
c  --------
c     ******************************************************************
c     *                                                                *
c     *      Define the scintillator as a sensitive set,               *
c     *      define the hit and digitization parameters.               *
c     *                                                                *
c     *    ==>Called by : GELAS                                        *
c     *       Author    M.Guckes       Oct.  87                        *
c     *                                                                *
c     *          MODIFIED by M. Guidal March 93                        *
c     *          ==============================                        *
c     ******************************************************************

c  Major revisions:
c  ----------------
c     Elliott Wolin, College of William and Mary, 2-dec-94

c_end_doc


      implicit none
      character*(*) cfile,crevis,cstate,cdate,cautho,crname,crauth
      character*132 crcsid
      parameter (cfile=  '$RCSfile: sets_sc.F,v $')
      parameter (crevis= '$Revision: 1.4 $')
      parameter (cstate= '$State: Exp $')
      parameter (cdate=  '$Date: 1998/04/26 09:51:06 $')
      parameter (cautho= '$Author: holtrop $')
      parameter (crname= 'sets_sc')
      parameter (crauth= 'Elliott Wolin')
      data crcsid/'
     1$Id: sets_sc.F,v 1.4 1998/04/26 09:51:06 holtrop Exp $
     1 '/
      save


c  include files:
c  --------------
#include "gcunit.inc"
#include "gclist.inc"
#include "gcsets.inc"
#include "gconst.inc"
#include "scsets.inc"
c_end_inc

c  local variables:
c  ----------------

c  for data
      character*4 namesv(nvol_sc),namesh(nhits_sc),namesd(ndigi_sc)
      integer nbitsv(nvol_sc),nbitsh(nhits_sc),nbitsd(ndigi_sc)
      real orig(nhits_sc),fact(nhits_sc)

      data namesv/'SC  ','SCIN'/
      data nbitsv/3,9/

C      data namesh /'X   ','Y   ','Z   ','STEP','TOF ','ELOS'/
C      data nbitsh / 0,     0,     0,       0,     0,     0 /
C      data orig   / 500.,  500.,  500.,    0.,    0.,    0./
C      data fact   / 100.,  100.,  100.,  1.e4,  1.e5,  1.e5/

      data namesh /'X   ','Y   ','Z   ','TOF ','ELOS'/
      data nbitsh / 0,     0,     0,      0,     0 /
      data orig   / 500.,  500.,  500.,  0.,    0./
      data fact   / 100.,  100.,  100., 1.e5,  1.e5/

      data namesd /'ADC1','TDC1','ADC2','TDC2'/
      data nbitsd /    0,     0,     0,     0/

c  for mc hits
      character*4 nameshh(nhits_sch)
      real origh(nhits_sch),facth(nhits_sch)
      integer nbitshh(nhits_sch)
      data nameshh/'X','Y','Z','CX','CY','CZ','PTOT','STAK','PART'/
      data origh  /500.,500.,500.,1.,1.,1.,0.,0.,0./
      data facth  /100.,100.,100.,100.,100.,100.,1.e4,1.,1./
      data nbitshh /nhits_sch*0/

      integer get_idtype
c_end_var


c  executable code:
c  ----------------

c  set for data
      idtype_sc=get_idtype()

      write(6,*)' idtype_sc = ',idtype_sc

      call  gsdet('SC  ','SCIN',nvol_sc,namesv,nbitsv,
     1     idtype_sc,1000,1000,set_sc,det_sc)
      call gsdeth('SC  ','SCIN',nhits_sc,namesh,nbitsh,orig,fact)
      call gsdetd('SC  ','SCIN',ndigi_sc,namesd,nbitsd)


c  set for MC hits

      idtype_sch=get_idtype()
      write(6,*)' idtype_sch = ',idtype_sch
      call gsdetv('SCH ','SC  ',idtype_sch,1000,1,set_sch,det_sch)
      call gsdeth('SCH ','SC  ',nhits_sch,nameshh,nbitshh,origh,facth)

      return
      end

c-----------------------------------------------------------------------
