      subroutine sets_dc

c_begin_doc

c  Documentation for subroutine sets_dc

c  $Id: sets_dc.F,v 1.4 1999/07/30 19:54:01 feuerbac Exp $

c  Purpose:
c  --------
c     ******************************************************************
c     *                                                                *
c     *      Define drift chambers as a sensitive set, define the hit  *
c     *      and digitization parameters.                              *
c     *                                                                *
c     *       Author    M.Guckes       Oct.  87                        *
c     *                                                                *
c     *                                                                *
c     *          MODIFIED by M. Guidal March 93                        *
c     *          ==============================                        *
c     ******************************************************************

c  Notes:
c  ------
c     each superlayer is a different set, but all have the same idtype


c  Major revisions:
c  ----------------
c     Elliott Wolin, College of William and Mary, 9-nov-94
c     Kyungseon Joo, 14-Apr-1998
c      change NAMESD and NBITSD to add "DOCA".
c
c_end_var   
c_end_doc


      implicit none
      character*(*) cfile,crevis,cstate,cdate,cautho,crname,crauth
      character*132 crcsid
      parameter (cfile=  '$RCSfile: sets_dc.F,v $')
      parameter (crevis= '$Revision: 1.4 $')
      parameter (cstate= '$State: Exp $')
      parameter (cdate=  '$Date: 1999/07/30 19:54:01 $')
      parameter (cautho= '$Author: feuerbac $')
      parameter (crname= 'sets_dc')
      parameter (crauth= 'Elliott Wolin')
      data crcsid/'
     1$Id: sets_dc.F,v 1.4 1999/07/30 19:54:01 feuerbac Exp $
     1 '/
      save


c  include files:
c  --------------
#include "gcbank.inc"
#include "gcvolu.inc"
#include "gcunit.inc"
#include "gclist.inc"
#include "gcsets.inc"
#include "gconst.inc"

#include "lcgeom.inc"
#include "dcgeom.inc"
#include "dcpar.inc"
#include "dcsets.inc"
c_end_inc


c  local variables:
c  ----------------

      integer is,get_idtype

c  for dc hits
      character*4 namesv(nvdc), namesh(nhdc), namesd(nddc)
      integer nbitsv(nvdc),nbitsh(nhdc),nbitsd(nddc)
      real orig(nhdc),fact(nhdc)
      data NAMESV   /'DC1A', 'DS1A', 'DW1A'/
      data NBITSV /     3,      3,      6/
      data NAMESH /'X   ','Y   ','Z   ','STEP','DEDX','PTOT','TOFG'
     +     ,'CX','CY','CZ','STAK','PART'/
      data NBITSH /nhdc*32/
      data ORIG   /  400.,  400.,  500.,    0.,    0.,    0.,    0.
     +     ,100.,100.,100.,1.,1./
      data FACT   / 100000., 100000., 100000.,  100.,  1.E7,  1.E4,
     +     1.E4,100000.,100000.,100000.,1.,1./
      data NAMESD /'WIRE','ADC ','TIME','DOCA'/
      data NBITSD /     0,     0,     0,     0/
c_end_var


c  executable code:
c  ----------------


c  set for each superlayer, detectors are wire planes
      idtype_dc=get_idtype()
      do is=1,ndc_sl

        namesv(1)=cdet_dcr(is)
        namesv(2)=cdet_dcs(is)
        namesv(3)=cdet_dcw(is)

        call gsdet('DC  ',cdet_dcs(is), nvdc, namesv, nbitsv,
     1       idtype_dc, 1000, 1000,set_dc, det_dc(is))
        call gsdeth('DC  ',cdet_dcs(is), nhdc, namesh, nbitsh,
     1       orig, fact)
        call gsdetd('DC  ',cdet_dcs(is), nddc, namesd, nbitsd)

	if (is.le.2) then
           namesv(1)=cdet_str(is)
           namesv(2)=cdet_sts(is)
           namesv(3)=cdet_stw(is)
           call gsdet('DC  ',cdet_sts(is), nvdc, namesv, nbitsv,
     1          idtype_dc, 1000, 1000, set_dc,det_dcst(is))
           call gsdeth('DC  ',cdet_sts(is), nhdc, namesh, nbitsh,
     1          orig, fact)
	endif

      enddo

      return
      end

c---------------------------------------------------------------------------------
