      subroutine clas_ffky

c_begin_doc

c  Documentation for subroutine clas_ffky

c  $Id: clas_ffky.F,v 1.20 2005/09/26 18:16:41 avakian Exp $

c  Purpose:
c  --------
c       define ffread cards

c  Notes:
c  ------

c  Author:
c  -------
c     Elliott Wolin, College of William and Mary, 18-nov-94

c_end_doc

      implicit none
      character*(*) cfile,crevis,cstate,cdate,cautho,crname,crauth
      character*132 crcsid
      parameter (cfile=  '$RCSfile: clas_ffky.F,v $')
      parameter (crevis= '$Revision: 1.20 $')
      parameter (cstate= '$State: Exp $')
      parameter (cdate=  '$Date: 2005/09/26 18:16:41 $')
      parameter (cautho= '$Author: avakian $')
      parameter (crname= 'clas_ffky')
      parameter (crauth= 'Elliott Wolin')
      data crcsid/'
     1$Id: clas_ffky.F,v 1.20 2005/09/26 18:16:41 avakian Exp $
     1 '/
      save


c  include files:
c  --------------
#include "ffpar.inc"
#include "gsimpar.inc"
#include "gsimcontrol.inc"
#include "gckine.inc"
c_end_inc
*
*  change size of COMMON /CFREAD/ (used in packlib/ffread) 
*  in order to allow more FFREAD data cards
        integer  FFspace, space
        parameter ( FFspace = 2000 )
        common /CFREAD/ space(FFspace)
c
c  local variables:
c  ----------------
c_end_var


c  executable code:
c  ----------------

       call ffinit(FFspace)
       call ffset('SIZE',4)

c  general purpose control cards
      call ffkey('NOGEOM',nogeom_ff,   max_ff,'MIXED')
      call ffkey('NOFIELD',nofield,     1,      'LOGI')
      call ffkey('NOMATE',nomate_ff,   max_ff,'MIXED')
      call ffkey('NOSEC',nosec_ff,    max_ff,'MIXED')
      call ffkey('NOHITS',nohits_ff,   max_ff,'MIXED')
      call ffkey('NODIGI',nodigi_ff,   max_ff,'MIXED')
      call ffkey('NOBOSOUT',nobosout_ff, max_ff,'MIXED')
      call ffkey('NODATA',nodata_ff,   max_ff,'MIXED')
      call ffkey('NOMCDATA',nomcdata_ff, max_ff,'MIXED')
      call ffkey('FASTCODE',fast_code_ff,max_ff,'MIXED')

c   Pregenerated event, input file has raw banks.
      call ffkey('RAWBANKSIN',rawbanks_in,        1,'LOGI' )
      rawbanks_in=.false.

c  controls global secondary generation...default is track, no stack
      call ffkey('IFLGK',iflgk_ff,         1,'INTE' )
      iflgk_ff=0

c   max number steps per track
      call ffkey('NSTEPMAX',nstepmax_ff,        1,'INTE' )
      nstepmax_ff=10000

c   max track length
      call ffkey('SLENGMAX',slengmax_ff,        1,'REAL' )
      slengmax_ff=20000.0

c   max cylindrical radius
      call ffkey('RMAXCYLRAD',rmax_ff,            1,'REAL' )
      rmax_ff=600.

c   min z 
      call ffkey('ZMIN',zmin_ff,            1,'REAL' )
      zmin_ff=-300.0

c   max z 
      call ffkey('ZMAX',zmax_ff,            1,'REAL' )
      zmax_ff=650.

c   helium only
      call ffkey('HELIUMONLY',helium_only,        1,'LOGI' )
      helium_only=.false.

c   zero electron acceptance parameter definition
      call ffkey('NOACC',noacc_ff,3,'REAL')
      noacc_ff(1)=0
      noacc_ff(2)=0
      noacc_ff(3)=0

c   electron beam energy as used for photoproduction (Tagger info)
      call ffkey('BEAM',Beam_energy_ff,6,'REAL')
      Beam_energy_ff(1)=0.0
      Beam_energy_ff(2)=0.0

c   beam position and emittance (assuming no deflection, i.e. parallel to z-axis)
      call ffkey('POSBEAM',Beam_offset_ff,2,'REAL')
      call ffkey('SIGBEAM',Beam_sigma_ff,1,'REAL')
      Beam_offset_ff(1)=0.0
      Beam_offset_ff(2)=0.0
      Beam_sigma_ff=0.0

c   default ikine=0 (call guser_kine)
c   pkine for kine_bogdan (kine 3) are:  id,pmin,pmax,thetamin,thetamax,phimin,phimax
c                             defaults:  electron, 0.8<p<3.5, 30<theta<40, phi=+-10 degrees

      ikine=0
      call ffkey('PKINE',pkine, 10,'REAL')
      pkine(1)=3
      pkine(2)=0.8
      pkine(3)=3.5
      pkine(4)=30.
      pkine(5)=40.
      pkine(6)=-10.
      pkine(7)=10.
      pkine(8)=0.
      pkine(9)=0.
      pkine(10)=0.


c  detector specific card definitions
      call ffky_mg
      call ffky_ptg
      call ffky_tg
      call ffky_cc
      call ffky_dc
      call ffky_ec
      call ffky_ec1
      call ffky_sc
      call ffky_st
      call ffky_bckgr
      call ffky_foil
      call ffky_tg_e2
      call ffky_tg_g7 
      call ffky_ic
      call ffky_dvcs
      return
      end

c------------------------------------------------------------------------










