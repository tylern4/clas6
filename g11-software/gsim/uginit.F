      subroutine uginit

c_begin_doc

c  Documentation for subroutine uginit

c  $Id: uginit.F,v 1.4 1999/03/24 20:30:04 fklein Exp $

c  Purpose:
c  --------
c     Geant standard initialization routine

c  Notes:
c  ------
c     Overrides default igauto=1, which lets Geant calculate tmed parameters,
c     and forces Geant to use GSTMED parameters instead (igauto=0)


c  Author:
c  -------
c     Elliott Wolin, College of William and Mary, 9-nov-94

c  Major revisions:
c  ----------------

c_end_doc

      implicit none
      character*(*) cfile,crevis,cstate,cdate,cautho,crname,crauth
      character*132 crcsid
      parameter (cfile=  '$RCSfile: uginit.F,v $')
      parameter (crevis= '$Revision: 1.4 $')
      parameter (cstate= '$State: Exp $')
      parameter (cdate=  '$Date: 1999/03/24 20:30:04 $')
      parameter (cautho= '$Author: fklein $')
      parameter (crname= 'UGINIT')
      parameter (crauth= 'Elliott Wolin')
      data crcsid/'
     1$Id: uginit.F,v 1.4 1999/03/24 20:30:04 fklein Exp $
     1 '/
      save


c  include files:
c  --------------
#include "gcnum.inc"
#include "gctrak.inc"
#include "gckine.inc"
#include "gcflag.inc"
#include "gsimpar.inc"
#include "gsimcontrol.inc"
c_end_inc


c  input/output variables:
c  -----------------------

c  local variables:
c  ----------------
      integer find_arg
c_end_var

c  external statements: (required to ensure proper linking)
c  --------------------
      external uglast

      external gudigi
      external gufld
      external gukine
      external guout
      external guskip
      external gustep
      external gutrak	
      external gutrev
c_end_ext


c  executable code:
c  ----------------

c  generate help message
      if((find_arg('h').gt.0).or.(find_arg('help').gt.0))then
         call gsim_help
         stop
      endif

      call recmes(crname,'I','*** Starting Geant Initialization ***')

c  initialize geant
      call ginit

c  override geant default IGAUTO...force geant to use GSTMED parameters
      igauto=0

c  allow more rotation matrices
      NROTM = 150

c  define user ffread data cards.
      call clas_ffky
      call guser_ffky

c  read data cards.
      call clas_ffgo

c  initialize Zebra data structures.
      call gzinit

c  load standard geant materials
      call gmate

c  load standard geant particles
      call gpart

c  define user particles
      call clas_part
      call guser_part

c  user initialization, mate and tmed definition, etc.
      call clas_init
      call guser_init

c  geometry definition...define user materials, mixtures, media, rotm, geom, sets, digi, etc.
      call clas_geom
      call guser_geom

c  close geometry definition
      call ggclos

c  compute cross section and energy loss tables.
      call gphysi

c  extra user physics initializations
      call clas_phys
      call guser_phys

c  book user histograms and other final user initializations
      call guser_book

c  last chance for user to modify geometry, dump geant parameters, etc. before run begins
      call guser_beg

c  check zebra bank structures
      call dzveri('UGINIT: check bank structure',0,'CLFSU')

c  write begin run record
      if(.not.nobosout(all))then
c  write input parameters into GPAR bos bank
         call gsimpar_2_bos
         call bos_head(17,idrun,0)     !class 17 is BOR, event number is 0
         call bos_out
      endif

c  dump summary of control parameters, etc.
      call beg_run_summary

      call recmes(crname,'I','*** Geant initialization done ***')

 999  return
      end

c---------------------------------------------------------------------------
