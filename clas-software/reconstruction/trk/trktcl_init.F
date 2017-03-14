      subroutine trktcl_init()
*_begin_doc
*  RCS ID string
*  $Id: trktcl_init.F,v 1.20 2004/12/08 15:26:40 avakian Exp $
*
*  Documentation for subroutine TRK_INIT
*
*  Purpose:  This routine will allow RecSis to connect sda tuneable
*  --------  parameters to tcl variables. It is called once at the
*            start of the job. Note that tcl only talks to double
*            precision variables. As such, all "real" variables 
*            that need to be tuned have to be copied from the
*            double precision to the real in the trk_evnt()
*            routine. This is done via a call to trktcl_evnt().
*                     ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^changed Nov.97
*  Nov.97    all tcl variables for Tracking are described in 'trktcl_var.inc':
*            prefix:  ipar_trk_...  for INTEGER  variables
*                     dpar_trk_...  for DOUBLE   variables
*                     spar_trk_...  for STRING   variables
*
*  Input Parameters: NONE
*  ----------------
*
*  Output Parameters: NONE
*  -----------------
*
*  Other routines:
*  ---------------
*
*  Notes:
*  ------
*
*  Author:         Curtis A. Meyer 
*  -------         Carnegie Mellon University
*
*  Creation Date:  03 June, 1996
*  --------------
*
*  Major revisions: 27 March 1997 -- Curtis A. Meyer
*  ---------------- I have added four new tcl variables to allow the
*                   user to control the name and location of both the
*                   prlink and bgrid files.
*         
*                   1 April 1997 -- Curtis A. Meyer
*                   Control the name and location of the minitorus
*                   field maps through tcl variables.
*
*                   1 April 1997 -- Curtis A. Meyer
*                   Control the torus and minitorus fields through
*                   tcl variables:
*
*		    24 July 1997 -- Joseph J. Manak
*                   Changed Torus and MiniTorus default currents to -9999
*		    to enable the map database.
*
*                   Aug.97 -- F.Klein   some more options!
*                   Nov.97 -- F.Klein   remove obsolete common block 'sdakeys.inc'
*                                       (& change all tcl variable names)
*                   14 Jan 1998 -- J. J. Manak
*                   Moved defaults to trk_set_def.F, torus current and prlink
*                   filename now only controled by the map database
*
*                   20 Jan 1999 -- C. Marchand
*                   Moved defaults for region1 tcl's to trk_reg1_set_def.F

*  tcl-variable     fortran-v  purpose
*  ------------     ---------  -------
*  bfield_file_env  bfloc      environment variable to bfield location.
*                              Default: CLAS_PARMS
*  bfield_file_name bfname     Bfield file name.
*                              Default: bgrid_t.fpk
*  mfield_file_env  bmloc      environment variable to mini-field location.
*                              Default: CLAS_PARMS
*  mfield_file_name bmname     Mini field file name.
*                              Default: bgrid_m.fpk
*  prlink_file_env  prloc      Environment for prlink file location
*                              Default: CLAS_PARMS
*  prlink_file_name prname     File name for the prlink file.
*                              Default: ' ' (determined wrt. magnetic field setting -> sda_dinit.F  (fklein))
*_end_doc
*
      Implicit None
      Save
*
*
*_begin_inc
*  include files :
*  ---------------------
*
#include "trktcl_var.inc"
*
*_end_inc
*
*_begin_var
*  input/output variables:
*  -----------------------
*
*  Local pre-defined variables:
*  ---------------------------
*  RCS information: 
      CHARACTER*132  CFILE, CREVIS, CSTATE, CDATE, CAUTHO, CRCSID
      PARAMETER ( CFILE = '$RCSfile: trktcl_init.F,v $')
      PARAMETER ( CREVIS = '$Revision: 1.20 $')
      PARAMETER ( CSTATE = '$State: Exp $')
      PARAMETER ( CDATE = '$Date: 2004/12/08 15:26:40 $')
      PARAMETER ( CAUTHO = '$Author: avakian $')
      DATA CRCSID/
     1'$Id: trktcl_init.F,v 1.20 2004/12/08 15:26:40 avakian Exp $
     3cmeyer'/
*  Module information:
      CHARACTER*132  CRNAME, CRAUTH
      CHARACTER*132  CRMESS
      PARAMETER (CRNAME='TRKTCL_INIT')
      PARAMETER (CRAUTH='Curtis A. Meyer')
*
*  Local User defined variables:
*  -----------------------------
*
      Character*32  my_string
*  not used:
*      Integer         Lenocc
*      External        Lenocc
*
*        itcl_int: Tell tcl the current variable is type integer
*        itcl_dbl: Tell tcl the current variable is type double.
*        itcl_str: Tell tcl the current variable is type string.
*
      Integer itcl_int,itcl_dbl,itcl_str,lstring
      Save    itcl_int,itcl_dbl,itcl_str,lstring
      Data    itcl_int / 1 /
      Data    itcl_dbl / 2 /
      Data    itcl_str / 4 /
      Data    lstring  / 128 /
*
      Integer  i     ! Loop Index
*
c ----------------------------------------------------------------------------
c Set up reasonable "default" values for the tcl control variables 
      call trk_set_def()
      call trk_reg1_set_def()

      Call Tcl_LinkChar('bfield_file_name',spar_torus_bfield_name,itcl_str,lstring)
      Call Tcl_LinkChar('bfield_file_env' ,spar_torus_bfield_loc,itcl_str,lstring)
      Call Tcl_LinkChar('mfield_file_name',spar_minitorus_bfield_name,itcl_str,lstring)
      Call Tcl_LinkChar('mfield_file_env' ,spar_minitorus_bfield_loc,itcl_str,lstring)
      Call Tcl_LinkChar('poltarget_field_file_name',spar_poltarget_bfield_name,itcl_str,lstring)
      Call Tcl_LinkChar('poltarget_field_file_env' ,spar_poltarget_bfield_loc,itcl_str,lstring)
      Call Tcl_LinkChar('prlink_file_name',spar_prlink_name,itcl_str,lstring)
      Call Tcl_LinkChar('prlink_file_env' ,spar_prlink_loc,itcl_str,lstring)
      do i=1,9
        write(my_string,'(A,I1,A)')'bfield_grid(',i,')'
        call Tcl_Linkvar(my_string(1:14),dpar_bfield_grid(i),itcl_dbl)
      enddo
      call Tcl_Linkvar('bfield_grid(10)',dpar_bfield_grid(10),itcl_dbl)
      do i=1,3
        write(my_string,'(A,I1,A)')'TargetPos(',i,')'
        call Tcl_Linkvar(my_string(1:12),dpar_TargetPos(i),itcl_dbl)
      enddo
c
      do i=1,3
        write(my_string,'(A,I1,A)')'TargetMagPos(',i,')'
        call Tcl_Linkvar(my_string(1:15),dpar_TargetMagPos(i),itcl_dbl)
      enddo
      do i=1,3
        write(my_string,'(A,I1,A)')'TargetMagAng(',i,')'
        call Tcl_Linkvar(my_string(1:15),dpar_TargetMagAng(i),itcl_dbl)
      enddo
c
      call Tcl_Linkvar('TargetLen',   dpar_TargetLen,itcl_dbl)
      call Tcl_Linkvar('SigmaBeam',   dpar_SigmaBeam,itcl_dbl)
      call Tcl_Linkvar('BeamOff(1)',  dpar_BeamOff(1),itcl_dbl)
      call Tcl_Linkvar('BeamOff(2)',  dpar_BeamOff(2),itcl_dbl)
      call Tcl_Linkvar('BeamAngle(1)',dpar_BeamAngle(1),itcl_dbl)
      call Tcl_Linkvar('BeamAngle(2)',dpar_BeamAngle(2),itcl_dbl)
      call Tcl_Linkvar('trk_prfit_chi2',   dpar_trk_prfit_chi2,itcl_dbl)
      call Tcl_Linkvar('trk_lrambfit_chi2',dpar_trk_lrambfit_chi2,itcl_dbl)
      call Tcl_Linkvar('trk_tbtfit_chi2',  dpar_trk_tbtfit_chi2,itcl_dbl)
      call Tcl_Linkvar('trk_vx2fit_chi2',  dpar_trk_vx2fit_chi2,itcl_dbl)
      call Tcl_Linkvar('trk_vxfit_chi2',   dpar_trk_vxfit_chi2,itcl_dbl)
      call Tcl_Linkvar('trk_zstep(1)',     dpar_trk_zstep(1),itcl_dbl)
      call Tcl_Linkvar('trk_zstep(2)',     dpar_trk_zstep(2),itcl_dbl)
      call Tcl_Linkvar('trk_zangle',       dpar_trk_zangle,itcl_dbl)
      call Tcl_Linkvar('trk_Chi2diff',     dpar_trk_Chi2diff,itcl_dbl)
      call Tcl_Linkvar('trk_SCerr_scale',  dpar_trk_SCerr_scale,itcl_dbl)
      call Tcl_LinkVar('trk_magtyp',       ipar_trk_magtyp,itcl_int)
      call Tcl_LinkVar('trk_bgridtyp',     ipar_trk_bgridtyp,itcl_int)
C
C Now read ONLY from the map - if the map is wrong - fix it!
      call Tcl_LinkVar('mini_torus_current',ipar_minitorus_current,itcl_int)
      call Tcl_LinkVar('torus_current',    ipar_torus_current,itcl_int)
      call Tcl_LinkVar('poltarget_current',ipar_poltarget_current,itcl_int)
C
C derived from torus and minitorus currents
C      call Tcl_LinkVar('Bsign_torus',   ipar_Bsign_torus,itcl_int)
C
      call Tcl_LinkVar('trk_statistics',ipar_trk_statistics,itcl_int)
      call Tcl_LinkVar('trk_level',     ipar_trk_level,itcl_int)
      call Tcl_LinkVar('trk_sector',    ipar_trk_sector,itcl_int)
      call Tcl_LinkVar('trk_mintracks', ipar_trk_mintracks,itcl_int)
      do i=1,6
        write(my_string,'(A,I1,A)') 'trk_minhits(',i,')'
        call Tcl_LinkVar(my_string(1:14),ipar_trk_minhits(i),itcl_int)
      enddo
      call Tcl_LinkVar('trk_maxsegm',    ipar_trk_maxsegm,itcl_int)
      call Tcl_LinkVar('trk_minlinks',   ipar_trk_minlinks,itcl_int)
      call Tcl_LinkVar('trk_startlink',  ipar_trk_startlink,itcl_int)
      call Tcl_LinkVar('trk_minlramb',   ipar_trk_minlramb,itcl_int)
      call Tcl_LinkVar('trk_electron_id',ipar_trk_electron_id,itcl_int)
      call Tcl_LinkVar('trk_make_prlink',ipar_trk_make_prlink,itcl_int)
      call Tcl_LinkVar('trk_prlink_param',ipar_trk_prlink_param,itcl_int)
      call Tcl_LinkVar('trk_multscat',   ipar_trk_multscat,itcl_int)
      call Tcl_LinkVar('trk_maxiter',    ipar_trk_maxiter,itcl_int)
      call Tcl_LinkVar('trk_VXconstr',   ipar_trk_VXconstr,itcl_int)
      call Tcl_LinkVar('trk_SCconstr',   ipar_trk_SCconstr,itcl_int)
      call Tcl_LinkVar('trk_fitRegion',  ipar_trk_fitRegion,itcl_int)
      call Tcl_LinkVar('trk_calcTM',     ipar_trk_calcTM,itcl_int)
      do i=1,8
        write(my_string,'(A,I1,A)') 'trk_print(',i,')'
        call Tcl_LinkVar(my_string(1:12),ipar_trk_print(i),itcl_int)
      enddo
      call Tcl_LinkVar('trk_histo',  ipar_trk_histo,itcl_int)
      call Tcl_LinkVar('trk_ntuple', ipar_trk_ntuple,itcl_int)

      call Tcl_LinkVar('trk_region1',ipar_trk_region1,itcl_int)
      call Tcl_LinkVar('trk_lramb',ipar_trk_lramb,itcl_int)
      call Tcl_LinkVar('ntswitch',ipar_ntswitch,itcl_int)

      call Tcl_LinkVar('reg1_use_dhcl',ipar_reg1_use_dhcl,itcl_int)
      call Tcl_LinkVar('reg1_min',ipar_reg1_min,itcl_int)
      call Tcl_LinkVar('reg1_max',ipar_reg1_max,itcl_int)

      call RecMes(CRNAME,'I',' tcl parameter initialized for Tracking')

c  Now take care of the magnet currents and prlink file

      End



