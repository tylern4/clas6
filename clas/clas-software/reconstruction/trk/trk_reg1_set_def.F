      subroutine trk_reg1_set_def()
*_begin_doc
*  RCS ID string
*  $Id: trk_reg1_set_def.F,v 1.1 1999/01/20 20:46:00 marchand Exp $
*
*  Documentation for subroutine TRK_REG1_SET_DEF
*
*  Purpose: sets specific tcl variables for independent segment finder reg1
*           !!!!! still relies on trktcl_var.inc and trktcl_init.F
*
*  Author:         C.Marchand
*  -------         Saclay
*
*  Creation Date:  20 Jan, 1999
*  --------------
*
*  tcl-variable   fortran-v           purpose
*  ------------   ---------           -------
*
*  reg1_use_dhcl  ipar_reg1_use_dhcl  if 1, uses DC0 and DHCL and 
*                                     reconstructs only segments not
*                                     already associated with a TBT track;
*                                     if 0, reconstructs all segments from DC0
*  reg1_min       ipar_reg1_min       lowest numbered region where to perform
*  reg1_max       ipar_reg1_max       highest numbered region where to perform
*                                     (e.g. min=1, max=2 will reconstruct 
*
*                                     segments in both region1 and region2)
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
      PARAMETER ( CFILE = '$RCSfile: trk_reg1_set_def.F,v $')
      PARAMETER ( CREVIS = '$Revision: 1.1 $')
      PARAMETER ( CSTATE = '$State: Exp $')
      PARAMETER ( CDATE = '$Date: 1999/01/20 20:46:00 $')
      PARAMETER ( CAUTHO = '$Author: marchand $')
      DATA CRCSID/
     1'$Id: trk_reg1_set_def.F,v 1.1 1999/01/20 20:46:00 marchand Exp $
     3cmeyer'/
*  Module information:
      CHARACTER*132  CRNAME, CRAUTH
      CHARACTER*132  CRMESS
      PARAMETER (CRNAME='TRKTCL_INIT')
      PARAMETER (CRAUTH='Joe Manak')
*
*  Local User defined variables:
*  -----------------------------
*
c ----------------------------------------------------------------------------
c Set up reasonable "default" values for the tcl control variables 


      ipar_reg1_use_dhcl=-1     ! use DHCL to construct clusters for region1

      ipar_reg1_min=1           ! min region

      ipar_reg1_max=2           ! max region

      End



