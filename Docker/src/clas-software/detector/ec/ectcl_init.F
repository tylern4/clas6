      SUBROUTINE ECTCL_INIT
c
c_begin_doc
c  RCS ID string
c  $Id: ectcl_init.F,v 1.7 2006/06/15 14:46:27 stepanya Exp $
c
c  Documentation for subroutine ECTCL_INIT
c
c  Purpose:  initialize runtime control variables for EGN package
c  --------  put your default vaules in ec_set_def.F
c
c  Input Parameters: none
c  ----------------
c
c  Output Parameters: none
c  -----------------
c
c  Other routines:
c  ---------------
c
c  Notes:
c  ------
c
c  Author:   Arne Freyberger      Created:  Thu Nov 30 16:32:25 EST 1995
c  -------
c
c  Major revisions:
c  ----------------
c     
c
c_end_doc
c
      IMPLICIT NONE
      SAVE
c
c_begin_inc
c  include files :
c  ---------------------
c BOS common block  uncomment the next line for BOS include file
#include "ec_control.inc"
c_end_inc
c
c_begin_var
c  input/output variables:
c  -----------------------
c
c  Local pre-defined variables:
c  ---------------------------
c  RCS information: 
      CHARACTER*132  CFILE, CREVIS, CSTATE, CDATE, CAUTHO, CRCSID
      PARAMETER (CFILE = '$RCSfile: ectcl_init.F,v $')
      PARAMETER (CREVIS = '$Revision: 1.7 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 2006/06/15 14:46:27 $')
      PARAMETER (CAUTHO = '$Author: stepanya $')
      DATA CRCSID/   
     1'$Id: ectcl_init.F,v 1.7 2006/06/15 14:46:27 stepanya Exp $'   
     2/   
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='ECTCL_INIT')
      PARAMETER (CRAUTH='Arne Freyberger')
c
c  Local User defined variables:
c  -----------------------------
       integer i_integer, i_double
       data i_integer, i_double/1,2/
c_end_var
c
c  executable code for routine ECTCL_INIT:
c----6----------------------------------------------------------------72
c
c  initialize default tcl variables
       call ec_set_def
c  now link the FORTRAN variables to tcl variables
*
*- minimum threshold for strips to be significant
*
      call tcl_linkvar('EcFitEdge_strip_threshold',
     1      EcFitEdge_strip_threshold,i_double)
*
*- minimum threshold for peaks to be significant
*
      call tcl_linkvar('EcFitEdge_peak_threshold',
     1      EcFitEdge_peak_threshold,i_double)
*
*- minimum threshold for hits to be significant
*
      call tcl_linkvar('EcFitEdge_hit_threshold',
     1      EcFitEdge_hit_threshold,i_double)
*
*- use or don't use fix attenuation lengths
*
      call tcl_linkvar('def_atten',
     1      def_atten,i_integer)
*
*- use or not default values for ADC constants
*
      call tcl_linkvar('def_adc',
     1      def_ADC,i_integer)
*
*- use or not default values for TDC constants
*
      call tcl_linkvar('def_tdc',
     1      def_TDC,i_integer)
*
*- use or not ideal geometry settings
*
      call tcl_linkvar('def_geom',
     1      def_geom,i_integer)
*
*- use or not right way of ec time calculation 
*
      call tcl_linkvar('time_def',
     1      time_def,i_integer)
*
*- use or not logarithmic weights hit position determination. 
*
      call tcl_linkvar('ec_ln_weights',
     1      ec_ln_weights,i_integer)
*
*- Define planes for Whole, Inner and Outer parts.
*
*        *_surf = Layer (can be part of the layer)                  
*  
*
      call tcl_linkvar('whole_surf',
     1      whole_surf,i_double)
      call tcl_linkvar('inner_surf',
     1      inner_surf,i_double)
      call tcl_linkvar('outer_surf',
     1      outer_surf,i_double)
*
* Do the analyses by:
*                    1 - standart way EcFit_strips_peaks.F
*                                     EcFit_peaks_hits.F
*                    2 - correct attenuation corrections
*                        efsp.F and efph.F
*
      call tcl_linkvar('anal_opt',
     1      anal_opt,i_integer)
*
* Defines scheme of grouping the strips into the peaks
*
*
*
      call tcl_linkvar('touch_id',
     1      touch_id,i_integer)
*
*
      call tcl_linkvar('maxecstrips',
     1      MaxEcStrips,i_integer)
*
*
      call tcl_linkvar('maxpcalstrips',
     1      MaxPcalStrips,i_integer)
*
*
*- Control matching of EC hits.
*
      call tcl_linkvar('ec_match',
     1      ec_match,i_double)
      RETURN
      END
c
c------------------------------------------------------------------------------




