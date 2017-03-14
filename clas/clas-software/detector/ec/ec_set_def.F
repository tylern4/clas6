      SUBROUTINE EC_SET_DEF
c
c_begin_doc
c  RCS ID string
c  $Id: ec_set_def.F,v 1.7 2007/08/27 17:25:45 stepanya Exp $
c
c  Documentation for subroutine EC_SET_DEF
c
c  Purpose:  initialize runtime control variables for EGN package
c  --------
c
c  Input Parameters: none
c  ----------------
c
c  Output Parameters: none
c  -----------------
c  sets to default values the structure in ec_control.inc
c
c  Other routines:
c  ---------------
c
c  Notes:
c  ------
c
c  Author:   Joe Manak      Created:  Thu Nov 30 16:32:25 EST 1995
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
#include "ec_control.inc"
c
c_begin_var
c  input/output variables:
c  -----------------------
c
c  Local pre-defined variables:
c  ---------------------------
c  RCS information: 
      CHARACTER*132  CFILE, CREVIS, CSTATE, CDATE, CAUTHO, CRCSID
      PARAMETER (CFILE = '$RCSfile: ec_set_def.F,v $')
      PARAMETER (CREVIS = '$Revision: 1.7 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 2007/08/27 17:25:45 $')
      PARAMETER (CAUTHO = '$Author: stepanya $')
      DATA CRCSID/   
     1'$Id: ec_set_def.F,v 1.7 2007/08/27 17:25:45 stepanya Exp $'   
     2/   
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='EC_SET_DEF')
      PARAMETER (CRAUTH='Joe Manak')
c_end_var
c
c  just initialize the FORTRAN EC control common
*
*- minimum threshold for strips to be significant
*
C should be mapmanaged - on my to do list - JM
      EcFitEdge_strip_threshold = 0.001
*
*- minimum threshold for peaks to be significant
*
C should be mapmanaged - on my to do list - JM
      EcFitEdge_peak_threshold = 0.003
*
*- minimum threshold for hits to be significant
*
C should be mapmanaged
      EcFitEdge_hit_threshold = 0.010
*
*- use or don't use fix attenuation lengths
*
      def_atten = .false. 
*
*- use or not default values for ADC constants
*
      def_ADC = .false. 
*
*- use or not default values for TDC constants
*
      def_TDC = .false. 
*
*- use or not ideal geometry settings
*
      def_geom = .true. 
*
*- use or not logarithmic weights hit position determination. 
*
      ec_ln_weights = .true. 
*
*- Define planes for Whole, Inner and Outer parts.
*
*        *_surf = Layer (can be part of the layer)                  
*  
*
      whole_surf = 16. 
      inner_surf = 1. 
      outer_surf = 16. 
      cover_surf = 1 
*
* Do the analyses by:
*                    1 - standard way EcFit_strips_peaks.F
*                                     EcFit_peaks_hits.F
*                    2 - correct attenuation corrections
*                        efsp.F and efph.F
*
      anal_opt = 2 
*
* Defines scheme of grouping the strips into the peaks
*
*
      touch_id = 0 
*
*
*- Control matching of EC hits.
*
      ec_match = 3.
*
      MaxEcStrips=36
*
      MaxPcalStrips=108
*
      time_def=.true.
      RETURN
      END
c
c------------------------------------------------------------------------------




