c      include file EGN_CONTROL
c
c_begin_doc
c  RCS ID string
c  $Id: ec_control.inc,v 1.6 2008/03/16 04:33:14 fklein Exp $
c
c  Documentation for include file EGN_CONTROL
c
c  Purpose: holds the egn control variables
c  --------
c
c  Used in routines:
c  ----------------
c
c  Notes:
c  ------
c
c  Author:   Arne Freyberger      Created:  Tue Nov 21 10:11:31 EST 1995
c  -------
c
c  Major revisions:
c  ----------------
c
c_end_doc
c
c
c  Remember NO NESTED INCLUDES
c
c  COMMON BLOCK NAME should be the same as include file name!
c
c_begin_var
c  variables for include file EGN_CONTROL:
c  -----------------------------------
c  RCS information: 
      CHARACTER*(132)  CIEGN_CONTROL
      DATA CIEGN_CONTROL/
     1'$rcsId$'
     2/
c----6----------------------------------------------------------------72
c  
c  Variables contained in EGN_CONTROL common block
c  --------------------------------------------
c
c_end_var
c
       REAL*8 EcFitEdge_strip_threshold
       REAL*8 EcFitEdge_peak_threshold
       REAL*8 EcFitEdge_hit_threshold
       Logical def_atten, time_def
       INTEGER anal_opt
       INTEGER touch_id
       Logical def_geom
       Logical def_ADC
       Logical def_TDC
       Logical ec_ln_weights
       REAL*8 whole_surf
       REAL*8 inner_surf
       REAL*8 outer_surf
       REAL*8 cover_surf
       REAL*8 ec_match
       INTEGER MaxEcStrips
       INTEGER MaxPcalStrips

       COMMON/EC_CONTROL/EcFitEdge_strip_threshold,
     1      EcFitEdge_peak_threshold,EcFitEdge_hit_threshold,
     3      whole_surf,inner_surf,outer_surf,cover_surf,ec_match,
     4      anal_opt,touch_id,MaxEcStrips,MaxPcalStrips,
     5      def_geom,def_atten,time_def,def_ADC,def_TDC,ec_ln_weights 

c
c----6----------------------------------------------------------------72




