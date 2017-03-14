c      include file DC_XVST
c
c_begin_doc
c  RCS ID string
c  $Id: dc_xvst.inc,v 1.5 1997/10/15 14:46:19 gilfoyle Exp $
c
c  Documentation for include file DC_XVST
c
c  Purpose:
c  --------
c  Contains variables for the drift-time --> drift-distance
c  algorithm.
c
c  Used in routines:
c  ----------------
c  dc_getcal_xvst
c  dc_dist_to_time
c  dc_time_to_dist
c
c  Notes:
c  ------
c  This include file also contains parameters that define the layers 
c  and number of wires in each layer.
c
c  Author:   Gerard Gilfoyle      Created:  Fri Jul 12 10:05:03 EDT 1996
c  -------
c
c  Major revisions:
c  ---------------
c_end_doc
c
c
c  Remember NO NESTED INCLUDES
c
c  COMMON BLOCK NAME should be the same as include file name!
c
c_begin_var
c  variables for include file DC_XVST:
c  -----------------------------------
c  RCS information: 
      CHARACTER*(132)  CIDC_XVST
      DATA CIDC_XVST/
     1'$Id: dc_xvst.inc,v 1.5 1997/10/15 14:46:19 gilfoyle Exp $'
     2/
c----6----------------------------------------------------------------72
c
c  Parameters for the drift-time --> drift-distance calibration
c  ------------------------------------------------------------
      integer*4 r1layers, r2layers, r3layers
      integer*4 r1columns, r2columns, r3columns
      integer*4 nsectors, nt_drift_bins
      integer*4 nx_drift_bins_r1, nx_drift_bins_r2, nx_drift_bins_r3
      integer*4 nlayers
c
      parameter (r1layers=12, r2layers=12, r3layers=12)
      parameter (r1columns=5, r2columns=14, r3columns=5)
      parameter (nsectors=6, nt_drift_bins=3000)
      parameter (nx_drift_bins_r1=165, nx_drift_bins_r2=300)
      parameter (nx_drift_bins_r3=460)
      parameter (nlayers=36)
c
c  
c  Variables contained in DC_XVST common block
c  -------------------------------------------
c These arrays hold the T->D parameters and the tables of T->D values.
      real*4    dc_xvst_params_r1(nsectors,r1layers,r1columns)
      real*4    dc_xvst_params_r2(nsectors,r2layers,r2columns)
      real*4    dc_xvst_params_r3(nsectors,r3layers,r3columns)
      real*4    dc_xvst_table_r1(nsectors,r1layers,nt_drift_bins)
      real*4    dc_xvst_table_r2(nsectors,r2layers,nt_drift_bins)
      real*4    dc_xvst_table_r3(nsectors,r3layers,nt_drift_bins)
      real*4    dc_tvsx_table_r1(nsectors,r1layers,nx_drift_bins_r1)
      real*4    dc_tvsx_table_r2(nsectors,r2layers,nx_drift_bins_r2)
      real*4    dc_tvsx_table_r3(nsectors,r3layers,nx_drift_bins_r3)
c
c array of parameters for default T->D function calibrated from data
c from the june, 1997 run. the numbers are 
c defined in the routine dc_getcal_xvst.F gpg 10/1/97
c
      real*4    dc_xvst_default(3,4)
c
c stick the array for the maximum drift distances in here. the numbers are 
c defined in the routine dc_getcal_xvst.F gpg 9/27/96
      real*4    dc_cellsize(nlayers)

c variables needed in the inversion of T->D algorithm.
      integer*4 dc_sector, dc_layer
      real*4    dc_pick_doca, dc_alpha, dc_alpha_max
      real*4    dc_x_drift_binsize
      parameter (dc_x_drift_binsize=0.0050, dc_alpha_max=0.523599)

c_end_var      
      COMMON/DC_XVST/ dc_xvst_params_r1, 
     1                dc_xvst_params_r2, 
     2                dc_xvst_params_r3,
     3                dc_xvst_table_r1, 
     4                dc_xvst_table_r2,
     5                dc_xvst_table_r3,
     6                dc_tvsx_table_r1,  
     7                dc_tvsx_table_r2,
     8                dc_tvsx_table_r3,   
     9                dc_cellsize,
     1                dc_xvst_default,
     2                dc_sector, dc_layer, dc_pick_doca, dc_alpha

      save /dc_xvst/
c----6----------------------------------------------------------------72




