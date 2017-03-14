c--------------------------------------------------------------------------
c  include file dc_xvst_ana.inc, Liming Qin
c  cm ns KG
c--------------------------------------------------------------------------
      real pi
      parameter (pi = 3.14159)

c--------------------------------------------------------------------------
      INTEGER MAXREGION
      INTEGER MAXSECTOR
      INTEGER MAXSUPER
      INTEGER MAXLAYER
      
      PARAMETER (MAXREGION = 3)
      PARAMETER (MAXSECTOR = 6)
      PARAMETER (MAXSUPER = 6)
      PARAMETER (MAXLAYER = 36)

c--------------------------------------------------------------------------
      integer nsectors
      integer r1layers, r2layers, r3layers
      integer n1_dtime_bins, n2_dtime_bins, n3_dtime_bins
      integer n_lang_bins,n_mag_bins
      real ns_per_bin
      real langle_value_hi, langle_value_lo
      real B_value_lo, B_value_del
      real B_angle_slope
      integer n_beta_bins,n_twtime_bins
      real twtime_lo
      integer n_rdoca_bins
      
      parameter (nsectors=6)
      parameter (r1layers=10, r2layers=12, r3layers=12)
      parameter (n1_dtime_bins=150,n2_dtime_bins=400,n3_dtime_bins=800)
      parameter (n_lang_bins=2, n_mag_bins=15)
      parameter (ns_per_bin=2.0)
      parameter (langle_value_lo=0.0,langle_value_hi=0.52360)
      parameter (B_value_lo=0.0,B_value_del=1.0)
      parameter (B_angle_slope=0.002)
      parameter (n_beta_bins=10,n_twtime_bins=450)
      parameter (twtime_lo=-50)
      parameter (n_rdoca_bins=50)
      
c--------------------------------------------------------------------------
c  tdc offsets for each superlayer
      real tdc_zero(MAXSECTOR,MAXSUPER)

c--------------------------------------------------------------------------
c  hex cell size of each superlayer
      real dc_hexsize(MAXLAYER)

c--------------------------------------------------------------------------
c  tables of t->x values 
      real dc_xvst_table_r1e(MAXSECTOR,r1layers,n1_dtime_bins,n_lang_bins)
      real dc_xvst_table_r2e(MAXSECTOR,r2layers,n2_dtime_bins,n_lang_bins,n_mag_bins)
      real dc_xvst_table_r3e(MAXSECTOR,r3layers,n3_dtime_bins,n_lang_bins)

c--------------------------------------------------------------------------
c  tables of tmax
      real tmax_r1e(MAXSECTOR,r1layers)
      real tmax_r2e(MAXSECTOR,r2layers,n_mag_bins)
      real tmax_r3e(MAXSECTOR,r3layers)

c--------------------------------------------------------------------------
c  tables for time walk correction
      real dc_tw_table(MAXSUPER,n_beta_bins,n_twtime_bins)

c--------------------------------------------------------------------------
c  tables for sigma (resolution) as function for reduced doca
      real dc_sigma_table(MAXSUPER,n_rdoca_bins)
      
      common /dc_xvst_parm/  
     1                    tdc_zero, dc_hexsize, 
     1                    dc_xvst_table_r1e,
     1                    dc_xvst_table_r2e,
     1                    dc_xvst_table_r3e,
     1                    tmax_r1e,
     1                    tmax_r2e,
     1                    tmax_r3e,
     1                    dc_tw_table,
     1                    dc_sigma_table
     

