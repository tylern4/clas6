c      include file EC1_CONTROL
c
c  COMMON BLOCK NAME should be the same as include file name!
c
c_begin_var
c  variables for include file EC1_CONTROL:
c  -----------------------------------
c  
c  Variables contained in EC1_CONTROL common block
c  --------------------------------------------
c
c  Ec1_threshold 
c  ec1StEnMin_in 
c  ec1StEnMin_out 
c  Ec1_timeresol
c  ec1_caliba1 
c  ec1_calibt1 
c  ec1_caliba2 
c  ec1_calibt2
c  Ec1_reco_type 
c  ec1Maxclust 
c  Ec1_calib
c
c  Modified the order of the common block to eliminate
c  alignment problems. g.p.gilfoyle 7/7/97
c
c_end_var
c
       REAL*8 Ec1_threshold,ec1StEnMin_in,ec1StEnMin_out,Ec1_timeresol
       REAL ec1_caliba1,ec1_calibt1,ec1_caliba2,ec1_calibt2
       INTEGER Ec1_reco_type,ec1Maxclust,Ec1_calib
       COMMON/EC1_CONTROL/Ec1_threshold,ec1StEnMin_in,ec1StEnMin_out,
     1                            Ec1_timeresol,
     2                    ec1_caliba1,ec1_calibt1,ec1_caliba2,
     3                            ec1_calibt2,
     4                    Ec1_reco_type,ec1Maxclust,Ec1_calib
c
c----6----------------------------------------------------------------72























