c      include file EC1_CALIBR
c
c  COMMON BLOCK NAME should be the same as include file name!
c
c_begin_var
c  variables for include file EC1_CALIBR
c  -----------------------------------
c  
c  Variables contained in EC1_CALIBR common block
c  --------------------------------------------
c
c_end_var
c
       REAL ec1_cala1l(40,4,2),ec1_calt1l(40,4,2),ec1_cala1r(40,4,2),ec1_calt1r(40,4,2)
       REAL ec1_cala2l(40,4,2),ec1_calt2l(40,4,2),ec1_cala2r(40,4,2),ec1_calt2r(40,4,2)
       INTEGER ec1_calupdt
       COMMON/EC1_CALIBR/ec1_calupdt
     1,ec1_cala1l,ec1_calt1l,ec1_cala2l,ec1_calt2l
     1,ec1_cala1r,ec1_calt1r,ec1_cala2r,ec1_calt2r
c
c      ec1 calibration data
c----6----------------------------------------------------------------72























