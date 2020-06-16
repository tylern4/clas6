c      include file mfm_control
c
c_begin_doc
c  RCS ID string
c  $Id: mfm_control.inc,v 1.1 1999/02/16 16:50:40 kossov Exp $
c
c  Documentation for include file MFM_CONTROL
c
c  Purpose: CONTROL KEYS for the MFM package (Magnetic Field Modified)
c  -------- 
c
c  Used in routines: MFM_TCL_INIT.F, MFM_BRUN
c  ----------------
c
c  Notes: Parameters MFM_BigTorus, MFM_MinTorus, MFM_PolTargt are temporary
c  ------ They should be extracted from the real run information
c
c  Author:   Mikhail Kossov      Created:  Fri May  9 10:29:00 EST 1997
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
c  variables for include file MFM_CONTROL:
c  ----------------------------------------
c  RCS information: 
      CHARACTER*(132)  CIMFM_CONTROL
      DATA CIMFM_CONTROL/
     & '$Id: mfm_control.inc,v 1.1 1999/02/16 16:50:40 kossov Exp $'/
c----6----------------------------------------------------------------72
c  
c  Variables contained in MFM_CONTROL
c  -----------------------------------
c
C--------------------------------------------------------
C MFM_MAG_type - 0 - read, 1 - create BigTor, 2 - cr. MinTor, 3 - cr. PolarTarg
C MFM_comp_fld =0:new,1:p.new,2:(new-old)/new(one of MFM_B,M,P=1),3:old,4:p.old
C MFM_BigTorus - portion of magnetic field of Big Torus 
C MFM_MinTorus - portion of magnetic field of Mini Torus 
C MFM_PolTargt - portion of magnetic field of Polarised Target 
C-----------------------------------------------------------------------
C
      REAL*8  MFM_BigTorus, MFM_MinTorus, MFM_PolTargt
C
      INTEGER MFM_MAG_type, MFM_comp_fld
C
      COMMON/MFM_CONTROL/
C - REAL*8 parameters
     &       MFM_BigTorus, MFM_MinTorus, MFM_PolTargt,
C - INTEGER parameters
     &       MFM_MAG_type, MFM_comp_fld
C
      SAVE  /MFM_CONTROL/
c
c_end_var
c
c----6----------------------------------------------------------------72




