      SUBROUTINE DC_SET_DEF
c
c_begin_doc
c  RCS ID string
c  $Id: dc_set_def.F,v 1.14 2001/08/28 19:50:19 feuerbac Exp $
c
c  Documentation for subroutine DC_TCL_INIT
c
c  Purpose:
c  --------
c
c  Set default dc values in a separate file that can be called from other 
c  programs - manak
c
c  Input Parameters:  (Name - Type - Meaning)
c  ----------------
c
c   None
c
c  Output Parameters:  (Name - Type - Meaning)
c  -----------------
c
c  Other routines:
c  ---------------
c
c  Notes:
c  ------
c
c  Author: Manak     Created:  Fri Nov 15 09:48:36 EST 1996
c  -------
c
c  Major revisions:  Jan. 1998 - Created by Manak
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
#include "dc_tcl.inc"
#include "dc_cal_tdly.inc"
c_end_inc
c
c_begin_var
c  input/output variables:
c  -----------------------
      INTEGER	IRUN,IEVNT,IREC
c  Local pre-defined variables:
c  ---------------------------
c  RCS information: 
      CHARACTER*132  CFILE, CREVIS, CSTATE, CDATE, CAUTHO, CRCSID
      PARAMETER (CFILE = '$RCSfile: dc_set_def.F,v $')
      PARAMETER (CREVIS = '$Revision: 1.14 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 2001/08/28 19:50:19 $')
      PARAMETER (CAUTHO = '$Author: feuerbac $')
      DATA CRCSID/   
     1'$Id: dc_set_def.F,v 1.14 2001/08/28 19:50:19 feuerbac Exp $'   
     2/   
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='DC_SET_DEF')
      PARAMETER (CRAUTH=' ')
c
c  Local User defined variables:
c  -----------------------------
      INTEGER  j, i
c_end_var
c

      dc_Sigma_doca(1) = 0.03D0			!realistic params (fk)
      dc_Sigma_doca(2) = 0.03D0
      dc_Sigma_doca(3) = 0.035D0
      dc_Sigma_doca(4) = 0.035D0
      dc_Sigma_doca(5) = 0.04D0
      dc_Sigma_doca(6) = 0.04D0

      dc_efficiency = 0.98D0

c adjustment of time delays for common stop [nsec]  (i.e. betw.DC & SC/Trig)

      dc_trig_time=0.0D0

c adjustment of pulser delays for three regions [nsec]  

      dc_vel_prop=DC_V_signal
      do i=1,3
        dc_reg_pulser_time(i)=0.0D0
      enddo

c choose a function for the T-->D and D-->T algorithms
      dc_xvst_choice = 2         ! default linear with the slope hereafter

c choose the slope if choice = 0
      dc_xvst_default_slope = 0.005

c choose the power of t/tmax if choice = 1 (curve used)
      dc_xvst_n_power = 1.32
      dc_xvst_m_power = 7.50

c multipliers -- for alignment/calibration purposes
      dc_mult_Sigma(1) = 1.00D0		
      dc_mult_Sigma(2) = 1.00D0
      dc_mult_Sigma(3) = 1.00D0
      dc_mult_Sigma(4) = 1.00D0
      dc_mult_Sigma(5) = 1.00D0
      dc_mult_Sigma(6) = 1.00D0

      RETURN
      END
c
c------------------------------------------------------------------------------




