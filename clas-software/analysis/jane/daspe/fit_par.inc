c      include file FIT_PAR
c
c_begin_doc
c  RCS ID string
c  $Id$
c
c  Documentation for include file FIT_PAR
c
c  Purpose:
c  --------
c
c  Used in routines:
c  ----------------
c
c  Notes:
c  ------
c
c  Author:         Created:  Wed Dec 11 17:25:11 EST 1996
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
c  variables for include file FIT_PAR:
c  -----------------------------------
c  RCS information: 
      CHARACTER*(132)  CIFIT_PAR
      DATA CIFIT_PAR/
     1'$Id$'
     2/
c----6----------------------------------------------------------------72
c  
c  Variables contained in FIT_PAR common block
c  --------------------------------------------
c
c_end_var
      INTEGER MAXpar
      PARAMETER (MAXpar = 99)
      CHARACTER*10 pname(MAXpar)
      REAL start_value(MAXpar),step_size(MAXpar),low_lim(MAXpar),up_lim(MAXpar)
      INTEGER pi_type, pi_type2fit, obsv2fit
      INTEGER par_stat(MAXpar),plist(MAXpar),parms,lmini,lmino,lminf,f_out,ndf
      	DOUBLE PRECISION cM,cE,cS                                                      
	DOUBLE PRECISION ap11,sp11,as11,ss11,a1d13,a3d13,sd13                          
	DOUBLE PRECISION cas11,css11,cas31,css31,a1f15,a3f15,sf15                      
	DOUBLE PRECISION ca1d13,ca3d13,csd13,ca1d33,ca3d33,csd33                       
	DOUBLE PRECISION cap11,csp11,a1d15,a3d15,sd15                                  
	DOUBLE PRECISION a1p13,a3p13,sp13,a1p33,a3p33,sp33                             
	DOUBLE PRECISION cw1,cw2,cw3,cw4,cw5,cw6,cw7,cw8,cw9,cw10
      	DOUBLE PRECISION cw11,cw12,cw13,cw14,cw15
      REAL ChiMax,fix_param,tolorance, maxcalls, errorup
c
      COMMON/FIT_PAR/cM,cE,cS,
     +ap11,sp11,as11,ss11,a1d13,a3d13,sd13,
     +cas11,css11,cas31,css31,a1f15,a3f15,sf15,
     +ca1d13,ca3d13,csd13,ca1d33,ca3d33,csd33,
     +cap11,csp11,a1d15,a3d15,sd15,
     +a1p13,a3p13,sp13,a1p33,a3p33,sp33,
     +cw1,cw2,cw3,cw4,cw5,cw6,cw7,cw8,cw9,cw10,
     +cw11,cw12,cw13,cw14,cw15,
     +ChiMax,fix_param,tolorance, maxcalls,errorup,pi_type2fit,
     +obsv2fit,pi_type,
     +start_value,step_size,low_lim,up_lim,par_stat,plist,lmini,lmino,
     +lminf,f_out,parms,ndf,pname
c----6----------------------------------------------------------------72




