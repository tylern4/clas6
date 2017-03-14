      INTEGER MAXpar
      PARAMETER (MAXpar = 99)
      CHARACTER*10 pname(MAXpar)
      REAL start_value(MAXpar),step_size(MAXpar)
      REAL low_lim(MAXpar),up_lim(MAXpar)
      INTEGER pi_type, pi_type2fit, obsv2fit, ndf
      INTEGER par_stat(MAXpar),plist(MAXpar),parms
      INTEGER lmini,lmino,lminf,f_out      
      PARAMETER (f_out=49)
      
      DOUBLE PRECISION cM1,cE1,cS1					
      DOUBLE PRECISION cM2,cE2,cS2					
      DOUBLE PRECISION cM3,cE3,cS3					
      DOUBLE PRECISION cMP1,cSP1,cEP2,cSP2,cEP5,cSP5
      DOUBLE PRECISION cMP3,cEP3,cSP3
      DOUBLE PRECISION cMP4,cEP4,cSP4
      DOUBLE PRECISION cMP6,cEP6,cSP6
      DOUBLE PRECISION cMP7,cEP7,cSP7
      DOUBLE PRECISION cMP8,cEP8,cSP8
      DOUBLE PRECISION cMP9,cEP9
      DOUBLE PRECISION cMP10,cEP10
      DOUBLE PRECISION cMP11,cEP11
      DOUBLE PRECISION cSPA,a_pip
      REAL ChiMax,fix_param,tolorance, maxcalls,errorup

      COMMON/FIT_PAR/cM1,cE1,cS1,cM2,cE2,cS2,cM3,cE3,cS3,
     +cMP1,cSP1,cEP2,cSP2,cEP5,cSP5,cMP3,cEP3,cSP3,
     +cMP4,cEP4,cSP4,cMP6,cEP6,cSP6,cMP7,cEP7,cSP7,
     +cMP8,cEP8,cSP8,cMP9,cEP9,cMP10,cEP10,cMP11,cEP11,
     +cSPA,a_pip
     
      COMMON/FIT_PARC/ChiMax,fix_param,tolorance, maxcalls,
     +errorup,pi_type2fit,obsv2fit,pi_type,start_value,
     +step_size,low_lim,up_lim,par_stat,plist,parms,
     +lmini,lmino,lminf,ndf,pname




