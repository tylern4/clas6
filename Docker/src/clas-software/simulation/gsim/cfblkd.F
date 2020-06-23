      SUBROUTINE CFBLKD
CA)========================================(begin of short description)
C Block data for the default values of CF package
C fake statement type 'SUBROUTINE' in order to get the code linked
C  on all platforms
CB)-----------------------------------------(begin of long description)
C
C    Purpose and methods :
C
C    Called by:
C
C    Input arguments :  None
C
C    Input/output arguments :  None
C
C    Output arguments:  None
C
C    Side effects (other variables/banks modified, etc.)
C
C    Created:   20-APR-1994  Serguei Boiarinov
C    Modified:  30-JUN-1996  David Tedeschi
C
CC)-------------------------------------------(end of long description)
      implicit none
CD)----------------------------------(declare common blocks, variables)
C  Common blocks
#include "cfpara.inc"
#include "cflset.inc"
#include "cftmed.inc"
c
C===============================================(end of declarations)
C
C COMMON/CFPARA/
C
c shower paramertization control variables
c see cfking.F
c
c 0=no param, 1=func 1, 2=func 2
c elec,had shower
c
      DATA IPARAE,IPARAH
     >   /      2,     0 /      
c
c  note - energy values are in radiation lengths
c
c  hiecut -> specut(med) energy below which a particle is parameterized
c  hiect2 -> spect2(med) energy below which all secondaries are lumped together
c  shcute - shower cutoff energy (particle disappears)
c  shspoe - min energy deposit
c  stepX0 - step size in radiation lengths
c
      DATA HIECUT,HIECT2,SHCUTE,SHSPOE,STEPX0
     >   /    0.01,  0.005,  0.005,  0.01,   0.01 /
c
c values for parameterization function 1 or 2
c see cfemsh.F, cfstat.F
c
c reso   - resolution function linear parameter
c resoc  - resolution function constant
c scale  - 1.0 
c emuto  - 1.0
c sigthe - 0.6
c
      DATA RESO,RESOC,SCALE,EMUTO,SIGTHE
     >   / 0.09,  0.0,  -1.,  -1.,   -1. /
c
c parameters for hadronic parameterization
c
      DATA HIHCUT,HIHCT2,SHCUTH,SHSPOH,STEPL0
     >   /    0.0,  10.0,  0.05,  0.01,   0.5 /
C
      DATA RHSO,RHSOC,SCALH,PMUTO,SIGTHH
     >   / 0.50,  0.0,  -1.,  -1.,   -1. /
c
c
c
      DATA EINT / 0.1, 0.2, 0.3, 0.5, 0.7, 1.0, 1.5, 2.0 /
      DATA ALFE / 96*0.,
     >          1.405,1.534,1.494,1.667,1.907,1.949,2.285,2.352,
     >            152*0.,
     >          1.456,1.892,2.120,2.354,2.567,2.872,2.837,3.165,
     >            528*0./
      DATA BETE / 96*0.,
     >          2.000,1.253,0.8681,0.7114,0.7083,0.5922,0.6543,0.5877,
     >            152*0.,
     >          0.3816,0.4303,0.4150,0.4106,0.4442,0.4587,0.4100,0.4350,
     >            528*0./
      DATA CONE / 96*0.,
     >          0.1, 0.2, 0.3, 0.5, 0.7, 1.0, 1.5, 2.0,
     >            152*0.,
     >          0.1, 0.2, 0.3, 0.5, 0.7, 1.0, 1.5, 2.0,
     >            528*0./
      DATA ALFN / 96*0.,
     >          1.200,1.433,1.329,1.501,1.712,1.789,2.074,2.132,
     >            152*0.,
     >          1.378,1.709,2.028,2.226,2.474,2.713,2.661,3.030,
     >            528*0./
      DATA BETN / 96*0.,
     >          2.000,1.408,0.8812,0.7082,0.6891,0.5796,0.6232,0.5562,
     >            152*0.,
     >          0.3865,0.4166,0.4314,0.4145,0.4546,0.4586,0.4026,0.4349,
     >            528*0./
      DATA CONN / 96*0.,
     >          0.9900,1.896,2.975,5.263,7.607,10.78,16.36,22.19,
     >            152*0.,
     >          8.300,16.86,25.17,41.52,58.91,84.89,127.2,169.3,
     >            528*0./
C
c control flags for shower generation
c see cfemsh.F and cfhdsh.F
c
c  ltrne  - transverse fluctuations
c  lespot - nspot proportional to energy
c
C COMMON/CFLSET/
C
      DATA  LTRNE,  LESPOT,  LTRNH,  LHSPOT
     >   / .TRUE., .TRUE., .TRUE., .FALSE. /
C
c
c media control arrays initialized to -1
c 
c nsht  - number of media in cf list
c isht  - cf list
c enorm - energy multiplier 
c
C COMMON/CFTMED/
C
      DATA NSHT   / 0 /
      DATA ISHT   / 99*-1 /
      DATA SPECUT / 99*-1. /
      DATA SPECT2 / 99*-1. /
      DATA ENORM  / 99*-1. /
      DATA SPHCUT / 99*-1. /
      DATA SPHCT2 / 99*-1. /
      DATA HNORM  / 99*-1. /
C
      RETURN
      END
